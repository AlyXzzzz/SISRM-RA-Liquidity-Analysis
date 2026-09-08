function [mgrid, bgrid, m_star, b_star, c1_star, c2_star, theta, info] = ...
    solve_model_optimized(rho, sigma_theta, msup, options)
%SOLVE_MODEL_OPTIMIZED Memory-bounded cash-credit policy solve.
% Same asset grids, discrete c2 choices and economics as cashcredit02_unoptimized.
% Example: [m,b,mp,bp,c1,c2,theta,info] = ...
%     solve_model_optimized(rho,sigma_theta,msup,struct('N',200));
% options: N=100, K=20, tol=1e-6, maxiter=1000, howard_steps=50,
%          verbose=true, V0=[] (optional N-by-N-by-7 initial value).
% maxiter counts full Bellman searches. howard_steps=0 gives ordinary VFI.
% No N^4 or N^4*K tensors; working storage scales as O(N^2*S + N*K).

    if nargin < 4
        options = struct();
    end
    defaults = struct('N',100,'K',20,'tol',1e-6,'maxiter',1000, ...
        'howard_steps',50,'verbose',true,'V0',[]);
    names = fieldnames(options);
    for f = 1:numel(names)
        if ~isfield(defaults,names{f})
            error('cashcredit:Option','Unknown option: %s.',names{f});
        end
        defaults.(names{f}) = options.(names{f});
    end
    opts = defaults;
    validateattributes(rho,{'numeric'},{'real','finite','scalar','>',-1,'<',1});
    validateattributes(sigma_theta,{'numeric'},{'real','finite','scalar','positive'});
    validateattributes(msup,{'numeric'},{'real','finite','scalar','nonnegative'});
    validateattributes(opts.N,{'numeric'},{'scalar','integer','>=',2,'<=',65535});
    validateattributes(opts.K,{'numeric'},{'scalar','integer','>=',2,'<=',65535});
    validateattributes(opts.tol,{'numeric'},{'real','finite','scalar','positive'});
    validateattributes(opts.maxiter,{'numeric'},{'scalar','integer','positive'});
    validateattributes(opts.howard_steps,{'numeric'},{'scalar','integer','nonnegative'});

    started = tic;
    N = opts.N;
    K = opts.K;
    S = 7;
    bet = 0.97;
    sig = 2;
    y = 1;
    gama = 0.02;
    r = 0.04;
    tau = msup*gama/(1+gama);
    mgrid = linspace(0.1,3.5,N)';
    bgrid = linspace(0.1,3.5,N)';
    [z,QQ] = tauchen(S,0,rho,sigma_theta,3);
    theta = exp(z);
    cash = mgrid/(1+gama);
    income = y+tau+(1+r)*bgrid;
    c2grid = zeros(K,N);
    for j = 1:N
        c2grid(:,j) = linspace(0,income(j),K)';
    end

    % Identical uniform asset spacings imply n+q-1 identifies m(n)+b(q).
    % Use the first pair in original MATLAB column-major choice order as
    % the representative. Other pairs differ only by floating-point roundoff.
    L = 2*N-1;
    saving = [mgrid+bgrid(1); mgrid(N)+bgrid(2:end)]';
    q = (1:N)';
    n = (1:L)-q+1;
    action_lookup = n+N*(q-1);
    action_lookup(n < 1 | n > N) = N*N+1; % sentinel with value -Inf
    action_lookup = uint32(action_lookup);
    clear n q

    if isempty(opts.V0)
        V = ut(y,sig)*ones(N*N,S)/(1-bet);
    else
        validateattributes(opts.V0,{'double'},{'real','finite','size',[N,N,S]});
        V = reshape(opts.V0,N*N,S);
    end
    policy = ones(N*N,S,'uint32');
    idx_c2 = ones(N*N,S,'uint16');
    policy_reward = zeros(N*N,S);
    V_new = zeros(N*N,S);
    residual = Inf;
    evaluations = 0;
    bellman_seconds = 0;
    evaluation_seconds = 0;

    for iter = 1:opts.maxiter
        sweep_started = tic;
        EV = V*QQ';
        for s = 1:S
            % For each total saving, select the split with largest EV.
            % Rows are increasing q, preserving the original first-max rule.
            ev_s = [EV(:,s); -Inf];
            [best_ev,best_q] = max(ev_s(action_lookup),[],1);
            best_action = action_lookup(best_q+N*(0:L-1));
            [~,choice_order] = sort(best_action);
            for j = 1:N
                [reward,best_k] = consumption_returns( ...
                    cash,income(j),saving,c2grid(:,j),theta(s),sig);
                % Sorting ensures ties across saving levels also respect
                % the original order: n varies first, then q.
                W = reward(:,choice_order)+bet*best_ev(choice_order);
                [values,position] = max(W,[],2);
                if any(~isfinite(values))
                    error('cashcredit:Infeasible', ...
                        'No finite choice for some states; increase K or change grids.');
                end
                total_index = reshape(choice_order(position),[],1);
                selected = (1:N)'+N*(total_index-1);
                rows = (1:N)+N*(j-1);
                V_new(rows,s) = values;
                policy(rows,s) = best_action(total_index);
                idx_c2(rows,s) = best_k(selected);
                policy_reward(rows,s) = reward(selected);
            end
        end
        residual = max(abs(V_new(:)-V(:)));
        bellman_seconds = bellman_seconds+toc(sweep_started);
        if opts.verbose && (iter == 1 || mod(iter,10) == 0 || residual <= opts.tol)
            fprintf('Bellman search = %d, residual = %e\n',iter,residual);
        end
        % Only a full greedy Bellman residual can certify convergence.
        % Returned policies are greedy with respect to the returned V.
        if residual <= opts.tol
            break
        end
        V = V_new;
        if iter < opts.maxiter && opts.howard_steps > 0
            evaluation_started = tic;
            policy_linear = double(policy)+(0:S-1)*(N*N);
            for h = 1:opts.howard_steps
                EV = V*QQ';
                V = policy_reward+bet*EV(policy_linear);
            end
            evaluations = evaluations+opts.howard_steps;
            evaluation_seconds = evaluation_seconds+toc(evaluation_started);
        end
    end
    if residual > opts.tol
        error('cashcredit:Convergence', ...
            'VFI failed after %d Bellman searches (residual = %e).',iter,residual);
    end

    idx_m = mod(double(policy)-1,N)+1;
    idx_b = floor((double(policy)-1)/N)+1;
    m_star = reshape(mgrid(idx_m),N,N,S);
    b_star = reshape(bgrid(idx_b),N,N,S);
    c2_index = reshape(double(idx_c2),N,N,S)+(0:N-1)*K;
    c2_star = reshape(c2grid(c2_index),N,N,S);
    % Reconstruct the budget in the same arithmetic order as the original.
    c1_star = cash+y+tau+(1+r)*bgrid'-c2_star-m_star-b_star;
    assert(all(isfinite(c1_star(:))) && all(c1_star(:) > 0), ...
        'Nonpositive or nonfinite cash-good consumption.');
    assert(all(c2_star(:) > 0),'Nonpositive credit-good consumption.');
    slack = c1_star-cash;
    assert(all(slack(:) <= 1e-10),'CIA violated somewhere.');

    if nargout >= 8
        info = struct('N',N,'K',K,'S',S,'bellman_iterations',iter, ...
            'policy_evaluations',evaluations,'bellman_residual',residual, ...
            'value_error_bound',residual/(1-bet), ...
            'elapsed_seconds',toc(started),'bellman_seconds',bellman_seconds, ...
            'evaluation_seconds',evaluation_seconds, ...
            'V',reshape(V,N,N,S),'QQ',QQ, ...
            'idx_m',reshape(uint16(idx_m),N,N,S), ...
            'idx_b',reshape(uint16(idx_b),N,N,S), ...
            'idx_c2',reshape(idx_c2,N,N,S));
    end
    if opts.verbose
        fprintf('Converged: N=%d, K=%d, %.2f seconds, %d policy evaluations.\n', ...
            N,K,toc(started),evaluations);
    end
end

function [reward,best_k] = consumption_returns(cash,income,saving,c2grid,theta,sig)
% Utility is strictly concave in c2 for fixed total consumption C.
% The continuous optimum is C/(1+theta^(1/sig)), projected onto the CIA
% interval [max(0,income-saving), min(income,C)]. The best DISCRETE choice
% is an adjacent grid point. Include one extra point on each side to handle
% roundoff at grid/feasibility boundaries, then enforce original constraints.
% This retains the original K-point choice set; c2 is not made continuous.
    N = numel(cash);
    K = numel(c2grid);
    C = cash+income-saving;
    target = max(max(0,income-saving),min(min(income,C),C/(1+theta^(1/sig))));
    base_k = floor(target/(income/(K-1)))+1;
    base_k = min(K,max(1,base_k));
    reward = -Inf(size(C));
    best_k = ones(size(C),'uint16');
    for offset = -1:2
        k = min(K,max(1,base_k+offset));
        c2 = reshape(c2grid(k),N,[]);
        c1 = cash+income-c2-saving;
        feasible = saving >= income-c2 & saving <= cash+income-c2 ...
            & c1 > 0 & c2 > 0;
        candidate = theta*ut(max(c1,0),sig)+ut(c2,sig);
        improve = feasible & candidate > reward;
        reward(improve) = candidate(improve);
        best_k(improve) = uint16(k(improve));
    end
end
