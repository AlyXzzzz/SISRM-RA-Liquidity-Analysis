function results = validate_cashcredit02_optimized(run_large)
%VALIDATE_CASHCREDIT02_OPTIMIZED Compare to original tensors, then benchmark.
% Run validate_cashcredit02_optimized(false) for only the small-grid checks.
% Large-grid runs save policies and diagnostics in cashcredit02_optimized_results.
    if nargin < 1
        run_large = true;
    end
    folder = fileparts(mfilename('fullpath'));
    scratch = tempname;
    mkdir(scratch);
    addpath(scratch);
    cleanup = onCleanup(@() clean_reference(scratch));

    % Extract the untouched original local function. Only expose diagnostics
    % and grid/tolerance options; leave its objective and iterations intact.
    source = fileread(fullfile(folder,'cashcredit02_unoptimized.m'));
    first = strfind(source,'function [mgrid');
    source = source(first(1):end);
    source = strrep(source,'c2_star, theta]','c2_star, theta, reference]');
    source = strrep(source,'solve_model_unoptimized(rho, sigma_theta, msup)', ...
        'cashcredit02_reference_temp(rho, sigma_theta, msup, options)');
    source = strrep(source,'N = 20;','N = options.N;');
    source = strrep(source,'K = 20;','K = options.K;');
    source = strrep(source,'tol = 1e-6;','tol = options.tol;');
    source = strrep(source,'maxiter = 1000;','maxiter = 2000;');
    last = regexp(source,'end\s*$','start');
    source = [source(1:last-1), ...
        sprintf(['reference = struct(''V'',V0,''R_max'',R_max,' ...
        '''best_k'',best_k,''QQ'',QQ,''iterations'',iter);\nend\n'])];
    fid = fopen(fullfile(scratch,'cashcredit02_reference_temp.m'),'w');
    assert(fid ~= -1,'Cannot create temporary reference solver.');
    fwrite(fid,source);
    fclose(fid);

    params = [0.6768841969525026,0.577006677083328,0.9233706737453602];
    cases = [8,41,params; 12,31,0.4,0.3,1.1; 20,20,params];
    results = struct();
    for test = 1:size(cases,1)
        p = cases(test,3:5);
        opts = struct('N',cases(test,1),'K',cases(test,2),'tol',1e-8,'verbose',false);
        started = tic;
        [m,b,mp0,bp0,c10,c20,theta0,ref] = ...
            cashcredit02_reference_temp(p(1),p(2),p(3),opts);
        reference_seconds = toc(started);
        [m1,b1,mp,bp,c1,c2,theta,info] = ...
            solve_model_optimized(p(1),p(2),p(3),opts);
        assert(isequal(m,m1) && isequal(b,b1) && isequal(theta0,theta));
        value_difference = max(abs(ref.V(:)-info.V(:)));
        assert(value_difference <= 2*opts.tol/(1-0.97), ...
            'Values differ by more than the contraction error bounds.');
        [original_residual,policy_loss] = check_original_bellman( ...
            ref,m,b,mp,bp,c1,c2,theta,info);
        assert(original_residual <= opts.tol+1e-9, ...
            'Optimized value does not solve the original Bellman equation.');
        assert(policy_loss <= 1e-9, ...
            'A returned policy is suboptimal under the original operator.');
        policy_difference = max(abs([mp(:)-mp0(:);bp(:)-bp0(:); ...
            c1(:)-c10(:);c2(:)-c20(:)]));
        results.small(test) = struct('N',opts.N,'K',opts.K, ...
            'reference_seconds',reference_seconds,'optimized_seconds',info.elapsed_seconds, ...
            'value_difference',value_difference,'policy_difference',policy_difference, ...
            'original_bellman_residual',original_residual,'policy_loss',policy_loss);
        fprintf(['PASS N=%d K=%d: original %.3fs, optimized %.3fs, ' ...
            'value gap %.3g, policy gap %.3g, optimality loss %.3g\n'], ...
            opts.N,opts.K,reference_seconds,info.elapsed_seconds, ...
            value_difference,policy_difference,policy_loss);
    end

    % Ordinary VFI, accelerated VFI, and reuse of a converged warm start.
    opts = struct('N',8,'K',41,'tol',1e-8,'howard_steps',0,'verbose',false);
    [~,~,~,~,~,~,~,plain] = solve_model_optimized(params(1),params(2),params(3),opts);
    opts.howard_steps = 50;
    [~,~,~,~,~,~,~,fast] = solve_model_optimized(params(1),params(2),params(3),opts);
    assert(max(abs(plain.V(:)-fast.V(:))) <= 2*opts.tol/(1-0.97));
    opts.V0 = fast.V;
    [~,~,~,~,~,~,~,warm] = solve_model_optimized(params(1),params(2),params(3),opts);
    assert(warm.bellman_iterations == 1 && warm.bellman_residual <= opts.tol);
    fprintf('PASS ordinary VFI, Howard acceleration, and warm start.\n');
    try
        solve_model_optimized(params(1),params(2),params(3), ...
            struct('N',8,'K',2,'verbose',false));
        error('test:MissingError','Infeasible grid should have been rejected.');
    catch exception
        assert(strcmp(exception.identifier,'cashcredit:Infeasible'));
        fprintf('PASS infeasible-grid failure is explicit.\n');
    end

    if run_large
        output_folder = fullfile(folder,'cashcredit02_optimized_results');
        if ~isfolder(output_folder)
            mkdir(output_folder);
        end
        sizes = [100,200];
        for test = 1:numel(sizes)
            opts = struct('N',sizes(test),'K',20,'verbose',true);
            [mgrid,bgrid,m_star,b_star,c1_star,c2_star,theta,info] = ...
                solve_model_optimized(params(1),params(2),params(3),opts);
            assert(info.bellman_residual <= 1e-6);
            assert(isequal(size(m_star),[opts.N,opts.N,7]));
            sampled_policy_loss = check_large_states( ...
                mgrid,bgrid,m_star,b_star,c1_star,c2_star,theta,info,params(3));
            assert(sampled_policy_loss <= 1e-9, ...
                'Large-grid policies failed exhaustive sampled-state checks.');
            large_result = rmfield(info,{'V','QQ','idx_m','idx_b','idx_c2'});
            large_result.sampled_policy_loss = sampled_policy_loss;
            results.large(test) = large_result;
            save(fullfile(output_folder,sprintf('policies_N%d.mat',opts.N)), ...
                'mgrid','bgrid','m_star','b_star','c1_star','c2_star','theta','info','params');
        end
        save(fullfile(output_folder,'validation_results.mat'),'results');
        fid = fopen(fullfile(output_folder,'validation_results.json'),'w');
        fwrite(fid,jsonencode(results,PrettyPrint=true));
        fclose(fid);
    end
end

function loss = check_large_states(m,b,mp,bp,c1,c2,theta,info,msup)
% Exhaust ALL (m',b',c2) choices at 27 boundary/interior/shock states.
% This independently checks both choice reductions on the large grids.
    N = numel(m);
    K = info.K;
    EV = reshape(reshape(info.V,N*N,7)*info.QQ',N,N,7);
    saving = m+b';
    tau = msup*0.02/1.02;
    loss = 0;
    for s = [1,4,7]
        for j = [1,ceil(N/2),N]
            income = 1+tau+1.04*b(j);
            for i = [1,ceil(N/2),N]
                cash = m(i)/1.02;
                reward = -Inf(N,N);
                for credit = linspace(0,income,K)
                    consumption = cash+1+tau+1.04*b(j)-credit-m-b';
                    candidate = theta(s)*ut(consumption,2)+ut(credit,2);
                    candidate(saving < income-credit | ...
                        saving > cash+income-credit) = -Inf;
                    reward = max(reward,candidate);
                end
                objective = reward+0.97*EV(:,:,s);
                optimum = max(objective(:));
                n = double(info.idx_m(i,j,s));
                q = double(info.idx_b(i,j,s));
                assert(mp(i,j,s) == m(n) && bp(i,j,s) == b(q));
                actual = theta(s)*ut(c1(i,j,s),2)+ut(c2(i,j,s),2)+0.97*EV(n,q,s);
                loss = max(loss,abs(optimum-actual));
            end
        end
    end
    fprintf('PASS N=%d: 27 exhaustive sampled-state checks, loss %.3g.\n',N,loss);
end

function [residual,loss] = check_original_bellman(ref,m,b,mp,bp,c1,c2,theta,info)
    N = numel(m);
    S = numel(theta);
    EV = reshape(reshape(info.V,N*N,S)*ref.QQ',N,N,S);
    residual = 0;
    loss = 0;
    for s = 1:S
        for j = 1:N
            for i = 1:N
                objective = reshape(ref.R_max(i,j,:,:,s),N,N)+0.97*EV(:,:,s);
                optimum = max(objective(:));
                n = double(info.idx_m(i,j,s));
                q = double(info.idx_b(i,j,s));
                assert(mp(i,j,s) == m(n) && bp(i,j,s) == b(q));
                actual = theta(s)*ut(c1(i,j,s),2)+ut(c2(i,j,s),2)+0.97*EV(n,q,s);
                loss = max(loss,abs(optimum-actual));
                residual = max(residual,abs(optimum-info.V(i,j,s)));
            end
        end
    end
end

function clean_reference(scratch)
    rmpath(scratch);
    rmdir(scratch,'s');
end
