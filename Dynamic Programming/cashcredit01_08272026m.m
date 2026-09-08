%--Implementing VFI with money only and with distributions--%
clear all;
close all;
clc;

%--Parameters for a single diagnostic model solve--%
rho = 0.6768841969525026;
sigma_theta = 0.577006677083328;
msup = 0.9233706737453602;

%--General-equilibrium output interface (disabled while editing policies)--%
% [scaled_residuals, residuals, params, mgrid, m_star, c1_star, ...
%     c2_star, dist, mu, theta] = solve_model(rho, sigma_theta, msup);
[mgrid, m_star, c1_star, c2_star, theta] = ...
    solve_model(rho, sigma_theta, msup);
S = size(c1_star,2);

%--Diagnostic plots--%
%--colors degrading from light to dark blue for low to high theta--%
colors = [linspace(0.85,0.05,S)', linspace(0.92,0.35,S)', ...
    linspace(1,0.85,S)'];

figure;

%--plot optimal m prime--%
subplot(3,1,1);
hold on;
for s = 1:S
    plot(mgrid, m_star(:,s), 'Color', colors(s,:), 'LineWidth', 1.5, ...
        'DisplayName', sprintf('\\theta = %.2f', theta(s)));
end
plot(mgrid, mgrid, 'k--', 'LineWidth', 1.2, ...
    'DisplayName', '45 degree line');
hold off;
title('Optimal $m^\prime$', 'Interpreter', 'latex', 'FontSize', 13);
xlabel('$m$', 'Interpreter', 'latex', 'FontSize', 12);
ylabel('$m^\prime$', 'Interpreter', 'latex', 'FontSize', 12);
legend('show', 'Location', 'northwest', 'FontSize', 8);
grid on;

%--plot optimal c1--%
subplot(3,1,2);
hold on;
for s = 1:S
    plot(mgrid, c1_star(:,s), 'Color', colors(s,:), 'LineWidth', 1.5, ...
        'DisplayName', sprintf('\\theta = %.2f', theta(s)));
end
hold off;
title('Optimal $c_1$ (cash good)', 'Interpreter', 'latex', 'FontSize', 13);
xlabel('$m$', 'Interpreter', 'latex', 'FontSize', 12);
ylabel('$c_1$', 'Interpreter', 'latex', 'FontSize', 12);
legend('show', 'Location', 'northwest', 'FontSize', 8);
grid on;

%--plot optimal c2--%
subplot(3,1,3);
hold on;
for s = 1:S
    plot(mgrid, c2_star(:,s), 'Color', colors(s,:), 'LineWidth', 1.5, ...
        'DisplayName', sprintf('\\theta = %.2f', theta(s)));
end
hold off;
title('Optimal $c_2$ (credit good)', 'Interpreter', 'latex', 'FontSize', 13);
xlabel('$m$', 'Interpreter', 'latex', 'FontSize', 12);
ylabel('$c_2$', 'Interpreter', 'latex', 'FontSize', 12);
legend('show', 'Location', 'northwest', 'FontSize', 8);
grid on;

%--overall title--%
sgtitle('Policy Functions: Cash-Credit CIA Model', ...
    'Interpreter', 'latex', 'FontSize', 14);

%--Stationary money-distribution plot (general equilibrium; disabled)--%
%{
figure;
stem(mgrid,mu,'filled');
xlabel('Money');
ylabel('Probability mass');
grid on;
%}

%--General-equilibrium output interface (disabled while editing policies)--%
% function [scaled_residuals, residuals, params, mgrid, m_star, c1_star, ...
%     c2_star, dist, mu, theta] = solve_model(rho, sigma_theta, msup)
function [mgrid, m_star, c1_star, c2_star, theta] = ...
solve_model(rho, sigma_theta, msup)

    S = 7;

    %--General-equilibrium moment targets (disabled)--%
    % c1_corr_target = 0.588505097651515;
    % log_c_var_target = 0.075127975874498015;

    %--Parameters--%
    bet   = 0.97;                              %--discount factor-------@
    sig   = 2;                                 %--risk aversion---------@
    y     = 1;                                 %--endowment-------------@
    gama  = 0.02;                              %--inflation rate--------@
    tau   = msup*gama/(1+gama);                %--real money transfers--@
    r     = 0.04;                              %--real interest rate----@ 
    
    K    = 100;                                %--number of points on the grid c2--@ 
    N    = 100;                                %--number of points on the grid-----@
    mup   = 3.5;                               %--upper value of the m grid--------@
    bup  = 3.5;                                %--upper value of the b grid--------@ 

    %--Compute transition matrix using tauchen method and 
    %--endogenous parameters--%
    [z,QQ] = tauchen(S,0,rho,sigma_theta,3);
    theta = exp(z);                            %--shocks----------------@

    %--Compute real money transfers--%
    tau     = msup*gama/(1+gama); 
    c2grid  = zeros(K,N);
    mgrid   = linspace(0.1,mup,N)';            %--grid for approximation-----------@
    bgrid   = linspace(0.1,bup,N)';            %--grid for approximation-----------@ 

    %--Credit-good grid conditional on the current bond holding b(j)--%
    for j = 1:N
        c2grid(:,j) = linspace(0,y+tau+(1+r)*bgrid(j),K)';
    end

    %--Construct current period returns before VFI loop--%
    % Dimensions: current m(i), current b(j), choice m'(n), choice b'(q),
    % and current shock s. The c2 choice k is maximized out block by block.
    R_max = -inf(N,N,N,N,S);
    best_k = zeros(N,N,N,N,S,'uint16');

    for j = 1:N
        %--One block contains every (m',b',c2) choice for current b(j)--%
        [mprime_block,bprime_block,c2_block] = ...
            ndgrid(mgrid,bgrid,c2grid(:,j));
        u2_block = ut(c2_block,sig);

        for i = 1:N
            %--c1 for every (m',b',c2) choice at current state (m(i),b(j))--%
            resources = mgrid(i)/(1+gama)+y+tau+(1+r)*bgrid(j);
            c1_block = resources-mprime_block-bprime_block-c2_block;

            %--CIA: 0 <= c1 <= current real money balances--%
            feasible = c1_block >= 0 & ...
                c1_block <= mgrid(i)/(1+gama);

            u1_block = -inf(N,N,K);
            u1_block(feasible) = ut(c1_block(feasible),sig);

            for s = 1:S
                %--Current return for all (m',b',c2) choices--%
                R_block = theta(s)*u1_block+u2_block;

                %--Maximize over c2 (dimension 3), leaving an (m',b') block--%
                [R_block_max,k_block] = max(R_block,[],3);

                R_max(i,j,:,:,s) = reshape(R_block_max,[1,1,N,N,1]);
                best_k(i,j,:,:,s) = reshape( ...
                    uint16(k_block),[1,1,N,N,1]);
            end
        end
    end
    
    %--VFI--%
    tol     = 1e-6;                                   %--tolerance----------------@
    diff    = Inf;                                    %--initialize distance------@
    iter    = 0;                                      %--iteration counter--------@
    maxiter = 1000;                                   %--maximum iterations-------@
    
    %--initial guess: value of consuming endowment forever--%
    V0      = ut(y,sig)*ones(N,N,S)/(1-bet);          %--initial guess------------@
    V_new   = zeros(N,N,S);                           %--storage for new V--------@
    idx_m   = zeros(N,N,S);                           %--index for m'-------------@
    idx_b   = zeros(N,N,S);                           %--index for b'-------------@
    idx_c2  = zeros(N,N,S);                           %--index for c2-------------@
    
    while diff > tol && iter < maxiter

        %--EV(n,q,s) = sum_{s'} V0(n,q,s')*QQ(s,s')--%
        EV = reshape(reshape(V0,N*N,S)*QQ',N,N,S);

        for s = 1:S
            continuation = bet*EV(:,:,s);             %--choices (m',b'): N x N--@

            for i = 1:N
                for j = 1:N
                    %--One choice block over (m',b') for current (m,b,theta)--%
                    current_returns = reshape( ...
                        R_max(i,j,:,:,s),[N,N]);
                    W_block = current_returns+continuation;

                    %--First maximize over m' for each possible b'--%
                    [value_by_bprime,best_n_by_bprime] = ...
                        max(W_block,[],1);

                    %--Then maximize over b'; together these maximize (m',b')--%
                    [V_new(i,j,s),best_q] = max(value_by_bprime);
                    best_n = best_n_by_bprime(best_q);

                    idx_m(i,j,s) = best_n;
                    idx_b(i,j,s) = best_q;
                    idx_c2(i,j,s) = double( ...
                        best_k(i,j,best_n,best_q,s));
                end
            end
        end
    
        %--check convergence--%
        diff = max(abs(V_new(:) - V0(:)));
        V0   = V_new;
        iter = iter + 1;
    
        if mod(iter,50) == 0
            fprintf('iter = %d, diff = %e\n', iter, diff);
        end
    end
    
    if diff > tol
        error('VFI failed to converge after %d iterations (diff = %e).', ...
            iter,diff);
    end
    fprintf('Convergence achieved in %d iterations\n', iter);
    
    %--recover policy functions--%
    m_star  = mgrid(idx_m);                          %--optimal m'---------------@
    c2_star = c2grid(idx_c2);                        %--optimal c2---------------@
    
    %--recover c1 residually--%
    c1_star = zeros(N,S);
    for s = 1:S
        for i = 1:N
            c1_star(i,s) = mgrid(i)/(1+gama)+y+tau-c2_star(i,s)-m_star(i,s);
        end
    end
    
    %--verify constraints--%
    assert(all(c1_star(:) >= -1e-10), 'c1 negative somewhere');
    assert(all(all(c1_star <= repmat(mgrid/(1+gama), 1, S) + 1e-10)), 'CIA violated somewhere');
    
    %--General-equilibrium distribution and moments (disabled)--%
    %{
    %--computing distributions
    
    [N,S] = size(idx_m);
    NS = N*S;
    n_entries = N*S*S;
    rows = zeros(n_entries,1);
    cols = zeros(n_entries,1);
    vals = zeros(n_entries,1);
    cursor = 0;
    
    for s = 1:S
        current_cols = (1:N)'+(s-1)*N;
        for ell = 1:S
            slots = cursor+(1:N);
            rows(slots) = idx_m(:,s)+(ell-1)*N;
            cols(slots) = current_cols;
            vals(slots) = QQ(s,ell);
            cursor = cursor+N;
        end
    end
    
    P = sparse(rows,cols,vals,NS,NS);
    
    %--psi initial guess--%
    psi = ones(NS,1)/NS;
    
    diff = Inf;
    iter = 0;
    
    %--Power iteration for stationary distribution--%
    
    while diff > tol && iter < maxiter
        psi_new = P*psi;
        psi_new = psi_new/sum(psi_new);
        diff = max(abs(psi_new-psi));
        psi = psi_new;
        iter = iter+1;
    end

    if diff > tol
        error(['Stationary distribution failed to converge after %d ' ...
            'iterations (diff = %e).'],iter,diff);
    end
    
    dist = reshape(psi,N,S);
    mu = sum(dist,2);
    
    %--Calculate liquid consumption Correlation Moment Difference--%
    
    mean_c = sum(dist(:).*c1_star(:));
    var_c = sum(dist(:).*(c1_star(:)-mean_c).^2);
    
    if var_c <= eps
        error('Cannot calculate consumption correlation: variance is zero.');
    end
    
    expected_product = 0;
    for s = 1:S
        next_c1 = c1_star(idx_m(:,s),:);
        expected_next_c1 = next_c1*QQ(s,:)';
        expected_product = expected_product + sum( ...
            dist(:,s).*c1_star(:,s).*expected_next_c1);
    end
    covariance = expected_product-mean_c^2;
    corr_c = covariance/var_c;
    corr_gap = c1_corr_target - corr_c;
    
    %--Calculate log liquid consumption variance moment difference--%
    
    positive_mass = dist > 0;
    log_c = zeros(size(c1_star));
    
    if any(c1_star(positive_mass) <= 0)
        error(['Positive stationary mass is assigned to nonpositive c1; ' ...
            'Var(log(c1)) is undefined.']);
    end
    
    log_c(positive_mass) = log(c1_star(positive_mass));
    mean_log_c = sum(dist(positive_mass).*log_c(positive_mass));
    variance_log_c = sum(dist(positive_mass).* ...
        (log_c(positive_mass)-mean_log_c).^2);
    log_c_var_gap = log_c_var_target - variance_log_c;
    
    %--Evaluate money market clearing--%
    
    mean_mprime = sum(dist(:).*m_star(:));
    msup_gap = msup - mean_mprime;
    
    params = [rho, sigma_theta, msup];
    residuals = [corr_gap; log_c_var_gap; msup_gap];
    
    %--Additionally scale residuals so they do not perturb results--%
    residual_scales = [
        c1_corr_target;
        log_c_var_target;
        msup  %--endogenous msup scaling--%
        ];

    scaled_residuals = residuals ./ residual_scales;
    %}
end
