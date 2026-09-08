%--S = 11 shock-grid robustness test--%
%--implementing VFI with money only and with distributions--%
clear all;
close all;
clc;


%--Parameters for the outer algorithm loop--%
outer_iter = 0;
obj_diff = Inf;
objective_change_tol = 1e-12;
residual_tol = 1e-3;
maxiter = 1000;

%--Initial Guess--%
rho = 0.6768841969525026;
sigma_theta = 0.577006677083328;
msup = 0.9233706737453602;

[scaled_residuals, residuals, params, mgrid, m_star, c1_star, c2_star, dist, mu, theta] = ...
    solve_model(rho, sigma_theta, msup);
residual_norm = norm(scaled_residuals,Inf);
S = size(c1_star,2);

%--Algorithm: Newton-Raphson Least squared residuals minimization--%
while residual_norm > residual_tol && outer_iter < maxiter

    %--Compute objective Q--%
    Q = 0.5*(scaled_residuals'*scaled_residuals);
    
    %--Compute the numerical Jacobian--%
    epsilon = 1e-3;
    
    F_rho_plus = solve_model(rho+epsilon, sigma_theta, msup);
    F_sigma_plus = solve_model(rho, sigma_theta+epsilon, msup);
    F_msup_plus = solve_model(rho, sigma_theta, msup+epsilon);
    
    J = zeros(3,3);
    J(:,1) = (F_rho_plus-scaled_residuals)/epsilon;
    J(:,2) = (F_sigma_plus-scaled_residuals)/epsilon;
    J(:,3) = (F_msup_plus-scaled_residuals)/epsilon;
    
    %--Compute next step parameter vector guess--%
    
    delta_x = -J \ scaled_residuals; %--Newton-Raphson--%
    x_new = [rho; sigma_theta; msup] + delta_x;
    if abs(x_new(1)) >= 1 || x_new(2) <= 0 || x_new(3) <= 0
        error('Parameter constraints violated. Adjust initial guesses or bounds.');
    end
    
    %--Solve model and compute new objective--%
    
    [scaled_residuals_new, residuals_new, params_new, ~, m_star_new, ...
        c1_star_new, c2_star_new, dist_new, mu_new, theta_new] = ...
        solve_model(x_new(1), x_new(2), x_new(3));
    Q_new = 0.5*(scaled_residuals_new'*scaled_residuals_new);

    %--Update parameters for the next iteration--%
    rho = x_new(1);
    sigma_theta = x_new(2);
    msup = x_new(3);
    scaled_residuals = scaled_residuals_new;
    residuals = residuals_new;
    params = params_new;
    m_star = m_star_new;
    c1_star = c1_star_new;
    c2_star = c2_star_new;
    dist = dist_new;
    mu = mu_new;
    theta = theta_new;
    obj_diff = (Q - Q_new)^2;
    residual_norm = norm(scaled_residuals,Inf);
    Q = Q_new;
    outer_iter = outer_iter + 1;

    if obj_diff <= objective_change_tol && residual_norm > residual_tol
        warning(['Outer algorithm stagnated before residual convergence: ' ...
            'objective change squared = %e, residual norm = %e.'], ...
            obj_diff,residual_norm);
        break
    end

end

if residual_norm <= residual_tol
    fprintf(['Outer algorithm converged in %d iterations ' ...
        '(residual norm = %e).\n'],outer_iter,residual_norm);
    disp(params);
elseif outer_iter >= maxiter
    warning(['Outer algorithm failed to converge after %d iterations ' ...
        '(residual norm = %e).'],outer_iter,residual_norm);
end

%--Plots--%
%--colors degrading from light to dark blue for low to high theta--%
colors = [linspace(0.85,0.05,S)', linspace(0.92,0.35,S)', linspace(1,0.85,S)'];

figure;

%--plot optimal m prime--%
subplot(3,1,1);
hold on;
for s = 1:S
    plot(mgrid, m_star(:,s), 'Color', colors(s,:), 'LineWidth', 1.5, ...
        'DisplayName', sprintf('\\theta = %.2f', theta(s)));
end
plot(mgrid, mgrid, 'k--', 'LineWidth', 1.2, 'DisplayName', '45 degree line');
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


figure;
%bar(mgrid,mu,1)
stem(mgrid,mu,'filled')
xlabel('Money')
ylabel('Probability mass')
grid on

function [scaled_residuals, residuals, params, mgrid, m_star, c1_star, c2_star, dist, mu, theta] = ...
solve_model(rho, sigma_theta, msup)

    S = 11;

    % Moment Matching Targets
    c1_corr_target = 0.588505097651515;
    log_c_var_target = 0.075127975874498015;

    %--Parameters--%
    bet   = 0.97;                              %--discount factor-------@
    sig   = 2;                                 %--risk aversion---------@
    y     = 1;                                 %--endowment-------------@
    gama  = 0.02;                              %--inflation rate--------@
    tau   = msup*gama/(1+gama);                %--real money transfers--@
    
    K    = 300;                                %--number of points on the grid c2--@ 
    N    = 300;                                %--number of points on the grid-----@
    mup   = 3.5;                               %--upper value of the grid----------@

    %--Compute transition matrix using tauchen method and 
    %--endogenous parameters--%
    [z,QQ] = tauchen(S,0,rho,sigma_theta,3);
    theta = exp(z);                            %--shocks----------------@

    %--Compute real money transfers--%
    tau   = msup*gama/(1+gama);                

    c2grid  = linspace(0,y+tau,K)';            %--grid for approximation c2--------@
    mgrid   = linspace(0.1,mup,N)';            %--grid for approximation-----------@
    
    u1 = zeros(N,N,K);
    u2 = zeros(N,N,K);
    
    for k = 1:K
        for i = 1:N
            for n = 1:N
                if mgrid(n) < y+tau-c2grid(k) || mgrid(n) > mgrid(i)/(1+gama)+y+tau-c2grid(k)
                    u1(i,n,k) = -inf;
                    u2(i,n,k) = -inf;
                else
                    u1(i,n,k) = ut(mgrid(i)/(1+gama)+y+tau-c2grid(k)-mgrid(n),sig);
                    u2(i,n,k) = ut(c2grid(k),sig);
                end
            end
        end
    end
    
    
    %--Construct current period returns before VFI loop--%
    R_max = zeros(N,N,S);
    best_k = zeros(N,N,S);
    
    for s = 1:S
    
        R_s = theta(s)*u1 + u2;     %--N x N x K--%
    
    %--maximize over k (c2), the third dimension--%
        [R_max(:,:,s), best_k(:,:,s)] = max(R_s, [], 3);
    
    end    
    
    
    %--VFI--%
    tol     = 1e-6;                                   %--tolerance----------------@
    diff    = Inf;                                    %--initialize distance------@
    iter    = 0;                                      %--iteration counter--------@
    maxiter = 1000;                                   %--maximum iterations-------@
    
    %--initial guess: value of consuming endowment forever--%
    V0      = ut(y,sig)*ones(N,S)/(1-bet);          %--initial guess------------@
    V_new   = zeros(N,S);                           %--storage for new V--------@
    idx_m   = zeros(N,S);                           %--index for m'-------------@
    idx_c2  = zeros(N,S);                           %--index for c2-------------@
    
    while diff > tol && iter < maxiter
    
        %--compute continuation value: N x S matrix--%
        %--EV(n,s) = sum_{s'} V0(n,s')*QQ(s,s')
        EV = V0*QQ';                                  %--N x S-----------------@
    
        for s = 1:S
            %--period utility for shock s:
            %--theta(s)*u1(i,n,k) + u2(i,n,k): N x N x K
            %--continuation: bet*EV(n,s): N x 1, needs broadcasting
    
            %--total return: N x N x K
            %--first dimension i: current state
            %--second dimension n: choice of m'
            %--third dimension k: choice of c2
    
            %--continuation value as a function of n only--%
            cont = bet*EV(:,s);                       %--N x 1-----------------@
    
            %--total value: N x N--%
            W = R_max(:,:,s) + cont';             %--N x N-------------@        
    
            %--maximize over n (dim 2) since k was already maximized over--%
            [V_new(:,s), best_n] = max(W,[],2); %--N x 1------------------@
    
            %--store indices--%
            idx_m(:,s)  = best_n;                     %--optimal m' index---------@
    
            %--recover optimal k for each i--%
            for i = 1:N
                idx_c2(i,s) = best_k(i, best_n(i),s);  %--optimal c2 index---------@
                % because best_k is now N x N x S in the first maximization, we
                % index additionally by s 
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
    
    params = [corr_c, variance_log_c, msup];
    residuals = [corr_gap; log_c_var_gap; msup_gap];
    
    %--Additionally scale residuals so they do not perturb results--%
    residual_scales = [
        c1_corr_target;
        log_c_var_target;
        msup  %--endogenous msup scaling--%
        ];

    scaled_residuals = residuals ./ residual_scales;
end
