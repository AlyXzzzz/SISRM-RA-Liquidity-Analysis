%--Telyukova-calibration fork; original files retained one directory above--%
%--implementing VFI with money only and with distributions--%
clear all;
close all;
clc;

%--liquidity-demand shock: z = log(theta)------------------------------%
%--The benchmark unconditional volatility is from the maintained 2018-2019
%--Telyukova-style estimate.  rho is calibrated below by matching the
%--benchmark adjacent-month LEVEL correlation of liquid consumption.
S              = 7;                            %--Tauchen shock states------@
theta_log_mu   = 0;                            %--unconditional E[log theta]@
theta_log_sd   = 0.27136654798584364;           %--unconditional sd(log theta)
theta_width    = 3;                            %--grid spans +/- 3 std. devs.
c1_corr_target = 0.588505097651515;             %--data Corr(c1_t,c1_t-1)---@

%--Final grid after earlier searches bracketed the target in [0.65,0.66].
rho_candidates = (0.650:0.001:0.660)';
bet   = 0.97;                              %--discount factor-------@
sig   = 2;                                 %--risk aversion---------@
y     = 1;                                 %--endowment-------------@
gama  = 0.02;                              %--inflation rate--------@
msup  = 1.127523876033966;
tau   = msup*gama/(1+gama);                %--real money transfers--@

K    = 250;                                %--number of points on the grid c2--@ 
N    = 250;                                %--number of points on the grid-----@
mup   = 3.5;                               %--upper value of the grid----------@

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

%--The unconditional shock variance is held fixed during calibration.
%--Therefore all rho candidates use the same z nodes; rho changes QQ.
[z,~] = tauchen(S,theta_log_mu,0,theta_log_sd,theta_width);
theta = exp(z);

%--Maximize over c2 once.  The continuation value depends on m' but not c2,
%--so this N x N x K maximization need not be repeated in every VFI iteration.
period_return = zeros(N,N,S);
best_c2_given_mprime = zeros(N,N,S);
for s = 1:S
    [period_return(:,:,s),best_c2_given_mprime(:,:,s)] = ...
        max(theta(s)*u1 + u2,[],3);
end
clear u1 u2

%--VFI and invariant-distribution tolerances----------------------------%
vfi_tol      = 1e-6;
vfi_maxiter  = 1000;
dist_tol     = 1e-12;
dist_maxiter = 50000;

%--Calibration storage--------------------------------------------------%
n_rho             = numel(rho_candidates);
model_c1_corr      = nan(n_rho,1);
correlation_gap    = nan(n_rho,1);
vfi_iterations     = nan(n_rho,1);
distribution_iters = nan(n_rho,1);

best_objective = Inf;
V_warm = ut(y,sig)*ones(N,S)/(1-bet);

fprintf('Calibrating rho to level-correlation target %.9f\n',c1_corr_target);

for j = 1:n_rho
    rho_j   = rho_candidates(j);
    sigma_j = theta_log_sd*sqrt(1-rho_j^2);
    [z_j,QQ_j] = tauchen(S,theta_log_mu,rho_j,sigma_j,theta_width);
    theta_j = exp(z_j);

    %--Holding unconditional volatility fixed should leave nodes unchanged.
    assert(max(abs(theta_j-theta)) < 1e-10, ...
        'Tauchen shock nodes changed across rho candidates');

    [V_j,idx_m_j,idx_c2_j,vfi_diff_j,vfi_iter_j] = solve_vfi( ...
        V_warm,QQ_j,period_return,best_c2_given_mprime, ...
        bet,vfi_tol,vfi_maxiter);

    if vfi_diff_j > vfi_tol
        error('VFI failed to converge for rho = %.4f',rho_j);
    end

    [m_star_j,c2_star_j,c1_star_j] = recover_policies( ...
        idx_m_j,idx_c2_j,mgrid,c2grid,gama,y,tau,S);

    assert(all(c1_star_j(:) >= -1e-10), ...
        'c1 negative for rho = %.4f',rho_j);
    assert(all(all(c1_star_j <= ...
        repmat(mgrid/(1+gama),1,S) + 1e-10)), ...
        'CIA violated for rho = %.4f',rho_j);

    [dist_j,dist_diff_j,dist_iter_j] = stationary_distribution( ...
        idx_m_j,QQ_j,dist_tol,dist_maxiter);

    if dist_diff_j > dist_tol
        error('Stationary distribution failed for rho = %.4f',rho_j);
    end

    corr_j = level_consumption_correlation(c1_star_j,idx_m_j,QQ_j,dist_j);
    gap_j  = corr_j-c1_corr_target;
    obj_j  = gap_j^2;

    model_c1_corr(j)      = corr_j;
    correlation_gap(j)    = gap_j;
    vfi_iterations(j)     = vfi_iter_j;
    distribution_iters(j) = dist_iter_j;

    fprintf(['rho = %.4f, sigma = %.6f, model corr = %.6f, ' ...
        'gap = %+.6f, VFI iter = %d\n'], ...
        rho_j,sigma_j,corr_j,gap_j,vfi_iter_j);

    if obj_j < best_objective
        best_objective = obj_j;
        theta_rho   = rho_j;
        theta_sigma = sigma_j;
        z           = z_j;
        QQ          = QQ_j;
        V0          = V_j;
        idx_m       = idx_m_j;
        idx_c2      = idx_c2_j;
        m_star      = m_star_j;
        c2_star     = c2_star_j;
        c1_star     = c1_star_j;
        dist        = dist_j;
    end

    %--A nearby converged value function is a faster starting guess.
    V_warm = V_j;
end

calibration_results = table(rho_candidates,model_c1_corr,correlation_gap, ...
    vfi_iterations,distribution_iters);
disp(calibration_results)

fprintf('\nSelected rho = %.6f\n',theta_rho);
fprintf('Implied innovation sigma = %.6f\n',theta_sigma);
fprintf('Target level correlation = %.6f\n',c1_corr_target);
selected_c1_corr = level_consumption_correlation(c1_star,idx_m,QQ,dist);
fprintf('Selected model correlation = %.6f\n',selected_c1_corr);
fprintf('Selected correlation gap = %+.6f\n', ...
    selected_c1_corr-c1_corr_target);

%--Calibration plot-----------------------------------------------------%
figure;
plot(rho_candidates,model_c1_corr,'bo-','LineWidth',1.5, ...
    'DisplayName','Model level correlation');
hold on;
yline(c1_corr_target,'k--','LineWidth',1.2, ...
    'DisplayName','Data target');
xline(theta_rho,'r:','LineWidth',1.2, ...
    'DisplayName','Selected rho');
hold off;
xlabel('$\rho$','Interpreter','latex');
ylabel('$\mathrm{Corr}(c_{1,t},c_{1,t-1})$','Interpreter','latex');
title('Liquidity-shock persistence calibration');
legend('show','Location','best');
grid on;



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


%--stationary money distribution for the selected rho------------------%
mu = sum(dist,2);

figure;
%bar(mgrid,mu,1)
stem(mgrid,mu,'filled')
xlabel('Money')
ylabel('Probability mass')
grid on


%=========================================================================%
% Local functions
%=========================================================================%

function [V,idx_m,idx_c2,diff,iter] = solve_vfi( ...
    V,QQ,period_return,best_c2_given_mprime,bet,tol,maxiter)

    [N,~,S] = size(period_return);
    V_new = zeros(N,S);
    idx_m = zeros(N,S);
    idx_c2 = zeros(N,S);
    diff = Inf;
    iter = 0;

    while diff > tol && iter < maxiter
        % EV(n,s) = sum_{s'} V(n,s')*QQ(s,s')
        EV = V*QQ';

        for s = 1:S
            % Continuation value varies over candidate m' (dimension 2).
            W_m = period_return(:,:,s) + bet*EV(:,s)';
            [V_new(:,s),best_n] = max(W_m,[],2);
            idx_m(:,s) = best_n;

            for i = 1:N
                idx_c2(i,s) = ...
                    best_c2_given_mprime(i,best_n(i),s);
            end
        end

        diff = max(abs(V_new(:)-V(:)));
        V = V_new;
        iter = iter+1;
    end
end


function [m_star,c2_star,c1_star] = recover_policies( ...
    idx_m,idx_c2,mgrid,c2grid,gama,y,tau,S)

    m_star = mgrid(idx_m);
    c2_star = c2grid(idx_c2);
    resources = repmat(mgrid/(1+gama)+y+tau,1,S);
    c1_star = resources-c2_star-m_star;
end


function [dist,diff,iter] = stationary_distribution( ...
    idx_m,QQ,tol,maxiter)

    [N,S] = size(idx_m);
    NS = N*S;
    n_entries = N*S*S;
    rows = zeros(n_entries,1);
    cols = zeros(n_entries,1);
    vals = zeros(n_entries,1);
    cursor = 0;

    % Column c is the current state; row r is the next state.
    for s = 1:S
        current_cols = (1:N)' + (s-1)*N;
        for ell = 1:S
            slots = cursor+(1:N);
            rows(slots) = idx_m(:,s) + (ell-1)*N;
            cols(slots) = current_cols;
            vals(slots) = QQ(s,ell);
            cursor = cursor+N;
        end
    end

    P = sparse(rows,cols,vals,NS,NS);
    psi = ones(NS,1)/NS;
    diff = Inf;
    iter = 0;

    while diff > tol && iter < maxiter
        psi_new = P*psi;
        psi_new = psi_new/sum(psi_new);
        diff = max(abs(psi_new-psi));
        psi = psi_new;
        iter = iter+1;
    end

    dist = reshape(psi,N,S);
end


function corr_c = level_consumption_correlation(c1,idx_m,QQ,dist)
    [~,S] = size(c1);
    mean_c = sum(dist(:).*c1(:));
    var_c = sum(dist(:).*(c1(:)-mean_c).^2);

    if var_c <= eps
        error('Cannot calculate consumption correlation: variance is zero');
    end

    expected_product = 0;
    for s = 1:S
        % For each current state (m_i,s), evaluate c1 tomorrow at the
        % policy-implied next money and integrate over next-period shocks.
        next_c1 = c1(idx_m(:,s),:);
        expected_next_c1 = next_c1*QQ(s,:)';
        expected_product = expected_product + sum( ...
            dist(:,s).*c1(:,s).*expected_next_c1);
    end

    covariance = expected_product-mean_c^2;
    corr_c = covariance/var_c;
end
