%--Telyukova-calibration fork; original files retained one directory above--%
%--implementing VFI with money only and with distributions--%
clear all;
close all;
clc;

%--liquidity-demand shock: z = log(theta)------------------------------%
%--rho is held fixed at the value obtained from the 250 x 250 calibration.
S              = 7;                            %--Tauchen shock states------@
theta_log_mu   = 0;                            %--unconditional E[log theta]@
theta_log_sd   = 0.27136654798584364;           %--unconditional sd(log theta)
theta_width    = 3;                            %--grid spans +/- 3 std. devs.
theta_rho      = 0.656;                        %--fixed persistence---------@
theta_sigma    = theta_log_sd*sqrt(1-theta_rho^2);
c1_corr_target = 0.588505097651515;             %--diagnostic target---------@

[z,QQ] = tauchen(S,theta_log_mu,theta_rho,theta_sigma,theta_width);
theta  = exp(z);

bet   = 0.97;                                  %--discount factor-----------@
sig   = 2;                                     %--risk aversion-------------@
y     = 1;                                     %--endowment-----------------@
gama  = 0.02;                                  %--stationary inflation rate-@

K    = 250;                                    %--c2 grid points------------@
N    = 250;                                    %--money grid points---------@
mup  = 3.5;                                    %--upper money-grid bound----@
mgrid = linspace(0.1,mup,N)';

%--Final refinement around the money demand found in the bracket search.
msup_candidates = [0.5385;0.5388;0.5390;0.539037135;0.5391;0.5393;0.5395];

%--VFI and invariant-distribution tolerances----------------------------%
vfi_tol      = 1e-6;
vfi_maxiter  = 1000;
dist_tol     = 1e-12;
dist_maxiter = 50000;

%--Equilibrium-search storage-------------------------------------------%
n_msup             = numel(msup_candidates);
money_demand       = nan(n_msup,1);
current_money_mean = nan(n_msup,1);
money_gap          = nan(n_msup,1);
stationarity_gap   = nan(n_msup,1);
goods_gap          = nan(n_msup,1);
model_c1_corr      = nan(n_msup,1);
vfi_iterations     = nan(n_msup,1);
distribution_iters = nan(n_msup,1);

best_objective = Inf;
V_warm = ut(y,sig)*ones(N,S)/(1-bet);

fprintf('Searching for stationary real-money-supply equilibrium\n');

for j = 1:n_msup
    msup_j = msup_candidates(j);
    tau_j  = msup_j*gama/(1+gama);

    %--Changing msup changes tau, resources, feasibility, and c2grid.
    [c2grid_j,period_return_j,best_c2_j] = build_period_returns( ...
        mgrid,K,S,theta,gama,y,tau_j,sig);

    [V_j,idx_m_j,idx_c2_j,vfi_diff_j,vfi_iter_j] = solve_vfi( ...
        V_warm,QQ,period_return_j,best_c2_j, ...
        bet,vfi_tol,vfi_maxiter);

    if vfi_diff_j > vfi_tol
        error('VFI failed to converge for msup = %.8f',msup_j);
    end

    [m_star_j,c2_star_j,c1_star_j] = recover_policies( ...
        idx_m_j,idx_c2_j,mgrid,c2grid_j,gama,y,tau_j,S);

    assert(all(c1_star_j(:) >= -1e-10), ...
        'c1 negative for msup = %.8f',msup_j);
    assert(all(all(c1_star_j <= ...
        repmat(mgrid/(1+gama),1,S) + 1e-10)), ...
        'CIA violated for msup = %.8f',msup_j);

    [dist_j,dist_diff_j,dist_iter_j] = stationary_distribution( ...
        idx_m_j,QQ,dist_tol,dist_maxiter);

    if dist_diff_j > dist_tol
        error('Stationary distribution failed for msup = %.8f',msup_j);
    end

    [mprime_j,mcurrent_j,stationarity_j,goods_j] = equilibrium_moments( ...
        mgrid,m_star_j,c1_star_j,c2_star_j,dist_j,y,S);
    market_j = mprime_j-msup_j;
    corr_j = level_consumption_correlation(c1_star_j,idx_m_j,QQ,dist_j);
    obj_j = market_j^2;

    money_demand(j)       = mprime_j;
    current_money_mean(j) = mcurrent_j;
    money_gap(j)          = market_j;
    stationarity_gap(j)   = stationarity_j;
    goods_gap(j)          = goods_j;
    model_c1_corr(j)      = corr_j;
    vfi_iterations(j)     = vfi_iter_j;
    distribution_iters(j) = dist_iter_j;

    fprintf(['msup = %.9f, demand = %.9f, gap = %+.3e, ' ...
        'goods gap = %+.3e, c1 corr = %.6f, VFI iter = %d\n'], ...
        msup_j,mprime_j,market_j,goods_j,corr_j,vfi_iter_j);

    if obj_j < best_objective
        best_objective = obj_j;
        selected_index = j;
        msup            = msup_j;
        tau             = tau_j;
        c2grid          = c2grid_j;
        V0              = V_j;
        idx_m           = idx_m_j;
        idx_c2          = idx_c2_j;
        m_star          = m_star_j;
        c2_star         = c2_star_j;
        c1_star         = c1_star_j;
        dist            = dist_j;
    end

    %--Warm-start the next nearby candidate.
    V_warm = V_j;
end

equilibrium_results = table(msup_candidates,money_demand,money_gap, ...
    stationarity_gap,goods_gap,model_c1_corr,vfi_iterations,distribution_iters);
disp(equilibrium_results)

selected_money_demand = money_demand(selected_index);
selected_money_gap = money_gap(selected_index);
selected_goods_gap = goods_gap(selected_index);
selected_c1_corr = model_c1_corr(selected_index);

fprintf('\nSelected real money supply = %.9f\n',msup);
fprintf('Associated transfer = %.9f\n',tau);
fprintf('Aggregate money demand = %.9f\n',selected_money_demand);
fprintf('Money-market gap = %+.3e\n',selected_money_gap);
fprintf('Goods-market gap = %+.3e\n',selected_goods_gap);
fprintf('Consumption correlation = %.6f (target %.6f)\n', ...
    selected_c1_corr,c1_corr_target);

lower_money_mass = sum(dist(1,:));
upper_money_mass = sum(dist(end,:));
lower_c2_choice_mass = sum(dist(idx_c2 == 1));
upper_c2_choice_mass = sum(dist(idx_c2 == K));
fprintf('Mass at lower money-grid state = %.3e\n',lower_money_mass);
fprintf('Mass at upper money-grid state = %.3e\n',upper_money_mass);
fprintf('Mass choosing lower c2-grid point = %.3e\n',lower_c2_choice_mass);
fprintf('Mass choosing upper c2-grid point = %.3e\n',upper_c2_choice_mass);

%--Money-market search plot---------------------------------------------%
figure;
subplot(2,1,1);
plot(msup_candidates,money_demand,'bo-','LineWidth',1.5, ...
    'DisplayName','Money demand');
hold on;
plot(msup_candidates,msup_candidates,'k--','LineWidth',1.2, ...
    'DisplayName','45 degree line');
hold off;
xlabel('$m^s$','Interpreter','latex');
ylabel('$E_\Psi[m^\prime]$','Interpreter','latex');
title('Stationary money market');
legend('show','Location','best');
grid on;

subplot(2,1,2);
plot(msup_candidates,money_gap,'ro-','LineWidth',1.5);
hold on;
yline(0,'k--','LineWidth',1.2);
hold off;
xlabel('$m^s$','Interpreter','latex');
ylabel('$E_\Psi[m^\prime]-m^s$','Interpreter','latex');
title('Money-market residual');
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


%--stationary money distribution for the selected equilibrium----------%
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

function [c2grid,period_return,best_c2_given_mprime] = ...
    build_period_returns(mgrid,K,S,theta,gama,y,tau,sig)

    N = numel(mgrid);
    c2grid = linspace(0,y+tau,K)';
    period_return = -inf(N,N,S);
    best_c2_given_mprime = ones(N,N,S);

    resources = mgrid/(1+gama)+y+tau;
    mprime = mgrid';

    % Stream across c2 choices to avoid storing two N x N x K arrays.
    for k = 1:K
        c2 = c2grid(k);
        c1 = resources-c2-mprime;
        feasible = repmat(mprime >= y+tau-c2,N,1) & c1 >= 0;

        if c2 <= 0 || ~any(feasible(:))
            continue
        end

        u1_k = -inf(N,N);
        u1_k(feasible) = ut(c1(feasible),sig);
        u2_k = ut(c2,sig);

        for s = 1:S
            candidate = theta(s)*u1_k+u2_k;
            improve = candidate > period_return(:,:,s);
            current_best = period_return(:,:,s);
            current_index = best_c2_given_mprime(:,:,s);
            current_best(improve) = candidate(improve);
            current_index(improve) = k;
            period_return(:,:,s) = current_best;
            best_c2_given_mprime(:,:,s) = current_index;
        end
    end
end


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


function [mean_mprime,mean_m,stationarity_gap,goods_gap] = ...
    equilibrium_moments(mgrid,m_star,c1_star,c2_star,dist,y,S)

    current_m = repmat(mgrid,1,S);
    mean_mprime = sum(dist(:).*m_star(:));
    mean_m = sum(dist(:).*current_m(:));
    stationarity_gap = mean_mprime-mean_m;
    goods_gap = sum(dist(:).*(c1_star(:)+c2_star(:)))-y;
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
