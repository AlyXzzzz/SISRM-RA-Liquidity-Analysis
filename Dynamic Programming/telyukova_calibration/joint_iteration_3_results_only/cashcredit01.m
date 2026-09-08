%--Telyukova-calibration fork; original files retained one directory above--%
%--implementing VFI with money only and with distributions--%
%--results-only fork: calibration diagnostic plots are omitted-------------%
clear all;
close all;
clc;

%--Joint iteration 3: correlation calibration and money equilibrium------%
S              = 7;
theta_log_mu   = 0;
theta_log_sd   = 0.27136654798584364;
theta_width    = 3;
c1_corr_target = 0.588505097651515;

bet   = 0.97;
sig   = 2;
y     = 1;
gama  = 0.02;

K    = 250;
N    = 250;
mup  = 3.5;
mgrid = linspace(0.1,mup,N)';

vfi_tol      = 1e-6;
vfi_maxiter  = 1000;
dist_tol     = 1e-12;
dist_maxiter = 50000;

%=========================================================================%
% Local functions
%=========================================================================%


% Function to maximize current period return grid
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


%========================================================================%
% Stage A: recalibrate rho at the previous money-supply equilibrium
%========================================================================%
msup_previous = 0.539416640;
tau_previous  = msup_previous*gama/(1+gama);
rho_candidates = (0.648:0.001:0.656)';

% Shock nodes stay fixed because unconditional log-shock volatility is fixed.
[z,~] = tauchen(S,theta_log_mu,0,theta_log_sd,theta_width);
theta = exp(z);

[c2grid_rho,period_return_rho,best_c2_rho] = build_period_returns( ...
    mgrid,K,S,theta,gama,y,tau_previous,sig);

n_rho            = numel(rho_candidates);
rho_model_corr    = nan(n_rho,1);
rho_corr_gap      = nan(n_rho,1);
rho_money_demand  = nan(n_rho,1);
rho_vfi_iters     = nan(n_rho,1);
rho_dist_iters    = nan(n_rho,1);
best_rho_objective = Inf;
V_warm = ut(y,sig)*ones(N,S)/(1-bet);

fprintf('Stage A: recalibrating rho at msup = %.9f\n',msup_previous);

for j = 1:n_rho
    rho_j = rho_candidates(j);
    sigma_j = theta_log_sd*sqrt(1-rho_j^2);
    [z_j,QQ_j] = tauchen(S,theta_log_mu,rho_j,sigma_j,theta_width);
    assert(max(abs(exp(z_j)-theta)) < 1e-10, ...
        'Tauchen shock nodes changed across rho candidates');

    [V_j,idx_m_j,idx_c2_j,vfi_diff_j,vfi_iter_j] = solve_vfi( ...
        V_warm,QQ_j,period_return_rho,best_c2_rho, ...
        bet,vfi_tol,vfi_maxiter);
    if vfi_diff_j > vfi_tol
        error('VFI failed to converge for rho = %.6f',rho_j);
    end

    [m_star_j,c2_star_j,c1_star_j] = recover_policies( ...
        idx_m_j,idx_c2_j,mgrid,c2grid_rho,gama,y,tau_previous,S);
    [dist_j,dist_diff_j,dist_iter_j] = stationary_distribution( ...
        idx_m_j,QQ_j,dist_tol,dist_maxiter);
    if dist_diff_j > dist_tol
        error('Stationary distribution failed for rho = %.6f',rho_j);
    end

    corr_j = level_consumption_correlation(c1_star_j,idx_m_j,QQ_j,dist_j);
    [demand_j,~,~,~] = equilibrium_moments( ...
        mgrid,m_star_j,c1_star_j,c2_star_j,dist_j,y,S);
    gap_j = corr_j-c1_corr_target;
    obj_j = gap_j^2;

    rho_model_corr(j)   = corr_j;
    rho_corr_gap(j)     = gap_j;
    rho_money_demand(j) = demand_j;
    rho_vfi_iters(j)    = vfi_iter_j;
    rho_dist_iters(j)   = dist_iter_j;

    fprintf(['rho = %.6f, sigma = %.6f, corr = %.6f, gap = %+.3e, ' ...
        'money demand = %.9f, VFI iter = %d\n'], ...
        rho_j,sigma_j,corr_j,gap_j,demand_j,vfi_iter_j);

    if obj_j < best_rho_objective
        best_rho_objective = obj_j;
        theta_rho   = rho_j;
        theta_sigma = sigma_j;
        z           = z_j;
        QQ          = QQ_j;
        V_rho       = V_j;
    end

    V_warm = V_j;
end

rho_results = table(rho_candidates,rho_model_corr,rho_corr_gap, ...
    rho_money_demand,rho_vfi_iters,rho_dist_iters);
disp(rho_results)
fprintf('Stage A selected rho = %.6f, sigma = %.6f\n', ...
    theta_rho,theta_sigma);

%========================================================================%
% Stage B: resolve money supply at the updated rho
%========================================================================%
money_tol = 1e-9;
money_maxiter = 10;
money_iter_msup    = nan(money_maxiter,1);
money_iter_demand  = nan(money_maxiter,1);
money_iter_gap     = nan(money_maxiter,1);
money_iter_goods   = nan(money_maxiter,1);
money_iter_corr    = nan(money_maxiter,1);
money_iter_vfi     = nan(money_maxiter,1);

msup_guess = msup_previous;
V_warm = V_rho;

fprintf('\nStage B: resolving money supply at rho = %.6f\n',theta_rho);

for q = 1:money_maxiter
    msup_j = msup_guess;
    tau_j = msup_j*gama/(1+gama);

    [c2grid_j,period_return_j,best_c2_j] = build_period_returns( ...
        mgrid,K,S,theta,gama,y,tau_j,sig);
    [V_j,idx_m_j,idx_c2_j,vfi_diff_j,vfi_iter_j] = solve_vfi( ...
        V_warm,QQ,period_return_j,best_c2_j, ...
        bet,vfi_tol,vfi_maxiter);
    if vfi_diff_j > vfi_tol
        error('VFI failed in money iteration %d',q);
    end

    [m_star_j,c2_star_j,c1_star_j] = recover_policies( ...
        idx_m_j,idx_c2_j,mgrid,c2grid_j,gama,y,tau_j,S);
    assert(all(c1_star_j(:) >= -1e-10), ...
        'c1 negative in money iteration %d',q);
    assert(all(all(c1_star_j <= ...
        repmat(mgrid/(1+gama),1,S) + 1e-10)), ...
        'CIA violated in money iteration %d',q);

    [dist_j,dist_diff_j,~] = stationary_distribution( ...
        idx_m_j,QQ,dist_tol,dist_maxiter);
    if dist_diff_j > dist_tol
        error('Stationary distribution failed in money iteration %d',q);
    end

    [demand_j,current_m_j,stationarity_j,goods_j] = equilibrium_moments( ...
        mgrid,m_star_j,c1_star_j,c2_star_j,dist_j,y,S);
    market_j = demand_j-msup_j;
    corr_j = level_consumption_correlation(c1_star_j,idx_m_j,QQ,dist_j);

    money_iter_msup(q)   = msup_j;
    money_iter_demand(q) = demand_j;
    money_iter_gap(q)    = market_j;
    money_iter_goods(q)  = goods_j;
    money_iter_corr(q)   = corr_j;
    money_iter_vfi(q)    = vfi_iter_j;

    fprintf(['iteration = %d, msup = %.9f, demand = %.9f, gap = %+.3e, ' ...
        'corr = %.6f, goods gap = %+.3e\n'], ...
        q,msup_j,demand_j,market_j,corr_j,goods_j);

    % Retain the model associated with the current equilibrium guess.
    msup          = msup_j;
    tau           = tau_j;
    c2grid        = c2grid_j;
    V0            = V_j;
    idx_m         = idx_m_j;
    idx_c2        = idx_c2_j;
    m_star        = m_star_j;
    c2_star       = c2_star_j;
    c1_star       = c1_star_j;
    dist          = dist_j;
    mean_m_current = current_m_j;
    selected_stationarity_gap = stationarity_j;

    if abs(market_j) <= money_tol
        break
    end

    % Direct fixed-point update: next supply guess equals current demand.
    msup_guess = demand_j;
    V_warm = V_j;
end

money_iter_msup    = money_iter_msup(1:q);
money_iter_demand  = money_iter_demand(1:q);
money_iter_gap     = money_iter_gap(1:q);
money_iter_goods   = money_iter_goods(1:q);
money_iter_corr    = money_iter_corr(1:q);
money_iter_vfi     = money_iter_vfi(1:q);

money_iteration_results = table((1:q)',money_iter_msup,money_iter_demand, ...
    money_iter_gap,money_iter_goods,money_iter_corr,money_iter_vfi, ...
    'VariableNames',{'iteration','msup','money_demand','money_gap', ...
    'goods_gap','c1_correlation','vfi_iterations'});
disp(money_iteration_results)

selected_money_demand = money_iter_demand(end);
selected_money_gap = money_iter_gap(end);
selected_goods_gap = money_iter_goods(end);
selected_c1_corr = money_iter_corr(end);
selected_corr_gap = selected_c1_corr-c1_corr_target;

fprintf('\nJoint-iteration-3 results\n');
fprintf('rho = %.6f\n',theta_rho);
fprintf('sigma = %.6f\n',theta_sigma);
fprintf('real money supply = %.9f\n',msup);
fprintf('money demand = %.9f\n',selected_money_demand);
fprintf('money gap = %+.3e\n',selected_money_gap);
fprintf('stationarity gap = %+.3e\n',selected_stationarity_gap);
fprintf('goods gap = %+.3e\n',selected_goods_gap);
fprintf('consumption correlation = %.6f\n',selected_c1_corr);
fprintf('correlation target = %.6f\n',c1_corr_target);
fprintf('correlation gap = %+.3e\n',selected_corr_gap);

lower_money_mass = sum(dist(1,:));
upper_money_mass = sum(dist(end,:));
lower_c2_choice_mass = sum(dist(idx_c2 == 1));
upper_c2_choice_mass = sum(dist(idx_c2 == K));
fprintf('Mass at lower money-grid state = %.3e\n',lower_money_mass);
fprintf('Mass at upper money-grid state = %.3e\n',upper_money_mass);
fprintf('Mass choosing lower c2-grid point = %.3e\n',lower_c2_choice_mass);
fprintf('Mass choosing upper c2-grid point = %.3e\n',upper_c2_choice_mass);

%--colors degrading from light to dark blue for low to high theta--%
colors = [linspace(0.85,0.05,S)', linspace(0.92,0.35,S)', linspace(1,0.85,S)'];

%--save result figures beside this script in a dedicated PDF folder------%
figure_dir = fullfile(fileparts(mfilename('fullpath')),'output','pdf');
if ~exist(figure_dir,'dir')
    mkdir(figure_dir);
end

policy_figure = figure('Color','w','Position',[100 100 800 1000]);

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
    'Interpreter', 'latex', 'FontSize', 14,'Color','k');

policy_axes = findall(policy_figure,'Type','axes');
set(policy_axes,'Color','w','XColor','k','YColor','k', ...
    'GridColor',[0.75 0.75 0.75]);
set(findall(policy_figure,'Type','text'),'Color','k');
set(findall(policy_figure,'Type','legend'),'Color','w','TextColor','k', ...
    'EdgeColor',[0.3 0.3 0.3]);

exportgraphics(policy_figure,fullfile(figure_dir,'policy_functions.pdf'), ...
    'ContentType','vector','BackgroundColor','white');


%--stationary money distribution for the selected equilibrium----------%
mu = sum(dist,2);

distribution_figure = figure('Color','w','Position',[100 100 900 520]);
%bar(mgrid,mu,1)
stem(mgrid,mu,'filled')
xlabel('Money')
ylabel('Probability mass')
grid on

set(gca,'Color','w','XColor','k','YColor','k', ...
    'GridColor',[0.75 0.75 0.75]);
set(findall(distribution_figure,'Type','text'),'Color','k');

exportgraphics(distribution_figure, ...
    fullfile(figure_dir,'stationary_money_distribution.pdf'), ...
    'ContentType','vector','BackgroundColor','white');

fprintf('Saved result plots to %s\n',figure_dir);
