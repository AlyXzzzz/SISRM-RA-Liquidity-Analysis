%--Three-block calibration of the cash-credit CIA model-----------------%
% This script jointly determines:
%   1. rho, the persistence of log(theta), from the level correlation of c1;
%   2. sigma_theta, the innovation sd of log(theta), from Var(log(c1)); and
%   3. real money supply (and hence tau), from money-market clearing.
%
% The blocks are iterated automatically until the parameters and all three
% equation residuals converge.  Unlike the earlier calibration, the
% empirical consumption innovation variance is not imposed directly on the
% structural preference-shock process.

clearvars;
close all;
clc;

script_dir = fileparts(mfilename('fullpath'));
addpath(script_dir);

%=========================================================================%
% Empirical targets
%=========================================================================%

% Maintained 2018-2019 Telyukova-style regression estimates.  Under the
% stationary empirical residual process epsilon_t = alpha*epsilon_{t-1}+eta_t,
% Var(epsilon) = Var(eta)/(1-alpha^2).  Because the model has neither X_it nor
% household fixed effects, the model counterpart is Var(log(c1)).
empirical_alpha  = 0.1405246873167233;
empirical_eta_sd = 0.27136654798584364;
log_c_var_target = empirical_eta_sd^2/(1-empirical_alpha^2);

% Benchmark adjacent-month level correlation of liquid consumption.
c1_corr_target = 0.588505097651515;

%=========================================================================%
% Model and numerical configuration
%=========================================================================%

cfg.S            = 7;
cfg.theta_log_mu = 0;
cfg.theta_width  = 3;

cfg.bet  = 0.97;
cfg.sig  = 2;
cfg.y    = 1;
cfg.gama = 0.02;

cfg.K     = 250;
cfg.N     = 250;
cfg.mup   = 3.5;
cfg.mgrid = linspace(0.1,cfg.mup,cfg.N)';

cfg.vfi_tol      = 1e-6;
cfg.vfi_maxiter  = 1000;
cfg.dist_tol     = 1e-12;
cfg.dist_maxiter = 50000;

cfg.rho_points          = 9;
cfg.rho_lower           = 0.00;
cfg.rho_upper           = 0.98;
cfg.rho_half_width_init = 0.08;
cfg.rho_half_width_min  = 0.001;

cfg.sigma_points          = 9;
cfg.sigma_lower           = 0.01;
cfg.sigma_upper           = 1.50;
cfg.sigma_half_width_init = 0.25;
cfg.sigma_half_width_min  = 0.002;

cfg.search_shrink = 0.50;

cfg.money_tol      = 1e-9;
cfg.money_maxiter  = 50;
cfg.money_damping  = 1.00;
cfg.money_floor    = 1e-8;

cfg.outer_maxiter      = 15;
cfg.rho_change_tol     = 5e-4;
cfg.sigma_change_tol   = 5e-4;
cfg.msup_change_tol    = 1e-6;
cfg.corr_gap_tol       = 5e-4;
cfg.log_var_gap_tol    = 1e-4;
cfg.market_gap_tol     = cfg.money_tol;

%=========================================================================%
% Starting values
%=========================================================================%

rho = 0.652;

% The old variance mapping is used only to provide a starting value
sigma_theta = empirical_eta_sd*sqrt(1-rho^2);
msup        = 0.539416640;

rho_half_width   = cfg.rho_half_width_init;
sigma_half_width = cfg.sigma_half_width_init;

warm.V    = ut(cfg.y,cfg.sig)*ones(cfg.N,cfg.S)/(1-cfg.bet);
warm.dist = [];

outer_rho              = nan(cfg.outer_maxiter,1);
outer_sigma            = nan(cfg.outer_maxiter,1);
outer_msup             = nan(cfg.outer_maxiter,1);
outer_tau              = nan(cfg.outer_maxiter,1);
outer_model_corr       = nan(cfg.outer_maxiter,1);
outer_corr_gap         = nan(cfg.outer_maxiter,1);
outer_model_log_var    = nan(cfg.outer_maxiter,1);
outer_log_var_gap      = nan(cfg.outer_maxiter,1);
outer_money_demand     = nan(cfg.outer_maxiter,1);
outer_market_gap       = nan(cfg.outer_maxiter,1);
outer_stationarity_gap = nan(cfg.outer_maxiter,1);
outer_goods_gap        = nan(cfg.outer_maxiter,1);
outer_rho_change       = nan(cfg.outer_maxiter,1);
outer_sigma_change     = nan(cfg.outer_maxiter,1);
outer_msup_change      = nan(cfg.outer_maxiter,1);
outer_vfi_iterations   = nan(cfg.outer_maxiter,1);
outer_dist_iterations  = nan(cfg.outer_maxiter,1);
outer_converged        = false(cfg.outer_maxiter,1);

output_dir = fullfile(script_dir,'output','cashcredit01_fixed');
if ~exist(output_dir,'dir')
    mkdir(output_dir);
end
checkpoint_path = fullfile(output_dir,'cashcredit01_fixed_checkpoint.mat');

fprintf('Three-block liquidity-shock calibration\n');
fprintf('Correlation target       = %.9f\n',c1_corr_target);
fprintf('Var(log consumption) target = %.9f\n',log_c_var_target);
fprintf('Initial rho              = %.6f\n',rho);
fprintf('Initial sigma_theta      = %.6f\n',sigma_theta);
fprintf('Initial real money supply = %.9f\n\n',msup);

converged = false;

%=========================================================================%
% Automated outer joint-iteration loop
%=========================================================================%

for outer = 1:cfg.outer_maxiter
    rho_previous   = rho;
    sigma_previous = sigma_theta;
    msup_previous  = msup;

    fprintf('\n============================================================\n');
    fprintf('Outer joint iteration %d\n',outer);
    fprintf('============================================================\n');

    %---------------------------------------------------------------------%
    % Block 1: rho from the adjacent-period level-consumption correlation
    %---------------------------------------------------------------------%
    rho_candidates = bounded_grid(rho,rho_half_width,cfg.rho_points, ...
        cfg.rho_lower,cfg.rho_upper);
    n_rho_candidates = numel(rho_candidates);

    rho_model_corr  = nan(n_rho_candidates,1);
    rho_corr_gap    = nan(n_rho_candidates,1);
    rho_model_var   = nan(n_rho_candidates,1);
    rho_money_demand = nan(n_rho_candidates,1);
    rho_vfi_iters   = nan(n_rho_candidates,1);
    rho_dist_iters  = nan(n_rho_candidates,1);

    best_rho_objective = Inf;
    best_rho_index = 1;
    solution_rho = [];
    warm_candidate = warm;

    fprintf('\nBlock 1: calibrating rho at sigma = %.8f, msup = %.9f\n', ...
        sigma_theta,msup);

    for rho_index = 1:n_rho_candidates
        rho_candidate = rho_candidates(rho_index);
        candidate_solution = solve_model( ...
            rho_candidate,sigma_theta,msup,warm_candidate,cfg);
        candidate_gap = candidate_solution.c1_corr-c1_corr_target;
        candidate_objective = candidate_gap^2;

        rho_model_corr(rho_index)   = candidate_solution.c1_corr;
        rho_corr_gap(rho_index)     = candidate_gap;
        rho_model_var(rho_index)    = candidate_solution.log_c_var;
        rho_money_demand(rho_index) = candidate_solution.money_demand;
        rho_vfi_iters(rho_index)    = candidate_solution.vfi_iterations;
        rho_dist_iters(rho_index)   = candidate_solution.dist_iterations;

        fprintf(['rho = %.8f, corr = %.8f, gap = %+.3e, ' ...
            'Var(log c1) = %.8f, VFI iter = %d\n'], ...
            rho_candidate,candidate_solution.c1_corr,candidate_gap, ...
            candidate_solution.log_c_var, ...
            candidate_solution.vfi_iterations);

        if candidate_objective < best_rho_objective
            best_rho_objective = candidate_objective;
            best_rho_index = rho_index;
            solution_rho = candidate_solution;
        end

        warm_candidate.V = candidate_solution.V;
        warm_candidate.dist = candidate_solution.dist;
    end

    rho = rho_candidates(best_rho_index);
    rho_hit_edge = best_rho_index == 1 || ...
        best_rho_index == n_rho_candidates;

    rho_results = table(rho_candidates,rho_model_corr,rho_corr_gap, ...
        rho_model_var,rho_money_demand,rho_vfi_iters,rho_dist_iters, ...
        'VariableNames',{'rho','model_correlation','correlation_gap', ...
        'model_log_c_variance','money_demand','vfi_iterations', ...
        'distribution_iterations'});

    fprintf('Block 1 selected rho = %.8f, correlation gap = %+.3e\n', ...
        rho,solution_rho.c1_corr-c1_corr_target);
    disp(rho_results)
    warm.V = solution_rho.V;
    warm.dist = solution_rho.dist;

    if ~rho_hit_edge
        rho_half_width = max( ...
            cfg.rho_half_width_min,cfg.search_shrink*rho_half_width);
    end

    %---------------------------------------------------------------------%
    % Block 2: sigma_theta from stationary Var(log(c1))
    %---------------------------------------------------------------------%
    sigma_candidates = bounded_grid( ...
        sigma_theta,sigma_half_width,cfg.sigma_points, ...
        cfg.sigma_lower,cfg.sigma_upper);
    n_sigma_candidates = numel(sigma_candidates);

    sigma_model_var    = nan(n_sigma_candidates,1);
    sigma_variance_gap = nan(n_sigma_candidates,1);
    sigma_model_corr   = nan(n_sigma_candidates,1);
    sigma_money_demand = nan(n_sigma_candidates,1);
    sigma_vfi_iters    = nan(n_sigma_candidates,1);
    sigma_dist_iters   = nan(n_sigma_candidates,1);

    best_sigma_objective = Inf;
    best_sigma_index = 1;
    solution_sigma = [];
    warm_candidate = warm;

    fprintf('\nBlock 2: calibrating sigma_theta at rho = %.8f, msup = %.9f\n', ...
        rho,msup);

    for sigma_index = 1:n_sigma_candidates
        sigma_candidate = sigma_candidates(sigma_index);
        candidate_solution = solve_model( ...
            rho,sigma_candidate,msup,warm_candidate,cfg);
        candidate_gap = candidate_solution.log_c_var-log_c_var_target;
        candidate_objective = candidate_gap^2;

        sigma_model_var(sigma_index)    = candidate_solution.log_c_var;
        sigma_variance_gap(sigma_index) = candidate_gap;
        sigma_model_corr(sigma_index)   = candidate_solution.c1_corr;
        sigma_money_demand(sigma_index) = candidate_solution.money_demand;
        sigma_vfi_iters(sigma_index)    = candidate_solution.vfi_iterations;
        sigma_dist_iters(sigma_index)   = candidate_solution.dist_iterations;

        fprintf(['sigma = %.8f, Var(log c1) = %.8f, gap = %+.3e, ' ...
            'corr = %.8f, VFI iter = %d\n'], ...
            sigma_candidate,candidate_solution.log_c_var,candidate_gap, ...
            candidate_solution.c1_corr, ...
            candidate_solution.vfi_iterations);

        if candidate_objective < best_sigma_objective
            best_sigma_objective = candidate_objective;
            best_sigma_index = sigma_index;
            solution_sigma = candidate_solution;
        end

        warm_candidate.V = candidate_solution.V;
        warm_candidate.dist = candidate_solution.dist;
    end

    sigma_theta = sigma_candidates(best_sigma_index);
    sigma_hit_edge = best_sigma_index == 1 || ...
        best_sigma_index == n_sigma_candidates;

    sigma_results = table(sigma_candidates,sigma_model_var, ...
        sigma_variance_gap,sigma_model_corr,sigma_money_demand, ...
        sigma_vfi_iters,sigma_dist_iters, ...
        'VariableNames',{'sigma_theta','model_log_c_variance', ...
        'log_variance_gap','model_correlation','money_demand', ...
        'vfi_iterations','distribution_iterations'});

    fprintf(['Block 2 selected sigma_theta = %.8f, ' ...
        'log-variance gap = %+.3e\n'], ...
        sigma_theta,solution_sigma.log_c_var-log_c_var_target);
    disp(sigma_results)
    warm.V = solution_sigma.V;
    warm.dist = solution_sigma.dist;

    if ~sigma_hit_edge
        sigma_half_width = max( ...
            cfg.sigma_half_width_min, ...
            cfg.search_shrink*sigma_half_width);
    end

    %---------------------------------------------------------------------%
    % Block 3: real money supply and tau from money-market clearing
    %---------------------------------------------------------------------%
    msup_guess = msup;
    warm_money = warm;

    money_iteration = nan(cfg.money_maxiter,1);
    money_msup       = nan(cfg.money_maxiter,1);
    money_demand     = nan(cfg.money_maxiter,1);
    money_market_gap = nan(cfg.money_maxiter,1);
    money_model_corr = nan(cfg.money_maxiter,1);
    money_model_var  = nan(cfg.money_maxiter,1);
    money_goods_gap  = nan(cfg.money_maxiter,1);
    money_vfi_iters  = nan(cfg.money_maxiter,1);

    fprintf('\nBlock 3: clearing the money market at rho = %.8f, sigma = %.8f\n', ...
        rho,sigma_theta);

    money_converged = false;

    for money_index = 1:cfg.money_maxiter
        candidate_solution = solve_model( ...
            rho,sigma_theta,msup_guess,warm_money,cfg);
        candidate_gap = candidate_solution.money_demand-msup_guess;

        money_iteration(money_index) = money_index;
        money_msup(money_index)       = msup_guess;
        money_demand(money_index)     = candidate_solution.money_demand;
        money_market_gap(money_index) = candidate_gap;
        money_model_corr(money_index) = candidate_solution.c1_corr;
        money_model_var(money_index)  = candidate_solution.log_c_var;
        money_goods_gap(money_index)  = candidate_solution.goods_gap;
        money_vfi_iters(money_index)  = candidate_solution.vfi_iterations;

        fprintf(['money iter = %d, msup = %.9f, demand = %.9f, ' ...
            'gap = %+.3e, corr = %.8f, Var(log c1) = %.8f\n'], ...
            money_index,msup_guess,candidate_solution.money_demand, ...
            candidate_gap,candidate_solution.c1_corr, ...
            candidate_solution.log_c_var);

        solution = candidate_solution;

        if abs(candidate_gap) <= cfg.money_tol
            money_converged = true;
            break
        end

        msup_next = (1-cfg.money_damping)*msup_guess + ...
            cfg.money_damping*candidate_solution.money_demand;
        msup_guess = max(cfg.money_floor,msup_next);
        warm_money.V = candidate_solution.V;
        warm_money.dist = candidate_solution.dist;
    end

    if ~money_converged
        warning(['Money-market block reached money_maxiter without ' ...
            'satisfying money_tol.']);
    end

    money_iteration = money_iteration(1:money_index);
    money_msup       = money_msup(1:money_index);
    money_demand     = money_demand(1:money_index);
    money_market_gap = money_market_gap(1:money_index);
    money_model_corr = money_model_corr(1:money_index);
    money_model_var  = money_model_var(1:money_index);
    money_goods_gap  = money_goods_gap(1:money_index);
    money_vfi_iters  = money_vfi_iters(1:money_index);

    money_results = table(money_iteration,money_msup,money_demand, ...
        money_market_gap,money_model_corr,money_model_var, ...
        money_goods_gap,money_vfi_iters, ...
        'VariableNames',{'iteration','real_money_supply','money_demand', ...
        'money_market_gap','model_correlation','model_log_c_variance', ...
        'goods_gap','vfi_iterations'});
    disp(money_results)

    msup = solution.msup;
    warm.V = solution.V;
    warm.dist = solution.dist;

    rho_change   = abs(rho-rho_previous);
    sigma_change = abs(sigma_theta-sigma_previous);
    msup_change  = abs(msup-msup_previous);

    corr_gap    = solution.c1_corr-c1_corr_target;
    log_var_gap = solution.log_c_var-log_c_var_target;
    market_gap  = solution.money_demand-msup;

    converged = ...
        rho_change   <= cfg.rho_change_tol   && ...
        sigma_change <= cfg.sigma_change_tol && ...
        msup_change  <= cfg.msup_change_tol  && ...
        abs(corr_gap)    <= cfg.corr_gap_tol    && ...
        abs(log_var_gap) <= cfg.log_var_gap_tol && ...
        abs(market_gap)  <= cfg.market_gap_tol;

    outer_rho(outer)              = rho;
    outer_sigma(outer)            = sigma_theta;
    outer_msup(outer)             = msup;
    outer_tau(outer)              = solution.tau;
    outer_model_corr(outer)       = solution.c1_corr;
    outer_corr_gap(outer)         = corr_gap;
    outer_model_log_var(outer)    = solution.log_c_var;
    outer_log_var_gap(outer)      = log_var_gap;
    outer_money_demand(outer)     = solution.money_demand;
    outer_market_gap(outer)       = market_gap;
    outer_stationarity_gap(outer) = solution.stationarity_gap;
    outer_goods_gap(outer)        = solution.goods_gap;
    outer_rho_change(outer)       = rho_change;
    outer_sigma_change(outer)     = sigma_change;
    outer_msup_change(outer)      = msup_change;
    outer_vfi_iterations(outer)   = solution.vfi_iterations;
    outer_dist_iterations(outer)  = solution.dist_iterations;
    outer_converged(outer)        = converged;

    outer_iteration = (1:outer)';
    outer_results = table(outer_iteration,outer_rho(1:outer), ...
        outer_sigma(1:outer),outer_msup(1:outer),outer_tau(1:outer), ...
        outer_model_corr(1:outer),outer_corr_gap(1:outer), ...
        outer_model_log_var(1:outer),outer_log_var_gap(1:outer), ...
        outer_money_demand(1:outer),outer_market_gap(1:outer), ...
        outer_stationarity_gap(1:outer),outer_goods_gap(1:outer), ...
        outer_rho_change(1:outer),outer_sigma_change(1:outer), ...
        outer_msup_change(1:outer),outer_vfi_iterations(1:outer), ...
        outer_dist_iterations(1:outer),outer_converged(1:outer), ...
        'VariableNames',{'iteration','rho','sigma_theta', ...
        'real_money_supply','tau','model_correlation','correlation_gap', ...
        'model_log_c_variance','log_variance_gap','money_demand', ...
        'money_market_gap','stationarity_gap','goods_gap','rho_change', ...
        'sigma_change','money_supply_change','vfi_iterations', ...
        'distribution_iterations','converged'});

    fprintf('\nOuter iteration %d summary\n',outer);
    fprintf('rho                  = %.8f (change %.3e)\n',rho,rho_change);
    fprintf('sigma_theta          = %.8f (change %.3e)\n', ...
        sigma_theta,sigma_change);
    fprintf('real money supply     = %.9f (change %.3e)\n',msup,msup_change);
    fprintf('tau                   = %.9f\n',solution.tau);
    fprintf('consumption corr gap  = %+.3e\n',corr_gap);
    fprintf('log-consumption var gap = %+.3e\n',log_var_gap);
    fprintf('money-market gap      = %+.3e\n',market_gap);
    fprintf('stationarity gap      = %+.3e\n',solution.stationarity_gap);
    fprintf('goods gap             = %+.3e\n',solution.goods_gap);

    save(checkpoint_path,'cfg','empirical_alpha','empirical_eta_sd', ...
        'c1_corr_target','log_c_var_target','rho','sigma_theta','msup', ...
        'solution','outer_results','rho_results','sigma_results', ...
        'money_results','rho_half_width','sigma_half_width','converged');
    writetable(outer_results, ...
        fullfile(output_dir,'outer_iteration_results.csv'));

    if converged
        fprintf('\nAll parameter-change and equation-gap tolerances satisfied.\n');
        break
    end
end

if ~converged
    warning(['Three-block calibration reached outer_maxiter without ' ...
        'satisfying every convergence tolerance. Inspect outer_results.']);
end

%=========================================================================%
% Final named outputs retained in the workspace for compatibility
%=========================================================================%

theta_rho   = rho;
theta_sigma = sigma_theta;
tau         = solution.tau;
z           = solution.z;
theta       = solution.theta;
QQ          = solution.QQ;
V0          = solution.V;
idx_m       = solution.idx_m;
idx_c2      = solution.idx_c2;
m_star      = solution.m_star;
c2_star     = solution.c2_star;
c1_star     = solution.c1_star;
dist        = solution.dist;

final_results = table( ...
    theta_rho,theta_sigma,msup,tau, ...
    solution.c1_corr,c1_corr_target, ...
    solution.c1_corr-c1_corr_target, ...
    solution.log_c_var,log_c_var_target, ...
    solution.log_c_var-log_c_var_target, ...
    solution.money_demand,solution.money_demand-msup, ...
    solution.stationarity_gap,solution.goods_gap,converged, ...
    'VariableNames',{ ...
    'rho','sigma_theta','real_money_supply','tau', ...
    'model_c1_correlation','target_c1_correlation','correlation_gap', ...
    'model_log_c_variance','target_log_c_variance','log_variance_gap', ...
    'money_demand','money_market_gap','stationarity_gap','goods_gap', ...
    'converged'});

disp(final_results)
writetable(final_results,fullfile(output_dir,'final_results.csv'));
save(fullfile(output_dir,'cashcredit01_fixed_results.mat'), ...
    'cfg','empirical_alpha','empirical_eta_sd','c1_corr_target', ...
    'log_c_var_target','final_results','outer_results','solution', ...
    'rho_results','sigma_results','money_results');

fprintf('\nFinal three-block results\n');
fprintf('rho                         = %.8f\n',theta_rho);
fprintf('sigma_theta (innovation sd) = %.8f\n',theta_sigma);
fprintf('real money supply            = %.9f\n',msup);
fprintf('tau                          = %.9f\n',tau);
fprintf('model consumption correlation = %.9f\n',solution.c1_corr);
fprintf('target consumption correlation = %.9f\n',c1_corr_target);
fprintf('model Var(log consumption)     = %.9f\n',solution.log_c_var);
fprintf('target Var(log consumption)    = %.9f\n',log_c_var_target);
fprintf('money-market gap               = %+.3e\n', ...
    solution.money_demand-msup);
fprintf('Saved results to %s\n',output_dir);

%=========================================================================%
% Final graph outputs
%=========================================================================%

% Colors run from light to dark blue as theta increases.
colors = [linspace(0.85,0.05,cfg.S)', ...
    linspace(0.92,0.35,cfg.S)',linspace(1,0.85,cfg.S)'];

%-------------------------------------------------------------------------%
% Policy functions at the final calibrated parameter values
%-------------------------------------------------------------------------%
policy_figure = figure('Color','w','Position',[100 100 800 1000]);

subplot(3,1,1);
hold on;
for s = 1:cfg.S
    plot(cfg.mgrid,m_star(:,s),'Color',colors(s,:),'LineWidth',1.5, ...
        'DisplayName',sprintf('\\theta = %.2f',theta(s)));
end
plot(cfg.mgrid,cfg.mgrid,'k--','LineWidth',1.2, ...
    'DisplayName','45 degree line');
hold off;
title('Optimal $m^\prime$','Interpreter','latex','FontSize',13);
xlabel('$m$','Interpreter','latex','FontSize',12);
ylabel('$m^\prime$','Interpreter','latex','FontSize',12);
legend('show','Location','northwest','FontSize',8);
grid on;

subplot(3,1,2);
hold on;
for s = 1:cfg.S
    plot(cfg.mgrid,c1_star(:,s),'Color',colors(s,:),'LineWidth',1.5, ...
        'DisplayName',sprintf('\\theta = %.2f',theta(s)));
end
hold off;
title('Optimal $c_1$ (cash good)','Interpreter','latex','FontSize',13);
xlabel('$m$','Interpreter','latex','FontSize',12);
ylabel('$c_1$','Interpreter','latex','FontSize',12);
legend('show','Location','northwest','FontSize',8);
grid on;

subplot(3,1,3);
hold on;
for s = 1:cfg.S
    plot(cfg.mgrid,c2_star(:,s),'Color',colors(s,:),'LineWidth',1.5, ...
        'DisplayName',sprintf('\\theta = %.2f',theta(s)));
end
hold off;
title('Optimal $c_2$ (credit good)','Interpreter','latex','FontSize',13);
xlabel('$m$','Interpreter','latex','FontSize',12);
ylabel('$c_2$','Interpreter','latex','FontSize',12);
legend('show','Location','northwest','FontSize',8);
grid on;

sgtitle('Policy Functions: Cash-Credit CIA Model', ...
    'Interpreter','latex','FontSize',14,'Color','k');

policy_axes = findall(policy_figure,'Type','axes');
set(policy_axes,'Color','w','XColor','k','YColor','k', ...
    'GridColor',[0.75 0.75 0.75]);
set(findall(policy_figure,'Type','text'),'Color','k');
set(findall(policy_figure,'Type','legend'),'Color','w','TextColor','k', ...
    'EdgeColor',[0.3 0.3 0.3]);

exportgraphics(policy_figure, ...
    fullfile(output_dir,'policy_functions.pdf'), ...
    'ContentType','vector','BackgroundColor','white');

%-------------------------------------------------------------------------%
% Money-supply equilibrium from the final outer iteration's Block 3 path
%-------------------------------------------------------------------------%
[money_plot_msup,money_plot_order] = sort( ...
    money_results.real_money_supply);
money_plot_demand = money_results.money_demand(money_plot_order);
money_plot_gap = money_results.money_market_gap(money_plot_order);

money_axis_values = [money_plot_msup;money_plot_demand; ...
    msup;solution.money_demand];
money_axis_lower = min(money_axis_values);
money_axis_upper = max(money_axis_values);
money_axis_span = money_axis_upper-money_axis_lower;
if money_axis_span <= eps(max(abs(money_axis_values)))
    money_axis_pad = max(1e-4,0.02*max(1,abs(msup)));
else
    money_axis_pad = 0.05*money_axis_span;
end
money_axis_limits = [money_axis_lower-money_axis_pad, ...
    money_axis_upper+money_axis_pad];
money_45_degree = linspace( ...
    money_axis_limits(1),money_axis_limits(2),100)';

money_figure = figure('Color','w','Position',[100 100 850 750]);

subplot(2,1,1);
plot(money_plot_msup,money_plot_demand,'bo-','LineWidth',1.5, ...
    'DisplayName','Money demand');
hold on;
plot(money_45_degree,money_45_degree,'k--','LineWidth',1.2, ...
    'DisplayName','45 degree line');
plot(msup,solution.money_demand,'ks','MarkerFaceColor','y', ...
    'MarkerSize',8,'DisplayName','Selected equilibrium');
hold off;
xlabel('$m^s$','Interpreter','latex');
ylabel('$E_\Psi[m^\prime]$','Interpreter','latex');
title('Stationary Money Market: Final Block 3');
legend('show','Location','best');
xlim(money_axis_limits);
ylim(money_axis_limits);
grid on;

subplot(2,1,2);
plot(money_plot_msup,money_plot_gap,'ro-','LineWidth',1.5, ...
    'DisplayName','Money-market residual');
hold on;
yline(0,'k--','LineWidth',1.2,'DisplayName','Zero residual');
plot(msup,solution.money_demand-msup,'ks','MarkerFaceColor','y', ...
    'MarkerSize',8,'DisplayName','Selected equilibrium');
hold off;
xlabel('$m^s$','Interpreter','latex');
ylabel('$E_\Psi[m^\prime]-m^s$','Interpreter','latex');
title('Money-Market Residual');
legend('show','Location','best');
xlim(money_axis_limits);
grid on;

money_axes = findall(money_figure,'Type','axes');
set(money_axes,'Color','w','XColor','k','YColor','k', ...
    'GridColor',[0.75 0.75 0.75]);
set(findall(money_figure,'Type','text'),'Color','k');
set(findall(money_figure,'Type','legend'),'Color','w','TextColor','k', ...
    'EdgeColor',[0.3 0.3 0.3]);

exportgraphics(money_figure, ...
    fullfile(output_dir,'money_supply_equilibrium.pdf'), ...
    'ContentType','vector','BackgroundColor','white');

fprintf('Saved policy and money-equilibrium graphs to %s\n',output_dir);

%=========================================================================%
% Local functions
%=========================================================================%

function solution = solve_model(rho,sigma_theta,msup,warm,cfg)
    if abs(rho) >= 1
        error('rho must lie strictly inside (-1,1).');
    end
    if sigma_theta <= 0
        error('sigma_theta must be strictly positive.');
    end
    if msup <= 0
        error('Real money supply must be strictly positive.');
    end

    tau = msup*cfg.gama/(1+cfg.gama);
    [z,QQ] = tauchen(cfg.S,cfg.theta_log_mu,rho,sigma_theta, ...
        cfg.theta_width);
    theta = exp(z);

    [c2grid,period_return,best_c2_given_mprime] = ...
        build_period_returns(cfg.mgrid,cfg.K,cfg.S,theta, ...
        cfg.gama,cfg.y,tau,cfg.sig);

    if isfield(warm,'V') && isequal(size(warm.V),[cfg.N,cfg.S])
        V_initial = warm.V;
    else
        V_initial = ut(cfg.y,cfg.sig)*ones(cfg.N,cfg.S)/(1-cfg.bet);
    end

    [V,idx_m,idx_c2,vfi_diff,vfi_iterations] = solve_vfi( ...
        V_initial,QQ,period_return,best_c2_given_mprime, ...
        cfg.bet,cfg.vfi_tol,cfg.vfi_maxiter);

    if ~isfinite(vfi_diff) || vfi_diff > cfg.vfi_tol
        error(['VFI failed for rho = %.8f, sigma = %.8f, ' ...
            'msup = %.9f.'],rho,sigma_theta,msup);
    end

    [m_star,c2_star,c1_star] = recover_policies( ...
        idx_m,idx_c2,cfg.mgrid,c2grid,cfg.gama,cfg.y,tau,cfg.S);

    if any(c1_star(:) < -1e-10)
        error('Negative c1 encountered in the recovered policy.');
    end
    cia_limit = repmat(cfg.mgrid/(1+cfg.gama),1,cfg.S);
    if any(c1_star(:) > cia_limit(:)+1e-10)
        error('Cash-in-advance constraint violated by recovered policy.');
    end

    if isfield(warm,'dist') && isequal(size(warm.dist),[cfg.N,cfg.S])
        dist_initial = warm.dist;
    else
        dist_initial = [];
    end

    [dist,dist_diff,dist_iterations] = stationary_distribution( ...
        idx_m,QQ,cfg.dist_tol,cfg.dist_maxiter,dist_initial);

    if ~isfinite(dist_diff) || dist_diff > cfg.dist_tol
        error(['Stationary distribution failed for rho = %.8f, ' ...
            'sigma = %.8f, msup = %.9f.'],rho,sigma_theta,msup);
    end

    [money_demand,mean_m,stationarity_gap,goods_gap] = ...
        equilibrium_moments(cfg.mgrid,m_star,c1_star,c2_star, ...
        dist,cfg.y,cfg.S);
    c1_corr = level_consumption_correlation(c1_star,idx_m,QQ,dist);
    log_c_var = log_consumption_variance(c1_star,dist);

    solution.rho              = rho;
    solution.sigma_theta      = sigma_theta;
    solution.msup             = msup;
    solution.tau              = tau;
    solution.z                = z;
    solution.theta            = theta;
    solution.QQ               = QQ;
    solution.c2grid           = c2grid;
    solution.V                = V;
    solution.idx_m            = idx_m;
    solution.idx_c2           = idx_c2;
    solution.m_star           = m_star;
    solution.c2_star          = c2_star;
    solution.c1_star          = c1_star;
    solution.dist             = dist;
    solution.money_demand     = money_demand;
    solution.mean_m           = mean_m;
    solution.stationarity_gap = stationarity_gap;
    solution.goods_gap        = goods_gap;
    solution.c1_corr          = c1_corr;
    solution.log_c_var        = log_c_var;
    solution.vfi_diff         = vfi_diff;
    solution.vfi_iterations   = vfi_iterations;
    solution.dist_diff        = dist_diff;
    solution.dist_iterations  = dist_iterations;
end


function [c2grid,period_return,best_c2_given_mprime] = ...
    build_period_returns(mgrid,K,S,theta,gama,y,tau,sig)

    N = numel(mgrid);
    c2grid = linspace(0,y+tau,K)';
    period_return = -inf(N,N,S);
    best_c2_given_mprime = ones(N,N,S);

    resources = mgrid/(1+gama)+y+tau;
    mprime = mgrid';

    for k = 1:K
        c2 = c2grid(k);
        c1 = resources-c2-mprime;
        feasible = repmat(mprime >= y+tau-c2,N,1) & c1 > 0;

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
        EV = V*QQ';

        for s = 1:S
            W_m = period_return(:,:,s)+bet*EV(:,s)';
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
    idx_m,QQ,tol,maxiter,dist_initial)

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

    if nargin >= 5 && ~isempty(dist_initial) && ...
            isequal(size(dist_initial),[N,S]) && ...
            all(isfinite(dist_initial(:))) && sum(dist_initial(:)) > 0
        psi = max(dist_initial(:),0);
        psi = psi/sum(psi);
    else
        psi = ones(NS,1)/NS;
    end

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
        error('Cannot calculate consumption correlation: variance is zero.');
    end

    expected_product = 0;
    for s = 1:S
        next_c1 = c1(idx_m(:,s),:);
        expected_next_c1 = next_c1*QQ(s,:)';
        expected_product = expected_product + sum( ...
            dist(:,s).*c1(:,s).*expected_next_c1);
    end

    covariance = expected_product-mean_c^2;
    corr_c = covariance/var_c;
end


function variance_log_c = log_consumption_variance(c1,dist)
    positive_mass = dist > 0;
    if any(c1(positive_mass) <= 0)
        error(['Positive stationary mass is assigned to nonpositive c1; ' ...
            'Var(log(c1)) is undefined.']);
    end

    log_c = zeros(size(c1));
    log_c(positive_mass) = log(c1(positive_mass));
    mean_log_c = sum(dist(positive_mass).*log_c(positive_mass));
    variance_log_c = sum(dist(positive_mass).* ...
        (log_c(positive_mass)-mean_log_c).^2);
end


function grid = bounded_grid(center,half_width,n_points,lower,upper)
    left = max(lower,center-half_width);
    right = min(upper,center+half_width);

    if left >= right
        error('Invalid scalar-search interval.');
    end

    grid = linspace(left,right,n_points)';
    grid = unique([grid;center]);
    grid = sort(grid(grid >= lower & grid <= upper));
end
