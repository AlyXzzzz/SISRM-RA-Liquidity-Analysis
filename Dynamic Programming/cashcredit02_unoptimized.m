%--Unoptimized cash-credit VFI with a current bond state--%
% This intermediate version adds current bonds b(j) to the original model,
% but does not yet introduce a choice of next-period bonds b'.
clear all;
close all;
clc;

%--Parameters for a single diagnostic model solve--%
rho = 0.6768841969525026;
sigma_theta = 0.577006677083328;
msup = 0.9233706737453602;

[mgrid, bgrid, m_star, b_star, c1_star, c2_star, theta] = ...
    solve_model_unoptimized(rho, sigma_theta, msup);

%--Diagnostic surface plots for all preference shocks--%
S = length(theta);
[B,M] = meshgrid(bgrid,mgrid);
colors = parula(S);

figure;

subplot(2,2,1);
hold on;
for s = 1:S
    surf(B,M,m_star(:,:,s),'FaceColor',colors(s,:), ...
        'FaceAlpha',0.35,'EdgeColor','none','DisplayName', ...
        sprintf('$\\theta = %.2f$',theta(s)));
end
hold off;
title('Optimal $m^\prime$','Interpreter','latex','FontSize',13);
xlabel('$b$','Interpreter','latex','FontSize',12);
ylabel('$m$','Interpreter','latex','FontSize',12);
zlabel('$m^\prime$','Interpreter','latex','FontSize',12);
legend('show','Interpreter','latex','Location','best','FontSize',7);
view(45,30);
grid on;

subplot(2,2,2);
hold on;
for s = 1:S
    surf(B,M,b_star(:,:,s),'FaceColor',colors(s,:), ...
        'FaceAlpha',0.35,'EdgeColor','none');
end
hold off;
title('Optimal $b^\prime$','Interpreter','latex','FontSize',13);
xlabel('$b$','Interpreter','latex','FontSize',12);
ylabel('$m$','Interpreter','latex','FontSize',12);
zlabel('$b^\prime$','Interpreter','latex','FontSize',12);
view(45,30);
grid on;

subplot(2,2,3);
hold on;
for s = 1:S
    surf(B,M,c1_star(:,:,s),'FaceColor',colors(s,:), ...
        'FaceAlpha',0.35,'EdgeColor','none');
end
hold off;
title('Optimal $c_1$ (cash good)','Interpreter','latex','FontSize',13);
xlabel('$b$','Interpreter','latex','FontSize',12);
ylabel('$m$','Interpreter','latex','FontSize',12);
zlabel('$c_1$','Interpreter','latex','FontSize',12);
view(45,30);
grid on;

subplot(2,2,4);
hold on;
for s = 1:S
    surf(B,M,c2_star(:,:,s),'FaceColor',colors(s,:), ...
        'FaceAlpha',0.35,'EdgeColor','none');
end
hold off;
title('Optimal $c_2$ (credit good)','Interpreter','latex','FontSize',13);
xlabel('$b$','Interpreter','latex','FontSize',12);
ylabel('$m$','Interpreter','latex','FontSize',12);
zlabel('$c_2$','Interpreter','latex','FontSize',12);
view(45,30);
grid on;

sgtitle('Policy Functions for All Preference Shocks', ...
    'Interpreter','latex','FontSize',14);

function [mgrid, bgrid, m_star, b_star, c1_star, c2_star, theta] = ...
solve_model_unoptimized(rho, sigma_theta, msup)

    S = 7;

    %--Parameters--%
    bet  = 0.97;                               %--discount factor--%
    sig  = 2;                                  %--risk aversion--%
    y    = 1;                                  %--endowment--%
    gama = 0.02;                               %--inflation rate--%
    r    = 0.04;                               %--real interest rate--%
    tau  = msup*gama/(1+gama);                 %--real money transfers--%

    N = 20;                                    %--money and bond grid size--%
    K = 20;                                    %--c2 grid size--%
    mup = 3.5;                                 %--upper money-grid value--%
    bup = 3.5;                                 %--upper bond-grid value--%

    %--Shock process--%
    [z,QQ] = tauchen(S,0,rho,sigma_theta,3);
    theta = exp(z);

    %--State and choice grids--%
    mgrid = linspace(0.1,mup,N)';
    bgrid = linspace(0.1,bup,N)';

    % Each column is the c2 grid conditional on current bonds b(j).
    c2grid = zeros(K,N);
    for j = 1:N
        c2grid(:,j) = linspace( ...
            0,y+tau+(1+r)*bgrid(j),K)';
    end

    %------------------------------------------------------------------%
    % Current-period utility
    %------------------------------------------------------------------%
    % i = current money m, j = current bonds b, n = chosen money m',
    % q = chosen bonds b', and k = chosen credit-good consumption c2.

    u1 = zeros(N,N,N,N,K);
    u2 = zeros(N,N,N,N,K);

    for k = 1:K
        for j = 1:N
            for i = 1:N
                for n = 1:N
                    c2 = c2grid(k,j);

                    for q = 1:N
                        if mgrid(n)+bgrid(q) < ...
                               y+tau+(1+r)*bgrid(j)-c2 || ...
                           mgrid(n)+bgrid(q) > ...
                               mgrid(i)/(1+gama)+y+tau+(1+r)*bgrid(j)-c2
                            u1(i,j,n,q,k) = -inf;
                            u2(i,j,n,q,k) = -inf;
                        else
                            c1 = mgrid(i)/(1+gama)+y+tau+ ...
                                (1+r)*bgrid(j)-c2-mgrid(n)-bgrid(q);
                            u1(i,j,n,q,k) = ut(c1,sig);
                            u2(i,j,n,q,k) = ut(c2,sig);
                        end
                    end
                end
            end
        end
    end

    %------------------------------------------------------------------%
    % Current-period returns
    %------------------------------------------------------------------%
    R_max = zeros(N,N,N,N,S);
    best_k = zeros(N,N,N,N,S);

    for s = 1:S
        R_s = theta(s)*u1+u2;

        % Maximize over c2, the fifth dimension. 
        [R_max(:,:,:,:,s),best_k(:,:,:,:,s)] = max(R_s,[],5);
    end

    %------------------------------------------------------------------%
    % Value-function iteration
    %------------------------------------------------------------------%
    tol = 1e-6;
    diff = Inf;
    iter = 0;
    maxiter = 1000;

    V0 = ut(y,sig)*ones(N,N,S)/(1-bet);
    V_new = zeros(N,N,S);
    idx_m = zeros(N,N,S);
    idx_c2 = zeros(N,N,S);
    idx_b = zeros(N,N,S);

    while diff > tol && iter < maxiter
        EV = reshape(reshape(V0,N*N,S)*QQ',N,N,S);

        for s = 1:S
            for j = 1:N
                for i = 1:N
                    R = reshape(R_max(i,j,:,:,s),[N,N]); %--N x N--%
                    cont = bet*EV(:,:,s);
                    W = R + cont;
    
                    % Maximize over n (m')
                    [value_by_q,best_n_by_q] = max(W,[],1); %--1 x N--%
    
                    % Maximize over q (b')
                    [V_new(i,j,s),best_q] = max(value_by_q);
                    best_n = best_n_by_q(best_q);
    
                    idx_m(i,j,s) = best_n;
                    idx_b(i,j,s) = best_q;
                    idx_c2(i,j,s) = ...
                        best_k(i,j,best_n,best_q,s);
                end
            end
        end
        
        diff = max(abs(V_new(:)-V0(:)));
        V0 = V_new;
        iter = iter+1;

        if mod(iter,50) == 0
            fprintf('iter = %d, diff = %e\n',iter,diff);
        end
    end

    if diff > tol
        error('VFI failed to converge after %d iterations (diff = %e).', ...
            iter,diff);
    end
    fprintf('Convergence achieved in %d iterations\n',iter);

    %------------------------------------------------------------------%
    % Recover policy functions
    %------------------------------------------------------------------%
    m_star = mgrid(idx_m);
    c1_star = zeros(N,N,S);
    c2_star = zeros(N,N,S);
    b_star = bgrid(idx_b);

    for s = 1:S
        for i = 1:N
            for j = 1:N
                c2_star(i,j,s) = c2grid(idx_c2(i,j,s),j);
                c1_star(i,j,s) = mgrid(i)/(1+gama)+y+tau+ ...
                    (1+r)*bgrid(j)-c2_star(i,j,s)-m_star(i,j,s)-b_star(i,j,s);
            end
        end
    end

    %--Verify feasibility of recovered policies--%
    assert(all(c1_star(:) >= -1e-10),'c1 negative somewhere');
    cash_available = repmat(reshape(mgrid/(1+gama),[N,1,1]), ...
        [1,N,S]);
    assert(all(c1_star(:) <= cash_available(:)+1e-10), ...
        'CIA violated somewhere');
end
