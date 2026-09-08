%--Memory-bounded cash-credit VFI with money and bond choices--%
% Original model and discrete choices; see solve_model_optimized.m.
% N=200 is also supported without allocating high-dimensional tensors.
clearvars;
close all;
clc;

%--Parameters for a single diagnostic model solve--%
rho = 0.6768841969525026;
sigma_theta = 0.577006677083328;
msup = 0.9233706737453602;

options = struct('N',100,'K',20,'howard_steps',50,'verbose',true);
[mgrid, bgrid, m_star, b_star, c1_star, c2_star, theta, info] = ...
    solve_model_optimized(rho, sigma_theta, msup, options);

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
