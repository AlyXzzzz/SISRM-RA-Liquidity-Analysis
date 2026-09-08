%--implementing VFI with money only and with distributions--%
clear all;
close all;
clc;

S = 7;
[z,QQ] = tauchen(S,0,0.8,0.5,3);

theta = exp(z);                            %--shocks----------------@
bet   = 0.97;                              %--discount factor-------@
sig   = 2;                                 %--risk aversion---------@
y     = 1;                                 %--endowment-------------@
gama  = 0.02;                              %--inflation rate--------@
msup  = 1.127523876033966;
tau   = msup*gama/(1+gama);                %--real money transfers--@

K    = 200;                                %--number of points on the grid c2--@ 
N    = 200;                                %--number of points on the grid-----@
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

        %--total value: N x N x K--%
        W = theta(s)*u1 + u2 + cont';             %--N x N x K-------------@

        %--maximize over n (dim 2) and k (dim 3) for each i (dim 1)--%
        %--first maximize over k (dim 3)--%
        [W_maxk, best_k] = max(W,[],3);           %--N x N-----------------@

        %--then maximize over n (dim 2)--%
        [V_new(:,s), best_n] = max(W_maxk,[],2); %--N x 1------------------@

        %--store indices--%
        idx_m(:,s)  = best_n;                     %--optimal m' index---------@

        %--recover optimal k for each i--%
        for i = 1:N
            idx_c2(i,s) = best_k(i, best_n(i));  %--optimal c2 index---------@
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

if diff <= tol
    fprintf('Convergence achieved in %d iterations\n', iter);
else
    fprintf('No convergence after %d iterations\n', iter);
end

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


%--computing distributions

P = zeros(N*S,N*S);

for s = 1:S
    for n = 1:N

        c = n+(s-1)*N;
        k = idx_m(n,s);

        for ell = 1:S

            r = k+(ell-1)*N;
            P(r,c) = QQ(s,ell);
        end
    end
end


% computing the distribution
[V,D] = eig(P);
[~,idx] = min(abs(diag(D)-1));
psi = V(:,idx);
psi = psi/sum(psi);

dist = zeros(N,S);
for i = 1:S
    dist(:,i) = psi(1+(i-1)*N:i*N,1);
end

mu = sum(dist,2);

figure;
%bar(mgrid,mu,1)
stem(mgrid,mu,'filled')
xlabel('Money')
ylabel('Probability mass')
grid on
