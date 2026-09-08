# Changes from `cashcredit01.m` to `cashcredit01_fixed.m`

Updated August 18, 2026.

## VFI Loop Optimization

There is a useful observation that improves the efficiency of the computation:
conditional on the candidate next-period money holding $m_n$, the continuation
value does not vary with the current choice of $c_{2,k}$. Consequently, for
every $(i,n,s)$ and for a fixed model evaluation, we can compute once

$$
R_s(i,n)
=
\max_{k\text{ feasible}}
\left\{
\theta_su(c_{1,ink})+u(c_{2,k})
\right\},
$$

and store the maximizing index

$$
\kappa_s(i,n)
=
\underset{k\text{ feasible}}{\operatorname{argmax}}
\left\{
\theta_su(c_{1,ink})+u(c_{2,k})
\right\}.
$$

For fixed values of $(\rho,\sigma_\theta,m^{sup})$, neither $R_s(i,n)$ nor
$\kappa_s(i,n)$ changes across Bellman iterations. They can therefore be
constructed before the VFI loop rather than recomputed during every Bellman
iteration. They must still be rebuilt when a parameter change alters the shock
nodes or transfers.

In each VFI iteration, the code updates

$$
EV(n,s)=\sum_{s'=1}^{S}Q_{ss'}V_0(m_n,\theta_{s'}),
$$

forms

$$
W_s(i,n)=R_s(i,n)+\beta EV(n,s),
$$

and computes

$$
V_{\mathrm{new}}(m_i,\theta_s)=\max_n W_s(i,n).
$$

The maximizing column gives the money-policy index $g_m(i,s)$. The optimal
credit-good index is then recovered from the stored conditional optimizer:

$$
g_{c_2}(i,s)=\kappa_s\bigl(i,g_m(i,s)\bigr).
$$

Finally, $c_1^*(i,s)$ is recovered from the budget constraint.

## Stationary Distribution Calculation Optimization

The full state is $(m_i,\theta_s)$, so the state-transition matrix $P$ has
dimensions $(NS)\times(NS)$. With $N=250$ and $S=7$, this is a
$1750\times1750$ matrix. The updated code stores the transition matrix using
MATLAB's `sparse(...)` constructor, with

- `rows(q)` equal to the next-state index;
- `cols(q)` equal to the current-state index; and
- `vals(q)` equal to the transition probability.

The policy selects one next-period money index $g_m(i,s)$ for each current
state $(m_i,\theta_s)$. That money choice can be paired with at most $S=7$
possible next-period shock states, so each column of $P$ has at most seven
nonzero entries.

The stationary distribution is calculated by power iteration,

$$
\psi_{j+1}=P\psi_j,
$$

rather than by computing all the eigenvalues of the full transition matrix.
After each multiplication, the code normalizes the probability vector and
continues until its maximum absolute change is below the distribution
tolerance.

## Computation of the Level Adjacent-Month Stationary Correlation

The stationary probability of current state $(m_i,\theta_s)$ is $\Psi(i,s)$,
and current cash-good consumption is $c_1^*(i,s)$. Therefore, stationary mean
consumption is

$$
\bar c_1
=
\mathbb E[c_{1,t}]
=
\sum_{i=1}^{N}\sum_{s=1}^{S}
\Psi(i,s)c_1^*(i,s).
$$

The stationary variance is

$$
\operatorname{Var}(c_{1,t})
=
\sum_{i=1}^{N}\sum_{s=1}^{S}
\Psi(i,s)\left(c_1^*(i,s)-\bar c_1\right)^2.
$$

Starting from $(m_i,\theta_s)$, next period's money index is $g_m(i,s)$,
while the next shock is $s'$ with probability $Q_{ss'}$. Hence,

$$
c_{1,t+1}=c_1^*\left(g_m(i,s),s'\right)
$$

along that transition. The adjacent-period cross moment is

$$
\mathbb E[c_{1,t}c_{1,t+1}]
=
\sum_{i=1}^{N}\sum_{s=1}^{S}\sum_{s'=1}^{S}
\Psi(i,s)Q_{ss'}
c_1^*(i,s)c_1^*\left(g_m(i,s),s'\right).
$$

Equivalently, conditional expected next-period consumption is

$$
\mathbb E[c_{1,t+1}\mid m_i,\theta_s]
=
\sum_{s'=1}^{S}Q_{ss'}c_1^*\left(g_m(i,s),s'\right).
$$

Because the economy is evaluated under its stationary distribution,
$\mathbb E[c_{1,t+1}]=\mathbb E[c_{1,t}]=\bar c_1$, and the two dates have the
same variance. Thus,

$$
\operatorname{Cov}(c_{1,t},c_{1,t+1})
=
\mathbb E[c_{1,t}c_{1,t+1}]-\bar c_1^2,
$$

and the level adjacent-month correlation is

$$
\operatorname{Corr}(c_{1,t},c_{1,t+1})
=
\frac{
\mathbb E[c_{1,t}c_{1,t+1}]-\bar c_1^2
}{
\operatorname{Var}(c_{1,t})
}.
$$

## Previous Two-Block Procedure

Before the model reparameterization described below, the calibration imposed a
direct variance mapping between the empirical regression residual and the
structural preference shock. Under that earlier mapping, the algorithm
alternated between a $\rho$ search and money-market clearing:

```text
initialize money supply

repeat joint iteration:
    Stage A: calibrate rho at the current fixed money supply
        for each rho candidate:
            use the old direct mapping to calculate sigma_theta(rho)
            construct the Tauchen transition matrix
            solve the household problem
            compute the stationary distribution
            evaluate the consumption-correlation gap
        end
        select the rho with the smallest absolute correlation gap

    Stage B: solve money equilibrium at the selected rho
        repeat:
            calculate tau from the current money-supply guess
            rebuild current-period returns
            solve the household problem
            compute the stationary distribution
            calculate stationary money demand
            update money supply toward money demand
        until the money-market gap is sufficiently small

until rho and equilibrium money supply stop changing
```

In the previous implementation, the third joint iteration returned the same
$\rho$ and equilibrium money supply as the second iteration at the employed
grid resolution. This convergence result pertains only to the earlier
two-block parameterization.

## Model Reparameterization and New Moment

The maintained Telyukova-style empirical regression can be represented as

$$
\log c^{liq}_{it}=\mu_i+X_{it}'\beta+\epsilon_{it},
\qquad
\epsilon_{it}=\alpha\epsilon_{i,t-1}+\eta_{it}.
$$

There is no one-to-one correspondence between the empirical innovation
$\eta_{it}$ and the model's innovation to $\log\theta_t$. In particular, the
model parameter $\sigma_\theta$ should not be set directly equal to either the
standard deviation or variance of $\eta_{it}$.

For a stationary empirical AR(1) residual process,

$$
\operatorname{Var}(\epsilon_{it})
=
\frac{\operatorname{Var}(\eta_{it})}{1-\alpha^2}
=
\frac{\sigma_\eta^2}{1-\alpha^2}.
$$

Because the model has neither $X_{it}$ nor household fixed effects $\mu_i$, the
corresponding model moment is the unconditional stationary variance
$\operatorname{Var}(\log c_1)$. The code therefore uses

$$
\operatorname{Var}(\log c_1)
=
\frac{\sigma_\eta^2}{1-\alpha^2}
$$

as an additional moment condition. This moment identifies the structural shock
innovation standard deviation $\sigma_\theta$ through the model solution; it
does not impose a direct analytical mapping from $\sigma_\eta$ to
$\sigma_\theta$.

The three unknowns and their primary conditions are:

- $\rho$: the adjacent-month level-consumption correlation;
- $\sigma_\theta$: the stationary variance of log liquid consumption; and
- real money supply $m^{sup}$: money-market clearing.

The transfer $\tau$ is not a fourth independent unknown. Given $m^{sup}$ and
the fixed money-growth rate $\gamma$, it is determined by

$$
\tau=\frac{\gamma}{1+\gamma}m^{sup}.
$$

## Current Three-Block Calibration

The current code places all three blocks inside one automated outer loop:

```text
initialize rho, sigma_theta, money supply, and warm-start objects

repeat outer joint iteration:
    save the previous rho, sigma_theta, and money supply

    Block 1: calibrate rho at fixed sigma_theta and money supply
        construct a bounded local grid around the current rho
        for each rho candidate:
            solve the model
            compute the stationary distribution and model moments
            evaluate Corr(c1_t,c1_t+1) - correlation target
            warm-start the next candidate evaluation
        end
        select the rho with the smallest squared correlation gap
        use the selected solution to warm-start Block 2
        if the selected rho is interior, shrink the next rho search window
        if it is on an edge, retain the current width and recenter next time

    Block 2: calibrate sigma_theta at fixed selected rho and money supply
        construct a bounded local grid around the current sigma_theta
        for each sigma_theta candidate:
            solve the model
            compute the stationary distribution and model moments
            evaluate Var(log(c1)) - empirical log-variance target
            warm-start the next candidate evaluation
        end
        select sigma_theta with the smallest squared log-variance gap
        use the selected solution to warm-start Block 3
        if selected sigma_theta is interior, shrink the next search window
        if it is on an edge, retain the current width and recenter next time

    Block 3: clear the money market at fixed selected rho and sigma_theta
        initialize the money-supply guess from the previous outer iteration
        repeat fixed-point iteration:
            calculate tau from the current money-supply guess
            solve the model
            compute gap = stationary money demand - money supply
            if the gap is outside tolerance:
                update money supply toward stationary money demand
                warm-start the next fixed-point evaluation
        until the money-market gap is sufficiently small

    evaluate all convergence conditions using the final Block 3 solution:
        changes in rho, sigma_theta, and money supply
        level-consumption correlation gap
        log-consumption variance gap
        money-market gap

until every parameter-change and equation-gap tolerance is satisfied,
or the maximum number of outer iterations is reached
```

Blocks 1 and 2 select the best points on finite local grids, whereas Block 3 is
a fixed-point iteration rather than a grid search. Because later blocks can
move moments previously targeted by earlier blocks, the full outer loop repeats
until the final Block 3 solution satisfies all three conditions jointly.

## `bounded_grid()` Interface

The function signature is

```matlab
grid = bounded_grid(center,half_width,n_points,lower,upper)
```

It constructs a sorted local candidate grid for one scalar parameter. The
current script calls it for $\rho$ and $\sigma_\theta$ only; it is not used for
money supply or $\tau$.

### Inputs

- `center`: the current parameter estimate around which the local search is
  centered.
- `half_width`: the requested distance from `center` to each side of the local
  search interval.
- `n_points`: the nominal number of evenly spaced points produced by
  `linspace` before explicitly inserting `center`.
- `lower`: the hard lower bound for the parameter.
- `upper`: the hard upper bound for the parameter.

The effective interval is

$$
[\max(\texttt{lower},\texttt{center}-\texttt{half\_width}),
  \min(\texttt{upper},\texttt{center}+\texttt{half\_width})].
$$

### Output

- `grid`: a sorted column vector of unique candidate values lying within the
  hard bounds. Provided `center` itself lies within those bounds, as it does in
  the calibration loop, it is always included. Therefore, if `center` is not
  already one of the `linspace` nodes, the output can contain `n_points + 1`
  values rather than exactly `n_points` values.

The function reports an error if truncation by the hard bounds produces an
empty or degenerate interval. After the calibration block selects a point, the
calling outer loop decides whether the next search window should shrink;
`bounded_grid()` itself does not update the center or the width.

## `solve_model()` Interface

The function signature is

```matlab
solution = solve_model(rho,sigma_theta,msup,warm,cfg)
```

It solves the household problem and stationary distribution once at a supplied
parameter triple. It computes model-implied moments but does not select a
parameter candidate or calculate the correlation and log-variance calibration
objectives. Those comparisons with empirical targets occur in the outer
calibration blocks.

### Inputs

- `rho`: persistence of the AR(1) process for $\log\theta_t$. The function
  requires $|\rho|<1$.
- `sigma_theta`: standard deviation of the innovation to $\log\theta_t$, not
  its unconditional standard deviation. The function requires
  `sigma_theta > 0`.
- `msup`: candidate real money supply. The function requires `msup > 0` and
  internally calculates $\tau=m^{sup}\gamma/(1+\gamma)$.
- `warm`: a structure containing optional numerical starting values:
  - `warm.V` is an $N\times S$ initial value-function array;
  - `warm.dist` is an $N\times S$ initial stationary-distribution array.

  `warm.V` is used only if it has the required dimensions. `warm.dist` must
  additionally contain finite values and have positive total mass; before use,
  negative entries are clipped to zero and the distribution is normalized.
  Otherwise, the function falls back to its default initialization.
- `cfg`: the configuration structure containing model parameters, grids,
  dimensions, tolerances, and maximum iteration counts. Important fields
  include `S`, `N`, `K`, `mgrid`, `bet`, `sig`, `y`, `gama`, the Tauchen-grid
  settings, and the VFI and distribution tolerances.

### Output

The single output `solution` is a structure. Its fields are grouped below.

Parameter values and shock process:

- `rho`, `sigma_theta`, and `msup`: the supplied parameter values;
- `tau`: the transfer implied by `msup`;
- `z`: the Tauchen nodes for $\log\theta$;
- `theta`: the preference-shock nodes, calculated as `exp(z)`;
- `QQ`: the Tauchen transition matrix.

Value and policy solution:

- `c2grid`: the credit-good choice grid;
- `V`: the converged value function;
- `idx_m`: indices of optimal next-period money holdings;
- `idx_c2`: indices of optimal credit-good choices;
- `m_star`, `c2_star`, and `c1_star`: the recovered policy functions.

Stationary distribution and equilibrium diagnostics:

- `dist`: the stationary joint distribution over $(m,\theta)$;
- `money_demand`: stationary mean optimal next-period money holdings;
- `mean_m`: stationary mean current money holdings;
- `stationarity_gap`: `money_demand - mean_m`;
- `goods_gap`: stationary mean $c_1+c_2$ minus the endowment $y$.

Model-implied calibration moments:

- `c1_corr`: stationary adjacent-period correlation of liquid consumption in
  levels;
- `log_c_var`: stationary variance of log liquid consumption.

Numerical diagnostics:

- `vfi_diff` and `vfi_iterations`: final VFI error and iteration count;
- `dist_diff` and `dist_iterations`: final stationary-distribution error and
  iteration count.

Internally, `solve_model()` performs the following sequence:

1. Calculate $\tau$ and discretize the shock process with `tauchen()`.
2. Construct current-period returns with `build_period_returns()`.
3. Solve the Bellman equation with `solve_vfi()`.
4. Recover choices with `recover_policies()`.
5. Calculate the stationary distribution with `stationary_distribution()`.
6. Calculate money demand and equilibrium diagnostics with
   `equilibrium_moments()`.
7. Calculate the calibration moments with
   `level_consumption_correlation()` and `log_consumption_variance()`.

The outer calibration code then uses these returned moments to construct the
candidate-specific gaps and objectives, select parameter values, update warm
starts, and test joint convergence.
