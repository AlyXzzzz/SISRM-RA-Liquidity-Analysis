# Changes to `cashcredit01.m`

## VFI Loop Optimization

There is a useful observation that we leverage to improve the efficiency of the
computation: conditional on $m_n$, the continuation value does not
vary with $c_{2,k}$. Consequently, for every $(i,n,s)$ we can compute once

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

Neither $R_s(i,n)$ nor $\kappa_s(i,n)$ changes across Bellman iterations, since
it records the entire table of current period returns, and thus does not need to
be computed each time the VFI iteration runs. In each VFI iteration, the code 
updates

$$
EV(n,s)=\sum_{s'=1}^{S}Q_{ss'}V_0(m_n,\theta_{s'}),
$$

forms

$$
W_s(i,n)=R_s(i,n)+\beta EV(n,s),
$$

and then computes

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
$1750\times1750$ matrix.The GPT updated code uses a more efficient way to store
the state transition matrix using the `sparse[]` function, where we specify 

- `rows(q)` = next-state index
- `cols(q)` = current-state index
- `vals(q)` = transition probability

This is because our policy function specifies only one next period choice for 
each current period money holding $m$ conditional on the next period shock state
$\theta_s'$, implying that each column of the state transition matrix will only
have at most 7 nonzero entries. 

To solve for the stationary distribution, instead of solving all the eigenvalues
of the whole state transition matrix, we use power iteration by leveraging the 
Markov property of the transition matrix. 

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

Starting from $(m_i,\theta_s)$, next period's money index is $g_m(i,s)$, while
the next shock is $s'$ with probability $Q_{ss'}$. Hence,

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
\sum_{s'=1}^{S}Q_{ss'}c_1^*\left(g_m(i,s),s'\right)
$$


Because the economy is evaluated under its stationary distribution,
$\mathbb E[c_{1,t+1}]=\mathbb E[c_{1,t}]=\bar c_1$ and the two dates have the
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


## Final Pseudocode Recap of the Whole Procedure

The implemented joint calibration uses alternating iterations rather than solving
a complete money-supply equilibrium separately for every candidate $\rho$:

```text
initialize money supply

repeat joint iteration:
    Stage A: calibrate rho at the current fixed money supply
    construct the current-period-return arrays once outside the rho loop because tau and the shock nodes are fixed
        for each rho candidate:
            calculate sigma_epsilon(rho)
            construct QQ using Tauchen's method, using sigma_epsilon(rho) as a parameter
            solve the household VFI
            compute the stationary distribution
            evaluate the consumption-correlation gap using the stationary distribution
        end
        select the rho with the smallest correlation gap

    Stage B: solve money equilibrium at the selected fixed rho
    % Since rho is fixed here, we do not need to recompute QQ and sigma_epsilon(rho)
        initialize a money-supply guess
        repeat:
            calculate tau(money supply)
            rebuild current-period returns since it depends on transfers
            solve the household VFI
            compute the stationary distribution
            calculate money demand E_Psi[m'] using stationary distribution
            evaluate F(money supply) = E_Psi[m'] - money supply
            update money supply <- money demand
        until the money-market gap is sufficiently small

until both the selected rho and equilibrium money supply stop changing
```

The third joint iteration returned the same $\rho$ and equilibrium money supply
as the second iteration, confirming convergence at the current grid resolution.

## Model Reparameterization 

In the most recent updated version of the code, I correct the previous mistake 
where the relationship between Telyukova and our model is treated with a one-to-
one correspondence, and instead use the implied variance of liquid consumption
as an additional moment to match and solve for $\rho$. 

The pseudocode procedural logic is again similar to the previous version of the 
code, except now we have an additional moment to match: 

```text
initialize money supply, rho, and sigma_theta

repeat joint iteration:
    Stage A: calibrate rho at the current fixed money supply and sigma_theta
        for each rho candidate:
            solve the household VFI using our two step maximization
            compute the stationary distribution
            evaluate the consumption-correlation gap using the stationary distribution
            update the warmup value function guess and stationary distribution for the next iteration
        end
        select the rho with the smallest correlation gap
        if rho is on the edge of the grid, shrink the search range by half
        
    Stage B: calibrate sigma_theta at the current fixed money supply and last solved rho
        for each sigma_theta candidate:
            solve the household VFI using our two step maximization
            compute the stationary distribution
            evaluate the consumption variance gap using the stationary distribution
            update the warmup value function guess and stationary distribution for the next iteration
        end
        select the sigma_theta with the smallest consumption variance gap
        if sigma_theta is on the edge of the grid, shrink the search range by half

    Stage C: solve money equilibrium at the selected fixed rho and sigma_theta
        for each money supply candidate:
            calculate tau(money supply)
            solve the household VFI using our two step maximization
            compute the stationary distribution
            calculate money demand E_Psi[m'] using stationary distribution
            evaluate F(money supply) = E_Psi[m'] - money supply
            update money supply <- money demand
            update the warmup value function guess and stationary distribution for the next iteration
        until the money-market gap is sufficiently small

until we verify all parameters and target moments converge 
```

The new version of the code introduces a few new functions to its pipeline:

`bounded_grid()`: function that constructs the grid for the values of $\rho, \sigma_{\theta}, \tau$
the algorithm searches over for.

`solve_model()`: function that stores the whole solution to the model, including
the parameters, the policy functions, the value functions, the moment matching
objective measures, the stationary distribution, and the iteration counter. It 
calls our previously defined functions `build_period_returns()`, `solve_vfi()`,
`stationary_distribution()`, `level_consumption_correlation()`, and a new one 
`log_consumption_variance`