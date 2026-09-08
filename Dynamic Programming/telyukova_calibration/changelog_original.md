# Changes to `cashcredit01.m`

## VFI Loop Optimization

We begin with the observation that the current period return $\theta_s u(c_1) + u(c_2)$
can be maximized using either one of $c_1$ or $c_2$, and the continuation value $\mathbb{E}[V(m', \theta_s') \mid \theta_s]$
can be maximized without choice of $c_1$ or $c_2$. Thus, the optimized code 
maximizes the current period return by choice of $c_2$ conditional on all possible 
choices of $m, \theta_s,$ and $c_1$ once, and the later VFI iterations maximize
over $m'$ instead without having to additionally recompute the current period
return. In other words, we first solve 

$$W(m, \theta_s, c_1) = \text{max}_{c_2} \{\theta_s u(c_1) + u(c_2)\}$$

Then solve 

$$V(m',\theta_s') = \text{max}_{m'} \{W(m, \theta_s, c_1) + \mathbb{E}[V(m', \theta_s') \mid \theta_s]\} \equiv \text{max}_{m'} \{\mathbb{E}[V(m', \theta_s') \mid \theta_s]\}$$

This means we only need to search two $N \times N$ grids to solve the problem 
instead of a $N \times N \times K$ grid. (I'm actually not sure why these grids
are these dimensions. Can you explain?)

### Why the matrices are $N\times N$

For a fixed shock $s$, the first dimension indexes the $N$ possible current money
states $m_i$, while the second dimension indexes the $N$ candidate choices of
next-period money $m_n$. Thus, both $R_s$ and $W_s$ are $N\times N$ matrices.
Maximizing `W_s` over its second dimension leaves one value for every current
money state, an $N\times1$ vector. Across all $S$ shocks, `period_return` is an
$N\times N\times S$ array and the value function is an $N\times S$ array.

There is no independent $c_1$ grid. For each $c_{2,k}$ considered during the
one-time preprocessing step, the budget constraint generates an $N\times N$
matrix of residual $c_1$ values. The code streams through the $K$ candidates for
$c_2$ and retains only the best return and its index. Hence, the repeated Bellman
maximization is reduced from a search over an $N\times N\times K$ object to a
search over an $N\times N$ matrix for each shock, although the one-time
precomputation must still examine all $K$ values.

## Stationary Distribution Calculation Optimization

The original code stores the entire 1400 by 1400 state transition matrix. The 
GPT updated code uses a more efficient way to store the state transition matrix
using the `sparse[]` function, where we specify 

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

## Computation of the Level adjacent month stationary correlation for calibration

Each component of the calculation is as follows:

$$\bar{c}_1 = \sum_{i,n} \Psi(i,n)c_1^*(i,n)$$

Where $i$ indexes the $c_1$ state and $n$ indexes the shock state.

$$\mathbb{V}[c_1] = \sum_{i,n} (c_1(i,n) - \bar{c_1}) \cdot QQ(i,n) $$

$$\mathbb{C}[c_1c_1'] = \mathbb{E}[c_1c_1'] - \mathbb{E}[c_1]\mathbb{E}[c_1'] = \mathbb{E}[c_1c_1'] - \bar{c}_1^2$$

$$\mathbb{E}[c_1c_1'] = \sum_{i,n} \Psi(i,n)c_1^*(i,n)c_1^*(i,n) \cdot QQ(i,n)$$

The transition matrix does not appear directly in the formulas for the current
mean or variance because the stationary distribution $\Psi$ already gives the
unconditional probabilities of the current states. It appears explicitly in the
cross moment because that calculation must connect each current state to its
possible next-period shock states.

## Final pseudo-code recap of whole procedure 

`outer loop over rho`
   `calculate sigma(rho)`
    `construct QQ`

    inner search over msup
        calculate tau(msup)
        solve VFI
        compute distribution
        evaluate money-market gap
    end

    at equilibrium msup:
        evaluate consumption-correlation gap
`end`

