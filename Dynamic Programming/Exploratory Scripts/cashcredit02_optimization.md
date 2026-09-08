# Cash-credit VFI optimization

The original `cashcredit02_unoptimized.m` is unchanged. Run
`cashcredit02_optimized.m` from this directory for the same four policy plots.
Its `options.N` defaults to 100; set it to 200 for the larger grid.
Both sizes have converged successfully with the original parameters, K=20,
seven shocks, double precision, and Bellman tolerance 1e-6.

For repeated solves, call the standalone function directly:

```matlab
options = struct('N',200,'K',20,'verbose',false);
[mgrid,bgrid,m_star,b_star,c1_star,c2_star,theta,info] = ...
    solve_model_optimized(rho,sigma_theta,msup,options);

% Optional warm start for a later solve on the same grids:
options.V0 = info.V;
[mgrid,bgrid,m_star,b_star,c1_star,c2_star,theta,info] = ...
    solve_model_optimized(rho_new,sigma_theta_new,msup_new,options);
```

The first seven outputs have the same order and shapes as the original local
`solve_model_unoptimized`. The optional eighth output includes the value
function, transition matrix, policy indices, timings, and Bellman residual.
No distribution, moments, or outer parameter iteration is run.

## What changed

1. **Combine asset choices by total saving.** For fixed `(m,b,theta)`, current
   utility depends on `m'+b'`. These equal, uniformly spaced grids have only
   `2*N-1` possible totals. Each Bellman search first finds the best split
   between money and bonds for each total using the continuation value.
   Both assets remain independent policy choices.
2. **Compute rewards in small blocks.** The solver processes one shock and
   one current bond state at a time. A reward block is `N` by `2*N-1`.
   It never stores the original five-dimensional utility/return tensors.
3. **Exploit concavity without changing the consumption grid.** For each
   total saving, the continuous consumption optimum locates the neighboring
   discrete `c2` choices. Four nearby grid points are checked, including two
   extra neighbors for floating-point boundaries. The original feasibility
   inequalities are enforced; every returned `c2` is on the original K-point
   grid. This avoids scanning all K choices.
4. **Use Howard updates.** After a full greedy search, 50 cheap value updates
   hold the policy fixed. Set `howard_steps=0` for ordinary VFI. Convergence
   is accepted only after a full greedy Bellman search has residual below
   `tol`. `maxiter` limits full searches; `info.policy_evaluations` counts
   the additional fixed-policy updates.

Working storage scales as `O(N^2*S + N*K)` instead of
`O(N^4*(K+S))`; full policy searches scale as `O(N^3*S)` instead of
`O(N^4*S)`. The original utility preprocessing also scans K choices.
Values and utilities remain double precision; only policy indices use
compact integer types.

For perspective, the original named tensors `u1`, `u2`, `R_s`, `R_max`, and
`best_k` require approximately `8*N^4*(3*K+2*S)` bytes combined, excluding
expression temporaries and MATLAB overhead:

| N | Original main tensors | New reward block (double) |
|---:|---:|---:|
| 20 | 94.7 MB | 6.24 KB |
| 100 | 59.2 GB | 159 KB |
| 200 | 947 GB | 638 KB |

These are decimal array sizes, not process-memory measurements. The new
solver also keeps value/policy arrays and temporary blocks. At N=200, one
`N*N*S` double array is 2.24 MB. See `cashcredit02_optimized_results/` for the
separate measured process-memory benchmark, including MATLAB itself.
The fresh N=200 benchmark process peaked at **726,810,624 bytes (727 MB,
693 MiB) resident memory**, including MATLAB, code analysis, and the sampled
policy checks. This is total process RSS, not an estimate of solver-only
allocations. macOS reported zero swaps for that process.

## Validation and observed timings

Measured with MATLAB R2026a Update 4 on this Mac. Times are single observed
solver calls, excluding MATLAB startup and plotting; they are not averages.

| Grid | Original | Optimized | Check |
|---|---:|---:|---|
| N=8, K=41 | 0.65 s | 0.08 s | All four policies identical |
| N=12, K=31 | 1.33 s | 0.08 s | All four policies identical |
| N=20, K=20 | 4.87 s | 0.34 s | All four policies identical |
| N=100, K=20 | Not run | 12.78 s | Residual 2.73e-7 |
| N=200, K=20 | Not run | 67.23 s | Residual 8.73e-7 |

A separate N=200 call during the memory benchmark took 73.95 seconds and
produced the same residual. Runtime will vary with system load and parameters.

The small-grid comparisons use tolerance 1e-8 and an extracted copy of the
original function with its Bellman logic intact. Every small-grid state is
checked against the original full-tensor Bellman objective at the optimized
value function, including the utility of the returned consumption policy.
Values agree within the contraction error bounds; all four policy arrays
match exactly. Ordinary VFI and a converged warm start are also checked.

For each large grid, the validator exhausts all `(m',b',c2)` choices at 27
combinations of boundary/interior asset states and low/middle/high shocks.
Both large grids passed with zero measured objective loss in these checks.
The saved JSON records these policy optimality checks alongside residuals
and timings. A full-tensor solve at N=100 or 200 is deliberately avoided.

Run `validate_cashcredit02_optimized(false)` for small-grid regression checks,
or `validate_cashcredit02_optimized` to also regenerate both larger policy
files. Results are saved in `cashcredit02_optimized_results/`:

- `policies_N100.mat` and `policies_N200.mat`: grids, four policies, shocks,
  parameters, and solver diagnostics including `info.V` and policy indices.
- `validation_results.json` / `.mat`: comparison and convergence results.
- `memory_benchmark_N200.txt` / `.json`: process peak memory and a separate
  timed N=200 solve. The text is `/usr/bin/time -l` output.
- `policies_N100.png`: the four policy surfaces exported by the new script.

## Scope and numerical details

The reduction by total saving relies on the equal uniform spacings in this
file. If money and bond grids later use different spacings or nonlinear
points, the grouping must be changed; do not reuse `n+q-1` unchanged.
Likewise, the discrete consumption shortcut relies on the current separable
CRRA utility and linear budget/CIA constraints.

Different floating-point additions of equal mathematical saving totals can
differ by a few ulps. The solver uses a representative total and preserves
the original first-maximum ordering for exact ties. Policies match exactly
in the tested small cases; arbitrary parameter changes can still move a
near-tied policy by rounding. Returned consumption is reconstructed with
the original budget arithmetic and checked for feasibility.

Grids too coarse to offer a finite feasible choice are rejected explicitly.
Some very coarse N/K combinations also fail feasibility checks in the
original solver. The successful N=100 and 200 tests are for the supplied
parameters; future outer-loop parameter candidates must still converge.
