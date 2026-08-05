# Telyukova Replication: Variable Construction, Sample Selection, and Targets

This document describes the preferred specification implemented in
`scripts/telyukova/main/telyukova_replication_final.R`. It records the exact raw or summary variables,
coding rules, missing-value conventions, sample restrictions, paper targets,
and current replication results.

## 1. Data and General Conventions

### Data inputs

| Source | Input | Use |
|---|---|---|
| SCF 2001 full public file | `output/scf2001_raw_allvars.rds` | Sample restrictions, debt, assets, rate, and selected raw demographics |
| SCF 2001 summary extract | `raw/SCFP2001.csv` | Preferred race, children, occupation, and education variables |
| CEX FMLI, 2000-2002 | `output/cex/cex_fmli_2000_2002_raw.rds` | CU characteristics, income, liquid assets, weights, and panel timing |
| CEX FNA | `raw/cex/intrvw00.zip` through `intrvw02.zip` | Fifth-interview credit-card balance |
| CEX FN2 | Same ZIP files | Second-interview credit-card balance |
| CEX FNB | Same ZIP files | Fifth-interview annual credit-card finance charges |

### Numeric and missing-value conventions

- Character values are converted with `parse_number()`; `""`, `.`, `NA`, and
  `NaN` are treated as missing.
- Dollar amounts use `pmax(value, 0)`, so negative SCF codes such as `-1`
  become zero.
- CEX amount flags are treated as follows:
  - `A`: valid blank, set to zero.
  - `B`: invalid blank, set to missing.
  - `C`: do not know/refusal/other nonresponse, set to missing.
  - `D`: valid reported value, retained.
  - `T`: topcoded valid value, retained.
- All percentages and means are weighted.

## 2. Common Borrower/Saver Groups

The same group thresholds are used for the SCF and CEX after constructing the
source-specific credit-card debt and liquid-asset measures.

| Group | Operational definition |
|---|---|
| Borrow | Credit-card debt `> $500` and liquid assets `< $500` |
| Borrow and save | Credit-card debt `> $500` and liquid assets `>= $500` |
| Save | Credit-card debt `<= $500`, regardless of liquid assets |

An exact `$500` liquid-asset balance is therefore assigned to Borrow and save.
An exact `$500` credit-card balance is assigned to Save.

## 3. SCF Sample Selection

SCF restrictions are imposed at the household level across all five implicates.
A household is retained only when every implicate passes each applicable rule.

| Restriction | Implementation | Households remaining |
|---|---|---:|
| Parsed SCF public sample | Household identifier `YY1`; implicate is `Y1 - 10*YY1` | 4,442 |
| Age 25-64 | `X14` is between 25 and 64 in every implicate | 3,345 |
| Income floor | `X5729 >= $2,400` annually in every implicate | 3,312 |
| Complete income | No implicate has `J5729 == 1094`, the total-income refusal/no-bound code | 3,280 |
| Valid analysis information | Numeric income; a response among debt variables; a response among liquid-asset variables; nonmissing `X432`; no relevant SCF shadow code `>= 1000`; positive weight; exactly five implicates | 2,878 |

Paper target: **2,878 SCF households**. Replication: **2,878**.

### SCF identification and weight

| Construct | Variables | Formula |
|---|---|---|
| Household | `YY1` | `case_id = YY1` |
| Implicate | `Y1`, `YY1` | `implicate = Y1 - 10*YY1` |
| Analysis weight | `X42001` | `weight = X42001 / 5` so the five implicates jointly carry one household weight |

## 4. SCF Table 1 Variables

### Revolving credit-card debt

| Component | Variables | Construction |
|---|---|---|
| Bank/general-purpose card balance | `X413` | Balance still owed after the last payment |
| Store-card balance | `X421` | Balance still owed after the last payment |
| Raw card balance | `X413`, `X421` | `max(X413,0) + max(X421,0)` |
| Habitual revolver | `X432` | One if payoff frequency is `3` (sometimes) or `5` (hardly ever); zero otherwise |
| Grouping debt | Above variables | Raw card balance when habitual revolver is one; zero otherwise |

Thus a household that reports a balance but always or almost always pays the
total balance is treated as having zero revolving debt for group assignment.

### Liquid assets

| Component | Variables | Construction |
|---|---|---|
| Checking accounts | `X3506`, `X3510`, `X3514`, `X3518`, `X3522`, `X3526`, `X3529` | Sum of nonnegative balances |
| Savings accounts | `X3804`, `X3807`, `X3810`, `X3813`, `X3816`, `X3818` | Sum of nonnegative balances |
| Brokerage cash/call account | `X3930` | Nonnegative cash balance |
| Total liquid assets | All variables above | Checking + savings + brokerage cash |

### Credit-card interest rate

| Construct | Variable | Construction |
|---|---|---|
| Self-reported card APR | `X7132` | Interest rate on the card with the largest balance, coded as percent times 100 |
| Table 1 rate | `X7132` | Positive values divided by 100; zero and negative codes treated as a zero rate; missing remains missing; no upper cap |
| Missing-origin diagnostic | `J7132` | Retained for diagnostics but not used to drop observations in the final weighted mean |

The reported group rate is the weighted mean of this constructed percentage.

### SCF Table 1 targets and results

| Statistic | Group | Paper target | Replication |
|---|---|---:|---:|
| Puzzle size (%) | Borrow | 5.00 | 4.92 |
| Puzzle size (%) | Borrow and save | 27.00 | 26.76 |
| Puzzle size (%) | Save | 68.00 | 68.32 |
| Credit-card interest rate (%) | Borrow | 14.80 | 16.04 |
| Credit-card interest rate (%) | Borrow and save | 13.70 | 13.68 |
| Credit-card interest rate (%) | Save | 9.80 | 9.28 |

## 5. CEX Sample Selection

### Parsing and panel construction

- FMLI files use one copy of each calendar quarter from 2000Q1 through 2002Q4.
- CEX expense files contain the following year's Q1 as a boundary file. FNA,
  FN2, and FNB are filtered to the release year before stacking so that 2001Q1
  and 2002Q1 are not duplicated and summed twice.
- Full `NEWID` identifies an interview record. The last digit is interview
  number 2-5; removing it gives the longitudinal CU identifier.
- A complete 12-month public panel has all interviews 2, 3, 4, and 5.
- MTBI diagnostics confirmed that four public interviews correspond exactly to
  12 distinct nonmissing expenditure-reference months.

### CEX restrictions

| Restriction | Implementation | Households | Complete 12-month panels |
|---|---|---:|---:|
| Appendix cohort | Fifth interview implies a first interview in 2000Q2-2001Q1; equivalently, fifth interview is four quarters later | 7,667 | 5,480 |
| Age 25-64 | `AGE_REF` is between 25 and 64 at every observed interview | 5,413 | 3,881 |
| Income floor | Fifth-interview `FINCBTAX >= $2,400` annually | 4,512 | 3,292 |
| Complete income | Fifth-interview `RESPSTAT == 1` | 4,396 | 3,213 |
| Valid assets and card debt | Valid checking and savings amounts; no `B/C` flag on the fifth-interview card balance or annual finance charges | 2,953 | 2,206 |
| Positive weight | Fifth-interview `FINLWT21 > 0` | 2,953 | 2,206 |
| Partial-panel rule | Exclude panels whose only observed public interview is interview 5 | 2,758 | 2,206 |

Paper targets: **2,743 CEX households**, of which **2,164** are present for all
12 months. Replication: **2,758 households**, of which **2,206** are complete.

### CEX identification and weight

| Construct | Variables | Formula |
|---|---|---|
| Interview record | `NEWID` | Full value |
| Longitudinal CU | `NEWID` | Remove final interview digit |
| Interview number | `NEWID` | Final digit |
| Calendar quarter | Parsed file year/quarter | `year*10 + quarter` |
| Analysis weight | `FINLWT21` | `weight = FINLWT21 / 4` |

## 6. CEX Table 1 Variables

### Liquid assets

| Component | Variables | Construction |
|---|---|---|
| Checking and brokerage accounts | FMLI `CKBKACTX`, flag `CKBK_CTX` | Fifth-interview amount after flag treatment |
| Savings accounts | FMLI `SAVACCTX`, flag `SAVA_CTX` | Fifth-interview amount after flag treatment |
| Total liquid assets | Above variables | Checking/brokerage + savings |

Topcoded values (`T`) are retained. A household is dropped if either liquid
asset component is invalid (`B` or `C`).

### Revolving credit-card debt proxy

| Component | File and variables | Construction |
|---|---|---|
| Fifth-interview card balance | FNA: `CREDITR5 == 100`, `CREDITX5`, `CRED_TX5` | Sum of revolving-account balances; a `B/C` flag invalidates the household's debt information |
| Second-interview card balance | FN2: `CREDITR1 == 100`, `CREDITX1`, `CRED_TX1` | Sum after flag treatment; a `B/C` amount becomes missing and contributes zero to the sum but does not independently trigger sample exclusion |
| Annual finance charges | FNB: `CRDCARDX`, `CRDC_RDX` | Annual finance charges after flag treatment; a `B/C` flag invalidates the household's debt information |
| Average balance | Second and fifth balances | Mean when both are positive; otherwise the one positive balance; zero if neither is positive |
| Grouping debt | Average balance and finance charges | Average balance if annual finance charges are positive; zero otherwise |

The final preferred proxy uses positive finance charges to identify a revolver
and applies the `$500` debt threshold to the second/fifth average balance. The
declared 14 percent APR is not imposed as `finance charges / balance >= 0.14` in
the preferred result; those literal-APR variants were tested diagnostically and
fit the paper's Table 1 shares substantially less well.

When no matching FNA, FN2, or FNB item record exists, the corresponding amount
is set to zero. Requiring an explicit FNB record was tested and did not change
the preferred sample.

### CEX Table 1 targets and results

| Statistic | Group | Paper target | Replication |
|---|---|---:|---:|
| Puzzle size (%) | Borrow | 7.00 | 7.34 |
| Puzzle size (%) | Borrow and save | 29.00 | 29.53 |
| Puzzle size (%) | Save | 64.00 | 63.13 |

## 7. SCF Table 2 Variables

Each Table 2 entry is the weighted mean of a binary indicator within the three
SCF groups. The population column uses all 2,878 selected SCF households.

| Paper row | Preferred source | Exact coding |
|---|---|---|
| Race: white | SCF summary `RACE` | One when `RACE == 1`; zero otherwise |
| Marital status: married | Raw `X7372` and `X8023` | One when legally married (`X7372 == 1`), or when currently living with a partner (`X8023 == 2`) and legal status is divorced or widowed (`X7372` in `4,5`) |
| Have dependent children | SCF summary `FAMSTRUCT` | One when `FAMSTRUCT == 4`; this is the summary family-structure category for married/partnered households with children |
| Head works full-time | Raw `X4511` | One when head reports current work is full-time (`X4511 == 1`) |
| Head white-collar/prof. | SCF summary `OCCAT2` | One when `OCCAT2` is `1` or `2`; this is the preferred broad managerial/professional plus adjacent white-collar category proxy |
| Education: less than HS | SCF summary `EDCL` | One when `EDCL == 1` |
| HS/some college | SCF summary `EDCL` | One when `EDCL` is `2` or `3` |
| College degree or more | SCF summary `EDCL` | One when `EDCL == 4` |

The education summary therefore places four college years without a qualifying
degree according to the Federal Reserve's `EDCL` classification, rather than
automatically treating four reported years as college completion. Associate
degrees remain below the `EDCL == 4` college-degree-or-more category.

### SCF Table 2 targets and results

Each cell below is `paper target / replication`, in percent.

| Characteristic | Borrow | Borrow and save | Save | Population |
|---|---:|---:|---:|---:|
| Race: white | 70.0 / 70.6 | 78.0 / 79.8 | 74.0 / 74.2 | 75.0 / 75.5 |
| Marital status: married | 48.0 / 48.0 | 62.0 / 62.5 | 58.0 / 56.6 | 59.0 / 57.8 |
| Have dependent children | 45.0 / 42.5 | 41.0 / 43.8 | 39.0 / 37.1 | 40.0 / 39.2 |
| Head works full-time | 76.0 / 74.7 | 85.0 / 89.8 | 80.0 / 78.0 | 81.0 / 81.0 |
| Head white-collar/prof. | 48.0 / 50.0 | 61.0 / 66.9 | 58.0 / 57.6 | 58.0 / 59.7 |
| Education: less than HS | 13.0 / 13.7 | 5.0 / 4.2 | 13.0 / 14.9 | 11.0 / 12.0 |
| HS/some college | 73.0 / 73.2 | 61.0 / 63.7 | 51.0 / 52.6 | 55.0 / 56.6 |
| College degree or more | 14.0 / 13.1 | 33.0 / 32.1 | 36.0 / 32.5 | 34.0 / 31.4 |

## 8. Important Interpretation Notes

1. The SCF sample count is matched exactly, and its group shares nearly match
   exactly, which strongly supports the core SCF debt, asset, and sample rules.
2. CEX topcoded liquid assets are retained because `T` is a valid value and the
   appendix does not instruct us to exclude topcodes.
3. The CEX age rule is imposed throughout the observed panel. This is the most
   defensible tested interpretation of the paper's age restriction and moves
   the sample from 2,817/2,258 to 2,758/2,206.
4. CEX MTBI month coverage, exact three-month interview spacing, `RESPSTAT`
   versus `INCLASS` completeness, and explicit FNB record presence do not
   explain the remaining 15-household and 42-complete-panel gaps.
5. The SCF white-collar/professional row remains the least securely mapped
   Table 2 concept because the public occupation information is collapsed.
6. The CEX finance-charge proxy is empirically preferred but is not a literal
   implementation of the paper's 14 percent APR sentence.
