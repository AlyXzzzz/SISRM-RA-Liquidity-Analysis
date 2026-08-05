# 2019 DCPC liquid-payment category codebook

## Purpose

This document records how the 2019 Diary of Consumer Payment Choice (DCPC)
transactions are converted into liquid-payment shares for the expenditure
groups used in the Telyukova-style CEX consumption construction. It describes
the rules without requiring the reader to reconstruct them from the R code.

The maintained calculation is in
[`2019_liquid_consumption_weights.R`](../../scripts/liquid_consumption_weight/2019_liquid_consumption_weights.R).
The diagnostic-free version is in
[`2019_liquid_consumption_weights_compact.R`](../../scripts/liquid_consumption_weight/2019_liquid_consumption_weights_compact.R).
Both scripts produce the same final file:
[`shares_by_group_output.csv`](../../output/tables/dcpc_2019_liquid_payment_shares/shares_by_group_output.csv).

## Unit of observation and analysis sample

The transaction file has one row per reported transaction. It is joined to the
day file by `id + diary_day` to obtain the day-of-week weight `dow_weight`.

A transaction is retained when all of the following are true:

- `payment == 1`, so the record is a payment rather than income or another
  movement of funds.
- `diary_day` is 1, 2, or 3.
- The matched diary date is in October 2019.
- `dow_weight` is observed and positive.
- `amnt` is observed and positive.
- `pi` is a recognized payment-instrument code.
- The merchant and follow-up fields match one of the category rules below.

The category rules are mutually exclusive. Their order matters. In particular,
a bill-reminder transaction at merchant 6 or 11 is classified as household
operations before the general merchant-11 contractor rule is evaluated.

## Payment-instrument definitions

The narrow definition contains cash, check, and debit card. The broad
definition adds electronic methods funded directly from a bank account.

| `pi` | Payment instrument | Narrow liquid | Broad liquid | Treatment |
|---:|---|:---:|:---:|---|
| 0 | Multiple instruments | — | — | Excluded from count shares; parsed into instrument-specific dollar amounts |
| 1 | Cash | Yes | Yes | Liquid |
| 2 | Check | Yes | Yes | Liquid |
| 3 | Credit card | No | No | Credit instrument |
| 4 | Debit card | Yes | Yes | Liquid |
| 5 | Prepaid, gift, or EBT card | No | No | Funding source is not treated as liquid in the primary definitions |
| 6 | Bank-account-number payment | No | Yes | Added to the broad definition |
| 7 | Online banking bill payment | No | Yes | Added to the broad definition |
| 8 | Money order | No | No | Cash-like, but the funding source is unobserved |
| 10 | Residual mobile-app balance | No | No | Funding source is unobserved after DCPC recodes |
| 11 | Account-to-account transfer | No | Yes | Added to the broad definition |
| 13 | Other payment method | No | No | Unclassified |
| 14 | Deduction from income | No | No | Not treated as a liquid consumption payment |

For a multiple-instrument transaction, `multipi_breakdown` is parsed into
pieces such as `Cash: 20` and `Debit Card: 7.14`. The transaction enters a
dollar-share calculation only if every piece is identified and the parsed
amount is within $0.02 of the reported transaction total. Multiple-instrument
transactions do not enter count-share calculations because they have no unique
payment method.

## DCPC category rules and Telyukova-group aggregation

The `tely_category` column is the observable DCPC category. One or more of
these categories are then aggregated into the broader `tely_group` used by the
CEX weighting script.

| `tely_group` | DCPC category | Identification rule | Match quality and limitation |
|---|---|---|---|
| `food_alcohol_tobacco_proxy` | `food_grocery_proxy` | `merch == 1` | Grocery, convenience, and pharmacy merchant proxy; food, alcohol, tobacco, and pharmacy items cannot be separated |
| `food_alcohol_tobacco_proxy` | `food_restaurant_bar_proxy` | `merch == 3` | Sit-down restaurant or bar; food and alcohol cannot be separated |
| `food_alcohol_tobacco_proxy` | `food_fast_service_proxy` | `merch == 4` | Fast food, coffee shop, cafeteria, or food truck; merchant rather than purchased item is observed |
| `rent` | `rent` | `merch == 14` | Direct merchant match for rent; may contain some nonresidential building rent |
| `mortgage` | `mortgage_payment` | `merch == 15`, `pay010 == 2`, and `pay011 == 1` | Financial-provider payment identified as a mortgage; DCPC observes the full payment while the CEX consumption script uses mortgage interest |
| `utilities` | `utilities` | `merch == 8`, or `merch == 19`, `pay040 == 1`, and `pay041 == 1` | Nongovernment utility merchant or government payment identified as a utility |
| `utilities` | `communications_entertainment_proxy` | `merch == 10` | Phone, internet, cable, streaming, and movie theaters cannot be separated |
| `household_operations` | `household_operations_bill_proxy` | `from_bill_section == 1` and `merch` is 6 or 11 | Bill-reminder proxy for yard or housing maintenance. Merchant 6 is general services and merchant 11 is contractors. The public file does not retain the exact bill prompt, and the sample is small |
| `owned_dwelling_repairs_insurance_other` | `household_repairs` | `merch == 11` after removing the bill-reminder household-operations matches | Contractor, plumber, electrician, or HVAC merchant proxy; it does not capture every kind of repair |
| `owned_dwelling_repairs_insurance_other` | `homeowners_renters_insurance` | `merch == 15`, `pay010 == 3`, and `pay016` is 1 or 2 | Financial-provider insurance payment identified as homeowners or renters insurance |
| `property_taxes` | `property_taxes` | `merch == 19`, `pay040 == 2`, and `pay042 == 4` | Government tax payment identified as property tax; very small diary sample is expected |
| `public_transportation` | `public_transport_tolls` | `merch == 21`, or `merch == 19`, `pay040 == 1`, and `pay041` is 5 or 7 | Public transportation and tolls cannot always be separated |
| `public_transportation` | `taxi_air_delivery_proxy` | `merch == 9` | Taxi, airplane, or delivery merchant proxy; delivery cannot be separated from passenger transportation |
| `health_insurance` | `health_insurance` | `merch == 15`, `pay010 == 3`, and `pay016 == 3`; or `merch == 18` and `pay030 == 4`; or `merch == 19`, `pay040 == 1`, and `pay041 == 8` | Health-insurance payments identified through financial, medical, or government follow-ups |
| `childcare` | `childcare` | `merch == 20` and `pay020 == 3`; or `merch == 19`, `pay040 == 1`, and `pay041` is 3 or 9 | Childcare or daycare identified through education or government follow-ups; informal childcare paid to a person is generally not identified |
| `cash_contributions` | `cash_contributions` | `merch == 17` and `pay050` is 1 or 2 | Donation, offering, or tithe to a charitable or religious organization; gifts to individuals are not included |

The cash-contributions share is retained in the DCPC output for possible
sensitivity work. It is **not** applied in the maintained
[`preference_shock_volatility_2019_monthly.R`](../../scripts/preference_shock/main/preference_shock_volatility_2019_monthly.R),
because that script preserves the Telyukova–Visschers benchmark exclusion of
`CASHCO`.

## Share formulas

All weighted statistics use `dow_weight`.

- Weighted count share: weighted number of liquid, single-instrument payments
  divided by the weighted number of eligible single-instrument payments.
- Weighted dollar share: weighted liquid dollars divided by weighted eligible
  transaction dollars. Parsed multiple-instrument payments are included.
- Unweighted analogues are retained by the full diagnostic script.
- The preference-shock script uses `weighted_dollar_share_broad`, because it
  scales expenditure amounts and the broad definition includes modern
  checking-account-backed payment methods.

## Current 2019 group estimates

These estimates were regenerated after introducing the household-operations
proxy. Counts are unweighted sample counts; the final column is the weighted
broad liquid-dollar share used in the CEX mapping.

| `tely_group` | Transactions | Respondents | Broad weighted dollar share |
|---|---:|---:|---:|
| `cash_contributions` | 208 | 180 | 0.9074531 |
| `childcare` | 17 | 16 | 0.9322621 |
| `food_alcohol_tobacco_proxy` | 4,336 | 1,836 | 0.6433479 |
| `health_insurance` | 53 | 43 | 0.8547378 |
| `household_operations` | 22 | 20 | 0.8120219 |
| `mortgage` | 108 | 92 | 0.9718516 |
| `owned_dwelling_repairs_insurance_other` | 36 | 35 | 0.8537780 |
| `property_taxes` | 8 | 7 | 0.8293022 |
| `public_transportation` | 207 | 135 | 0.3405566 |
| `rent` | 99 | 82 | 0.9128200 |
| `utilities` | 819 | 556 | 0.8987864 |

## Mapping into the 2019 monthly CEX construction

| CEX component | DCPC `tely_group` |
|---|---|
| Food, alcohol, and tobacco | `food_alcohol_tobacco_proxy` |
| Rents | `rent` |
| Mortgage interest | `mortgage` |
| Owned-dwelling repairs, insurance, and other expenses | `owned_dwelling_repairs_insurance_other` |
| Utilities | `utilities` |
| Childcare UCCs separated from household operations | `childcare` |
| Residual household-operations UCCs | `household_operations` |
| Property taxes | `property_taxes` |
| Public transportation | `public_transportation` |
| Health insurance | `health_insurance` |

The share is joined to every UCC in its CEX component and applied once, before
UCC records are summed to the consumer-unit month. Food and property-tax
exclusion variants are then computed from these already weighted real amounts.

## Compact CEX parser

The maintained parser
[`parse_cex_2019_interview_release.R`](../../scripts/preference_shock/data_prep/parse_cex_2019_interview_release.R)
writes the parsed FMLI and MTBI files plus file-index, flag, panel-coverage, and
reference-window diagnostics. The compact parser
[`parse_cex_2019_interview_release_compact.R`](../../scripts/preference_shock/data_prep/parse_cex_2019_interview_release_compact.R)
keeps the structural checks but writes only:

- `output/cex/cex_2019_release_fmli_parsed.rds`
- `output/cex/cex_2019_release_mtbi_parsed.rds`

The full and compact parsers have been checked to produce semantically
identical FMLI and MTBI objects.
