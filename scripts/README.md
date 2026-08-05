# Scripts

- `telyukova/main/`: maintained Telyukova replication scripts, including the
  Table 1/Table 2 work and the 2019 DCPC liquid-payment-share construction.
- `telyukova/exploratory/`: historical exploratory and variant scripts, grouped by whether they originally lived at the repo root or under `scripts/`.
- `telyukova/diagnostics/`: diagnostic checks and sample-rule investigations.
- `telyukova/data_prep/`: data extraction and parsing helpers.
- `liquid_consumption_weight/`: 2019 DCPC construction of liquid-payment
  shares for DCPC proxy categories and broader Telyukova/CEX groups.
- `liquid_consumption_shares/`: compact 2019 liquid-payment-share scripts.
  `2019_liquid_consumption_shares_by_payment_type.R` adds payment-instrument
  shares by consumer, their consumer mean, and pooled totals for both the
  selected liquid-consumption groups and the unrestricted payment sample.
- `preference_shock/main/`: maintained preference-shock replication scripts.
  `preference_shock_replication_2019_monthly.R` is the calendar-2019 monthly
  MTBI/CPI variant of the maintained quarterly construction.
  `preference_shock_volatility_2019_monthly.R` additionally downweights matched
  consumption components using the 2019 DCPC liquid-payment shares.
  `preference_shock_volatility_2018_2019_monthly.R` extends that regression to
  calendar years 2018–2019 and adds a year fixed effect.
- `preference_shock/data_prep/`: CEX parsing scripts for preference-shock
  analysis. `parse_cex_2019_interview_release.R` parses the five-quarter 2019
  Interview release into validated FMLI and monthly MTBI RDS files.
  `combine_cex_2018_interview_files.R` is the minimal 2018 fork used by the
  two-year volatility regression.
- `preference_shock/exploratory/`: preference-shock construction and tuning experiments.
