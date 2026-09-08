# MTBI: Interview Survey monthly tabulation file. Contains many detailed monthly
# expenditure records per consumer unit-interview, organized by UCC.

# FMLI: Interview Survey family/consumer-unit file. Contains one row per
# consumer unit-interview, with household characteristics and controls.

# Compute the preference shock volatility, sd(eta_{it}), using 2018 and 2019.

library(tidyverse)
library(fixest)

# -----------------------------------------------------------------------------
# 1. Paths and analysis choices
# -----------------------------------------------------------------------------

analysis_calendar_years <- 2018:2019
zero_tolerance <- 1e-8

# Dollar shares are the relevant DCPC statistic for scaling CEX expenditure
# amounts. The broad definition includes cash, check, debit card, bank-account
# number payments, online banking bill payment, and account-to-account transfer.
# Change this value to another column in shares_by_group_output.csv to run a
# narrow-definition or count-share sensitivity check.

liquid_share_column <- "weighted_dollar_share_broad"

cex_fmli_paths <- file.path(
  "output", "cex",
  paste0("cex_", analysis_calendar_years, "_release_fmli_parsed.rds")
)
cex_mtbi_paths <- file.path(
  "output", "cex",
  paste0("cex_", analysis_calendar_years, "_release_mtbi_parsed.rds")
)
liquid_share_path <- file.path(
  "output", "tables", "dcpc_2019_liquid_payment_shares",
  "shares_by_group_output.csv"
)
output_dir <- file.path(
  "output", "tables", "preference_shock_volatility_2018_2019_monthly"
)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

to_num <- function(x) {
  suppressWarnings(parse_number(
    as.character(x),
    na = c("", ".", "NA", "NaN")
  ))
}

# Helper functions that convert year/month to and from an index value 
month_index <- function(year, month) {
  as.integer(year) * 12 + as.integer(month)
}

month_from_index <- function(index) {
  tibble(
    ref_year = as.integer((index - 1) %/% 12),
    ref_month = as.integer(((index - 1) %% 12) + 1)
  )
}

# Helper function that fetches the corresponding FRED CPI data for consumption 
# good 

fetch_fred <- function(series_id) {
  url <- paste0(
    "https://fred.stlouisfed.org/graph/fredgraph.csv?id=", series_id
  )
  tmp <- tempfile(fileext = ".csv")
  status <- system2(
    "curl",
    c("-fsSL", "--connect-timeout", "15", "--max-time", "60", url, "-o", tmp)
  )
  if (as.integer(status) != 0) {
    stop("Could not download FRED series: ", series_id, call. = FALSE)
  }

  fred_raw <- read_csv(tmp, show_col_types = FALSE)
  date_column <- intersect(c("observation_date", "DATE", "date"), names(fred_raw))[1]
  if (is.na(date_column) || !series_id %in% names(fred_raw)) {
    stop("Unexpected FRED CSV fields for series: ", series_id, call. = FALSE)
  }

  fred_raw %>%
    transmute(
      series_id = series_id,
      date = as.Date(.data[[date_column]]),
      year = as.integer(format(date, "%Y")),
      month = as.integer(format(date, "%m")),
      cpi = to_num(.data[[series_id]])
    )
}

# Construct rows of the consumption crosswalk, including the detailed category,
# broader component, sensitivity-exclusion group, CPI series, and CEX UCCs.

ucc_rows <- function(detail, component, exclusion_group, fred_series, codes) {
  tibble(
    detail = detail,
    component = component,
    exclusion_group = exclusion_group,
    fred_series = fred_series,
    ucc = as.character(codes)
  )
}

# -----------------------------------------------------------------------------
# 2. MTBI translation of the maintained cash-good basket
# -----------------------------------------------------------------------------

# This catalog translates the expenditure components in the maintained
# FMLI-based construction in preference_shock_replication.R into monthly MTBI
# UCCs. It preserves that construction's consumption-basket exclusions.
#
# The component_liquid_group crosswalk below separately maps these CEX
# components to the DCPC/Telyukova groups used to assign liquid-payment shares.
# household furnishings/equipment, cash contributions, non-health insurance and
# pensions, and mortgage principal are not included. Reported gifts are removed
# below. Domestic services/childcare are not added on top of the broader
# household-operations block. The identifiable childcare UCCs within that block
# are separated below only so the DCPC childcare share can be applied to them.

ucc_catalog <- bind_rows(
  ucc_rows(
    "food_total", "food", "food_alcohol_tobacco", "CPIUFDNS",
    c(
      "190904", "790220", "790230", "190901", "190902", "190903",
      "790410", "790430", "800700"
    )
  ),
  ucc_rows(
    "alcohol", "alcohol", "food_alcohol_tobacco", "CUUR0000SAF116",
    c("200900", "790310", "790320", "790420")
  ),
  ucc_rows(
    "tobacco", "tobacco", "food_alcohol_tobacco", "CUUR0000SEGA",
    c("630110", "630210")
  ),
  ucc_rows(
    "rent_excluding_as_pay", "rents", "other", "CUUR0000SEHA",
    c(
      "210110", "230121", "230141", "230150", "240111", "240121",
      "240211", "240221", "240311", "240321", "320611", "320621",
      "320631", "350110", "790690", "990920"
    )
  ),
  ucc_rows(
    "rent_as_pay", "rents", "other", "CUUR0000SEHA", "800710"
  ),
  ucc_rows(
    "mortgage_interest", "mortgages", "other", "CUUR0000SEHC",
    c("220311", "220313", "220321", "880110")
  ),
  ucc_rows(
    "owned_dwelling_repairs_insurance_other",
    "owned_dwelling_repairs_insurance_other", "other", "CUUR0000SAH3",
    c(
      "210901", "220121", "220901", "230112", "230113", "230114",
      "230115", "230122", "230142", "230151", "230901", "240112",
      "240122", "240212", "240213", "240222", "240312", "240322",
      "320612", "320622", "320632", "340911", "990930"
    )
  ),
  ucc_rows(
    "utilities", "utilities", "other", "CUUR0000SAH2",
    c(
      "260211", "260212", "260213", "260214", "260111", "260112",
      "260113", "260114", "250111", "250112", "250113", "250114",
      "250211", "250212", "250213", "250214", "250221", "250222",
      "250223", "250224", "250901", "250902", "250903", "250904",
      "270101", "270102", "270103", "270104", "270211", "270212",
      "270213", "270214", "270411", "270412", "270413", "270414",
      "270901", "270902", "270903", "270904"
    )
  ),
  ucc_rows(
    "household_operations_total", "household_operations", "other",
    "CUUR0000SAH3",
    c(
      "340310", "340410", "340420", "340520", "340530", "340903",
      "340906", "340910", "340914", "340915", "330511", "340510",
      "340620", "340630", "340901",
      "340907", "340908", "690113", "690114", "990900"
    )
  ),
  ucc_rows(
    "childcare_within_household_operations", "childcare", "other",
    "CUUR0000SAH3", c("340211", "340212", "670310")
  ),
  ucc_rows(
    "property_taxes", "property_taxes", "property_taxes", "CUUR0000SEHC",
    "220211"
  ),
  ucc_rows(
    "public_transportation", "public_transportation", "other",
    "CUUR0000SETG",
    c(
      "530110", "530210", "530312", "530411", "530510", "530901",
      "530311", "530412", "530902"
    )
  ),
  ucc_rows(
    "health_insurance", "health_insurance", "other", "CUUR0000SAM2",
    c(
      "580111", "580112", "580113", "580114", "580311", "580312",
      "580901", "580903", "580904", "580905", "580906"
    )
  )
)

# Map each CEX component to the DCPC group used to estimate its liquid-payment
# share.

# Cash contributions remain excluded to preserve the maintained
# Telyukova-Visschers CEX benchmark. Their DCPC share is available only for a
# separately labeled sensitivity construction.

component_liquid_group <- tribble(
  ~component, ~tely_group,
  "food", "food_alcohol_tobacco_proxy",
  "alcohol", "food_alcohol_tobacco_proxy",
  "tobacco", "food_alcohol_tobacco_proxy",
  "rents", "rent",
  "mortgages", "mortgage",
  "owned_dwelling_repairs_insurance_other",
    "owned_dwelling_repairs_insurance_other",
  "utilities", "utilities",
  "childcare", "childcare",
  "household_operations", "household_operations",
  "property_taxes", "property_taxes",
  "public_transportation", "public_transportation",
  "health_insurance", "health_insurance"
)

# Read the already computed liquid consumption shares file

liquid_shares <- read_csv(
  liquid_share_path,
  col_types = cols(tely_group = col_character(), .default = col_double()),
  show_col_types = FALSE
)

# Select the Telyukova group column and the predefined liquid share column we
# are using (weighted, broad liquid definition in this script) from the liquid 
# consumption shares file. 

liquid_shares_selected <- liquid_shares %>%
  transmute(
    tely_group,
    liquid_payment_share = .data[[liquid_share_column]]
  )

# Join the liquid-payment shares to their corresponding CEX components.

component_weight_mapping <- component_liquid_group %>%
  left_join(
    liquid_shares_selected,
    by = "tely_group",
    relationship = "many-to-one"
  ) %>%
  mutate(liquid_payment_share = coalesce(liquid_payment_share, 1))

# Join weights to UCC catalog for later analysis

ucc_catalog <- ucc_catalog %>%
  left_join(
    component_weight_mapping,
    by = "component",
    relationship = "many-to-one"
  )

# -----------------------------------------------------------------------------
# 3. Monthly CPI deflators
# -----------------------------------------------------------------------------

fred_series <- unique(ucc_catalog$fred_series)

# Use each series' 2019 mean as the real-value base

cpi <- bind_rows(lapply(fred_series, fetch_fred)) %>%
  group_by(series_id) %>%
  mutate(cpi_base_2019 = mean(cpi[year == 2019], na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(cpi_norm = cpi / cpi_base_2019)

# -----------------------------------------------------------------------------
# 4. FMLI controls and exact monthly panel skeleton
# -----------------------------------------------------------------------------

fmli <- bind_rows(lapply(cex_fmli_paths, readRDS))
mtbi <- bind_rows(lapply(cex_mtbi_paths, readRDS))

# EARNINCX was removed after the 2013 income redesign. Its 2019-equivalent
# earned-income control is wage/salary income plus self-employment income.

fmli_controls <- fmli %>%
  transmute(
    archive_year,
    newid,
    panel_id,
    interview,
    interview_year,
    interview_month,
    interview_month_index,
    age = to_num(AGE_REF),
    age2 = age^2,
    age3 = age^3,
    education = as.factor(EDUC_REF),
    marital_status = as.factor(MARITAL1),
    race = as.factor(REF_RACE),
    earnings_annual = to_num(FSALARYX) + to_num(FSMPFRMX),
    income_before_tax = to_num(FINCBTAX),
    family_size = to_num(FAM_SIZE),
    homeownership = as.factor(CUTENURE)
  )

# Compute and bind the reference month indices to the FMLI data, so they match
# structure of MTBI data for downstream joining. 

monthly_skeleton <- fmli_controls %>%
  crossing(reference_lag = -3:-1) %>%
  mutate(ref_month_index = interview_month_index + reference_lag) %>%
  bind_cols(month_from_index(.$ref_month_index)) %>%
  filter(
    ref_year %in% analysis_calendar_years,
    archive_year == ref_year
  ) %>%
  arrange(panel_id, ref_month_index, interview)

# -----------------------------------------------------------------------------
# 5. Monthly nominal and real cash-good consumption
# -----------------------------------------------------------------------------

mtbi_analysis <- mtbi %>%
  filter(
    is_standard_reference_window,
    ref_year %in% analysis_calendar_years,
    archive_year == ref_year,
    !is_gift
  )

# Join all analysis data frames

mtbi_cash_real <- mtbi_analysis %>%
  inner_join(ucc_catalog, by = "ucc", relationship = "many-to-one") %>%
  inner_join(
    monthly_skeleton %>%
      select(archive_year, newid, ref_year, ref_month, ref_month_index),
    by = c(
      "archive_year", "newid", "ref_year", "ref_month", "ref_month_index"
    ),
    relationship = "many-to-one"
  ) %>%
  left_join(
    cpi,
    by = c(
      "fred_series" = "series_id",
      "ref_year" = "year",
      "ref_month" = "month"
    ),
    relationship = "many-to-one"
  ) %>%
  mutate(
    nominal = cost,
    real_unweighted = nominal / cpi_norm,
    liquid_nominal = nominal * liquid_payment_share,
    liquid_real = real_unweighted * liquid_payment_share,
    # Downstream measures sum `real`, so the DCPC share is applied exactly once
    # here, before the household-month aggregation.
    real = liquid_real
  )

# Compute the three consumption specifications for each consumer-unit month.
# We also retain ref_month_index for the subsequent join.

monthly_cash <- mtbi_cash_real %>%
  group_by(archive_year, newid, ref_year, ref_month, ref_month_index) %>%
  summarise(
    cash_good_benchmark_real = sum(real, na.rm = TRUE),
    cash_good_excluding_food_real =
      sum(real[exclusion_group != "food_alcohol_tobacco"], na.rm = TRUE),
    cash_good_excluding_food_property_taxes_real =
      sum(
        real[
          !exclusion_group %in% c("food_alcohol_tobacco", "property_taxes")
        ],
        na.rm = TRUE
      ),
    .groups = "drop"
  )

# Join the computed monthly consumption data back to the FMLI skeleton

monthly_panel <- monthly_skeleton %>%
  left_join(
    monthly_cash,
    by = c(
      "archive_year", "newid", "ref_year", "ref_month", "ref_month_index"
    ),
    relationship = "one-to-one"
  ) %>%
  
  # Explicitly handle missing consumption values for months that do not have 
  # computed consumption amounts (set to 0), and retain some metadata.
  mutate(
    across(starts_with("cash_good_"), ~ replace_na(.x, 0)),
    analysis_frequency = "monthly",
    analysis_calendar_years = paste(analysis_calendar_years, collapse = "-"),
    consumption_weighting = liquid_share_column
  )

# -----------------------------------------------------------------------------
# 6. Monthly FE residuals and monthly AR(1) innovations
# -----------------------------------------------------------------------------

long_panel <- monthly_panel %>%
  pivot_longer(
    cols = starts_with("cash_good_"),
    names_to = "measure",
    values_to = "liquid_consumption"
  ) %>%
  mutate(
    measure = recode(
      measure,
      cash_good_benchmark_real = "benchmark",
      cash_good_excluding_food_real = "excluding_food",
      cash_good_excluding_food_property_taxes_real =
        "excluding_food_property_taxes"
    ),
    positive_liquid_consumption =
      !is.na(liquid_consumption) & liquid_consumption > zero_tolerance,
    log_liquid_consumption = if_else(
      positive_liquid_consumption,
      log(pmax(liquid_consumption, .Machine$double.xmin)),
      NA_real_
    )
  )

measure_order <- c(
  "benchmark",
  "excluding_food",
  "excluding_food_property_taxes"
)

# Run both regressions for each consumption specification.

estimate_measure <- function(data, measure_name) {
  reg_data <- data %>%
    filter(
      measure == measure_name,
      positive_liquid_consumption
    ) %>%
    group_by(panel_id) %>%
    filter(n() >= 2) %>%
    ungroup()

  # Month-of-year and year fixed effects control for seasonality and common
  # annual shifts in the two-year sample.
  
  first_stage <- feols(
    log_liquid_consumption ~
      age + age2 + age3 +
      i(education) + i(marital_status) + i(race) +
      earnings_annual + family_size + i(homeownership) +
      i(ref_month) + i(ref_year) |
      panel_id,
    data = reg_data,
    notes = FALSE,
    warn = FALSE
  )

  # Direct sample variance of the first-stage residual. Conditional on the
  # included controls and consumer-unit fixed effects, this is the empirical
  # variance of log liquid consumption left unexplained by the first stage.
  first_stage_residual_variance <- var(resid(first_stage))

  # Second regression data frame with explicit handling of missing values and 
  # non-consecutive indices
  ar_data <- reg_data %>%
    mutate(epsilon = resid(first_stage)) %>%
    arrange(panel_id, ref_month_index, interview) %>%
    group_by(panel_id) %>%
    mutate(
      epsilon_lag = lag(epsilon),
      ref_month_index_lag = lag(ref_month_index),
      consecutive_month_lag = ref_month_index - ref_month_index_lag == 1
    ) %>%
    ungroup() %>%
    filter(
      consecutive_month_lag,
      !is.na(epsilon),
      !is.na(epsilon_lag)
    )

  # Second Regression
  ar_model <- feols(
    epsilon ~ 0 + epsilon_lag,
    data = ar_data,
    notes = FALSE,
    warn = FALSE
  )

  innovations <- ar_data %>%
    mutate(
      eta = resid(ar_model),
      measure = measure_name
    ) %>%
    select(
      panel_id, newid, interview, ref_year, ref_month, ref_month_index,
      ref_month_index_lag,
      measure, liquid_consumption, log_liquid_consumption,
      epsilon, epsilon_lag, eta
    )

  alpha_hat <- unname(coef(ar_model)[["epsilon_lag"]])
  eta_sd_log_points <- sd(innovations$eta)
  ar1_implied_residual_variance <-
    eta_sd_log_points^2 / (1 - alpha_hat^2)

  result <- tibble(
    frequency = "monthly",
    calendar_years = paste(analysis_calendar_years, collapse = "-"),
    liquid_share_column = liquid_share_column,
    measure = measure_name,
    n_stage1 = nrow(reg_data),
    n_ar = nrow(ar_data),
    n_households_stage1 = n_distinct(reg_data$panel_id),
    n_households_ar = n_distinct(ar_data$panel_id),
    rho = alpha_hat,
    eta_sd_log_points = eta_sd_log_points,
    eta_sd_percent = 100 * eta_sd_log_points,
    first_stage_residual_variance = first_stage_residual_variance,
    ar1_implied_residual_variance = ar1_implied_residual_variance,
    residual_variance_gap =
      first_stage_residual_variance - ar1_implied_residual_variance,
    residual_variance_relative_gap =
      residual_variance_gap / ar1_implied_residual_variance
  )

  list(
    result = result,
    innovations = innovations,
    first_stage = first_stage,
    ar_model = ar_model
  )
}

fits <- set_names(
  lapply(measure_order, function(x) estimate_measure(long_panel, x)),
  measure_order
)

results <- bind_rows(lapply(fits, `[[`, "result")) %>%
  mutate(measure = factor(measure, levels = measure_order)) %>%
  arrange(measure) %>%
  mutate(measure = as.character(measure))

innovations <- bind_rows(lapply(fits, `[[`, "innovations")) %>%
  arrange(match(measure, measure_order), panel_id, ref_month_index)

key_results <- results %>%
  select(
    "measure", "rho", "eta_sd_log_points", "eta_sd_percent",
    "first_stage_residual_variance", "ar1_implied_residual_variance",
    "residual_variance_gap", "residual_variance_relative_gap"
  )

# Verify the maintained benchmark against the unrounded empirical values used
# to construct the existing calibration target. The direct first-stage sample
# variance and the AR(1)-implied stationary variance are reported separately:
# they estimate the same population object under the maintained AR(1), but they
# need not be exactly identical in a finite, unbalanced panel.
empirical_alpha <- 0.1405246873167233
empirical_eta_sd <- 0.27136654798584364
log_c_var_target <- empirical_eta_sd^2 / (1 - empirical_alpha^2)
variance_check_tolerance <- 1e-12

benchmark_variance_check <- results %>%
  filter(measure == "benchmark") %>%
  transmute(
    estimated_alpha = rho,
    supplied_alpha = .env$empirical_alpha,
    estimated_eta_sd = eta_sd_log_points,
    supplied_eta_sd = .env$empirical_eta_sd,
    direct_first_stage_residual_variance = first_stage_residual_variance,
    ar1_implied_residual_variance = ar1_implied_residual_variance,
    supplied_log_c_var_target = .env$log_c_var_target,
    ar1_implied_minus_supplied_target =
      ar1_implied_residual_variance - supplied_log_c_var_target,
    direct_minus_ar1_implied =
      direct_first_stage_residual_variance - ar1_implied_residual_variance,
    direct_relative_gap =
      direct_minus_ar1_implied / ar1_implied_residual_variance,
    ar1_implied_matches_supplied_target =
      abs(ar1_implied_minus_supplied_target) <= .env$variance_check_tolerance,
    direct_matches_ar1_implied_exactly =
      abs(direct_minus_ar1_implied) <= .env$variance_check_tolerance
  )

stopifnot(benchmark_variance_check$ar1_implied_matches_supplied_target)
print(benchmark_variance_check, width = Inf)

# -----------------------------------------------------------------------------
# 7. Outputs
# -----------------------------------------------------------------------------

write_rds(
  long_panel,
  file.path(output_dir, "cash_good_consumption_panel_2018_2019_monthly_long.rds")
)
write_csv(
  long_panel,
  file.path(
    output_dir, "cash_good_consumption_panel_2018_2019_monthly_long.csv.gz"
  )
)
write_rds(
  innovations,
  file.path(output_dir, "preference_shock_innovations_2018_2019_monthly.rds")
)
write_csv(
  innovations,
  file.path(
    output_dir, "preference_shock_innovations_2018_2019_monthly.csv.gz"
  )
)
write_csv(
  results,
  file.path(
    output_dir, "preference_shock_volatility_2018_2019_monthly_full.csv"
  )
)
write_csv(
  key_results,
  file.path(output_dir, "preference_shock_volatility_2018_2019_monthly.csv")
)
write_csv(
  benchmark_variance_check,
  file.path(output_dir, "benchmark_residual_variance_check.csv")
)
