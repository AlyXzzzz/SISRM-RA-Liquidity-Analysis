library(dplyr)
library(fixest)
library(readr)
library(stringr)
library(tidyr)
library(tibble)


# Resolve paths from the repository root even when this script is run from its
# organized subfolder.
script_args <- commandArgs(trailingOnly = FALSE)
script_file <- sub("^--file=", "", script_args[grepl("^--file=", script_args)][1])
if (!is.na(script_file)) {
  script_dir <- dirname(normalizePath(script_file))
  setwd(normalizePath(file.path(script_dir, "..", "..", "..")))
}

cex_fmli_path <- file.path("output", "cex", "cex_fmli_2000_2002_raw.rds")

zero_tolerance <- 1e-8

# Convert CEX/FRED columns that may arrive as strings with commas.
to_num <- function(x) {
  suppressWarnings(as.numeric(str_replace_all(as.character(x), ",", "")))
}

qyear_to_index <- function(year, quarter) {
  year * 4 + quarter
}

# Helper function that correctly adds and subtracts month from year - used 
# when we average CPI over the previous 3 months
month_add <- function(year, month, k) {
  index <- year * 12 + month + k
  tibble(
    year = (index - 1) %/% 12,
    month = ((index - 1) %% 12) + 1
  )
}

# GPT Function for extracting FRED data for CPI deflation

fetch_fred <- function(series_id) {
  url <- paste0("https://fred.stlouisfed.org/graph/fredgraph.csv?id=", series_id)
  tmp <- tempfile(fileext = ".csv")
  status <- system2(
    "curl",
    c("-fsSL", "--connect-timeout", "15", "--max-time", "60", url, "-o", tmp)
  )
  if (!identical(as.integer(status), 0L)) {
    stop("Could not download FRED series: ", series_id, call. = FALSE)
  }

  read_csv(tmp, show_col_types = FALSE) %>%
    rename(date = observation_date, cpi = all_of(series_id)) %>%
    mutate(
      series_id = series_id,
      date = as.Date(date),
      year = as.integer(format(date, "%Y")),
      month = as.integer(format(date, "%m")),
      cpi = to_num(cpi)
    ) %>%
    select(series_id, year, month, cpi)
}

cex_fmli_raw <- readRDS(cex_fmli_path)

# For external file path, reading the RDS file directly
# cex_fmli_raw <- readRDS("cex_fmli_2000_2002_raw.rds")

names(cex_fmli_raw) <- toupper(names(cex_fmli_raw))

# Benchmark liquid-consumption construction:
# All variables are FMLI summary variables measured as previous quarter plus
# current quarter (PQ + CQ), then deflated using the component CPI series below.
# We measure PQ + CQ because the interview says the reflection period is 3 
# months prior, so it may cross two different calendar quarters. 
#
# Included in the benchmark:
#   FOODCQ/FOODPQ       food at home and away from home
#   ALCBEVCQ/ALCBEVPQ   alcoholic beverages
#   TOBACCCQ/TOBACCPQ   tobacco products and smoking supplies
#   RENDWECQ/RENDWEPQ   rent for rented dwellings
#   MRTINTCQ/MRTINTPQ   mortgage interest
#   MRPINSCQ/MRPINSPQ   owned-dwelling maintenance, repairs, insurance, other
#   UTILCQ/UTILPQ       utilities, fuels, and public services
#   HOUSOPCQ/HOUSOPPQ   household operations
#   PROPTXCQ/PROPTXPQ   property taxes
#   PUBTRACQ/PUBTRAPQ   public transportation
#   HLTHINCQ/HLTHINPQ   health insurance
#
# Excluded from the benchmark construction:
#   HOUSEQCQ/HOUSEQPQ   household furnishings/equipment, treated as durables
#   DOMSRVCQ/DOMSRVPQ   domestic services, not added separately to HOUSOP
#   BBYDAYCQ/BBYDAYPQ   babysitting/childcare, not added separately to HOUSOP
#   CASHCOCQ/CASHCOPQ   cash contributions
#   PERINSCQ/PERINSPQ   personal insurance and pensions
#   LIFINSCQ/LIFINSPQ   life and other personal insurance
#   VEHINSCQ/VEHINSPQ   vehicle insurance
#   mortgage principal outlays, not included in this interest-expense measure
#
# The second Table 2 row removes food, alcohol, and tobacco. The third row also
# removes property taxes.

cash_good_components <- tribble(
  ~component, ~exclusion_group, ~cq_variable, ~pq_variable, ~fred_series,
  "food", "food_alcohol_tobacco", "FOODCQ", "FOODPQ", "CPIUFDNS",
  "alcohol", "food_alcohol_tobacco", "ALCBEVCQ", "ALCBEVPQ", "CUUR0000SAF116",
  "tobacco", "food_alcohol_tobacco", "TOBACCCQ", "TOBACCPQ", "CUUR0000SEGA",
  "rents", "other", "RENDWECQ", "RENDWEPQ", "CUUR0000SEHA",
  "mortgages", "other", "MRTINTCQ", "MRTINTPQ", "CUUR0000SEHC",
  "owned_dwelling_repairs_insurance_other", "other", "MRPINSCQ", "MRPINSPQ", "CUUR0000SAH3",
  "utilities", "other", "UTILCQ", "UTILPQ", "CUUR0000SAH2",
  "household_operations", "other", "HOUSOPCQ", "HOUSOPPQ", "CUUR0000SAH3",
  "property_taxes", "property_taxes", "PROPTXCQ", "PROPTXPQ", "CUUR0000SEHC",
  "public_transportation", "other", "PUBTRACQ", "PUBTRAPQ", "CUUR0000SETG",
  "health_insurance", "other", "HLTHINCQ", "HLTHINPQ", "CUUR0000SAM2"
)

component_vars <- unique(c(cash_good_components$cq_variable, cash_good_components$pq_variable))
fred_series <- unique(cash_good_components$fred_series)

# CPI crosswalk uses monthly, not-seasonally-adjusted FRED CPI-U series. Each
# component is normalized by its own 2001 mean, then averaged over the three
# months prior to the CEX interview month, matching the CEX collection window.

# Normalize to 2001 mean
cpi <- bind_rows(lapply(fred_series, fetch_fred)) %>%
  group_by(series_id) %>%
  mutate(cpi_base_2001 = mean(cpi[year == 2001], na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(cpi_norm = cpi / cpi_base_2001)

# Extract the CEX variables from the FMLI dataframe
cex_panel <- cex_fmli_raw %>%
  mutate(
    row_id = row_number(), 
    newid = as.character(NEWID), # NEWID = consumer-unit id + interview number
    case_id = str_sub(newid, 1L, -2L),
    interview = to_num(str_sub(newid, -1L, -1L)),
    cex_year = to_num(CEX_YEAR),
    cex_quarter = to_num(CEX_QUARTER),
    qyear_index = qyear_to_index(cex_year, cex_quarter),
    qintrv_month = to_num(QINTRVMO),
    qintrv_year = to_num(QINTRVYR),
    age = to_num(AGE_REF),
    age2 = age^2,
    age3 = age^3,
    education = as.factor(EDUC_REF),
    marital_status = as.factor(MARITAL1),
    race = as.factor(REF_RACE),
    earnings_annual = to_num(EARNINCX),
    income_before_tax = to_num(FINCBTAX),
    family_size = to_num(FAM_SIZE),
    homeownership = as.factor(CUTENURE)
  ) %>%
  select(
    row_id, newid, case_id, interview, cex_year, cex_quarter,
    qyear_index, qintrv_month, qintrv_year,
    age, age2, age3, education, marital_status, race, earnings_annual,
    income_before_tax, family_size, homeownership,
    all_of(component_vars)
  )

# Compute the 3-month prior to interview month CPI average deflator
cpi_deflator <- cex_panel %>%
  distinct(qintrv_year, qintrv_month) %>%
  crossing(fred_series = fred_series) %>%
  expand_grid(lag_month = -3:-1) %>%
  rowwise() %>%
  mutate(month_add(qintrv_year, qintrv_month, lag_month)) %>%
  ungroup() %>%
  left_join(
    cpi,
    by = c("fred_series" = "series_id", "year" = "year", "month" = "month")
  ) %>%
  group_by(qintrv_year, qintrv_month, fred_series) %>%
  summarise(cpi_norm = mean(cpi_norm, na.rm = TRUE), .groups = "drop")

# Sum previous-quarter and current-quarter calendar components, then deflate.
component_real <- bind_rows(lapply(seq_len(nrow(cash_good_components)), function(i) {
  row <- cash_good_components[i, ]

  tibble(
    row_id = cex_panel$row_id,
    qintrv_year = cex_panel$qintrv_year,
    qintrv_month = cex_panel$qintrv_month,
    component = row$component,
    exclusion_group = row$exclusion_group,
    fred_series = row$fred_series,
    nominal =
      to_num(cex_panel[[row$cq_variable]]) +
      to_num(cex_panel[[row$pq_variable]])
  )
})) %>%
  left_join(cpi_deflator, by = c("qintrv_year", "qintrv_month", "fred_series")) %>%
  mutate(real = nominal / cpi_norm)

# Collapse the cash goods to one row per interview household and compute the 
# three table 2 benchmarks 
cash_good_real <- component_real %>%
  group_by(row_id) %>%
  summarise(
    cash_good_benchmark_real = sum(real, na.rm = TRUE),
    cash_good_excluding_food_real =
      sum(real[exclusion_group != "food_alcohol_tobacco"], na.rm = TRUE),
    cash_good_excluding_food_property_taxes_real =
      sum(real[!exclusion_group %in% c("food_alcohol_tobacco", "property_taxes")], na.rm = TRUE),
    .groups = "drop"
  )

cex_panel <- cex_panel %>%
  left_join(cash_good_real, by = "row_id")

# Variables for the X-vector regressors
control_vars <- c(
  "age", "age2", "age3", "education", "marital_status", "race",
  "earnings_annual", "family_size", "homeownership",
  "qintrv_month", "qintrv_year"
)

# Pivot to long for regression, and create filtering variables for analysis
long_panel <- cex_panel %>%
  select(
    newid, case_id, interview, cex_year, cex_quarter, qyear_index,
    qintrv_month, qintrv_year,
    all_of(control_vars),
    cash_good_benchmark_real,
    cash_good_excluding_food_real,
    cash_good_excluding_food_property_taxes_real
  ) %>%
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
    ),
    complete_controls = if_all(all_of(control_vars), ~ !is.na(.x))
  )

output_dir <- file.path("output", "tables", "preference_shock_adjacent_correlation")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Keep the constructed panel for downstream moment-matching scripts.
write_rds(
  long_panel,
  file.path(output_dir, "cash_good_consumption_panel_long.rds")
)
write_csv(
  long_panel,
  file.path(output_dir, "cash_good_consumption_panel_long.csv.gz")
)

reg_data <- long_panel %>%
  filter(
    measure == "benchmark",
    positive_liquid_consumption,
    complete_controls
  ) %>%
  group_by(case_id) %>%
  filter(n() >= 2) %>%
  ungroup()

# First Regression - back out idiosyncratic component of liquid consumption
liquid_consumption_fe <- feols(
  log_liquid_consumption ~
    age + age2 + age3 +
    i(education) + i(marital_status) + i(race) +
    earnings_annual + family_size + i(homeownership) +
    i(qintrv_month) + i(qintrv_year) |
    case_id,
  data = reg_data
)

# Create lags of the idiosyncratic component for AR(1) regression
reg_data <- reg_data %>%
  mutate(epsilon = resid(liquid_consumption_fe)) %>%
  arrange(case_id, qyear_index, interview) %>%
  group_by(case_id) %>%
  mutate(
    epsilon_lag = lag(epsilon),
    qyear_index_lag = lag(qyear_index),
    consecutive_lag = qyear_index - qyear_index_lag == 1
  ) %>%
  ungroup()

ar_data <- reg_data %>%
  filter(consecutive_lag, !is.na(epsilon), !is.na(epsilon_lag))

# Regression for idiosyncratic component, residual is innovation which matches 
# liquidity preference shock volatility

ar1_regression <- feols(
  epsilon ~ 0 + epsilon_lag,
  data = ar_data
)

ar_data <- ar_data %>%
  mutate(eta = resid(ar1_regression))

print(liquid_consumption_fe)
print(ar1_regression)
cat("sd(eta_it):", sd(ar_data$eta), "\n")
cat("sd(eta_it), percent:", 100 * sd(ar_data$eta), "\n")


# Sensitivity check regressions from Table 2
# Row 2 uses the same two-step FE + AR(1) procedure after excluding food,
# alcohol, and tobacco from liquid consumption. Row 3 also excludes property
# taxes.

reg_data_excluding_food <- long_panel %>%
  filter(
    measure == "excluding_food",
    positive_liquid_consumption,
    complete_controls
  ) %>%
  group_by(case_id) %>%
  filter(n() >= 2) %>%
  ungroup()

liquid_consumption_fe_excluding_food <- feols(
  log_liquid_consumption ~
    age + age2 + age3 +
    i(education) + i(marital_status) + i(race) +
    earnings_annual + family_size + i(homeownership) +
    i(qintrv_month) + i(qintrv_year) |
    case_id,
  data = reg_data_excluding_food
)

reg_data_excluding_food <- reg_data_excluding_food %>%
  mutate(epsilon = resid(liquid_consumption_fe_excluding_food)) %>%
  arrange(case_id, qyear_index, interview) %>%
  group_by(case_id) %>%
  mutate(
    epsilon_lag = lag(epsilon),
    qyear_index_lag = lag(qyear_index),
    consecutive_lag = qyear_index - qyear_index_lag == 1
  ) %>%
  ungroup()

ar_data_excluding_food <- reg_data_excluding_food %>%
  filter(consecutive_lag, !is.na(epsilon), !is.na(epsilon_lag))

ar1_regression_excluding_food <- feols(
  epsilon ~ 0 + epsilon_lag,
  data = ar_data_excluding_food
)

ar_data_excluding_food <- ar_data_excluding_food %>%
  mutate(eta = resid(ar1_regression_excluding_food))

reg_data_excluding_food_property_taxes <- long_panel %>%
  filter(
    measure == "excluding_food_property_taxes",
    positive_liquid_consumption,
    complete_controls
  ) %>%
  group_by(case_id) %>%
  filter(n() >= 2) %>%
  ungroup()

liquid_consumption_fe_excluding_food_property_taxes <- feols(
  log_liquid_consumption ~
    age + age2 + age3 +
    i(education) + i(marital_status) + i(race) +
    earnings_annual + family_size + i(homeownership) +
    i(qintrv_month) + i(qintrv_year) |
    case_id,
  data = reg_data_excluding_food_property_taxes
)

reg_data_excluding_food_property_taxes <- reg_data_excluding_food_property_taxes %>%
  mutate(epsilon = resid(liquid_consumption_fe_excluding_food_property_taxes)) %>%
  arrange(case_id, qyear_index, interview) %>%
  group_by(case_id) %>%
  mutate(
    epsilon_lag = lag(epsilon),
    qyear_index_lag = lag(qyear_index),
    consecutive_lag = qyear_index - qyear_index_lag == 1
  ) %>%
  ungroup()

ar_data_excluding_food_property_taxes <- reg_data_excluding_food_property_taxes %>%
  filter(consecutive_lag, !is.na(epsilon), !is.na(epsilon_lag))

ar1_regression_excluding_food_property_taxes <- feols(
  epsilon ~ 0 + epsilon_lag,
  data = ar_data_excluding_food_property_taxes
)

ar_data_excluding_food_property_taxes <- ar_data_excluding_food_property_taxes %>%
  mutate(eta = resid(ar1_regression_excluding_food_property_taxes))

print(liquid_consumption_fe_excluding_food)
print(ar1_regression_excluding_food)
cat(
  "sd(eta_it), excluding food/alcohol/tobacco, percent:",
  100 * sd(ar_data_excluding_food$eta), "\n"
)

print(liquid_consumption_fe_excluding_food_property_taxes)
print(ar1_regression_excluding_food_property_taxes)
cat(
  "sd(eta_it), excluding food/alcohol/tobacco/property taxes, percent:",
  100 * sd(ar_data_excluding_food_property_taxes$eta), "\n"
)

table2_sensitivity_results <- tibble(
  measure = c(
    "benchmark",
    "excluding_food_alcohol_tobacco",
    "excluding_food_alcohol_tobacco_property_taxes"
  ),
  eta_sd_percent = c(
    100 * sd(ar_data$eta),
    100 * sd(ar_data_excluding_food$eta),
    100 * sd(ar_data_excluding_food_property_taxes$eta)
  )
)

print(table2_sensitivity_results)

write_csv(
  table2_sensitivity_results,
  file.path(output_dir, "table2_sensitivity_results.csv")
)
