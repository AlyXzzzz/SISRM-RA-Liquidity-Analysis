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

# Paper benchmark categories:
# food, alcohol, tobacco, rents, mortgages, utilities, household repairs,
# childcare, other household operations, property taxes, insurance, public
# transportation, and health insurance.
#

cash_good_components <- tribble(
  ~component, ~exclusion_group, ~variable, ~fred_series,
  "food", "food_alcohol_tobacco", "FOODCQ", "CPIUFDNS",
  "alcohol", "food_alcohol_tobacco", "ALCBEVCQ", "CUUR0000SAF116",
  "tobacco", "food_alcohol_tobacco", "TOBACCCQ", "CUUR0000SEGA",
  "rents", "other", "RNTXRPCQ", "CUUR0000SEHA",
  "rents", "other", "RNTAPYCQ", "CUUR0000SEHA",
  "mortgages", "other", "MRTINTCQ", "CUUR0000SEHC",
  "mortgages", "other", "MRTPRNOC", "CUUR0000SEHC",
  "mortgages", "other", "MRPINSCQ", "CUUR0000SEHC",
  "utilities", "other", "UTILCQ", "CUUR0000SAH2",
  "household_repairs_operations", "other", "HOUSEQCQ", "CUUR0000SAH3",
  "household_repairs_operations", "other", "DOMSRVCQ", "CUUR0000SAH3",
  "household_repairs_operations", "other", "HOUSOPCQ", "CUUR0000SAH3",
  "childcare", "other", "BBYDAYCQ", "CUUR0000SEEB",
  "property_taxes", "property_taxes", "PROPTXCQ", "CUUR0000SEHC",
  "insurance", "other", "PERINSCQ", "CUUR0000SEHD",
  "insurance", "other", "LIFINSCQ", "CPIAUCNS",
  "insurance", "other", "VEHINSCQ", "CPIAUCNS",
  "public_transportation", "other", "PUBTRACQ", "CUUR0000SETG",
  "health_insurance", "other", "HLTHINCQ", "CUUR0000SAM2"
)

component_vars <- unique(cash_good_components$variable)
fred_series <- unique(cash_good_components$fred_series)

# CPI crosswalk uses monthly, not-seasonally-adjusted FRED CPI-U series. Each
# component is normalized by its own 2001 mean, then averaged over the three
# months prior to the CEX interview month, matching the CQ recall window.

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
    newid = as.character(NEWID),
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

# Pivot to long and deflate the nominal CPI with the deflator
component_real <- cex_panel %>%
  select(row_id, qintrv_year, qintrv_month, all_of(component_vars)) %>%
  pivot_longer(
    cols = all_of(component_vars),
    names_to = "variable",
    values_to = "nominal"
  ) %>%
  mutate(nominal = to_num(nominal)) %>%
  left_join(cash_good_components, by = "variable") %>%
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
      sum(
        real[!exclusion_group %in% c("food_alcohol_tobacco", "property_taxes")],
        na.rm = TRUE
      ),
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
