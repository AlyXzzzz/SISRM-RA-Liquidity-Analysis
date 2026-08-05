library(tidyverse)
library(fixest)

# Resolve paths from the repository root even when this script is run from its
# organized subfolder.
script_args <- commandArgs(trailingOnly = FALSE)
script_file <- sub("^--file=", "", script_args[grepl("^--file=", script_args)][1])
if (!is.na(script_file)) {
  script_dir <- dirname(normalizePath(script_file))
  setwd(normalizePath(file.path(script_dir, "..", "..", "..")))
}

# -----------------------------------------------------------------------------
# 1. Paths and analysis choices
# -----------------------------------------------------------------------------

analysis_calendar_year <- 2019L
zero_tolerance <- 1e-8

# Dollar shares are the relevant DCPC statistic for scaling CEX expenditure
# amounts. The broad definition includes cash, check, debit card, bank-account
# number payments, online banking bill payment, and account-to-account transfer.
# Change this value to another column in shares_by_group_output.csv to run a
# narrow-definition or count-share sensitivity check.
liquid_share_column <- "weighted_dollar_share_broad"

cex_fmli_path <- file.path(
  "output", "cex", "cex_2019_release_fmli_parsed.rds"
)
cex_mtbi_path <- file.path(
  "output", "cex", "cex_2019_release_mtbi_parsed.rds"
)
liquid_share_path <- file.path(
  "output", "tables", "dcpc_2019_liquid_payment_shares",
  "shares_by_group_output.csv"
)
output_dir <- file.path(
  "output", "tables", "preference_shock_volatility_2019_monthly"
)

input_paths <- c(cex_fmli_path, cex_mtbi_path, liquid_share_path)
missing_inputs <- input_paths[!file.exists(input_paths)]
if (length(missing_inputs) > 0L) {
  stop(
    "Missing required input(s): ", paste(missing_inputs, collapse = ", "),
    ". Generate the parsed 2019 CEX files and DCPC group-share output first.",
    call. = FALSE
  )
}
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

to_num <- function(x) {
  suppressWarnings(parse_number(
    as.character(x),
    na = c("", ".", "NA", "NaN")
  ))
}

month_index <- function(year, month) {
  as.integer(year) * 12L + as.integer(month)
}

month_from_index <- function(index) {
  tibble(
    ref_year = as.integer((index - 1L) %/% 12L),
    ref_month = as.integer(((index - 1L) %% 12L) + 1L)
  )
}

fetch_fred <- function(series_id) {
  url <- paste0(
    "https://fred.stlouisfed.org/graph/fredgraph.csv?id=", series_id
  )
  tmp <- tempfile(fileext = ".csv")
  status <- system2(
    "curl",
    c("-fsSL", "--connect-timeout", "15", "--max-time", "60", url, "-o", tmp)
  )
  if (!identical(as.integer(status), 0L)) {
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

# This catalog translates the FMLI categories in preference_shock_replication.R
# into monthly MTBI UCCs. In particular, it preserves that script's exclusions:
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

if (anyDuplicated(ucc_catalog$ucc)) {
  stop("The monthly UCC catalog contains duplicate UCC mappings.", call. = FALSE)
}

# Map each CEX component to the DCPC group used to estimate its liquid-payment
# share. Components without a conceptually corresponding DCPC group remain in
# the consumption basket at weight one. Cash contributions are absent because
# they were already excluded from the maintained CEX basket.
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
  "household_operations", NA_character_,
  "property_taxes", "property_taxes",
  "public_transportation", "public_transportation",
  "health_insurance", "health_insurance"
)

unmapped_components <- anti_join(
  ucc_catalog %>% distinct(component),
  component_liquid_group,
  by = "component"
)
if (nrow(unmapped_components) > 0L) {
  stop(
    "The following CEX components lack an explicit DCPC mapping decision: ",
    paste(unmapped_components$component, collapse = ", "),
    call. = FALSE
  )
}

liquid_shares <- read_csv(
  liquid_share_path,
  col_types = cols(tely_group = col_character(), .default = col_double()),
  show_col_types = FALSE
)

if (!liquid_share_column %in% names(liquid_shares)) {
  stop(
    "Requested liquid-payment share column is absent: ", liquid_share_column,
    call. = FALSE
  )
}
if (anyDuplicated(liquid_shares$tely_group)) {
  stop("DCPC liquid-payment shares contain duplicate group rows.", call. = FALSE)
}

liquid_shares_selected <- liquid_shares %>%
  transmute(
    tely_group,
    liquid_payment_share = .data[[liquid_share_column]]
  )

if (any(
  !is.finite(liquid_shares_selected$liquid_payment_share) |
    liquid_shares_selected$liquid_payment_share < 0 |
    liquid_shares_selected$liquid_payment_share > 1
)) {
  stop(
    "DCPC liquid-payment shares must be finite and between zero and one.",
    call. = FALSE
  )
}

required_liquid_groups <- component_liquid_group %>%
  filter(!is.na(tely_group)) %>%
  distinct(tely_group)
missing_liquid_groups <- anti_join(
  required_liquid_groups,
  liquid_shares_selected,
  by = "tely_group"
)
if (nrow(missing_liquid_groups) > 0L) {
  stop(
    "Missing DCPC share(s) for mapped group(s): ",
    paste(missing_liquid_groups$tely_group, collapse = ", "),
    call. = FALSE
  )
}

component_weight_mapping <- component_liquid_group %>%
  left_join(
    liquid_shares_selected,
    by = "tely_group",
    relationship = "many-to-one"
  ) %>%
  mutate(
    liquid_payment_share = coalesce(liquid_payment_share, 1),
    share_column = if_else(
      is.na(tely_group),
      "unmatched_component_default_one",
      liquid_share_column
    ),
    application_status = if_else(
      is.na(tely_group),
      "no_corresponding_dcpc_group_weight_one",
      "dcpc_share_applied"
    )
  )

unused_liquid_share_groups <- anti_join(
  liquid_shares_selected,
  component_weight_mapping %>%
    filter(!is.na(tely_group)) %>%
    distinct(tely_group),
  by = "tely_group"
)

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

# Use each series' 2019 mean as the real-value base. Unlike the quarterly script,
# no three-month CPI average is formed: each MTBI record gets the CPI for its
# exact REF_YR/REF_MO month.
cpi <- bind_rows(lapply(fred_series, fetch_fred)) %>%
  group_by(series_id) %>%
  mutate(cpi_base_2019 = mean(cpi[year == analysis_calendar_year], na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(cpi_norm = cpi / cpi_base_2019)

bad_cpi_series <- cpi %>%
  distinct(series_id, cpi_base_2019) %>%
  filter(!is.finite(cpi_base_2019) | cpi_base_2019 <= 0)
if (nrow(bad_cpi_series) > 0L) {
  stop(
    "Missing or invalid 2019 CPI base for: ",
    paste(bad_cpi_series$series_id, collapse = ", "),
    call. = FALSE
  )
}

# -----------------------------------------------------------------------------
# 4. FMLI controls and exact monthly panel skeleton
# -----------------------------------------------------------------------------

fmli <- readRDS(cex_fmli_path)
mtbi <- readRDS(cex_mtbi_path)

required_fmli_fields <- c(
  "newid", "panel_id", "interview", "interview_year", "interview_month",
  "interview_month_index", "AGE_REF", "EDUC_REF", "MARITAL1", "REF_RACE",
  "FSALARYX", "FSMPFRMX", "FINCBTAX", "FAM_SIZE", "CUTENURE"
)
required_mtbi_fields <- c(
  "newid", "panel_id", "interview", "ucc", "ref_year", "ref_month",
  "ref_month_index", "cost", "is_gift", "is_published_integrated",
  "is_cost_topcoded", "is_cost_negative", "is_standard_reference_window"
)

missing_fmli_fields <- setdiff(required_fmli_fields, names(fmli))
missing_mtbi_fields <- setdiff(required_mtbi_fields, names(mtbi))
if (length(missing_fmli_fields) > 0L) {
  stop(
    "Parsed FMLI is missing: ", paste(missing_fmli_fields, collapse = ", "),
    call. = FALSE
  )
}
if (length(missing_mtbi_fields) > 0L) {
  stop(
    "Parsed MTBI is missing: ", paste(missing_mtbi_fields, collapse = ", "),
    call. = FALSE
  )
}

# EARNINCX was removed after the 2013 income redesign. Its 2019-equivalent
# earned-income control is wage/salary income plus self-employment income.
# BLS definitions: https://www.bls.gov/cex/csxguide.pdf
fmli_controls <- fmli %>%
  transmute(
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

control_vars <- c(
  "age", "age2", "age3", "education", "marital_status", "race",
  "earnings_annual", "family_size", "homeownership"
)

# Each interview reports the three calendar months immediately before the
# interview month. The parsed five-quarter release has one row per NEWID, so
# this skeleton neither duplicates collection quarters nor duplicates panel
# months.
monthly_skeleton <- fmli_controls %>%
  crossing(reference_lag = -3L:-1L) %>%
  mutate(ref_month_index = interview_month_index + reference_lag) %>%
  bind_cols(month_from_index(.$ref_month_index)) %>%
  filter(ref_year == analysis_calendar_year) %>%
  arrange(panel_id, ref_month_index, interview)

duplicate_panel_months <- monthly_skeleton %>%
  count(panel_id, ref_year, ref_month, ref_month_index, name = "n_rows") %>%
  filter(n_rows > 1L)
if (nrow(duplicate_panel_months) > 0L) {
  stop(
    "The 2019 monthly skeleton contains duplicated panel-month cells.",
    call. = FALSE
  )
}

# -----------------------------------------------------------------------------
# 5. Monthly nominal and real cash-good consumption
# -----------------------------------------------------------------------------

mtbi_analysis <- mtbi %>%
  filter(
    is_standard_reference_window,
    ref_year == analysis_calendar_year,
    !is_gift
  )

ucc_coverage <- mtbi_analysis %>%
  count(ucc, name = "n_2019_records") %>%
  right_join(ucc_catalog, by = "ucc") %>%
  mutate(
    n_2019_records = coalesce(n_2019_records, 0L),
    present_in_2019_mtbi = n_2019_records > 0L
  ) %>%
  arrange(component, detail, ucc)

mtbi_cash_real <- mtbi_analysis %>%
  inner_join(ucc_catalog, by = "ucc", relationship = "many-to-one") %>%
  inner_join(
    monthly_skeleton %>% select(newid, ref_year, ref_month, ref_month_index),
    by = c("newid", "ref_year", "ref_month", "ref_month_index"),
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

if (any(is.na(mtbi_cash_real$cpi_norm))) {
  missing_cpi <- mtbi_cash_real %>%
    filter(is.na(cpi_norm)) %>%
    distinct(fred_series, ref_year, ref_month)
  print(missing_cpi)
  stop("Selected MTBI records are missing monthly CPI values.", call. = FALSE)
}

monthly_cash <- mtbi_cash_real %>%
  group_by(newid, ref_year, ref_month, ref_month_index) %>%
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

monthly_panel <- monthly_skeleton %>%
  left_join(
    monthly_cash,
    by = c("newid", "ref_year", "ref_month", "ref_month_index"),
    relationship = "one-to-one"
  ) %>%
  mutate(
    across(starts_with("cash_good_"), ~ replace_na(.x, 0)),
    analysis_frequency = "monthly",
    analysis_calendar_year = analysis_calendar_year,
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
    ),
    complete_controls = if_all(all_of(control_vars), ~ !is.na(.x))
  )

measure_order <- c(
  "benchmark",
  "excluding_food",
  "excluding_food_property_taxes"
)

estimate_measure <- function(data, measure_name) {
  reg_data <- data %>%
    filter(
      measure == measure_name,
      positive_liquid_consumption,
      complete_controls
    ) %>%
    group_by(panel_id) %>%
    filter(n() >= 2L) %>%
    ungroup()

  if (nrow(reg_data) == 0L) {
    stop("No usable observations for measure: ", measure_name, call. = FALSE)
  }

  # The analysis is restricted to one calendar year, so a year effect would be
  # constant. Reference-month effects replace the interview-quarter/month timing
  # controls used in the quarterly construction.
  first_stage <- feols(
    log_liquid_consumption ~
      age + age2 + age3 +
      i(education) + i(marital_status) + i(race) +
      earnings_annual + family_size + i(homeownership) +
      i(ref_month) |
      panel_id,
    data = reg_data,
    notes = FALSE,
    warn = FALSE
  )

  ar_data <- reg_data %>%
    mutate(epsilon = resid(first_stage)) %>%
    arrange(panel_id, ref_month_index, interview) %>%
    group_by(panel_id) %>%
    mutate(
      epsilon_lag = lag(epsilon),
      ref_month_index_lag = lag(ref_month_index),
      consecutive_month_lag = ref_month_index - ref_month_index_lag == 1L
    ) %>%
    ungroup() %>%
    filter(
      consecutive_month_lag,
      !is.na(epsilon),
      !is.na(epsilon_lag)
    )

  if (nrow(ar_data) == 0L) {
    stop("No consecutive monthly AR observations for: ", measure_name, call. = FALSE)
  }

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

  result <- tibble(
    frequency = "monthly",
    calendar_year = analysis_calendar_year,
    liquid_share_column = liquid_share_column,
    measure = measure_name,
    n_stage1 = nrow(reg_data),
    n_ar = nrow(ar_data),
    n_households_stage1 = n_distinct(reg_data$panel_id),
    n_households_ar = n_distinct(ar_data$panel_id),
    rho = unname(coef(ar_model)[["epsilon_lag"]]),
    eta_sd_log_points = sd(innovations$eta),
    eta_sd_percent = 100 * eta_sd_log_points
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

# -----------------------------------------------------------------------------
# 7. Diagnostics and outputs
# -----------------------------------------------------------------------------

panel_diagnostics <- long_panel %>%
  group_by(measure) %>%
  summarise(
    n_rows = n(),
    n_households = n_distinct(panel_id),
    n_positive = sum(positive_liquid_consumption, na.rm = TRUE),
    n_zero = sum(liquid_consumption == 0, na.rm = TRUE),
    n_negative = sum(liquid_consumption < 0, na.rm = TRUE),
    n_positive_complete_controls =
      sum(positive_liquid_consumption & complete_controls, na.rm = TRUE),
    mean_liquid = mean(liquid_consumption, na.rm = TRUE),
    p50_liquid = median(liquid_consumption, na.rm = TRUE),
    p95_liquid = unname(quantile(liquid_consumption, 0.95, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(measure = factor(measure, levels = measure_order)) %>%
  arrange(measure) %>%
  mutate(measure = as.character(measure))

month_coverage <- monthly_panel %>%
  group_by(ref_year, ref_month, ref_month_index) %>%
  summarise(
    n_rows = n(),
    n_households = n_distinct(panel_id),
    n_interviews = n_distinct(newid),
    .groups = "drop"
  ) %>%
  arrange(ref_month_index)

component_diagnostics <- mtbi_cash_real %>%
  group_by(
    detail, component, tely_group, exclusion_group, fred_series,
    liquid_payment_share, share_column, application_status
  ) %>%
  summarise(
    n_records = n(),
    n_households = n_distinct(panel_id),
    unweighted_nominal_sum = sum(nominal, na.rm = TRUE),
    liquid_nominal_sum = sum(liquid_nominal, na.rm = TRUE),
    unweighted_real_sum = sum(real_unweighted, na.rm = TRUE),
    liquid_real_sum = sum(liquid_real, na.rm = TRUE),
    liquid_to_unweighted_real_ratio =
      liquid_real_sum / unweighted_real_sum,
    n_negative_cost = sum(is_cost_negative, na.rm = TRUE),
    n_topcoded_cost = sum(is_cost_topcoded, na.rm = TRUE),
    n_published_integrated = sum(is_published_integrated, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(component, detail)

input_diagnostics <- tibble(
  statistic = c(
    "fmli_release_rows",
    "mtbi_release_rows",
    "calendar_2019_monthly_rows",
    "calendar_2019_households",
    "duplicate_panel_month_cells",
    "selected_non_gift_mtbi_records",
    "selected_cash_good_mtbi_records",
    "gift_records_excluded_in_2019",
    "outside_reference_window_records_excluded_in_2019",
    "cex_components_with_dcpc_share",
    "cex_components_default_weight_one",
    "unweighted_selected_cash_good_real_sum",
    "liquid_weighted_cash_good_real_sum"
  ),
  value = c(
    nrow(fmli),
    nrow(mtbi),
    nrow(monthly_panel),
    n_distinct(monthly_panel$panel_id),
    nrow(duplicate_panel_months),
    nrow(mtbi_analysis),
    nrow(mtbi_cash_real),
    sum(mtbi$ref_year == analysis_calendar_year & mtbi$is_gift, na.rm = TRUE),
    sum(
      mtbi$ref_year == analysis_calendar_year &
        !mtbi$is_standard_reference_window,
      na.rm = TRUE
    ),
    sum(component_weight_mapping$application_status == "dcpc_share_applied"),
    sum(
      component_weight_mapping$application_status ==
        "no_corresponding_dcpc_group_weight_one"
    ),
    sum(mtbi_cash_real$real_unweighted, na.rm = TRUE),
    sum(mtbi_cash_real$liquid_real, na.rm = TRUE)
  )
)

write_rds(
  long_panel,
  file.path(output_dir, "cash_good_consumption_panel_2019_monthly_long.rds")
)
write_csv(
  long_panel,
  file.path(output_dir, "cash_good_consumption_panel_2019_monthly_long.csv.gz")
)
write_rds(
  innovations,
  file.path(output_dir, "preference_shock_innovations_2019_monthly.rds")
)
write_csv(
  innovations,
  file.path(output_dir, "preference_shock_innovations_2019_monthly.csv.gz")
)
write_csv(
  results,
  file.path(output_dir, "preference_shock_volatility_2019_monthly.csv")
)
write_csv(
  panel_diagnostics,
  file.path(output_dir, "panel_diagnostics_2019_monthly.csv")
)
write_csv(
  month_coverage,
  file.path(output_dir, "month_coverage_2019.csv")
)
write_csv(
  component_diagnostics,
  file.path(output_dir, "component_diagnostics_2019_monthly.csv")
)
write_csv(
  ucc_coverage,
  file.path(output_dir, "ucc_crosswalk_and_2019_coverage.csv")
)
write_csv(
  component_weight_mapping,
  file.path(output_dir, "liquid_payment_weight_mapping.csv")
)
write_csv(
  unused_liquid_share_groups,
  file.path(output_dir, "unused_liquid_payment_share_groups.csv")
)
write_csv(
  cpi %>% filter(year == analysis_calendar_year),
  file.path(output_dir, "monthly_cpi_2019.csv")
)
write_csv(
  input_diagnostics,
  file.path(output_dir, "input_diagnostics_2019_monthly.csv")
)

cat("\n2019 liquid-weighted monthly preference-shock construction complete.\n")
cat("Liquid share column:", liquid_share_column, "\n")
cat("Results written to:", output_dir, "\n\n")
print(results, n = Inf)
cat("\nPanel diagnostics:\n")
print(panel_diagnostics, n = Inf)
cat("\nMonth coverage:\n")
print(month_coverage, n = Inf)
