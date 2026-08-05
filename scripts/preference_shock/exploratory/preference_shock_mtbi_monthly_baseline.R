# Resolve paths from the repository root even when this script is run from its
# organized subfolder.
script_args <- commandArgs(trailingOnly = FALSE)
script_file <- sub("^--file=", "", script_args[grepl("^--file=", script_args)][1])
if (!is.na(script_file)) {
  script_dir <- dirname(normalizePath(script_file))
  setwd(normalizePath(file.path(script_dir, "..", "..", "..")))
}

# Test a raw monthly MTBI/UCC cash-good baseline for the Telyukova
# preference-shock volatility regression. This script is exploratory and does
# not modify preference_shock_replication.R.

suppressPackageStartupMessages({
  library(dplyr)
  library(fixest)
  library(purrr)
  library(readr)
  library(stringr)
  library(tibble)
  library(tidyr)
})

output_dir <- file.path("output", "tables", "preference_shock_mtbi_monthly_baseline")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

cex_fmli_path <- file.path("output", "cex", "cex_fmli_2000_2002_raw.rds")
cex_zip_dir <- file.path("raw", "cex")
zero_tolerance <- 1e-8

paper_targets <- c(
  benchmark = 19.6,
  excluding_food = 27.5,
  excluding_food_property_taxes = 29.4
)

# These raw MTBI mortgage-principal UCCs are stored as negative COST values.
# BLS converts them to positive outlays in the family-file summaries.
mortgage_principal_abs_ucc <- c(
  "830101", "830102", "830201", "830202", "830203", "830204",
  "880120", "880220", "880320"
)

to_num <- function(x) {
  suppressWarnings(as.numeric(str_replace_all(as.character(x), ",", "")))
}

month_add <- function(year, month, k) {
  index <- year * 12 + month + k
  tibble(
    ref_year = (index - 1) %/% 12,
    ref_month = ((index - 1) %% 12) + 1
  )
}

month_index <- function(year, month) {
  year * 12 + month
}

qyear_to_index <- function(year, quarter) {
  year * 4 + quarter
}

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

ucc_rows <- function(detail, component, exclusion_group, fred_series, codes) {
  tibble(
    detail = detail,
    component = component,
    exclusion_group = exclusion_group,
    fred_series = fred_series,
    ucc = as.character(codes)
  )
}

# Baseline interpretation:
# - Use raw MTBI monthly records and drop reported gifts.
# - Remove food/alcohol/tobacco together in the second row.
# - Keep health insurance, but exclude non-health insurance and pensions.
# - Include cash contributions, which are explicit in the paper's cash-good list.
# - Avoid double-counting domestic services/childcare separately when using the
#   broader household-operations UCC block.
ucc_catalog <- bind_rows(
  ucc_rows("food_total", "food", "food_alcohol_tobacco", "CPIUFDNS", c(
    "190904", "790220", "790230", "190901", "190902", "190903",
    "790410", "790430", "800700"
  )),
  ucc_rows("alcohol", "alcohol", "food_alcohol_tobacco", "CUUR0000SAF116", c(
    "200900", "790310", "790320", "790420"
  )),
  ucc_rows("tobacco", "tobacco", "food_alcohol_tobacco", "CUUR0000SEGA", c(
    "630110", "630210"
  )),
  ucc_rows("rent_excluding_as_pay", "rents", "other", "CUUR0000SEHA", c(
    "210110", "230121", "230141", "230150", "240111", "240121", "240211", "240221",
    "240311", "240321", "320611", "320621", "320631", "350110", "790690", "990920"
  )),
  ucc_rows("rent_as_pay", "rents", "other", "CUUR0000SEHA", "800710"),
  ucc_rows("mortgage_interest", "mortgages", "other", "CUUR0000SEHC", c(
    "220311", "220313", "220321", "880110"
  )),
  ucc_rows("vacation_home_mortgage_principal", "mortgages", "other", "CUUR0000SEHC", c(
    "830102", "830202", "830204", "880320"
  )),
  ucc_rows("owned_dwelling_repairs_insurance_other", "mortgages", "other", "CUUR0000SEHC", c(
    "210901", "220121", "220901", "230112", "230113", "230114", "230115", "230122",
    "230142", "230151", "230901", "240112", "240122", "240212", "240213", "240222",
    "240312", "240322", "320612", "320622", "320632", "340911", "990930"
  )),
  ucc_rows("utilities", "utilities", "other", "CUUR0000SAH2", c(
    "260211", "260212", "260213", "260214", "260111", "260112", "260113", "260114",
    "250111", "250112", "250113", "250114", "250211", "250212", "250213", "250214",
    "250221", "250222", "250223", "250224", "250901", "250902", "250903", "250904",
    "270101", "270102", "270103", "270104", "270211", "270212", "270213", "270214",
    "270411", "270412", "270413", "270414", "270901", "270902", "270903", "270904"
  )),
  ucc_rows("household_furnishings_equipment", "household_repairs_operations", "other", "CUUR0000SAH3", c(
    "280110", "280120", "280130", "280210", "280220", "280230", "280900",
    "290110", "290120", "290210", "290310", "290320", "290410", "290420", "290430", "290440",
    "230133", "230134", "320111", "320163",
    "230117", "230118", "300111", "300112", "300211", "300212", "300221", "300222",
    "300311", "300312", "300321", "300322", "300331", "300332", "300411", "300412",
    "320511", "320512", "320310", "320320", "320330", "320340", "320350", "320360",
    "320370", "320521", "320522", "320120", "320130", "320150", "320210", "320220",
    "320231", "320232", "320410", "320420", "320901", "320902", "320903", "320904",
    "340904", "430130", "690111", "690112", "690210", "690220", "690230", "690241",
    "690242", "690243", "690244", "690245"
  )),
  ucc_rows("household_operations_total", "household_repairs_operations", "other", "CUUR0000SAH3", c(
    "340310", "340410", "340420", "340520", "340530", "340903", "340906", "340910",
    "340914", "340915", "340211", "340212", "670310", "330511", "340510", "340620",
    "340630", "340901", "340907", "340908", "690113", "690114", "990900"
  )),
  ucc_rows("property_taxes", "property_taxes", "property_taxes", "CUUR0000SEHC", "220211"),
  ucc_rows("public_transportation", "public_transportation", "other", "CUUR0000SETG", c(
    "530110", "530210", "530312", "530411", "530510", "530901", "530311", "530412", "530902"
  )),
  ucc_rows("health_insurance", "health_insurance", "other", "CUUR0000SAM2", c(
    "580111", "580112", "580113", "580114", "580311", "580312", "580901", "580903",
    "580904", "580905", "580906"
  )),
  ucc_rows("cash_contributions", "cash_contributions", "other", "CPIAUCNS", c(
    "800111", "800121", "800804", "800811", "800821", "800831", "800841", "800851", "800861"
  ))
)

fred_series <- unique(ucc_catalog$fred_series)
cpi <- bind_rows(lapply(fred_series, fetch_fred)) %>%
  group_by(series_id) %>%
  mutate(cpi_base_2001 = mean(cpi[year == 2001], na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(cpi_norm = cpi / cpi_base_2001)

cex_fmli_raw <- readRDS(cex_fmli_path)
names(cex_fmli_raw) <- toupper(names(cex_fmli_raw))

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
  )

control_vars <- c(
  "age", "age2", "age3", "education", "marital_status", "race",
  "earnings_annual", "family_size", "homeownership"
)

panel_months <- cex_panel %>%
  select(
    row_id, newid, case_id, interview, cex_year, cex_quarter, qyear_index,
    qintrv_month, qintrv_year, income_before_tax, all_of(control_vars)
  ) %>%
  crossing(lag_month = -3:-1) %>%
  rowwise() %>%
  mutate(month_add(qintrv_year, qintrv_month, lag_month)) %>%
  ungroup() %>%
  mutate(month_index = month_index(ref_year, ref_month))

month_duplicate_diagnostics <- panel_months %>%
  count(case_id, ref_year, ref_month, month_index, name = "n_interview_rows") %>%
  filter(n_interview_rows > 1)

index_zip_csvs <- function(zip_path, pattern = NULL) {
  files <- unzip(zip_path, list = TRUE)$Name
  files <- files[str_detect(str_to_lower(files), "[.]csv$")]
  if (!is.null(pattern)) {
    files <- files[str_detect(str_to_lower(files), pattern)]
  }
  tibble(source_zip = zip_path, source_file = files)
}

mtbi_index <- map_dfr(
  list.files(cex_zip_dir, pattern = "[.]zip$", full.names = TRUE),
  index_zip_csvs,
  pattern = "(^|/)mtbi[0-9]{3}x?[.]csv$"
)

read_mtbi <- function(source_zip, source_file) {
  read_csv(
    unz(source_zip, source_file),
    col_types = cols(.default = col_character()),
    show_col_types = FALSE
  ) %>%
    rename_with(toupper) %>%
    transmute(
      newid = as.character(NEWID),
      ucc = as.character(UCC),
      nominal = to_num(COST),
      gift = as.character(GIFT),
      pubflag = as.character(PUBFLAG),
      ref_month = to_num(REF_MO),
      ref_year = to_num(REF_YR),
      source_zip = basename(source_zip),
      source_file = source_file
    )
}

mtbi <- pmap_dfr(mtbi_index, read_mtbi)

mtbi_cash_real <- mtbi %>%
  filter(gift != "1" | is.na(gift)) %>%
  inner_join(ucc_catalog, by = "ucc", relationship = "many-to-many") %>%
  inner_join(
    panel_months %>% select(row_id, newid, ref_year, ref_month),
    by = c("newid", "ref_year", "ref_month"),
    relationship = "many-to-many"
  ) %>%
  left_join(
    cpi,
    by = c("fred_series" = "series_id", "ref_year" = "year", "ref_month" = "month")
  ) %>%
  mutate(
    nominal_for_sum = if_else(ucc %in% mortgage_principal_abs_ucc, abs(nominal), nominal),
    real = nominal_for_sum / cpi_norm
  )

monthly_cash <- mtbi_cash_real %>%
  group_by(row_id, ref_year, ref_month) %>%
  summarise(
    cash_good_benchmark_real = sum(real, na.rm = TRUE),
    cash_good_excluding_food_real =
      sum(real[exclusion_group != "food_alcohol_tobacco"], na.rm = TRUE),
    cash_good_excluding_food_property_taxes_real =
      sum(real[!exclusion_group %in% c("food_alcohol_tobacco", "property_taxes")], na.rm = TRUE),
    .groups = "drop"
  )

monthly_panel <- panel_months %>%
  left_join(monthly_cash, by = c("row_id", "ref_year", "ref_month")) %>%
  mutate(
    across(starts_with("cash_good_"), ~ replace_na(.x, 0)),
    period = "monthly_ref_month",
    time_index = month_index,
    time_month = ref_month,
    time_year = ref_year
  )

collection_panel <- monthly_panel %>%
  group_by(
    row_id, newid, case_id, interview, cex_year, cex_quarter, qyear_index,
    qintrv_month, qintrv_year,
    age, age2, age3, education, marital_status, race, earnings_annual,
    income_before_tax, family_size, homeownership
  ) %>%
  summarise(
    cash_good_benchmark_real = sum(cash_good_benchmark_real, na.rm = TRUE),
    cash_good_excluding_food_real = sum(cash_good_excluding_food_real, na.rm = TRUE),
    cash_good_excluding_food_property_taxes_real =
      sum(cash_good_excluding_food_property_taxes_real, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    period = "three_month_collection_sum",
    time_index = qyear_index,
    time_month = qintrv_month,
    time_year = qintrv_year
  )

make_long_panel <- function(panel) {
  panel %>%
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
        cash_good_excluding_food_property_taxes_real = "excluding_food_property_taxes"
      ),
      positive_liquid_consumption =
        !is.na(liquid_consumption) & liquid_consumption > zero_tolerance,
      log_liquid_consumption = if_else(
        positive_liquid_consumption,
        log(pmax(liquid_consumption, .Machine$double.xmin)),
        NA_real_
      ),
      complete_controls =
        if_all(all_of(control_vars), ~ !is.na(.x)) & !is.na(time_month) & !is.na(time_year)
    )
}

long_panel <- bind_rows(
  make_long_panel(monthly_panel),
  make_long_panel(collection_panel)
)

estimate_measure <- function(data, period_name, measure_name) {
  reg_data <- data %>%
    filter(
      period == period_name,
      measure == measure_name,
      positive_liquid_consumption,
      complete_controls
    ) %>%
    group_by(case_id) %>%
    filter(n() >= 2) %>%
    ungroup()

  if (nrow(reg_data) == 0) {
    return(tibble(
      period = period_name,
      measure = measure_name,
      n_stage1 = 0L,
      n_ar = 0L,
      n_households = 0L,
      rho = NA_real_,
      eta_sd_pct = NA_real_
    ))
  }

  first_stage <- feols(
    log_liquid_consumption ~
      age + age2 + age3 +
      i(education) + i(marital_status) + i(race) +
      earnings_annual + family_size + i(homeownership) +
      i(time_month) + i(time_year) |
      case_id,
    data = reg_data,
    notes = FALSE,
    warn = FALSE
  )

  ar_data <- reg_data %>%
    mutate(epsilon = resid(first_stage)) %>%
    arrange(case_id, time_index, interview) %>%
    group_by(case_id) %>%
    mutate(
      epsilon_lag = lag(epsilon),
      time_index_lag = lag(time_index),
      consecutive_lag = time_index - time_index_lag == 1
    ) %>%
    ungroup() %>%
    filter(consecutive_lag, !is.na(epsilon), !is.na(epsilon_lag))

  ar_model <- feols(epsilon ~ 0 + epsilon_lag, data = ar_data, notes = FALSE, warn = FALSE)

  tibble(
    period = period_name,
    measure = measure_name,
    n_stage1 = nrow(reg_data),
    n_ar = nrow(ar_data),
    n_households = n_distinct(reg_data$case_id),
    rho = unname(coef(ar_model)[["epsilon_lag"]]),
    eta_sd_pct = 100 * sd(resid(ar_model))
  )
}

results <- crossing(
  period = unique(long_panel$period),
  measure = names(paper_targets)
) %>%
  pmap_dfr(~ estimate_measure(long_panel, ..1, ..2)) %>%
  mutate(
    paper_eta_sd_pct = unname(paper_targets[measure]),
    diff_from_paper_pct = eta_sd_pct - paper_eta_sd_pct,
    abs_diff_from_paper_pct = abs(diff_from_paper_pct)
  ) %>%
  arrange(period, match(measure, names(paper_targets)))

panel_diagnostics <- long_panel %>%
  group_by(period, measure) %>%
  summarise(
    n_rows = n(),
    n_positive = sum(positive_liquid_consumption, na.rm = TRUE),
    n_zero = sum(liquid_consumption == 0, na.rm = TRUE),
    n_positive_complete_controls =
      sum(positive_liquid_consumption & complete_controls, na.rm = TRUE),
    mean_liquid = mean(liquid_consumption, na.rm = TRUE),
    p50_liquid = median(liquid_consumption, na.rm = TRUE),
    p95_liquid = unname(quantile(liquid_consumption, 0.95, na.rm = TRUE)),
    .groups = "drop"
  )

component_diagnostics <- mtbi_cash_real %>%
  group_by(detail, component, exclusion_group) %>%
  summarise(
    n_records = n(),
    mean_nominal = mean(nominal, na.rm = TRUE),
    mean_nominal_for_sum = mean(nominal_for_sum, na.rm = TRUE),
    p95_nominal_for_sum = unname(quantile(nominal_for_sum, 0.95, na.rm = TRUE)),
    gift_records_remaining = sum(gift == "1", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(component, detail)

write_csv(results, file.path(output_dir, "mtbi_monthly_baseline_results.csv"))
write_csv(panel_diagnostics, file.path(output_dir, "mtbi_monthly_baseline_panel_diagnostics.csv"))
write_csv(component_diagnostics, file.path(output_dir, "mtbi_monthly_baseline_component_diagnostics.csv"))
write_csv(month_duplicate_diagnostics, file.path(output_dir, "mtbi_monthly_duplicate_case_months.csv"))
write_csv(ucc_catalog, file.path(output_dir, "mtbi_monthly_baseline_ucc_crosswalk.csv"))

cat("\nRaw MTBI monthly baseline complete.\n")
cat("Results written to:", output_dir, "\n\n")
print(results, n = Inf)
cat("\nPanel diagnostics:\n")
print(panel_diagnostics, n = Inf)
cat("\nDuplicate case-month rows before estimation:", nrow(month_duplicate_diagnostics), "\n")
