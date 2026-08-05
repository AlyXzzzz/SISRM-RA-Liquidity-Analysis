# Resolve paths from the repository root even when this script is run from its
# organized subfolder.
script_args <- commandArgs(trailingOnly = FALSE)
script_file <- sub("^--file=", "", script_args[grepl("^--file=", script_args)][1])
if (!is.na(script_file)) {
  script_dir <- dirname(normalizePath(script_file))
  setwd(normalizePath(file.path(script_dir, "..", "..", "..")))
}

# Explore Appendix A.3-consistent FMLI cash-good constructions while leaving
# preference_shock_replication.R untouched.
#
# All variants use FMLI PQ+CQ to approximate the full three-month interview
# collection window. Row 2 always excludes the broad food/alcohol/tobacco block,
# following the paper's robustness language.

suppressPackageStartupMessages({
  library(dplyr)
  library(fixest)
  library(purrr)
  library(readr)
  library(stringr)
  library(tibble)
  library(tidyr)
})

output_dir <- file.path("output", "tables", "preference_shock_appendix_group_exploration")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

cex_fmli_path <- file.path("output", "cex", "cex_fmli_2000_2002_raw.rds")
zero_tolerance <- 1e-8

paper_targets <- c(
  benchmark = 19.6,
  excluding_food = 27.5,
  excluding_food_property_taxes = 29.4
)

to_num <- function(x) {
  suppressWarnings(as.numeric(str_replace_all(as.character(x), ",", "")))
}

qyear_to_index <- function(year, quarter) {
  year * 4 + quarter
}

month_add <- function(year, month, k) {
  index <- year * 12 + month + k
  tibble(
    year = (index - 1) %/% 12,
    month = ((index - 1) %% 12) + 1
  )
}

fetch_fred <- function(series_id) {
  url <- paste0("https://fred.stlouisfed.org/graph/fredgraph.csv?id=", series_id)
  tmp <- tempfile(fileext = ".csv")
  status <- system2(
    "curl",
    c("-fsSL", "--connect-timeout", "15", "--max-time", "60", url, "-o", tmp)
  )
  if (!identical(as.integer(status), 0L)) {
    if (series_id == "CPIAUCNS") {
      stop("Could not download FRED series: ", series_id, call. = FALSE)
    }
    warning("Could not download FRED series ", series_id, "; using CPIAUCNS fallback.")
    fallback_series_id <- series_id
    return(fetch_fred("CPIAUCNS") %>% mutate(series_id = fallback_series_id))
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

component_row <- function(detail, component, exclusion_group, cq_var, pq_var,
                          fred_series, appendix_role, note = "") {
  tibble(
    detail = detail,
    component = component,
    exclusion_group = exclusion_group,
    cq_var = cq_var,
    pq_var = pq_var,
    fred_series = fred_series,
    appendix_role = appendix_role,
    note = note
  )
}

component_catalog <- bind_rows(
  component_row("food_total", "food", "food_alcohol_tobacco", "FOODCQ", "FOODPQ", "CPIUFDNS", "cash_good", "Food in/out in Appendix A.3."),
  component_row("alcohol", "alcohol", "food_alcohol_tobacco", "ALCBEVCQ", "ALCBEVPQ", "CUUR0000SAF116", "cash_good", "Special liquid-use category despite credit option."),
  component_row("tobacco", "tobacco", "food_alcohol_tobacco", "TOBACCCQ", "TOBACCPQ", "CUUR0000SEGA", "cash_good", "Special liquid-use category despite credit option."),

  component_row("rent_total", "rent", "other", "RENDWECQ", "RENDWEPQ", "CUUR0000SEHA", "cash_good", "Rented dwelling = rent excluding as pay + rent as pay."),
  component_row("rent_excluding_as_pay", "rent", "other", "RNTXRPCQ", "RNTXRPPQ", "CUUR0000SEHA", "cash_good", "Compact-script rent component."),
  component_row("rent_as_pay", "rent", "other", "RNTAPYCQ", "RNTAPYPQ", "CUUR0000SEHA", "cash_good", "Compact-script rent component."),

  component_row("mortgage_interest", "mortgage", "other", "MRTINTCQ", "MRTINTPQ", "CUUR0000SEHC", "cash_good", "Mortgage-interest expense summary."),
  component_row("owned_home_mortgage_principal", "mortgage", "other", "EMRTPNOC", "EMRTPNOP", "CUUR0000SEHC", "cash_good", "Main-home mortgage-principal outlay."),
  component_row("vacation_home_mortgage_principal", "mortgage", "other", "EMRTPNVC", "EMRTPNVP", "CUUR0000SEHC", "cash_good", "Vacation-home mortgage-principal outlay."),

  component_row("owned_dwelling_repairs_insurance_other", "home_repairs_maintenance", "other", "MRPINSCQ", "MRPINSPQ", "CUUR0000SAH3", "cash_good", "Owned-dwelling repairs, maintenance, housing insurance, other."),
  component_row("utilities", "utilities", "other", "UTILCQ", "UTILPQ", "CUUR0000SAH2", "cash_good", "Utilities, fuels, public services."),
  component_row("property_taxes", "property_taxes", "property_taxes", "PROPTXCQ", "PROPTXPQ", "CUUR0000SEHC", "cash_good", "Property taxes."),

  component_row("household_operations_total", "household_operations", "other", "HOUSOPCQ", "HOUSOPPQ", "CUUR0000SAH3", "cash_good", "HOUSOP = domestic services + other household expenses; includes childcare through DOMSRV."),
  component_row("other_household_expenses", "household_operations", "other", "OTHHEXCQ", "OTHHEXPQ", "CUUR0000SAH3", "cash_good", "Narrower household operations excluding domestic services."),
  component_row("domestic_services_ex_childcare", "household_operations", "other", "DMSXCCCQ", "DMSXCCPQ", "CUUR0000SAH3", "cash_good", "Domestic services excluding childcare."),
  component_row("childcare", "childcare", "other", "BBYDAYCQ", "BBYDAYPQ", "CUUR0000SEEB", "cash_good", "Babysitting and child daycare."),

  component_row("household_furnishings_equipment", "household_furnishings_equipment", "other", "HOUSEQCQ", "HOUSEQPQ", "CUUR0000SAH3", "durable_sensitivity", "Appendix A.3 puts household furnishings/major appliances under durables."),

  component_row("public_transportation", "public_transportation", "other", "PUBTRACQ", "PUBTRAPQ", "CUUR0000SETG", "cash_good", "Public transportation."),
  component_row("health_insurance", "health_insurance", "other", "HLTHINCQ", "HLTHINPQ", "CUUR0000SAM2", "cash_good", "Health insurance is explicitly included."),
  component_row("cash_contributions", "cash_contributions", "other", "CASHCOCQ", "CASHCOPQ", "CPIAUCNS", "cash_good", "Cash contributions; charity contributions are liquid-payment-dominant in Appendix A.3."),

  component_row("personal_insurance_pensions", "nonhealth_insurance_pensions", "other", "PERINSCQ", "PERINSPQ", "CUUR0000SEHD", "insurance_sensitivity", "Broad personal insurance/pensions; final paper suggests pension/insurance payments excluded."),
  component_row("life_other_personal_insurance", "nonhealth_insurance_pensions", "other", "LIFINSCQ", "LIFINSPQ", "CPIAUCNS", "insurance_sensitivity", "Life/other personal insurance."),
  component_row("vehicle_insurance", "nonhealth_insurance_pensions", "other", "VEHINSCQ", "VEHINSPQ", "CPIAUCNS", "insurance_sensitivity", "Auto insurance; final paper explicitly excludes auto insurance."),

  component_row("domestic_services_total", "household_operations", "other", "DOMSRVCQ", "DOMSRVPQ", "CUUR0000SAH3", "double_count_sensitivity", "DOMSRV = DMSXCC + BBYDAY; double-counts if added to HOUSOP."),
  component_row("vacation_home_mortgage_principal_compact", "mortgage", "other", "MRTPRNOC", "MRTPRNOP", "CUUR0000SEHC", "legacy_sensitivity", "Compact-script legacy vacation-home principal summary.")
)

base_food <- c("food_total", "alcohol", "tobacco")
base_common <- c(
  base_food,
  "rent_total",
  "mortgage_interest",
  "owned_dwelling_repairs_insurance_other",
  "utilities",
  "property_taxes",
  "public_transportation",
  "health_insurance",
  "cash_contributions"
)

appendix_housop_interest <- c(base_common, "household_operations_total")
appendix_housop_outlay <- c(
  appendix_housop_interest,
  "owned_home_mortgage_principal", "vacation_home_mortgage_principal"
)
appendix_narrow_ops_interest <- c(base_common, "other_household_expenses", "childcare")
appendix_decomposed_ops_interest <- c(
  base_common,
  "other_household_expenses", "domestic_services_ex_childcare", "childcare"
)

current_compact <- c(
  base_food,
  "rent_excluding_as_pay", "rent_as_pay",
  "mortgage_interest", "vacation_home_mortgage_principal_compact",
  "owned_dwelling_repairs_insurance_other",
  "utilities",
  "household_furnishings_equipment", "domestic_services_total",
  "household_operations_total", "childcare",
  "property_taxes",
  "personal_insurance_pensions", "life_other_personal_insurance", "vehicle_insurance",
  "public_transportation", "health_insurance"
)
prior_best_fit <- c(
  setdiff(current_compact, c("personal_insurance_pensions", "life_other_personal_insurance", "vehicle_insurance")),
  "cash_contributions"
)

specs <- tribble(
  ~spec_id, ~details, ~note,
  "current_compact_broad_food", list(current_compact),
  "Current compact-style component list with broad food/alcohol/tobacco exclusion; included for comparison.",
  "prior_best_fit_broad_food", list(prior_best_fit),
  "Prior near-fit: compact list, drop PERINS/LIFINS/VEHINS, add CASHCO; still includes HOUSEQ and double counts HOUSOP/DOMSRV/BBYDAY.",
  "appendix_housop_interest_cashco", list(appendix_housop_interest),
  "Appendix-consistent preferred first pass: HOUSOP once, no HOUSEQ, no non-health insurance, mortgage interest only, add CASHCO.",
  "appendix_housop_outlay_cashco", list(appendix_housop_outlay),
  "Preferred first pass plus main/vacation mortgage-principal outlays.",
  "appendix_narrow_ops_interest_cashco", list(appendix_narrow_ops_interest),
  "Uses OTHHEXP + BBYDAY instead of HOUSOP, excluding domestic services except childcare.",
  "appendix_decomposed_ops_interest_cashco", list(appendix_decomposed_ops_interest),
  "Uses OTHHEXP + DMSXCC + BBYDAY, which decomposes HOUSOP without double-counting.",
  "appendix_housop_interest_cashco_houseq", list(c(appendix_housop_interest, "household_furnishings_equipment")),
  "Adds HOUSEQ to preferred first pass, even though Appendix A.3 labels household furnishings/major appliances as durables.",
  "appendix_housop_interest_cashco_nonhealth_insurance", list(c(appendix_housop_interest, "personal_insurance_pensions", "life_other_personal_insurance", "vehicle_insurance")),
  "Adds broad non-health insurance/pensions to preferred first pass as a literal broad-insurance sensitivity.",
  "appendix_housop_interest_no_cashco", list(setdiff(appendix_housop_interest, "cash_contributions")),
  "Preferred first pass without CASHCO, to isolate cash contributions."
)

fred_series <- unique(component_catalog$fred_series)
cpi <- bind_rows(lapply(fred_series, fetch_fred)) %>%
  group_by(series_id) %>%
  mutate(cpi_base_2001 = mean(cpi[year == 2001], na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(cpi_norm = cpi / cpi_base_2001)

cex_fmli_raw <- readRDS(cex_fmli_path)
names(cex_fmli_raw) <- toupper(names(cex_fmli_raw))

needed_vars <- unique(c(
  component_catalog$cq_var,
  component_catalog$pq_var,
  "NEWID", "CEX_YEAR", "CEX_QUARTER", "QINTRVMO", "QINTRVYR",
  "AGE_REF", "EDUC_REF", "MARITAL1", "REF_RACE", "EARNINCX",
  "FINCBTAX", "FAM_SIZE", "CUTENURE"
))
missing_vars <- setdiff(needed_vars, names(cex_fmli_raw))
if (length(missing_vars) > 0) {
  stop("Missing FMLI vars: ", paste(missing_vars, collapse = ", "), call. = FALSE)
}

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
  "earnings_annual", "family_size", "homeownership",
  "qintrv_month", "qintrv_year"
)

cpi_deflator <- cex_panel %>%
  distinct(qintrv_year, qintrv_month) %>%
  crossing(fred_series = fred_series) %>%
  expand_grid(lag_month = -3:-1) %>%
  rowwise() %>%
  mutate(month_add(qintrv_year, qintrv_month, lag_month)) %>%
  ungroup() %>%
  left_join(cpi, by = c("fred_series" = "series_id", "year" = "year", "month" = "month")) %>%
  group_by(qintrv_year, qintrv_month, fred_series) %>%
  summarise(cpi_norm = mean(cpi_norm, na.rm = TRUE), .groups = "drop")

build_component_real <- function(details) {
  details <- unlist(details, use.names = FALSE)
  selected <- component_catalog %>% filter(detail %in% details)

  map_dfr(seq_len(nrow(selected)), function(i) {
    row <- selected[i, ]
    tibble(
      row_id = cex_panel$row_id,
      qintrv_year = cex_panel$qintrv_year,
      qintrv_month = cex_panel$qintrv_month,
      detail = row$detail,
      component = row$component,
      exclusion_group = row$exclusion_group,
      appendix_role = row$appendix_role,
      fred_series = row$fred_series,
      nominal = to_num(cex_panel[[row$cq_var]]) + to_num(cex_panel[[row$pq_var]])
    )
  }) %>%
    left_join(cpi_deflator, by = c("qintrv_year", "qintrv_month", "fred_series")) %>%
    mutate(real = nominal / cpi_norm)
}

summarise_cash_goods <- function(component_real) {
  component_real %>%
    group_by(row_id) %>%
    summarise(
      cash_good_benchmark_real = sum(real, na.rm = TRUE),
      cash_good_excluding_food_real =
        sum(real[exclusion_group != "food_alcohol_tobacco"], na.rm = TRUE),
      cash_good_excluding_food_property_taxes_real =
        sum(real[!exclusion_group %in% c("food_alcohol_tobacco", "property_taxes")], na.rm = TRUE),
      .groups = "drop"
    )
}

build_long_panel <- function(cash_good_real, spec_id, note) {
  cex_panel %>%
    select(
      row_id, newid, case_id, interview, cex_year, cex_quarter,
      qyear_index, qintrv_month, qintrv_year, all_of(control_vars)
    ) %>%
    left_join(cash_good_real, by = "row_id") %>%
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
      spec_id = spec_id,
      note = note,
      positive_liquid_consumption =
        !is.na(liquid_consumption) & liquid_consumption > zero_tolerance,
      log_liquid_consumption = if_else(
        positive_liquid_consumption,
        log(pmax(liquid_consumption, .Machine$double.xmin)),
        NA_real_
      ),
      complete_controls = if_all(all_of(control_vars), ~ !is.na(.x))
    )
}

estimate_measure <- function(panel, measure_name) {
  reg_data <- panel %>%
    filter(measure == measure_name, positive_liquid_consumption, complete_controls) %>%
    group_by(case_id) %>%
    filter(n() >= 2) %>%
    ungroup()

  first_stage <- feols(
    log_liquid_consumption ~
      age + age2 + age3 +
      i(education) + i(marital_status) + i(race) +
      earnings_annual + family_size + i(homeownership) +
      i(qintrv_month) + i(qintrv_year) |
      case_id,
    data = reg_data,
    notes = FALSE,
    warn = FALSE
  )

  ar_data <- reg_data %>%
    mutate(epsilon = resid(first_stage)) %>%
    arrange(case_id, qyear_index, interview) %>%
    group_by(case_id) %>%
    mutate(
      epsilon_lag = lag(epsilon),
      qyear_index_lag = lag(qyear_index),
      consecutive_lag = qyear_index - qyear_index_lag == 1
    ) %>%
    ungroup() %>%
    filter(consecutive_lag, !is.na(epsilon), !is.na(epsilon_lag))

  ar_model <- feols(epsilon ~ 0 + epsilon_lag, data = ar_data, notes = FALSE, warn = FALSE)

  tibble(
    measure = measure_name,
    n_stage1 = nrow(reg_data),
    n_ar = nrow(ar_data),
    n_households = n_distinct(reg_data$case_id),
    rho = unname(coef(ar_model)[["epsilon_lag"]]),
    eta_sd_pct = 100 * sd(resid(ar_model))
  )
}

run_spec <- function(spec_id, details, note) {
  component_real <- build_component_real(details)
  panel <- build_long_panel(summarise_cash_goods(component_real), spec_id, note)

  results <- bind_rows(lapply(names(paper_targets), function(measure_name) {
    estimate_measure(panel, measure_name)
  })) %>%
    mutate(
      spec_id = spec_id,
      note = note,
      paper_eta_sd_pct = unname(paper_targets[measure]),
      diff_from_paper_pct = eta_sd_pct - paper_eta_sd_pct,
      abs_diff_from_paper_pct = abs(diff_from_paper_pct)
    ) %>%
    relocate(spec_id, note, measure)

  component_diagnostics <- component_real %>%
    group_by(detail, component, exclusion_group, appendix_role) %>%
    summarise(
      n_positive = sum(nominal > 0, na.rm = TRUE),
      mean_nominal = mean(nominal, na.rm = TRUE),
      p50_nominal = median(nominal, na.rm = TRUE),
      p95_nominal = unname(quantile(nominal, 0.95, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    mutate(spec_id = spec_id)

  panel_diagnostics <- panel %>%
    group_by(spec_id, measure) %>%
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

  list(
    results = results,
    component_diagnostics = component_diagnostics,
    panel_diagnostics = panel_diagnostics
  )
}

outputs <- pmap(specs, function(spec_id, details, note) {
  message("Running ", spec_id)
  run_spec(spec_id, details, note)
})

results <- bind_rows(lapply(outputs, `[[`, "results")) %>%
  arrange(spec_id, match(measure, names(paper_targets)))
component_diagnostics <- bind_rows(lapply(outputs, `[[`, "component_diagnostics"))
panel_diagnostics <- bind_rows(lapply(outputs, `[[`, "panel_diagnostics"))

write_csv(results, file.path(output_dir, "appendix_group_results.csv"))
write_csv(component_diagnostics, file.path(output_dir, "appendix_group_component_diagnostics.csv"))
write_csv(panel_diagnostics, file.path(output_dir, "appendix_group_panel_diagnostics.csv"))
write_csv(component_catalog, file.path(output_dir, "appendix_group_component_catalog.csv"))
write_csv(specs %>% mutate(details = map_chr(details, ~ paste(unlist(.x), collapse = ";"))), file.path(output_dir, "appendix_group_specs.csv"))

cat("\nAppendix group exploration complete.\n")
cat("Results written to:", output_dir, "\n\n")
print(results %>% select(spec_id, measure, eta_sd_pct, paper_eta_sd_pct, diff_from_paper_pct, n_stage1, n_ar), n = Inf)
