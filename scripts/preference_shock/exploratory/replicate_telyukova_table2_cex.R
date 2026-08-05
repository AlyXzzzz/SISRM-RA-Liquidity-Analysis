# Resolve paths from the repository root even when this script is run from its
# organized subfolder.
script_args <- commandArgs(trailingOnly = FALSE)
script_file <- sub("^--file=", "", script_args[grepl("^--file=", script_args)][1])
if (!is.na(script_file)) {
  script_dir <- dirname(normalizePath(script_file))
  setwd(normalizePath(file.path(script_dir, "..", "..", "..")))
}

# Replicate Telyukova and Visschers (2013), Section 4.2 / Table 2.
#
# Goal:
#   Estimate the unpredictable volatility of quarterly liquid/cash-good
#   consumption in the 2000-2002 CEX Interview Survey.
#
# Main outputs:
#   output/tables/table2_cex_liquid_consumption/table2_replication_results.csv
#   output/tables/table2_cex_liquid_consumption/frequency_diagnostics.csv
#   output/tables/table2_cex_liquid_consumption/zero_missing_diagnostics.csv
#
# Notes:
#   - The script uses the already parsed FMLI file for the quarter-level panel.
#   - It uses FMLI current-quarter expenditure summary variables for the
#     benchmark cash-good aggregate. This matches the paper's "quarterly CEX
#     data" language and keeps the first replication pass transparent.
#   - It separately reads MTBI reference months from the raw zips only to verify
#     observation frequency. No MTBI amounts enter the baseline regression.
#   - CPI component deflators are not part of the parsed CEX files. If a local
#     CPI file is present in raw/cpi or output/cpi, the script records it, but
#     the default baseline remains nominal and relies on month/year dummies over
#     the short 2000-2002 window.

suppressPackageStartupMessages({
  library(dplyr)
  library(fixest)
  library(purrr)
  library(readr)
  library(stringr)
  library(tidyr)
  library(tibble)
})

required_packages <- c("dplyr", "fixest", "purrr", "readr", "stringr", "tidyr", "tibble")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(
    "Missing required R package(s): ",
    paste(missing_packages, collapse = ", "),
    ". Install them before running this script.",
    call. = FALSE
  )
}

repo_root <- getwd()
cex_fmli_path <- file.path("output", "cex", "cex_fmli_2000_2002_raw.rds")
cex_zip_dir <- file.path("raw", "cex")
output_dir <- file.path("output", "tables", "table2_cex_liquid_consumption")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

run_mtbi_frequency_check <- TRUE

to_num <- function(x) {
  suppressWarnings(as.numeric(str_replace_all(as.character(x), ",", "")))
}

weighted_sd <- function(x, w = NULL) {
  ok <- is.finite(x)
  if (!is.null(w)) {
    ok <- ok & is.finite(w) & w > 0
  }
  x <- x[ok]
  if (length(x) <= 1) {
    return(NA_real_)
  }
  if (is.null(w)) {
    return(sd(x))
  }
  w <- w[ok]
  w <- w / sum(w)
  mu <- sum(w * x)
  sqrt(sum(w * (x - mu)^2))
}

retained_fixest_rows <- function(data, model) {
  removed <- integer(0)
  if (!is.null(model$obs_selection)) {
    if (!is.null(model$obs_selection$obsRemoved)) {
      raw_removed <- model$obs_selection$obsRemoved
    } else {
      raw_selection <- suppressWarnings(as.numeric(unlist(
        model$obs_selection,
        use.names = FALSE
      )))
      raw_removed <- raw_selection[!is.na(raw_selection) & raw_selection < 0]
    }

    removed <- suppressWarnings(as.integer(abs(as.numeric(raw_removed))))
    removed <- unique(removed[!is.na(removed)])
  }

  if (length(removed) > 0) {
    data <- data[-removed, , drop = FALSE]
  }

  data
}

check_vars <- function(data, vars, label) {
  missing <- setdiff(vars, names(data))
  if (length(missing) > 0) {
    stop(
      label, " is missing required variable(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
}

qyear_to_index <- function(year, quarter) {
  year * 4 + quarter
}

find_local_cpi_files <- function() {
  roots <- c(file.path("raw", "cpi"), file.path("output", "cpi"))
  roots <- roots[dir.exists(roots)]
  if (length(roots) == 0) {
    return(character())
  }
  unlist(lapply(roots, function(root) {
    list.files(
      root,
      pattern = "(cpi|price|deflat)",
      recursive = TRUE,
      full.names = TRUE,
      ignore.case = TRUE
    )
  }))
}

paper_targets <- tribble(
  ~measure, ~paper_eta_sd_pct,
  "benchmark", 19.6,
  "excluding_food", 27.5,
  "excluding_food_property_taxes", 29.4
)

# -----------------------------------------------------------------------------
# 1. Read parsed CEX FMLI panel
# -----------------------------------------------------------------------------

if (!file.exists(cex_fmli_path)) {
  stop("Missing parsed FMLI file: ", cex_fmli_path, call. = FALSE)
}

cex_fmli_raw <- readRDS(cex_fmli_path)
names(cex_fmli_raw) <- toupper(names(cex_fmli_raw))

needed_fmli_vars <- c(
  "NEWID", "CEX_YEAR", "CEX_QUARTER", "QINTRVMO", "QINTRVYR",
  "FINLWT21", "AGE_REF", "EDUC_REF", "MARITAL1", "REF_RACE",
  "EARNINCX", "FINCBTAX", "FAM_SIZE", "CUTENURE"
)
check_vars(cex_fmli_raw, needed_fmli_vars, "cex_fmli_raw")

# Paper benchmark categories:
# food, alcohol, tobacco, rents, mortgages, utilities, household repairs,
# childcare, other household operations, property taxes, insurance, public
# transportation, and health insurance.
#
# FMLI mapping for this first pass:
#   food                              FOODCQ
#   alcohol                           ALCBEVCQ
#   tobacco                           TOBACCCQ
#   rents                             RNTXRPCQ, RNTAPYCQ
#   mortgages                         MRTINTCQ, MRTPRNOC, MRPINSCQ
#   utilities                         UTILCQ
#   household repairs / operations    HOUSEQCQ, DOMSRVCQ, HOUSOPCQ
#   childcare                         BBYDAYCQ
#   property taxes                    PROPTXCQ
#   insurance                         PERINSCQ, LIFINSCQ, VEHINSCQ
#   public transportation             PUBTRACQ
#   health insurance                  HLTHINCQ
#
# This mapping is deliberately explicit so the component choices can be audited.
cash_good_components <- tribble(
  ~component, ~variable,
  "food", "FOODCQ",
  "alcohol", "ALCBEVCQ",
  "tobacco", "TOBACCCQ",
  "rents", "RNTXRPCQ",
  "rents", "RNTAPYCQ",
  "mortgages", "MRTINTCQ",
  "mortgages", "MRTPRNOC",
  "mortgages", "MRPINSCQ",
  "utilities", "UTILCQ",
  "household_repairs_operations", "HOUSEQCQ",
  "household_repairs_operations", "DOMSRVCQ",
  "household_repairs_operations", "HOUSOPCQ",
  "childcare", "BBYDAYCQ",
  "property_taxes", "PROPTXCQ",
  "insurance", "PERINSCQ",
  "insurance", "LIFINSCQ",
  "insurance", "VEHINSCQ",
  "public_transportation", "PUBTRACQ",
  "health_insurance", "HLTHINCQ"
)

missing_cash_vars <- setdiff(cash_good_components$variable, names(cex_fmli_raw))
if (length(missing_cash_vars) > 0) {
  stop(
    "Missing cash-good component variable(s): ",
    paste(missing_cash_vars, collapse = ", "),
    call. = FALSE
  )
}

cpi_files <- find_local_cpi_files()
deflation_note <- tibble(
  deflator_source = if (length(cpi_files) == 0) "none_found_nominal_baseline" else "local_cpi_file_found_not_applied",
  detail = if (length(cpi_files) == 0) {
    paste(
      "No CPI component file was found under raw/cpi or output/cpi.",
      "Parsed CEX FMLI does not contain CPI indexes.",
      "Baseline uses nominal component sums with month and year dummies."
    )
  } else {
    paste(
      "Found local CPI-like file(s), but no project schema is defined yet:",
      paste(cpi_files, collapse = "; "),
      "Baseline keeps nominal sums until a CPI component crosswalk is added."
    )
  }
)
write_csv(deflation_note, file.path(output_dir, "deflation_notes.csv"))

component_vars <- unique(cash_good_components$variable)
component_numeric <- cex_fmli_raw %>%
  transmute(across(all_of(component_vars), to_num))

component_summary <- map_dfr(component_vars, function(var) {
  x <- component_numeric[[var]]
  tibble(
    variable = var,
    component = paste(unique(cash_good_components$component[cash_good_components$variable == var]), collapse = ";"),
    n_nonmissing = sum(!is.na(x)),
    n_positive = sum(x > 0, na.rm = TRUE),
    n_zero = sum(x == 0, na.rm = TRUE),
    n_negative = sum(x < 0, na.rm = TRUE),
    mean = mean(x, na.rm = TRUE),
    p50 = median(x, na.rm = TRUE),
    p95 = unname(quantile(x, 0.95, na.rm = TRUE))
  )
})
write_csv(component_summary, file.path(output_dir, "liquid_consumption_component_summary.csv"))

cash_vars_by_component <- split(cash_good_components$variable, cash_good_components$component)
sum_vars <- function(data, vars) {
  rowSums(data[, vars, drop = FALSE], na.rm = TRUE)
}

cex_panel <- cex_fmli_raw %>%
  mutate(
    newid = as.character(NEWID),
    case_id = str_sub(newid, 1L, -2L),
    interview = to_num(str_sub(newid, -1L, -1L)),
    cex_year = to_num(CEX_YEAR),
    cex_quarter = to_num(CEX_QUARTER),
    qyear_index = qyear_to_index(cex_year, cex_quarter),
    qintrv_month = to_num(QINTRVMO),
    qintrv_year = to_num(QINTRVYR),
    weight = to_num(FINLWT21),
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
  bind_cols(
    component_numeric %>%
      transmute(
        cash_food = sum_vars(., cash_vars_by_component$food),
        cash_property_taxes = sum_vars(., cash_vars_by_component$property_taxes),
        cash_good_benchmark_nominal = sum_vars(., component_vars)
      )
  ) %>%
  mutate(
    cash_good_excluding_food_nominal =
      cash_good_benchmark_nominal - cash_food,
    cash_good_excluding_food_property_taxes_nominal =
      cash_good_benchmark_nominal - cash_food - cash_property_taxes,
    # Placeholder for future CPI component deflation.
    cash_good_benchmark_real = cash_good_benchmark_nominal,
    cash_good_excluding_food_real = cash_good_excluding_food_nominal,
    cash_good_excluding_food_property_taxes_real =
      cash_good_excluding_food_property_taxes_nominal
  )

# -----------------------------------------------------------------------------
# 2. Frequency diagnostics
# -----------------------------------------------------------------------------

panel_frequency <- cex_panel %>%
  arrange(case_id, qyear_index, interview) %>%
  group_by(case_id) %>%
  summarise(
    n_interviews_observed = n_distinct(interview),
    interviews_observed = paste(sort(unique(interview)), collapse = ""),
    n_quarters_observed = n_distinct(qyear_index),
    min_interview = min(interview, na.rm = TRUE),
    max_interview = max(interview, na.rm = TRUE),
    present_interviews_2_5 = all(2:5 %in% interview),
    qindex_i2 = first(qyear_index[interview == 2], default = NA_real_),
    qindex_i3 = first(qyear_index[interview == 3], default = NA_real_),
    qindex_i4 = first(qyear_index[interview == 4], default = NA_real_),
    qindex_i5 = first(qyear_index[interview == 5], default = NA_real_),
    .groups = "drop"
  ) %>%
  mutate(
    consecutive_interviews_2_5 =
      present_interviews_2_5 &
      qindex_i3 - qindex_i2 == 1 &
      qindex_i4 - qindex_i3 == 1 &
      qindex_i5 - qindex_i4 == 1
  )

frequency_diagnostics <- panel_frequency %>%
  count(
    interviews_observed,
    n_interviews_observed,
    present_interviews_2_5,
    consecutive_interviews_2_5,
    name = "n_households"
  ) %>%
  arrange(desc(n_households))
write_csv(frequency_diagnostics, file.path(output_dir, "frequency_diagnostics.csv"))

complete_panel_ids <- panel_frequency %>%
  filter(consecutive_interviews_2_5) %>%
  pull(case_id)

if (run_mtbi_frequency_check) {
  index_mtbi_files <- function(zip_path) {
    archive_year <- 2000L + as.integer(
      str_match(basename(zip_path), "intrvw([0-9]{2})[.]zip$")[, 2]
    )

    tibble(
      source_zip = zip_path,
      source_file = unzip(zip_path, list = TRUE)$Name
    ) %>%
      filter(str_detect(str_to_lower(source_file), "(^|/)mtbi[0-9]{3}x?[.]csv$")) %>%
      mutate(
        file_code = str_match(
          str_to_lower(basename(source_file)),
          "^mtbi([0-9]{3}x?)[.]csv$"
        )[, 2],
        cex_year = 2000L + as.integer(str_sub(file_code, 1, 2)),
        cex_quarter = as.integer(str_sub(file_code, 3, 3)),
        archive_year = archive_year
      ) %>%
      filter(
        cex_year == archive_year,
        cex_year %in% 2000:2002,
        cex_quarter %in% 1:4
      )
  }

  read_mtbi_frequency <- function(source_zip, source_file) {
    message("Frequency check: ", source_file)
    read_csv(
      unz(source_zip, source_file),
      col_select = all_of(c("NEWID", "REF_YR", "REF_MO")),
      col_types = cols(.default = col_character()),
      show_col_types = FALSE,
      progress = FALSE
    ) %>%
      distinct(NEWID, REF_YR, REF_MO) %>%
      transmute(
        newid = as.character(NEWID),
        ref_year = to_num(REF_YR),
        ref_month = to_num(REF_MO)
      ) %>%
      filter(!is.na(ref_year), !is.na(ref_month), ref_month %in% 1:12)
  }

  mtbi_index <- map_dfr(
    file.path(cex_zip_dir, sprintf("intrvw%02d.zip", 0:2)),
    index_mtbi_files
  )

  mtbi_frequency <- pmap_dfr(
    mtbi_index %>% select(source_zip, source_file),
    read_mtbi_frequency
  ) %>%
    distinct(newid, ref_year, ref_month) %>%
    group_by(newid) %>%
    summarise(n_reference_months = n(), .groups = "drop") %>%
    mutate(interview = to_num(str_sub(newid, -1L, -1L)))

  mtbi_frequency_summary <- mtbi_frequency %>%
    count(interview, n_reference_months, name = "n_interview_records") %>%
    arrange(interview, n_reference_months)
  write_csv(
    mtbi_frequency_summary,
    file.path(output_dir, "mtbi_reference_month_frequency.csv")
  )
}

# -----------------------------------------------------------------------------
# 3. Build X_it and regression panel
# -----------------------------------------------------------------------------

control_vars <- c(
  "age", "age2", "age3", "education", "marital_status", "race",
  "earnings_annual", "family_size", "homeownership",
  "qintrv_month", "qintrv_year"
)

control_missingness <- cex_panel %>%
  summarise(across(
    all_of(control_vars),
    ~ sum(is.na(.x)),
    .names = "missing_{.col}"
  )) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing") %>%
  mutate(variable = str_remove(variable, "^missing_"))
write_csv(control_missingness, file.path(output_dir, "control_missingness.csv"))

long_panel <- cex_panel %>%
  select(
    newid, case_id, interview, cex_year, cex_quarter, qyear_index,
    qintrv_month, qintrv_year, weight,
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
      !is.na(liquid_consumption) & liquid_consumption > 0,
    log_liquid_consumption = if_else(
      positive_liquid_consumption,
      log(pmax(liquid_consumption, .Machine$double.xmin)),
      NA_real_
    ),
    complete_controls = if_all(all_of(control_vars), ~ !is.na(.x)),
    positive_weight = !is.na(weight) & weight > 0,
    complete_public_panel_2_5 = case_id %in% complete_panel_ids
  )

zero_missing_diagnostics <- long_panel %>%
  group_by(measure) %>%
  summarise(
    n_observations = n(),
    n_missing = sum(is.na(liquid_consumption)),
    n_negative = sum(liquid_consumption < 0, na.rm = TRUE),
    n_zero = sum(liquid_consumption == 0, na.rm = TRUE),
    n_positive = sum(liquid_consumption > 0, na.rm = TRUE),
    n_positive_complete_controls =
      sum(positive_liquid_consumption & complete_controls),
    n_positive_complete_controls_weight =
      sum(positive_liquid_consumption & complete_controls & positive_weight),
    .groups = "drop"
  )
write_csv(zero_missing_diagnostics, file.path(output_dir, "zero_missing_diagnostics.csv"))

# -----------------------------------------------------------------------------
# 4. Run fixed-effect regression and pooled AR(1)
# -----------------------------------------------------------------------------

run_one_spec <- function(data, measure_name, sample_name, weighting_name) {
  reg_data <- data %>%
    filter(
      measure == measure_name,
      positive_liquid_consumption,
      complete_controls
    )

  if (sample_name == "complete_public_panel_2_5") {
    reg_data <- reg_data %>% filter(complete_public_panel_2_5)
  }

  if (weighting_name == "finlwt21") {
    reg_data <- reg_data %>% filter(positive_weight)
    weight_formula <- ~ weight
  } else {
    weight_formula <- NULL
  }

  if (nrow(reg_data) == 0) {
    return(list(
      summary = tibble(
        measure = measure_name,
        sample = sample_name,
        weighting = weighting_name,
        n_stage1 = 0L,
        n_ar = 0L,
        n_households_stage1 = 0L,
        n_households_ar = 0L,
        rho = NA_real_,
        eta_sd = NA_real_,
        eta_sd_pct = NA_real_
      ),
      stage1 = NULL,
      ar = NULL
    ))
  }

  first_stage_formula <- log_liquid_consumption ~
    age + age2 + age3 +
    i(education) + i(marital_status) + i(race) +
    earnings_annual + family_size + i(homeownership) +
    i(qintrv_month) + i(qintrv_year) |
    case_id

  first_stage <- feols(
    first_stage_formula,
    data = reg_data,
    weights = weight_formula,
    notes = FALSE,
    warn = FALSE
  )

  reg_data <- retained_fixest_rows(reg_data, first_stage) %>%
    mutate(epsilon = resid(first_stage)) %>%
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

  if (weighting_name == "finlwt21") {
    ar_weight_formula <- ~ weight
  } else {
    ar_weight_formula <- NULL
  }

  ar_model <- feols(
    epsilon ~ 0 + epsilon_lag,
    data = ar_data,
    weights = ar_weight_formula,
    notes = FALSE,
    warn = FALSE
  )

  ar_data <- ar_data %>%
    mutate(eta = resid(ar_model))

  eta_sd <- if (weighting_name == "finlwt21") {
    weighted_sd(ar_data$eta, ar_data$weight)
  } else {
    weighted_sd(ar_data$eta)
  }

  list(
    summary = tibble(
      measure = measure_name,
      sample = sample_name,
      weighting = weighting_name,
      n_stage1 = nrow(reg_data),
      n_ar = nrow(ar_data),
      n_households_stage1 = n_distinct(reg_data$case_id),
      n_households_ar = n_distinct(ar_data$case_id),
      rho = unname(coef(ar_model)[["epsilon_lag"]]),
      eta_sd = eta_sd,
      eta_sd_pct = 100 * eta_sd
    ),
    stage1 = first_stage,
    ar = ar_model
  )
}

spec_grid <- crossing(
  measure = c("benchmark", "excluding_food", "excluding_food_property_taxes"),
  sample = c("all_usable", "complete_public_panel_2_5"),
  weighting = c("unweighted", "finlwt21")
)

model_results <- pmap(
  spec_grid,
  ~ run_one_spec(
    data = long_panel,
    measure_name = ..1,
    sample_name = ..2,
    weighting_name = ..3
  )
)

table2_results <- bind_rows(lapply(model_results, `[[`, "summary")) %>%
  left_join(paper_targets, by = "measure") %>%
  mutate(diff_from_paper_pct = eta_sd_pct - paper_eta_sd_pct) %>%
  arrange(measure, sample, weighting)

write_csv(table2_results, file.path(output_dir, "table2_replication_results.csv"))

ar_coefficients <- map2_dfr(model_results, seq_len(nrow(spec_grid)), function(res, i) {
  if (is.null(res$ar)) {
    return(tibble())
  }
  tibble(
    measure = spec_grid$measure[[i]],
    sample = spec_grid$sample[[i]],
    weighting = spec_grid$weighting[[i]],
    term = names(coef(res$ar)),
    estimate = unname(coef(res$ar))
  )
})
write_csv(ar_coefficients, file.path(output_dir, "ar1_coefficients.csv"))

stage1_fit_stats <- map2_dfr(model_results, seq_len(nrow(spec_grid)), function(res, i) {
  if (is.null(res$stage1)) {
    return(tibble())
  }
  tibble(
    measure = spec_grid$measure[[i]],
    sample = spec_grid$sample[[i]],
    weighting = spec_grid$weighting[[i]],
    nobs = nobs(res$stage1),
    r2 = fitstat(res$stage1, "r2")[[1]],
    within_r2 = fitstat(res$stage1, "wr2")[[1]]
  )
})
write_csv(stage1_fit_stats, file.path(output_dir, "first_stage_fit_stats.csv"))

saveRDS(
  long_panel,
  file.path(output_dir, "table2_analysis_panel_long.rds")
)

message("Table 2 CEX liquid-consumption replication complete.")
message("Results written to: ", output_dir)
print(table2_results)
