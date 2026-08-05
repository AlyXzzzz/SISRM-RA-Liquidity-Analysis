# scf_sample_selection.R
#
# Fresh SCF-only setup for Telyukova (2013) Table 1 / Table 2 replication.
# This script builds the SCF sample-selection frame and then constructs the
# Borrow / Borrow and Save / Save groups used for the puzzle-size tabulations.
#
# Main input expected:
#   output/scf2001_raw_allvars.rds
#
# Main outputs:
#   output/tables/scf_sample_diagnostics_stepwise.csv
#   output/tables/scf_sample_diagnostics_final.csv
#   output/tables/scf_sample_selection_flags_household.csv
#   output/tables/scf_sample_keep_keys.csv
#   output/tables/scf_base_sample_selected.rds
#   output/tables/scf_table1_analysis_sample_selected.rds
#   output/tables/scf_table1_analysis_sample_selected.csv.gz
#   output/tables/scf_group_diagnostics.csv
#   output/tables/table1_scf.csv
#   output/tables/table1_scf_paper_target_comparison.csv
#   output/tables/scf_table2.csv
#   output/tables/scf_table2_child_variants.csv
#   output/tables/table2_scf_paper_target_comparison.csv
#   output/tables/table2_paper_target_comparison.csv
#   output/tables/scf_paper_target_comparison.csv
#   output/tables/paper_target_comparison.csv
#   output/tables/scf_cc_interest_rate_candidate_diagnostics.csv
#   output/tables/scf_rate_variable_map_candidate_diagnostics.csv
#   output/scf2001_raw_allvars_sample_selected.rds

library(dplyr)
library(readr)
library(stringr)
library(tidyr)

# Resolve paths from the repository root even when this script is run from its
# organized subfolder.
script_args <- commandArgs(trailingOnly = FALSE)
script_file <- sub("^--file=", "", script_args[grepl("^--file=", script_args)][1])
if (!is.na(script_file)) {
  script_dir <- dirname(normalizePath(script_file))
  setwd(normalizePath(file.path(script_dir, "..", "..", "..")))
}

# -----------------------------------------------------------------------------
# 0. Paths and sample-selection parameters
# -----------------------------------------------------------------------------

scf_raw_path <- "output/scf2001_raw_allvars.rds"
table_output_dir <- "output/tables"
raw_output_dir <- "output"

dir.create(table_output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(raw_output_dir, recursive = TRUE, showWarnings = FALSE)

# Telyukova appendix target for the SCF sample.
target_scf_households <- 2878L

# Appendix rule: drop households below $200/month.
income_annual_floor <- 200 * 12

# Income-completeness rule:
# The appendix says incomplete income reporters are dropped. In the 2001 SCF
# codebook, J5729 == 1094 is a total-income decision-tree refusal at Q1, which
# yields no numerical bounding information. Adjacent no-bound codes are retained
# as diagnostics because they are empirically too broad for the appendix count.
#
# Set this to FALSE to see the sample after only age + income floor + valid data.
apply_income_complete_filter <- TRUE
income_var <- "X5729"
income_shadow_var <- "J5729"
income_incomplete_refusal_codes <- c(1094)
income_no_bound_diagnostic_codes <- c(1094, 1095, 1000:1103)

# Grouping rule adapted from replication.R blocks 1-3:
# Borrow if revolving credit-card debt is above $500 and liquid assets are below
# the liquid-asset cutoff; Borrow and save if debt is above $500 and liquid
# assets are at/above the cutoff; Save otherwise. The original script used
# liquid_assets <= 500 for Borrow. Setting liquid_asset_cutoff_in_borrow_group
# to FALSE moves exact-$500 liquid-asset cases into Borrow and save, which is
# closer to the published 5/27/68 puzzle-size targets for the selected sample.
cc_debt_group_cutoff <- 500
liquid_asset_group_cutoff <- 500
cc_debt_cutoff_is_strict <- TRUE
liquid_asset_cutoff_in_borrow_group <- FALSE

# The public codebook maps Q86A1/B9_1 "INTEREST ON CARD W/HI BAL" to X7132.
# Q86A2-Q86A5 map to NULL in the public file, so X7132 is the only direct
# public credit-card interest-rate variable available here. It is coded as
# percent * 100. Keep optional caps unset unless we decide to report a purely
# diagnostic, empirically-tuned rate.
cc_rate_source_var <- "X7132"
cc_rate_shadow_var <- "J7132"
cc_rate_table1_cap_pct <- NA_real_
cc_rate_codebook_path <- "raw/codebook.rtf"

# Table 2 dependent-children definition. The main row uses a Fed-summary-style
# family-structure construct from the raw roster variables. Roster-under-age
# variants are still saved as diagnostics.
main_child_definition <- "famstruct4"

# -----------------------------------------------------------------------------
# 1. Helper functions
# -----------------------------------------------------------------------------

to_num <- function(x) {
  parse_number(as.character(x), na = c("", ".", "NA", "NaN"))
}

to_amount <- function(x) {
  z <- to_num(x)
  pmax(z, 0)
}

to_rate_pct <- function(x) {
  z <- to_num(x)

  case_when(
    is.na(z) ~ NA_real_,
    z == 0 ~ NA_real_,
    z == -1 ~ 0,
    TRUE ~ z / 100
  )
}

to_rate_pct_table1 <- function(x, cap_pct = NA_real_) {
  z <- to_num(x)

  rate <- case_when(
    is.na(z) ~ NA_real_,
    z <= 0 ~ 0,
    TRUE ~ z / 100
  )

  if (!is.na(cap_pct)) {
    rate <- pmin(rate, cap_pct)
  }

  rate
}

above_cutoff <- function(x, cutoff, strict = TRUE) {
  if (strict) {
    x > cutoff
  } else {
    x >= cutoff
  }
}

at_or_below_cutoff <- function(x, cutoff, inclusive = TRUE) {
  if (inclusive) {
    x <= cutoff
  } else {
    x < cutoff
  }
}

add_telyukova_group <- function(data) {
  data %>%
    mutate(
      cc_debt_above_group_cutoff = above_cutoff(
        cc_debt,
        cc_debt_group_cutoff,
        strict = cc_debt_cutoff_is_strict
      ),
      liquid_assets_at_or_below_group_cutoff = at_or_below_cutoff(
        liquid_assets,
        liquid_asset_group_cutoff,
        inclusive = liquid_asset_cutoff_in_borrow_group
      ),
      group = case_when(
        cc_debt_above_group_cutoff & liquid_assets_at_or_below_group_cutoff ~ "Borrow",
        cc_debt_above_group_cutoff & !liquid_assets_at_or_below_group_cutoff ~ "Borrow and save",
        !cc_debt_above_group_cutoff ~ "Save",
        TRUE ~ NA_character_
      ),
      group = factor(group, levels = c("Borrow", "Borrow and save", "Save"))
    )
}

check_vars <- function(data, vars, data_name = deparse(substitute(data))) {
  missing_vars <- setdiff(vars, names(data))
  if (length(missing_vars) > 0) {
    stop(
      data_name, " is missing these variables: ",
      paste(missing_vars, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

row_any_j_code_at_or_above <- function(data, vars, cutoff) {
  vars <- intersect(vars, names(data))
  if (length(vars) == 0) {
    return(rep(FALSE, nrow(data)))
  }

  mat <- as.data.frame(lapply(vars, function(v) {
    z <- to_num(data[[v]])
    !is.na(z) & z >= cutoff
  }))
  rowSums(mat, na.rm = TRUE) > 0
}

row_any_j_code_in <- function(data, vars, codes) {
  vars <- intersect(vars, names(data))
  if (length(vars) == 0) {
    return(rep(FALSE, nrow(data)))
  }

  mat <- as.data.frame(lapply(vars, function(v) {
    z <- to_num(data[[v]])
    !is.na(z) & z %in% codes
  }))
  rowSums(mat, na.rm = TRUE) > 0
}

row_count_codes <- function(data, vars, codes) {
  vars <- intersect(vars, names(data))
  if (length(vars) == 0) {
    return(rep(0L, nrow(data)))
  }

  mat <- as.data.frame(lapply(vars, function(v) {
    z <- to_num(data[[v]])
    !is.na(z) & z %in% codes
  }))
  rowSums(mat, na.rm = TRUE)
}

row_count_rel_under_age <- function(data, rel_vars, age_vars, rel_codes, age_cutoff) {
  present <- rel_vars %in% names(data) & age_vars %in% names(data)
  if (!any(present)) {
    return(rep(0L, nrow(data)))
  }

  mat <- as.data.frame(lapply(which(present), function(i) {
    rel <- to_num(data[[rel_vars[i]]])
    roster_age <- to_num(data[[age_vars[i]]])
    !is.na(rel) & rel %in% rel_codes & !is.na(roster_age) & roster_age < age_cutoff
  }))
  rowSums(mat, na.rm = TRUE)
}

weighted_mean_safe <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

score_rate_variant <- function(data, variant, rate, weight_multiplier = NULL) {
  if (is.null(weight_multiplier)) {
    weight_multiplier <- rep(1, nrow(data))
  }

  tibble(
    variant = variant,
    group = as.character(data$group),
    rate = rate,
    weight = data$weight * weight_multiplier
  ) %>%
    filter(!is.na(group)) %>%
    group_by(variant, group) %>%
    summarise(
      replication = weighted_mean_safe(rate, weight),
      n_nonmissing = sum(!is.na(rate) & !is.na(weight) & weight > 0),
      weight_nonmissing = sum(weight[!is.na(rate) & !is.na(weight) & weight > 0], na.rm = TRUE),
      .groups = "drop"
    )
}

sample_diag <- function(data, label) {
  n_rows <- nrow(data)
  n_households <- n_distinct(data$case_id)

  tibble(
    sample_status = label,
    n_rows = n_rows,
    n_households = n_households,
    rows_per_household = if_else(n_households > 0, n_rows / n_households, NA_real_),
    weighted_population = sum(data$weight, na.rm = TRUE),
    age_mean = weighted_mean_safe(data$age, data$weight),
    income_mean = weighted_mean_safe(data$income_annual, data$weight)
  )
}

apply_keep_keys <- function(data, keep_keys) {
  # Handles dataframes that already have case_id/implicate, or still have SCF
  # Y1/YY1 identifiers.
  if (all(c("case_id", "implicate") %in% names(data))) {
    data %>% semi_join(keep_keys, by = c("case_id", "implicate"))
  } else if (all(c("Y1", "YY1") %in% names(data))) {
    data %>%
      mutate(
        case_id = to_num(YY1),
        implicate = to_num(Y1) - 10 * to_num(YY1)
      ) %>%
      semi_join(keep_keys, by = c("case_id", "implicate"))
  } else if ("case_id" %in% names(data)) {
    # Fallback for one-row-per-household derived files.
    data %>% semi_join(distinct(keep_keys, case_id), by = "case_id")
  } else {
    stop(
      "Cannot apply sample keys: data must contain either ",
      "case_id/implicate, Y1/YY1, or case_id.",
      call. = FALSE
    )
  }
}

# -----------------------------------------------------------------------------
# 2. Read processed SCF full public data
# -----------------------------------------------------------------------------

scf_full <- readRDS(scf_raw_path)

# The raw all-variable RDS should have been created from the fixed-width SCF
# file and map. Here we assume parsing is already correct and only consume it.

# -----------------------------------------------------------------------------
# 3. Variables needed for sample selection and future Table 1 construction
# -----------------------------------------------------------------------------

# Credit-card debt variables used in the current baseline. Keep alternatives for
# later robustness checks, but do not switch them inside sample selection.
cc_debt_vars <- c("X413", "X421")

checking_vars <- c(
  "X3506", "X3510", "X3514",
  "X3518", "X3522", "X3526", "X3529"
)

savings_vars <- c(
  "X3804", "X3807", "X3810",
  "X3813", "X3816", "X3818"
)

brokerage_cash_vars <- c("X3930")
liquid_asset_vars <- c(checking_vars, savings_vars, brokerage_cash_vars)
asset_cc_info_vars <- c(liquid_asset_vars, cc_debt_vars, "X432")
asset_cc_j_vars <- paste0("J", sub("^X", "", asset_cc_info_vars))

table2_demog_vars <- c(
  "X6809", "X8023", "X4511", "X7401",
  "X5901", "X5902", "X5904", "X5905"
)

# Roster positions #3 through #11. The public 2001 file has these nine
# positions; the Fed summary-style family-structure logic counts children,
# grandchildren, and children of partner in these relationship fields.
roster_rel_vars <- c(
  "X108", "X114", "X120", "X126", "X132",
  "X202", "X208", "X214", "X220"
)
roster_age_vars <- c(
  "X110", "X116", "X122", "X128", "X134",
  "X204", "X210", "X216", "X222"
)
family_child_rel_codes <- c(4, 13, 36)

needed_scf_vars <- c(
  "Y1", "YY1", "X14", "X42001",
  income_var, income_shadow_var,
  "X432", "J432",
  cc_rate_source_var, cc_rate_shadow_var,
  cc_debt_vars,
  liquid_asset_vars,
  table2_demog_vars,
  roster_rel_vars,
  roster_age_vars,
  asset_cc_j_vars
)

check_vars(scf_full, needed_scf_vars, "scf_full")

if (apply_income_complete_filter && !(income_shadow_var %in% names(scf_full))) {
  warning(
    "apply_income_complete_filter is TRUE, but ", income_shadow_var,
    " was not found. The incomplete-income filter will be treated as passing ",
    "for all households. Verify the correct SCF income-completeness flag."
  )
}

# -----------------------------------------------------------------------------
# 4. Construct row-level analysis fields, without applying filters yet
# -----------------------------------------------------------------------------

child_count_famstruct <- row_count_codes(
  scf_full,
  roster_rel_vars,
  family_child_rel_codes
)
child_count_roster_under18 <- row_count_rel_under_age(
  scf_full,
  roster_rel_vars,
  roster_age_vars,
  rel_codes = 4,
  age_cutoff = 18
)
child_count_roster_under16 <- row_count_rel_under_age(
  scf_full,
  roster_rel_vars,
  roster_age_vars,
  rel_codes = 4,
  age_cutoff = 16
)

scf_base_rows <- scf_full %>%
  mutate(
    case_id = to_num(YY1),
    implicate = to_num(Y1) - 10 * to_num(YY1),
    weight = to_num(X42001) / 5,
    age = to_num(X14),
    income_annual = to_num(.data[[income_var]]),
    income_shadow = if (income_shadow_var %in% names(.)) {
      to_num(.data[[income_shadow_var]])
    } else {
      NA_real_
    },
    habitual_revolver = to_num(X432) %in% c(3, 5),
    cc_rate_raw_code = to_num(.data[[cc_rate_source_var]]),
    cc_rate_shadow = to_num(.data[[cc_rate_shadow_var]]),
    cc_rate_pct = to_rate_pct(.data[[cc_rate_source_var]]),
    cc_rate_pct_table1 = to_rate_pct_table1(
      .data[[cc_rate_source_var]],
      cap_pct = cc_rate_table1_cap_pct
    ),
    income_refusal_no_bound = row_any_j_code_in(
      .,
      income_shadow_var,
      income_incomplete_refusal_codes
    ),
    income_no_bound_diagnostic = row_any_j_code_in(
      .,
      income_shadow_var,
      income_no_bound_diagnostic_codes
    ),
    asset_cc_j_no_numeric_bound = row_any_j_code_at_or_above(
      .,
      asset_cc_j_vars,
      1000
    ),

    # Validity diagnostics before amounts are floored at zero.
    has_income_response = !is.na(income_annual),
    has_cc_debt_response = if_any(all_of(cc_debt_vars), ~ !is.na(to_num(.x))),
    has_liquid_asset_response = if_any(all_of(liquid_asset_vars), ~ !is.na(to_num(.x))),
    has_payoff_frequency_response = !is.na(to_num(X432)),

    # Table 2 demographics, adapted from replication.R.
    race_white = as.integer(to_num(X6809) == 1),
    married = as.integer(to_num(X8023) == 1),
    married_or_partnered = as.integer(to_num(X8023) %in% c(1, 2)),
    head_full_time = as.integer(to_num(X4511) == 1),
    head_white_collar_prof = as.integer(to_num(X7401) %in% c(1, 2)),
    educ_years = to_num(X5901),
    hs_diploma_or_ged = to_num(X5902) %in% c(1, 2),
    has_college_degree = to_num(X5904) == 1,
    highest_degree = to_num(X5905),
    kids_famstruct = child_count_famstruct,
    child_count_roster_under18 = child_count_roster_under18,
    child_count_roster_under16 = child_count_roster_under16,
    famstruct_raw = case_when(
      married_or_partnered != 1 & kids_famstruct >= 1 ~ 1L,
      married_or_partnered != 1 & kids_famstruct == 0 & age < 55 ~ 2L,
      married_or_partnered != 1 & kids_famstruct == 0 & age >= 55 ~ 3L,
      married_or_partnered == 1 & kids_famstruct >= 1 ~ 4L,
      married_or_partnered == 1 & kids_famstruct == 0 ~ 5L,
      TRUE ~ NA_integer_
    ),
    has_dependent_children_famstruct4 = as.integer(famstruct_raw == 4),
    has_dependent_children_roster_under18 = as.integer(child_count_roster_under18 > 0),
    has_dependent_children_roster_under16 = as.integer(child_count_roster_under16 > 0),
    has_dependent_children = case_when(
      main_child_definition == "famstruct4" ~ has_dependent_children_famstruct4,
      main_child_definition == "roster_under18" ~ has_dependent_children_roster_under18,
      main_child_definition == "roster_under16" ~ has_dependent_children_roster_under16,
      TRUE ~ has_dependent_children_famstruct4
    ),
    less_than_hs = as.integer(
      educ_years < 12 |
        (educ_years == 12 & !hs_diploma_or_ged)
    ),
    college_degree_or_more = as.integer(
      has_college_degree &
        highest_degree %in% c(2, 3, 4)
    ),
    hs_some_college = as.integer(
      less_than_hs == 0 & college_degree_or_more == 0
    )
  ) %>%
  mutate(
    cc_debt_raw = rowSums(
      across(all_of(cc_debt_vars), to_amount),
      na.rm = TRUE
    ),
    checking = rowSums(
      across(all_of(checking_vars), to_amount),
      na.rm = TRUE
    ),
    savings = rowSums(
      across(all_of(savings_vars), to_amount),
      na.rm = TRUE
    ),
    brokerage_cash = rowSums(
      across(all_of(brokerage_cash_vars), to_amount),
      na.rm = TRUE
    ),
    liquid_assets = checking + savings + brokerage_cash,
    cc_debt = if_else(habitual_revolver, cc_debt_raw, 0)
  ) %>%
  add_telyukova_group()

# -----------------------------------------------------------------------------
# 5. Collapse selection rules to the household level
# -----------------------------------------------------------------------------

# Important: the SCF public file has five implicates per household. Selection
# should not accidentally keep one implicate while dropping another. The rules
# below require the household to pass in every implicate. For variables that do
# not vary across implicates, this is equivalent to a simple row-level filter.

scf_household_flags <- scf_base_rows %>%
  group_by(case_id) %>%
  summarise(
    n_implicates = n_distinct(implicate),

    pass_age_25_64 = all(age >= 25 & age <= 64, na.rm = FALSE),

    # Baseline income rule: all implicates must have valid income above the
    # $2,400 annual threshold. If this proves too strict around the cutoff, use
    # the diagnostic variables below to compare all/any/mean rules.
    pass_income_floor_all_implicates = all(
      !is.na(income_annual) & income_annual >= income_annual_floor,
      na.rm = FALSE
    ),
    pass_income_floor_any_implicate = any(
      !is.na(income_annual) & income_annual >= income_annual_floor,
      na.rm = TRUE
    ),
    income_mean_across_implicates = mean(income_annual, na.rm = TRUE),
    pass_income_floor_mean = !is.na(income_mean_across_implicates) &
      income_mean_across_implicates >= income_annual_floor,

    income_shadow_values = paste(sort(unique(na.omit(income_shadow))), collapse = ";"),
    pass_complete_income_candidate = if (apply_income_complete_filter && income_shadow_var %in% names(scf_full)) {
      all(!income_refusal_no_bound, na.rm = FALSE)
    } else {
      TRUE
    },
    pass_income_no_bound_diagnostic = all(!income_no_bound_diagnostic, na.rm = FALSE),

    pass_valid_income = all(has_income_response, na.rm = FALSE),
    pass_valid_cc_debt = all(has_cc_debt_response, na.rm = FALSE),
    pass_valid_liquid_assets = all(has_liquid_asset_response, na.rm = FALSE),
    pass_valid_payoff_frequency = all(has_payoff_frequency_response, na.rm = FALSE),
    pass_valid_asset_cc_j_codes = all(!asset_cc_j_no_numeric_bound, na.rm = FALSE),
    pass_valid_weight = all(!is.na(weight) & weight > 0, na.rm = FALSE),
    pass_complete_implicates = n_implicates == 5L,

    .groups = "drop"
  ) %>%
  mutate(
    pass_valid_core_data = pass_valid_income &
      pass_valid_cc_debt &
      pass_valid_liquid_assets &
      pass_valid_payoff_frequency &
      pass_valid_asset_cc_j_codes &
      pass_valid_weight &
      pass_complete_implicates,

    keep_age_only = pass_age_25_64,
    keep_after_income_floor = keep_age_only & pass_income_floor_all_implicates,
    keep_after_income_complete = keep_after_income_floor & pass_complete_income_candidate,
    keep_final = keep_after_income_complete & pass_valid_core_data
  )

write_csv(
  scf_household_flags,
  file.path(table_output_dir, "scf_sample_selection_flags_household.csv")
)

# -----------------------------------------------------------------------------
# 6. Stepwise diagnostics
# -----------------------------------------------------------------------------

scf_rows_with_flags <- scf_base_rows %>%
  left_join(
    scf_household_flags %>%
      select(
        case_id,
        keep_age_only,
        keep_after_income_floor,
        keep_after_income_complete,
        keep_final
      ),
    by = "case_id"
  )

stepwise_diagnostics <- bind_rows(
  sample_diag(scf_rows_with_flags, "00 parsed SCF public sample"),
  sample_diag(filter(scf_rows_with_flags, keep_age_only), "01 age 25-64"),
  sample_diag(filter(scf_rows_with_flags, keep_after_income_floor), "02 + income >= $200/month"),
  sample_diag(filter(scf_rows_with_flags, keep_after_income_complete), "03 + complete income reporter candidate"),
  sample_diag(filter(scf_rows_with_flags, keep_final), "04 + valid income/asset/cc-debt info")
) %>%
  mutate(
    target_households = target_scf_households,
    household_gap_vs_target = n_households - target_scf_households
  )

print(stepwise_diagnostics)

write_csv(
  stepwise_diagnostics,
  file.path(table_output_dir, "scf_sample_diagnostics_stepwise.csv")
)

# For comparability with your previous two-row diagnostics.
final_diagnostics <- bind_rows(
  sample_diag(filter(scf_rows_with_flags, !keep_final), "Dropped"),
  sample_diag(filter(scf_rows_with_flags, keep_final), "Kept")
) %>%
  mutate(
    target_households = if_else(sample_status == "Kept", target_scf_households, NA_integer_),
    household_gap_vs_target = if_else(sample_status == "Kept", n_households - target_scf_households, NA_integer_)
  )

print(final_diagnostics)

write_csv(
  final_diagnostics,
  file.path(table_output_dir, "scf_sample_diagnostics_final.csv")
)

# -----------------------------------------------------------------------------
# 7. Save selected sample keys and selected dataframes
# -----------------------------------------------------------------------------

scf_sample_keep_keys <- scf_rows_with_flags %>%
  filter(keep_final) %>%
  distinct(case_id, implicate)

write_csv(
  scf_sample_keep_keys,
  file.path(table_output_dir, "scf_sample_keep_keys.csv")
)

# Selected full raw data: all original variables, restricted to the final sample.
scf_full_selected <- apply_keep_keys(scf_full, scf_sample_keep_keys)

saveRDS(
  scf_full_selected,
  file.path(raw_output_dir, "scf2001_raw_allvars_sample_selected.rds")
)
write_csv(
  scf_full_selected,
  file.path(raw_output_dir, "scf2001_raw_allvars_sample_selected.csv.gz")
)

# Selected base dataframe with constructed sample-selection and group fields.
scf_base_sample_selected <- scf_rows_with_flags %>%
  filter(keep_final)

saveRDS(
  scf_base_sample_selected,
  file.path(table_output_dir, "scf_base_sample_selected.rds")
)
write_csv(
  scf_base_sample_selected,
  file.path(table_output_dir, "scf_base_sample_selected.csv.gz")
)

# Grouped SCF analysis frame, matching the shape of the original
# replication.R blocks 1-3 output but restricted to the finalized sample.
scf_table1_analysis <- scf_base_sample_selected %>%
  select(
    case_id, implicate, weight, age,
    income_annual,
    cc_debt_raw, habitual_revolver, cc_debt,
    checking, savings, brokerage_cash, liquid_assets,
    cc_rate_raw_code, cc_rate_shadow,
    cc_rate_pct, cc_rate_pct_table1,
    cc_debt_above_group_cutoff,
    liquid_assets_at_or_below_group_cutoff,
    group,
    race_white,
    married,
    married_or_partnered,
    head_full_time,
    head_white_collar_prof,
    educ_years,
    less_than_hs,
    hs_some_college,
    college_degree_or_more,
    kids_famstruct,
    famstruct_raw,
    has_dependent_children,
    has_dependent_children_famstruct4,
    has_dependent_children_roster_under18,
    has_dependent_children_roster_under16
  )

saveRDS(
  scf_table1_analysis,
  file.path(table_output_dir, "scf_table1_analysis_sample_selected.rds")
)
write_csv(
  scf_table1_analysis,
  file.path(table_output_dir, "scf_table1_analysis_sample_selected.csv.gz")
)

# -----------------------------------------------------------------------------
# 8. SCF Table 1 puzzle-size outputs
# -----------------------------------------------------------------------------

scf_table1_denominator <- sum(scf_table1_analysis$weight, na.rm = TRUE)

scf_group_diagnostics <- scf_table1_analysis %>%
  filter(!is.na(group)) %>%
  group_by(group) %>%
  summarise(
    n_implicate_rows = n(),
    n_households_distinct = n_distinct(case_id),
    weighted_population = sum(weight, na.rm = TRUE),
    share_pct = 100 * weighted_population / scf_table1_denominator,
    cc_rate_pct = weighted_mean_safe(cc_rate_pct_table1, weight),
    .groups = "drop"
  )

write_csv(
  scf_group_diagnostics,
  file.path(table_output_dir, "scf_group_diagnostics.csv")
)

scf_table1 <- scf_group_diagnostics %>%
  transmute(
    source = "SCF",
    group,
    share_pct,
    cc_rate_pct
  )

print(scf_table1)

write_csv(
  scf_table1,
  file.path(table_output_dir, "table1_scf.csv")
)

scf_table1_targets <- tibble::tribble(
  ~statistic, ~group, ~paper,
  "Puzzle size (%)", "Borrow", 5.0,
  "Puzzle size (%)", "Borrow and save", 27.0,
  "Puzzle size (%)", "Save", 68.0,
  "Credit-card interest rate (%)", "Borrow", 14.8,
  "Credit-card interest rate (%)", "Borrow and save", 13.7,
  "Credit-card interest rate (%)", "Save", 9.8
)

scf_table1_replication <- bind_rows(
  scf_table1 %>%
    transmute(
      statistic = "Puzzle size (%)",
      group = as.character(group),
      replication = share_pct
    ),
  scf_table1 %>%
    transmute(
      statistic = "Credit-card interest rate (%)",
      group = as.character(group),
      replication = cc_rate_pct
    )
)

scf_table1_target_comparison <- scf_table1_targets %>%
  left_join(scf_table1_replication, by = c("statistic", "group")) %>%
  mutate(
    table = "Table 1",
    source = "SCF 2001",
    diff = replication - paper
  ) %>%
  select(table, source, statistic, group, paper, replication, diff)

print(scf_table1_target_comparison)

write_csv(
  scf_table1_target_comparison,
  file.path(table_output_dir, "table1_scf_paper_target_comparison.csv")
)

# -----------------------------------------------------------------------------
# 9. SCF Table 2 demographics
# -----------------------------------------------------------------------------

table2_vars <- c(
  "race_white",
  "married",
  "has_dependent_children",
  "head_full_time",
  "head_white_collar_prof",
  "less_than_hs",
  "hs_some_college",
  "college_degree_or_more"
)

table2_labels <- c(
  race_white = "Race: white",
  married = "Marital status: married",
  has_dependent_children = "Have dependent children",
  head_full_time = "Head works full-time",
  head_white_collar_prof = "Head white-collar/prof.",
  less_than_hs = "Education: less than HS",
  hs_some_college = "HS/some college",
  college_degree_or_more = "College degree or more"
)

scf_table2_by_group <- scf_table1_analysis %>%
  filter(!is.na(group)) %>%
  group_by(group) %>%
  summarise(
    across(
      all_of(table2_vars),
      ~ weighted_mean_safe(.x, weight)
    ),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = all_of(table2_vars),
    names_to = "characteristic",
    values_to = "share"
  ) %>%
  pivot_wider(
    names_from = group,
    values_from = share
  )

scf_table2_population <- scf_table1_analysis %>%
  summarise(
    across(
      all_of(table2_vars),
      ~ weighted_mean_safe(.x, weight)
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "characteristic",
    values_to = "Share in population"
  )

scf_table2 <- scf_table2_by_group %>%
  left_join(scf_table2_population, by = "characteristic") %>%
  mutate(
    characteristic = recode(characteristic, !!!table2_labels)
  ) %>%
  select(
    characteristic,
    Borrow,
    `Borrow and save`,
    Save,
    `Share in population`
  )

print(scf_table2)

write_csv(
  scf_table2,
  file.path(table_output_dir, "scf_table2.csv")
)

scf_table2_child_variants <- scf_table1_analysis %>%
  filter(!is.na(group)) %>%
  group_by(group) %>%
  summarise(
    dependent_children_main = 100 * weighted_mean_safe(has_dependent_children, weight),
    famstruct4 = 100 * weighted_mean_safe(has_dependent_children_famstruct4, weight),
    roster_under18 = 100 * weighted_mean_safe(has_dependent_children_roster_under18, weight),
    roster_under16 = 100 * weighted_mean_safe(has_dependent_children_roster_under16, weight),
    .groups = "drop"
  )

print(scf_table2_child_variants)

write_csv(
  scf_table2_child_variants,
  file.path(table_output_dir, "scf_table2_child_variants.csv")
)

paper_table2_targets <- tibble::tribble(
  ~characteristic, ~group, ~paper,
  "Race: white", "Borrow", 70.0,
  "Race: white", "Borrow and save", 78.0,
  "Race: white", "Save", 74.0,
  "Race: white", "Population", 75.0,
  "Marital status: married", "Borrow", 48.0,
  "Marital status: married", "Borrow and save", 62.0,
  "Marital status: married", "Save", 58.0,
  "Marital status: married", "Population", 59.0,
  "Have dependent children", "Borrow", 45.0,
  "Have dependent children", "Borrow and save", 41.0,
  "Have dependent children", "Save", 39.0,
  "Have dependent children", "Population", 40.0,
  "Head works full-time", "Borrow", 76.0,
  "Head works full-time", "Borrow and save", 85.0,
  "Head works full-time", "Save", 80.0,
  "Head works full-time", "Population", 81.0,
  "Head white-collar/prof.", "Borrow", 48.0,
  "Head white-collar/prof.", "Borrow and save", 61.0,
  "Head white-collar/prof.", "Save", 58.0,
  "Head white-collar/prof.", "Population", 58.0,
  "Education: less than HS", "Borrow", 13.0,
  "Education: less than HS", "Borrow and save", 5.0,
  "Education: less than HS", "Save", 13.0,
  "Education: less than HS", "Population", 11.0,
  "HS/some college", "Borrow", 73.0,
  "HS/some college", "Borrow and save", 61.0,
  "HS/some college", "Save", 51.0,
  "HS/some college", "Population", 55.0,
  "College degree or more", "Borrow", 14.0,
  "College degree or more", "Borrow and save", 33.0,
  "College degree or more", "Save", 36.0,
  "College degree or more", "Population", 34.0
)

table2_group_cols <- c(
  "Borrow",
  "Borrow and save",
  "Save",
  "Share in population"
)

table2_scale <- scf_table2 %>%
  select(all_of(table2_group_cols)) %>%
  unlist(use.names = FALSE) %>%
  max(na.rm = TRUE)

table2_multiplier <- if_else(table2_scale <= 1.5, 100, 1)

scf_table2_replication <- scf_table2 %>%
  pivot_longer(
    cols = all_of(table2_group_cols),
    names_to = "group",
    values_to = "replication"
  ) %>%
  mutate(
    group = recode(group, "Share in population" = "Population"),
    replication = replication * table2_multiplier
  )

scf_table2_target_comparison <- scf_table2_replication %>%
  right_join(
    paper_table2_targets,
    by = c("characteristic", "group")
  ) %>%
  mutate(
    table = "Table 2",
    source = "SCF 2001",
    statistic = "Share (%)",
    diff = replication - paper
  ) %>%
  select(
    table, source, statistic, characteristic, group,
    paper, replication, diff
  )

print(scf_table2_target_comparison)

write_csv(
  scf_table2_target_comparison,
  file.path(table_output_dir, "table2_scf_paper_target_comparison.csv")
)
write_csv(
  scf_table2_target_comparison,
  file.path(table_output_dir, "table2_paper_target_comparison.csv")
)

scf_paper_target_comparison <- bind_rows(
  scf_table1_target_comparison %>%
    mutate(characteristic = NA_character_) %>%
    select(table, source, statistic, characteristic, group, paper, replication, diff),
  scf_table2_target_comparison %>%
    select(table, source, statistic, characteristic, group, paper, replication, diff)
)

write_csv(
  scf_paper_target_comparison,
  file.path(table_output_dir, "scf_paper_target_comparison.csv")
)
write_csv(
  scf_paper_target_comparison,
  file.path(table_output_dir, "paper_target_comparison.csv")
)

# -----------------------------------------------------------------------------
# 10. Credit-card interest-rate diagnostics
# -----------------------------------------------------------------------------

scf_table1_rate_targets <- scf_table1_targets %>%
  filter(statistic == "Credit-card interest rate (%)") %>%
  select(group, paper)

rate_current <- case_when(
  is.na(scf_table1_analysis$cc_rate_raw_code) ~ NA_real_,
  scf_table1_analysis$cc_rate_raw_code <= 0 ~ 0,
  TRUE ~ scf_table1_analysis$cc_rate_raw_code / 100
)
rate_inapp_na_no_interest_zero <- case_when(
  is.na(scf_table1_analysis$cc_rate_raw_code) ~ NA_real_,
  scf_table1_analysis$cc_rate_raw_code == 0 ~ NA_real_,
  scf_table1_analysis$cc_rate_raw_code == -1 ~ 0,
  TRUE ~ scf_table1_analysis$cc_rate_raw_code / 100
)
rate_positive_only <- case_when(
  is.na(scf_table1_analysis$cc_rate_raw_code) ~ NA_real_,
  scf_table1_analysis$cc_rate_raw_code > 0 ~ scf_table1_analysis$cc_rate_raw_code / 100,
  TRUE ~ NA_real_
)

cc_rate_candidate_diagnostics_long <- bind_rows(
  score_rate_variant(scf_table1_analysis, "X7132: <=0 as 0", rate_current),
  score_rate_variant(scf_table1_analysis, "X7132: 0 inapplicable as NA, -1 no-interest as 0", rate_inapp_na_no_interest_zero),
  score_rate_variant(scf_table1_analysis, "X7132: positive rates only", rate_positive_only),
  score_rate_variant(
    scf_table1_analysis,
    "X7132: reported/nonmissing-origin J-codes only",
    if_else(scf_table1_analysis$cc_rate_shadow < 90, rate_current, NA_real_)
  ),
  score_rate_variant(
    scf_table1_analysis,
    "X7132: reported J-code only",
    if_else(scf_table1_analysis$cc_rate_shadow == 0, rate_current, NA_real_)
  ),
  score_rate_variant(
    scf_table1_analysis,
    "X7132: cap at 20%",
    pmin(rate_current, 20)
  ),
  score_rate_variant(
    scf_table1_analysis,
    "X7132: drop above 22.5%",
    if_else(rate_current > 22.5, NA_real_, rate_current)
  ),
  score_rate_variant(
    scf_table1_analysis,
    "X7132: positive rates, balance-weighted by raw card balance",
    rate_positive_only,
    weight_multiplier = pmax(scf_table1_analysis$cc_debt_raw, 0)
  )
) %>%
  left_join(scf_table1_rate_targets, by = "group") %>%
  mutate(
    diff = replication - paper,
    abs_diff = abs(diff)
  )

cc_rate_candidate_diagnostics <- cc_rate_candidate_diagnostics_long %>%
  group_by(variant) %>%
  summarise(
    mean_abs_diff = mean(abs_diff, na.rm = TRUE),
    max_abs_diff = max(abs_diff, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(mean_abs_diff) %>%
  left_join(
    cc_rate_candidate_diagnostics_long %>%
      select(variant, group, replication) %>%
      pivot_wider(names_from = group, values_from = replication, names_prefix = "rate_"),
    by = "variant"
  )

print(cc_rate_candidate_diagnostics)

write_csv(
  cc_rate_candidate_diagnostics_long,
  file.path(table_output_dir, "scf_cc_interest_rate_candidate_diagnostics_long.csv")
)
write_csv(
  cc_rate_candidate_diagnostics,
  file.path(table_output_dir, "scf_cc_interest_rate_candidate_diagnostics.csv")
)

if (file.exists(cc_rate_codebook_path)) {
  codebook_lines <- readLines(cc_rate_codebook_path, warn = FALSE)
  rate_map_lines <- grep(
    "^[A-Z0-9_]+[[:space:]]+[NC][[:space:]].*\\b(INTEREST|APR|RATE)\\b.*X[0-9]+",
    codebook_lines,
    ignore.case = TRUE,
    value = TRUE
  )
  rate_map_vars <- vapply(
    str_extract_all(rate_map_lines, "X[0-9]+[A-Z]?"),
    function(x) tail(x, 1),
    character(1)
  )
  rate_map_meta <- tibble(
    variable = rate_map_vars,
    codebook_label = str_squish(str_remove(rate_map_lines, "\\\\$"))
  ) %>%
    distinct(variable, .keep_all = TRUE) %>%
    filter(variable %in% names(scf_base_sample_selected))

  rate_map_candidate_long <- bind_rows(lapply(rate_map_meta$variable, function(v) {
    raw_code <- to_num(scf_base_sample_selected[[v]])
    rate <- case_when(
      is.na(raw_code) ~ NA_real_,
      raw_code <= 0 ~ 0,
      TRUE ~ raw_code / 100
    )

    score_rate_variant(
      scf_base_sample_selected,
      v,
      rate
    )
  })) %>%
    rename(variable = variant) %>%
    left_join(scf_table1_rate_targets, by = "group") %>%
    mutate(
      diff = replication - paper,
      abs_diff = abs(diff)
    ) %>%
    left_join(rate_map_meta, by = "variable")

  rate_map_candidate_diagnostics <- rate_map_candidate_long %>%
    group_by(variable, codebook_label) %>%
    summarise(
      mean_abs_diff = mean(abs_diff, na.rm = TRUE),
      max_abs_diff = max(abs_diff, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(mean_abs_diff)

  print(head(rate_map_candidate_diagnostics, 10))

  write_csv(
    rate_map_candidate_long,
    file.path(table_output_dir, "scf_rate_variable_map_candidate_diagnostics_long.csv")
  )
  write_csv(
    rate_map_candidate_diagnostics,
    file.path(table_output_dir, "scf_rate_variable_map_candidate_diagnostics.csv")
  )
}

# -----------------------------------------------------------------------------
# 11. Target check
# -----------------------------------------------------------------------------

selected_households <- n_distinct(scf_base_sample_selected$case_id)

message("Selected SCF households: ", selected_households)
message("Telyukova appendix target: ", target_scf_households)

if (selected_households != target_scf_households) {
  warning(
    "Selected household count differs from target by ",
    selected_households - target_scf_households,
    ". Inspect output/tables/scf_sample_diagnostics_stepwise.csv and ",
    "output/tables/scf_sample_selection_flags_household.csv. The most likely ",
    "remaining issue is the exact interpretation of incomplete income reporting."
  )
}
