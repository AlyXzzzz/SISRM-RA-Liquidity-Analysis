# replication_updated.R
#
# Replication script for the SCF/CEX pieces of Telyukova's credit-card
# borrowing/saving puzzle tables.
#
# Main restructuring relative to the previous script:
#   1. SCF sample restrictions are applied at the household/case_id level,
#      so all implicates are kept or dropped together.
#   2. The SCF analysis frame is built once, then reused for Table 1, Table 2,
#      and diagnostic outputs.
#   3. The Table 2 dependent-children row is treated as a family-structure
#      measure. The main definition below uses a Fed-summary-style family
#      structure variable: married/living with partner + children.
#   4. Roster-under-18 and other child definitions are still saved as
#      diagnostics so the key discrepancy can be audited.

library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(purrr)
library(tibble)

# Resolve paths from the repository root even when this script is run from its
# organized subfolder.
script_args <- commandArgs(trailingOnly = FALSE)
script_file <- sub("^--file=", "", script_args[grepl("^--file=", script_args)][1])
if (!is.na(script_file)) {
  script_dir <- dirname(normalizePath(script_file))
  setwd(normalizePath(file.path(script_dir, "..", "..", "..")))
}

# -----------------------------
# 0. Paths and switches
# -----------------------------

scf_path <- "output/scf2001_raw_allvars.rds"
scf_summary_path <- "raw/SCFP2001.csv"  # optional Fed summary extract, if present

cex_fmli_path <- "output/cex/cex_fmli_2000_2002_raw.rds"
cex_zip_dir <- "raw/cex"
cex_output_dir <- "output/cex"
table_output_dir <- "output/tables"

dir.create(cex_output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_output_dir, recursive = TRUE, showWarnings = FALSE)

# Appendix-style SCF restrictions. These are intentionally household-level.
# If a component is unavailable in the public file, the corresponding flag is
# set to TRUE and the diagnostic output records that limitation.
apply_scf_appendix_restrictions <- TRUE
scf_min_age <- 25
scf_max_age <- 64
scf_min_monthly_income <- 200
scf_min_annual_income <- 12 * scf_min_monthly_income
target_scf_households <- 2878L

# Main Table 2 child measure. The diagnostic file below also reports the
# literal household-roster under-18 child measure.
main_child_definition <- "famstruct4"  # options currently implemented: "famstruct4", "roster_under18"

# Main $500 classification convention. This matches the paper wording:
# debt >= 500; liquid assets < 500 for Borrow; liquid assets >= 500 for puzzle.
debt_cutoff <- 500
liquid_cutoff <- 500

scf_full <- readRDS(scf_path)

scf_summary_extract <- NULL
if (file.exists(scf_summary_path)) {
  scf_summary_extract <- read_csv(scf_summary_path, show_col_types = FALSE)
}

# -----------------------------
# 1. Helper functions
# -----------------------------

get_col <- function(data, var, default = NA_character_) {
  if (var %in% names(data)) {
    data[[var]]
  } else {
    rep(default, nrow(data))
  }
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

to_num <- function(x) {
  parse_number(
    as.character(x),
    na = c("", ".", "NA", "NaN")
  )
}

to_amount <- function(x) {
  z <- to_num(x)
  pmax(z, 0)
}

row_sum_amount <- function(data, vars) {
  vars <- intersect(vars, names(data))
  if (length(vars) == 0) {
    return(rep(0, nrow(data)))
  }

  mat <- as.data.frame(lapply(vars, function(v) to_amount(data[[v]])))
  rowSums(mat, na.rm = TRUE)
}

row_count_codes <- function(data, vars, codes) {
  vars <- intersect(vars, names(data))
  if (length(vars) == 0) {
    return(rep(0, nrow(data)))
  }

  mat <- as.data.frame(lapply(vars, function(v) to_num(data[[v]]) %in% codes))
  rowSums(mat, na.rm = TRUE)
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

to_rate_pct <- function(x) {
  z <- to_num(x)

  case_when(
    is.na(z) ~ NA_real_,
    z == 0  ~ NA_real_,  # inapplicable
    z == -1 ~ 0,         # no interest
    TRUE    ~ z / 100    # codebook says percent * 100
  )
}

to_rate_pct_table1 <- function(x) {
  z <- to_num(x)

  case_when(
    is.na(z) ~ NA_real_,
    z <= 0  ~ 0,
    TRUE    ~ z / 100
  )
}

wt_mean <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  if (!any(ok)) return(NA_real_)
  sum(w[ok] * x[ok]) / sum(w[ok])
}

safe_mean <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}

safe_median <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  median(x, na.rm = TRUE)
}

add_telyukova_group <- function(data,
                                debt_var = "cc_debt",
                                liquid_var = "liquid_assets",
                                debt_cutoff = 500,
                                liquid_cutoff = 500) {
  data %>%
    mutate(
      group = case_when(
        .data[[debt_var]] > debt_cutoff & .data[[liquid_var]] <= liquid_cutoff  ~ "Borrow",
        .data[[debt_var]] > debt_cutoff & .data[[liquid_var]] > liquid_cutoff ~ "Borrow and save",
        .data[[debt_var]] <= debt_cutoff                                        ~ "Save",
        TRUE                                                                   ~ NA_character_
      ),
      group = factor(group, levels = c("Borrow", "Borrow and save", "Save"))
    )
}

summarise_group_shares <- function(data, weight_var = "weight") {
  data %>%
    filter(!is.na(group)) %>%
    group_by(group) %>%
    summarise(
      share_pct = 100 * sum(.data[[weight_var]], na.rm = TRUE) /
        sum(data[[weight_var]], na.rm = TRUE),
      .groups = "drop"
    )
}

# -----------------------------
# 2. SCF variable lists
# -----------------------------

cc_debt_vars <- c("X413", "X421")

# Alternative credit-card debt definitions retained for sensitivity checks:
# cc_debt_vars <- c("X413", "X421", "X7575")
# cc_debt_vars <- c("X413", "X421", "X424", "X427", "X430", "X7575")

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

# J-codes at 1000 or above indicate no usable numerical bound or other missing
# data/problem codes in the SCF codebook. For the appendix "valid asset and
# credit-card debt information" rule, apply this to the variables used to build
# liquid assets, credit-card balances, and habitual revolver status.
asset_cc_info_vars <- c(liquid_asset_vars, cc_debt_vars, "X432")
asset_cc_j_vars <- paste0("J", sub("^X", "", asset_cc_info_vars))

# Appendix "incomplete income reporter" candidate. In the public 2001 SCF
# codebook, J5729 == 1094 is a total-income decision-tree refusal at Q1, which
# yields no numerical bounding information. Adjacent no-bound codes are retained
# in diagnostics below because they are empirically too broad for the published
# appendix count.
income_incomplete_refusal_codes <- c(1094)
income_no_bound_diagnostic_codes <- c(1094, 1095, 1000:1103)

# Household-list variables for positions #3 through #11.
rel_vars <- c("X108", "X114", "X120", "X126", "X132",
              "X202", "X208", "X214", "X220")

age_vars <- c("X110", "X116", "X122", "X128", "X134",
              "X204", "X210", "X216", "X222")

dep_vars <- c("X113", "X119", "X125", "X131", "X137",
              "X207", "X213", "X219", "X225")

live_vars <- c("X112", "X118", "X124", "X130", "X136",
               "X206", "X212", "X218", "X224")

needed_scf_vars <- c(
  "Y1", "YY1", "X14", "X42001",
  "X432", "J432", "X7132", "X5729", "J5729",
  "X6809", "X8023", "X4511", "X7401",
  "X5901", "X5902", "X5904", "X5905",
  cc_debt_vars,
  checking_vars,
  savings_vars,
  brokerage_cash_vars,
  asset_cc_j_vars,
  rel_vars,
  age_vars,
  dep_vars
)

check_vars(scf_full, needed_scf_vars, "scf_full")

# -----------------------------
# 3. SCF master analysis frame, before restrictions
# -----------------------------

scf_core_unrestricted <- tibble(
  case_id = to_num(scf_full$YY1),
  implicate = to_num(scf_full$Y1) - 10 * to_num(scf_full$YY1),
  weight = to_num(scf_full$X42001) / 5,

  age = to_num(scf_full$X14),
  income_annual = to_amount(scf_full$X5729),
  income_j = to_num(get_col(scf_full, "J5729")),
  income_incomplete_refusal_no_bound = row_any_j_code_in(
    scf_full,
    "J5729",
    income_incomplete_refusal_codes
  ),
  income_no_bound_diagnostic = row_any_j_code_in(
    scf_full,
    "J5729",
    income_no_bound_diagnostic_codes
  ),

  x432_raw = to_num(scf_full$X432),
  habitual_revolver = to_num(scf_full$X432) %in% c(3, 5),

  cc_debt_raw = row_sum_amount(scf_full, cc_debt_vars),
  checking = row_sum_amount(scf_full, checking_vars),
  savings = row_sum_amount(scf_full, savings_vars),
  brokerage_cash = row_sum_amount(scf_full, brokerage_cash_vars),
  asset_cc_j_no_numeric_bound = row_any_j_code_at_or_above(
    scf_full,
    asset_cc_j_vars,
    1000
  ),

  cc_rate_pct = to_rate_pct(scf_full$X7132),
  cc_rate_pct_table1 = to_rate_pct_table1(scf_full$X7132)
) %>%
  mutate(
    liquid_assets = checking + savings + brokerage_cash,
    cc_debt = if_else(habitual_revolver, cc_debt_raw, 0),
    valid_asset_cc_info = !asset_cc_j_no_numeric_bound &
      !is.na(cc_debt_raw) &
      !is.na(liquid_assets) &
      !is.na(x432_raw)
  )

# -----------------------------
# 4. SCF household-level sample flags
# -----------------------------

scf_household_flags <- scf_core_unrestricted %>%
  group_by(case_id) %>%
  summarise(
    n_implicates_raw = n_distinct(implicate),

    age_for_filter = safe_median(age),
    income_for_filter = safe_mean(income_annual),

    age_eligible = all(
      !is.na(age) & age >= scf_min_age & age <= scf_max_age,
      na.rm = FALSE
    ),

    income_ge_min = all(
      !is.na(income_annual) & income_annual >= scf_min_annual_income,
      na.rm = FALSE
    ),

    income_j_available = any(!is.na(income_j)),
    all_income_j_lt_997 = if_else(
      income_j_available,
      all(income_j < 997, na.rm = TRUE),
      TRUE
    ),
    all_income_j_lt_90 = if_else(
      income_j_available,
      all(income_j < 90, na.rm = TRUE),
      TRUE
    ),
    all_income_j_lt_1000 = if_else(
      income_j_available,
      all(income_j < 1000, na.rm = TRUE),
      TRUE
    ),
    no_income_refusal_no_bound = !any(income_incomplete_refusal_no_bound, na.rm = TRUE),
    no_income_no_bound_diagnostic = !any(income_no_bound_diagnostic, na.rm = TRUE),

    valid_asset_cc_info_all_implicates = all(valid_asset_cc_info, na.rm = TRUE),
    no_asset_cc_j_no_numeric_bound = !any(asset_cc_j_no_numeric_bound, na.rm = TRUE),

    keep_after_income_floor = age_eligible & income_ge_min,
    keep_after_income_complete = keep_after_income_floor & no_income_refusal_no_bound,
    keep_scf_main = keep_after_income_complete &
      valid_asset_cc_info_all_implicates,
    .groups = "drop"
  )

write_csv(scf_household_flags, file.path(table_output_dir, "scf_household_sample_flags.csv"))

sample_diag <- function(data, label) {
  n_rows <- nrow(data)
  n_households <- n_distinct(data$case_id)

  tibble(
    sample_status = label,
    n_rows = n_rows,
    n_households = n_households,
    rows_per_household = if_else(n_households > 0, n_rows / n_households, NA_real_),
    weighted_population = sum(data$weight, na.rm = TRUE),
    age_mean = wt_mean(data$age, data$weight),
    income_mean = wt_mean(data$income_annual, data$weight)
  )
}

scf_rows_with_flags <- scf_core_unrestricted %>%
  left_join(
    scf_household_flags %>%
      select(
        case_id,
        age_eligible,
        keep_after_income_floor,
        keep_after_income_complete,
        keep_scf_main
      ),
    by = "case_id"
  )

stepwise_diagnostics <- bind_rows(
  sample_diag(scf_rows_with_flags, "00 parsed SCF public sample"),
  sample_diag(filter(scf_rows_with_flags, age_eligible), "01 age 25-64"),
  sample_diag(filter(scf_rows_with_flags, keep_after_income_floor), "02 + income >= $200/month"),
  sample_diag(filter(scf_rows_with_flags, keep_after_income_complete), "03 + complete income reporter candidate"),
  sample_diag(filter(scf_rows_with_flags, keep_scf_main), "04 + valid income/asset/cc-debt info")
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

scf_sample_diagnostics <- scf_core_unrestricted %>%
  left_join(scf_household_flags, by = "case_id") %>%
  mutate(
    sample_status = if_else(keep_scf_main, "Kept", "Dropped")
  ) %>%
  group_by(sample_status) %>%
  summarise(
    n_rows = n(),
    n_households = n_distinct(case_id),
    rows_per_household = n_rows / n_households,
    weighted_population = sum(weight, na.rm = TRUE),
    age_mean = wt_mean(age, weight),
    income_mean = wt_mean(income_annual, weight),
    .groups = "drop"
  )

print(scf_sample_diagnostics)
write_csv(scf_sample_diagnostics, file.path(table_output_dir, "scf_sample_diagnostics.csv"))

# Apply all SCF sample restrictions at the household level.
scf_core <- scf_core_unrestricted %>%
  left_join(
    scf_household_flags %>% select(case_id, keep_scf_main),
    by = "case_id"
  ) %>%
  filter(if (apply_scf_appendix_restrictions) keep_scf_main else TRUE) %>%
  select(-keep_scf_main)

# Diagnostic: rows per household after restrictions should be exactly 5 for
# most public SCF workflows. If not, inspect scf_rows_per_household.csv.
scf_rows_per_household <- scf_core %>%
  count(case_id, name = "n_rows") %>%
  count(n_rows, name = "n_households")

print(scf_rows_per_household)
write_csv(scf_rows_per_household, file.path(table_output_dir, "scf_rows_per_household.csv"))

# -----------------------------
# 5. SCF demographics and family structure
# -----------------------------

# Fed-summary-style KIDS/FAMSTRUCT from raw roster variables.
# KIDS is intentionally a family-structure construct, not the same as the
# literal roster-under-18 child flag.
family_child_rel_codes <- c(4, 13, 36)

child_count_famstruct <- row_count_codes(scf_full, rel_vars, family_child_rel_codes)
child_count_roster_under18 <- rowSums(
  as.data.frame(lapply(seq_along(rel_vars), function(j) {
    rel <- to_num(scf_full[[rel_vars[j]]])
    child_age <- to_num(scf_full[[age_vars[j]]])
    rel == 4 & child_age < 18
  })),
  na.rm = TRUE
)

child_count_roster_under16 <- rowSums(
  as.data.frame(lapply(seq_along(rel_vars), function(j) {
    rel <- to_num(scf_full[[rel_vars[j]]])
    child_age <- to_num(scf_full[[age_vars[j]]])
    rel == 4 & child_age < 16
  })),
  na.rm = TRUE
)

scf_demog_family <- scf_full %>%
  transmute(
    case_id = to_num(YY1),
    implicate = to_num(Y1) - 10 * to_num(YY1),
    age = to_num(X14),

    race_white = as.integer(to_num(X6809) == 1),

    # Table row is labeled married. Keep legal/current married as the baseline
    # row, but family structure below treats married/living-with-partner as a
    # central couple, following the Fed summary extract convention.
    married = as.integer(to_num(X8023) == 1),
    married_or_partnered = as.integer(to_num(X8023) %in% c(1, 2)),

    head_full_time = as.integer(to_num(X4511) == 1),
    head_white_collar_prof_raw = as.integer(to_num(X7401) %in% c(1, 2)),

    educ_years = to_num(X5901),
    hs_diploma_or_ged = to_num(X5902) %in% c(1, 2),
    has_college_degree = to_num(X5904) == 1,
    highest_degree = to_num(X5905),

    kids_famstruct = child_count_famstruct,
    famstruct_raw = case_when(
      married_or_partnered != 1 & kids_famstruct >= 1       ~ 1L,
      married_or_partnered != 1 & kids_famstruct == 0 & age < 55  ~ 2L,
      married_or_partnered != 1 & kids_famstruct == 0 & age >= 55 ~ 3L,
      married_or_partnered == 1 & kids_famstruct >= 1       ~ 4L,
      married_or_partnered == 1 & kids_famstruct == 0       ~ 5L,
      TRUE ~ NA_integer_
    ),

    has_dependent_children_famstruct4 = as.integer(famstruct_raw == 4),
    has_dependent_children_roster_under18 = as.integer(child_count_roster_under18 > 0),
    has_dependent_children_roster_under16 = as.integer(child_count_roster_under16 > 0)
  ) %>%
  mutate(
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
    ),

    has_dependent_children = case_when(
      main_child_definition == "famstruct4" ~ has_dependent_children_famstruct4,
      main_child_definition == "roster_under18" ~ has_dependent_children_roster_under18,
      TRUE ~ has_dependent_children_famstruct4
    ),

    head_white_collar_prof = head_white_collar_prof_raw
  )

# Optional validation against the official Fed summary extract if raw/SCFP2001.csv
# is present. This does not drive the main estimates; it audits the reconstruction.
if (!is.null(scf_summary_extract)) {
  scf_summary_for_validation <- scf_summary_extract %>%
    transmute(
      case_id = to_num(YY1),
      implicate = to_num(Y1) - 10 * to_num(YY1),
      FAMSTRUCT_sum = if ("FAMSTRUCT" %in% names(scf_summary_extract)) to_num(FAMSTRUCT) else NA_real_,
      KIDS_sum = if ("KIDS" %in% names(scf_summary_extract)) to_num(KIDS) else NA_real_,
      OCCAT2_sum = if ("OCCAT2" %in% names(scf_summary_extract)) to_num(OCCAT2) else NA_real_,
      EDCL_sum = if ("EDCL" %in% names(scf_summary_extract)) to_num(EDCL) else NA_real_
    )

  scf_summary_validation <- scf_demog_family %>%
    select(case_id, implicate, famstruct_raw, kids_famstruct) %>%
    left_join(scf_summary_for_validation, by = c("case_id", "implicate")) %>%
    summarise(
      famstruct_match_rate = mean(famstruct_raw == FAMSTRUCT_sum, na.rm = TRUE),
      kids_match_rate = mean(kids_famstruct == KIDS_sum, na.rm = TRUE),
      n_compared = sum(!is.na(FAMSTRUCT_sum))
    )

  print(scf_summary_validation)
  write_csv(scf_summary_validation, file.path(table_output_dir, "scf_summary_extract_validation.csv"))

  # If desired for diagnostics, compare raw occupation/education definitions with
  # summary-extract classifications. The main table still uses raw definitions.
  scf_demog_family <- scf_demog_family %>%
    left_join(scf_summary_for_validation, by = c("case_id", "implicate")) %>%
    mutate(
      head_white_collar_prof_occat2 = if_else(!is.na(OCCAT2_sum), as.integer(OCCAT2_sum == 1), NA_integer_),
      less_than_hs_edcl = if_else(!is.na(EDCL_sum), as.integer(EDCL_sum == 1), NA_integer_),
      hs_some_college_edcl = if_else(!is.na(EDCL_sum), as.integer(EDCL_sum == 2), NA_integer_),
      college_degree_or_more_edcl = if_else(!is.na(EDCL_sum), as.integer(EDCL_sum %in% c(3, 4)), NA_integer_)
    )
}

# Master SCF analysis frame used for SCF tables.
scf_analysis <- scf_core %>%
  left_join(scf_demog_family, by = c("case_id", "implicate"), suffix = c("", "_demog")) %>%
  add_telyukova_group(
    debt_var = "cc_debt",
    liquid_var = "liquid_assets",
    debt_cutoff = debt_cutoff,
    liquid_cutoff = liquid_cutoff
  )

saveRDS(scf_analysis, file.path(table_output_dir, "scf_analysis_master.rds"))
write_csv(scf_analysis, file.path(table_output_dir, "scf_analysis_master.csv.gz"))

# Diagnostics for child definitions.
scf_child_definition_diagnostics <- scf_analysis %>%
  filter(!is.na(group)) %>%
  group_by(group) %>%
  summarise(
    famstruct4 = 100 * wt_mean(has_dependent_children_famstruct4, weight),
    roster_under18 = 100 * wt_mean(has_dependent_children_roster_under18, weight),
    roster_under16 = 100 * wt_mean(has_dependent_children_roster_under16, weight),
    .groups = "drop"
  ) %>%
  bind_rows(
    scf_analysis %>%
      summarise(
        group = factor("Population", levels = c("Borrow", "Borrow and save", "Save", "Population")),
        famstruct4 = 100 * wt_mean(has_dependent_children_famstruct4, weight),
        roster_under18 = 100 * wt_mean(has_dependent_children_roster_under18, weight),
        roster_under16 = 100 * wt_mean(has_dependent_children_roster_under16, weight)
      )
  )

print(scf_child_definition_diagnostics)
write_csv(scf_child_definition_diagnostics, file.path(table_output_dir, "scf_child_definition_diagnostics.csv"))

# -----------------------------
# 6. SCF Table 1
# -----------------------------

scf_table1 <- scf_analysis %>%
  filter(!is.na(group)) %>%
  group_by(group) %>%
  summarise(
    source = "SCF",
    share_pct = 100 * sum(weight, na.rm = TRUE) / sum(scf_analysis$weight, na.rm = TRUE),
    cc_rate_pct = wt_mean(cc_rate_pct_table1, weight),
    .groups = "drop"
  ) %>%
  select(source, group, share_pct, cc_rate_pct)

print(scf_table1)
write_csv(scf_table1, file.path(table_output_dir, "table1_scf.csv"))
saveRDS(scf_analysis, file.path(table_output_dir, "scf_table1_analysis.rds"))

# -----------------------------
# 7. CEX Table 1
# -----------------------------

read_cex_fna_from_zips <- function(zip_dir = cex_zip_dir) {
  fna_specs <- tibble::tribble(
    ~cex_year, ~zip_file,      ~fna_regex,
    2000,      "intrvw00.zip", "fna00[.]csv$",
    2001,      "intrvw01.zip", "fna01[.]csv$",
    2002,      "intrvw02.zip", "fna02[.]csv$"
  )

  pmap_dfr(fna_specs, function(cex_year, zip_file, fna_regex) {
    zip_path <- file.path(zip_dir, zip_file)
    if (!file.exists(zip_path)) {
      stop("Missing CEX zip file: ", zip_path, call. = FALSE)
    }

    zip_entries <- unzip(zip_path, list = TRUE)$Name
    fna_entry <- zip_entries[str_detect(str_to_lower(zip_entries), fna_regex)][1]

    if (is.na(fna_entry)) {
      stop("Could not find ", fna_regex, " inside ", zip_path, call. = FALSE)
    }

    read_csv(
      unz(zip_path, fna_entry),
      col_types = cols(.default = col_character()),
      show_col_types = FALSE
    ) %>%
      mutate(
        fna_release_year = cex_year,
        source_zip = zip_file,
        source_file = fna_entry
      )
  })
}

cex_fmli_raw <- readRDS(cex_fmli_path)

needed_cex_fmli_vars <- c(
  "NEWID", "FINLWT21", "AGE_REF", "CKBKACTX", "SAVACCTX",
  "CKBK_CTX", "SAVA_CTX"
)
check_vars(cex_fmli_raw, needed_cex_fmli_vars, "cex_fmli_raw")

cex_fna_raw <- read_cex_fna_from_zips(cex_zip_dir)
saveRDS(cex_fna_raw, file.path(cex_output_dir, "cex_fna_2000_2002_raw.rds"))
write_csv(cex_fna_raw, file.path(cex_output_dir, "cex_fna_2000_2002_raw.csv.gz"))

cex_calendar_qyears <- c(
  20001, 20002, 20003, 20004,
  20011, 20012, 20013, 20014,
  20021, 20022, 20023, 20024
)

cex_credit_codes <- c(100) # revolving credit-card debt code

flag_clean <- function(x) {
  str_trim(as.character(x))
}

cex_asset_amount <- function(x, flag) {
  z <- to_amount(x)
  f <- flag_clean(flag)

  case_when(
    f %in% c("B", "C") ~ NA_real_,
    TRUE ~ z
  )
}

cex_cc_debt <- cex_fna_raw %>%
  transmute(
    qyear = to_num(QYEAR),
    case_id = as.character(NEWID),
    credit_code = to_num(CREDITR5),
    cc_balance = to_amount(CREDITX5),
    cc_balance_prev_year = to_amount(OWEMONEY)
  ) %>%
  filter(
    qyear %in% cex_calendar_qyears,
    credit_code %in% cex_credit_codes
  ) %>%
  group_by(qyear, case_id) %>%
  summarise(
    cc_debt_raw = sum(cc_balance, na.rm = TRUE),
    cc_debt_prev_year = sum(cc_balance_prev_year, na.rm = TRUE),
    has_cc_fna_record = TRUE,
    .groups = "drop"
  )

cex_t1 <- cex_fmli_raw %>%
  mutate(
    case_id = as.character(NEWID),
    interview = to_num(str_sub(as.character(NEWID), -1L, -1L))
  ) %>%
  transmute(
    case_id,
    cex_year = to_num(cex_year),
    cex_quarter = to_num(cex_quarter),
    qyear = cex_year * 10 + cex_quarter,
    interview,
    weight_raw = to_num(FINLWT21),
    weight = to_num(FINLWT21) / 12,
    age = to_num(AGE_REF),
    checking_brokerage = cex_asset_amount(CKBKACTX, CKBK_CTX),
    savings = cex_asset_amount(SAVACCTX, SAVA_CTX)
  ) %>%
  filter(
    interview == 5,
    qyear %in% cex_calendar_qyears,
    age >= 25,
    age <= 64
  ) %>%
  left_join(cex_cc_debt, by = c("qyear", "case_id")) %>%
  mutate(
    cc_debt_raw = coalesce(cc_debt_raw, 0),
    cc_debt_prev_year = coalesce(cc_debt_prev_year, 0),
    has_cc_fna_record = coalesce(has_cc_fna_record, FALSE),

    liquid_assets = case_when(
      is.na(checking_brokerage) & is.na(savings) ~ NA_real_,
      TRUE ~ coalesce(checking_brokerage, 0) + coalesce(savings, 0)
    ),

    cc_debt = cc_debt_raw,
    cc_debt_persistent_proxy = if_else(cc_debt_prev_year > 0, cc_debt_raw, 0)
  ) %>%
  filter(!is.na(liquid_assets)) %>%
  add_telyukova_group(
    debt_var = "cc_debt",
    liquid_var = "liquid_assets",
    debt_cutoff = debt_cutoff,
    liquid_cutoff = liquid_cutoff
  )

cex_table1 <- cex_t1 %>%
  filter(!is.na(group)) %>%
  group_by(group) %>%
  summarise(
    source = "CEX",
    share_pct = 100 * sum(weight, na.rm = TRUE) / sum(cex_t1$weight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  select(source, group, share_pct)

print(cex_table1)
write_csv(cex_table1, file.path(table_output_dir, "table1_cex.csv"))
saveRDS(cex_t1, file.path(table_output_dir, "cex_table1_analysis.rds"))

# Combined Table 1 output. The CEX table has no credit-card-rate column.
table1_combined <- bind_rows(
  scf_table1 %>% mutate(source = "SCF 2001"),
  cex_table1 %>%
    mutate(source = "CEX", cc_rate_pct = NA_real_) %>%
    select(source, group, share_pct, cc_rate_pct)
)

print(table1_combined)
write_csv(table1_combined, file.path(table_output_dir, "table1_combined_scf_cex.csv"))

# -----------------------------
# 8. SCF Table 2: demographics
# -----------------------------

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

scf_table2_by_group <- scf_analysis %>%
  filter(!is.na(group)) %>%
  group_by(group) %>%
  summarise(
    across(
      all_of(table2_vars),
      ~ wt_mean(.x, weight)
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

scf_table2_population <- scf_analysis %>%
  summarise(
    across(
      all_of(table2_vars),
      ~ wt_mean(.x, weight)
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
write_csv(scf_table2, file.path(table_output_dir, "scf_table2.csv"))

# Diagnostic Table 2 variants for child definition. These make the dependent-
# children discrepancy transparent without changing the main table.
scf_table2_child_variants <- scf_analysis %>%
  filter(!is.na(group)) %>%
  group_by(group) %>%
  summarise(
    dependent_children_main = 100 * wt_mean(has_dependent_children, weight),
    famstruct4 = 100 * wt_mean(has_dependent_children_famstruct4, weight),
    roster_under18 = 100 * wt_mean(has_dependent_children_roster_under18, weight),
    roster_under16 = 100 * wt_mean(has_dependent_children_roster_under16, weight),
    .groups = "drop"
  )

print(scf_table2_child_variants)
write_csv(scf_table2_child_variants, file.path(table_output_dir, "scf_table2_child_variants.csv"))

# -----------------------------
# 9. Paper target comparison
# -----------------------------

# This block stores the paper's published target values and compares them
# directly with the current replication outputs. Differences are always:
# replication minus paper, in percentage points.

# ---- Table 1 targets ----

paper_table1_targets <- tibble::tribble(
  ~source,     ~statistic,                         ~group,              ~paper,
  "SCF 2001",  "Puzzle size (%)",                  "Borrow",              5.0,
  "SCF 2001",  "Puzzle size (%)",                  "Borrow and save",     27.0,
  "SCF 2001",  "Puzzle size (%)",                  "Save",               68.0,
  "CEX",       "Puzzle size (%)",                  "Borrow",              7.0,
  "CEX",       "Puzzle size (%)",                  "Borrow and save",     29.0,
  "CEX",       "Puzzle size (%)",                  "Save",               64.0,
  "SCF 2001",  "Credit-card interest rate (%)",    "Borrow",             14.8,
  "SCF 2001",  "Credit-card interest rate (%)",    "Borrow and save",    13.7,
  "SCF 2001",  "Credit-card interest rate (%)",    "Save",                9.8
)

rep_table1_puzzle <- table1_combined %>%
  transmute(
    source,
    statistic = "Puzzle size (%)",
    group = as.character(group),
    replication = share_pct
  )

rep_table1_rate <- table1_combined %>%
  filter(source == "SCF 2001", !is.na(cc_rate_pct)) %>%
  transmute(
    source,
    statistic = "Credit-card interest rate (%)",
    group = as.character(group),
    replication = cc_rate_pct
  )

table1_paper_comparison <- bind_rows(
  rep_table1_puzzle,
  rep_table1_rate
) %>%
  right_join(
    paper_table1_targets,
    by = c("source", "statistic", "group")
  ) %>%
  mutate(
    table = "Table 1",
    diff = replication - paper
  ) %>%
  select(
    table, source, statistic, group,
    paper, replication, diff
  )

print(table1_paper_comparison)
write_csv(
  table1_paper_comparison,
  file.path(table_output_dir, "table1_paper_target_comparison.csv")
)


# ---- Table 2 targets ----

paper_table2_targets <- tibble::tribble(
  ~characteristic,                  ~group,              ~paper,
  "Race: white",                    "Borrow",             70.0,
  "Race: white",                    "Borrow and save",    78.0,
  "Race: white",                    "Save",               74.0,
  "Race: white",                    "Population",         75.0,
  
  "Marital status: married",        "Borrow",             48.0,
  "Marital status: married",        "Borrow and save",    62.0,
  "Marital status: married",        "Save",               58.0,
  "Marital status: married",        "Population",         59.0,
  
  "Have dependent children",        "Borrow",             45.0,
  "Have dependent children",        "Borrow and save",    41.0,
  "Have dependent children",        "Save",               39.0,
  "Have dependent children",        "Population",         40.0,
  
  "Head works full-time",           "Borrow",             76.0,
  "Head works full-time",           "Borrow and save",    85.0,
  "Head works full-time",           "Save",               80.0,
  "Head works full-time",           "Population",         81.0,
  
  "Head white-collar/prof.",        "Borrow",             48.0,
  "Head white-collar/prof.",        "Borrow and save",    61.0,
  "Head white-collar/prof.",        "Save",               58.0,
  "Head white-collar/prof.",        "Population",         58.0,
  
  "Education: less than HS",        "Borrow",             13.0,
  "Education: less than HS",        "Borrow and save",     5.0,
  "Education: less than HS",        "Save",               13.0,
  "Education: less than HS",        "Population",         11.0,
  
  "HS/some college",                "Borrow",             73.0,
  "HS/some college",                "Borrow and save",    61.0,
  "HS/some college",                "Save",               51.0,
  "HS/some college",                "Population",         55.0,
  
  "College degree or more",         "Borrow",             14.0,
  "College degree or more",         "Borrow and save",    33.0,
  "College degree or more",         "Save",               36.0,
  "College degree or more",         "Population",         34.0
)

table2_group_cols <- c(
  "Borrow",
  "Borrow and save",
  "Save",
  "Share in population"
)

# scf_table2 currently stores shares as decimals, so convert to percentages.
# The scale check makes this robust if you later change scf_table2 to already
# report percentages.
table2_scale <- scf_table2 %>%
  select(all_of(table2_group_cols)) %>%
  unlist(use.names = FALSE) %>%
  max(na.rm = TRUE)

table2_multiplier <- if_else(table2_scale <= 1.5, 100, 1)

rep_table2 <- scf_table2 %>%
  pivot_longer(
    cols = all_of(table2_group_cols),
    names_to = "group",
    values_to = "replication"
  ) %>%
  mutate(
    group = recode(group, "Share in population" = "Population"),
    replication = replication * table2_multiplier
  )

table2_paper_comparison <- rep_table2 %>%
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

print(table2_paper_comparison)
write_csv(
  table2_paper_comparison,
  file.path(table_output_dir, "table2_paper_target_comparison.csv")
)


# ---- Combined paper-target comparison ----

paper_target_comparison <- bind_rows(
  table1_paper_comparison %>%
    mutate(characteristic = NA_character_) %>%
    select(table, source, statistic, characteristic, group, paper, replication, diff),
  
  table2_paper_comparison %>%
    select(table, source, statistic, characteristic, group, paper, replication, diff)
)

print(paper_target_comparison, n = Inf)

write_csv(
  paper_target_comparison,
  file.path(table_output_dir, "paper_target_comparison.csv")
)
