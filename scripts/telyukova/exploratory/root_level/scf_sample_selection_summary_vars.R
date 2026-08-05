# scf_sample_selection.R
#
# Experimental SCF-only setup for Telyukova (2013) Table 1 / Table 2
# replication. This fork keeps the same sample/group construction as
# scf_sample_selection.R, but uses Fed summary-extract variables for Table 2
# rows where there is a clean summary-variable analogue.
#
# Main inputs expected:
#   output/scf2001_raw_allvars.rds
#   raw/SCFP2001.csv
#
# Main outputs:
#   output/tables/summary_vars/scf_sample_diagnostics_stepwise.csv
#   output/tables/summary_vars/scf_sample_diagnostics_final.csv
#   output/tables/summary_vars/scf_sample_selection_flags_household.csv
#   output/tables/summary_vars/scf_sample_keep_keys.csv
#   output/tables/summary_vars/scf_base_sample_selected.rds
#   output/tables/summary_vars/scf_table1_analysis_sample_selected.rds
#   output/tables/summary_vars/scf_table1_analysis_sample_selected.csv.gz
#   output/tables/summary_vars/scf_group_diagnostics.csv
#   output/tables/summary_vars/table1_scf.csv
#   output/tables/summary_vars/table1_scf_paper_target_comparison.csv
#   output/tables/summary_vars/table1_cex.csv
#   output/tables/summary_vars/table1_combined_scf_cex.csv
#   output/tables/summary_vars/table1_paper_target_comparison.csv
#   output/tables/summary_vars/cex_sample_diagnostics_stepwise.csv
#   output/tables/summary_vars/cex_table1_debt_variant_scores.csv
#   output/tables/summary_vars/scf_table2.csv
#   output/tables/summary_vars/scf_table2_child_variants.csv
#   output/tables/summary_vars/scf_table2_work_summary_variant_comparison.csv
#   output/tables/summary_vars/scf_table2_work_summary_variant_scores.csv
#   output/tables/summary_vars/table2_scf_paper_target_comparison.csv
#   output/tables/summary_vars/table2_paper_target_comparison.csv
#   output/tables/summary_vars/scf_paper_target_comparison.csv
#   output/tables/summary_vars/replication_paper_target_comparison.csv
#   output/tables/summary_vars/paper_target_comparison.csv
#   output/tables/summary_vars/scf_cc_interest_rate_candidate_diagnostics.csv
#   output/tables/summary_vars/scf_rate_variable_map_candidate_diagnostics.csv
#   output/summary_vars/scf2001_raw_allvars_sample_selected.rds

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
scf_summary_path <- "raw/SCFP2001.csv"
cex_fmli_path <- "output/cex/cex_fmli_2000_2002_raw.rds"
cex_zip_dir <- "raw/cex"
cex_output_dir <- "output/cex"
table_output_dir <- "output/tables/summary_vars"
raw_output_dir <- "output/summary_vars"

dir.create(cex_output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(raw_output_dir, recursive = TRUE, showWarnings = FALSE)

# Telyukova appendix target for the SCF sample.
target_scf_households <- 2878L
target_cex_households <- 2743L
target_cex_complete_12_month_households <- 2164L

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

# CEX Appendix A.1/A.2 revolving-debt proxy. The fifth interview records
# annual credit-card finance charges; assuming a 14 percent APR gives an
# implied revolved balance finance_charges / 0.14. The primary CEX Table 1
# proxy below uses positive finance charges to distinguish revolvers and the
# average of the second- and fifth-interview credit-card balances as the debt
# amount; implied-balance variants are saved as diagnostics.
cex_finance_charge_apr <- 0.14

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

to_nonnegative_num <- function(x) {
  z <- to_num(x)
  if_else(is.na(z) | z <= 0, 0, z)
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

write_replication_comparison_latex <- function(
    comparison,
    output_path,
    caption = "Replication comparisons to Telyukova targets",
    label = "tab:replication-comparison") {
  bs <- intToUtf8(92)
  latex_command <- function(x) paste0(bs, x)
  latex_linebreak <- paste0(bs, bs)

  latex_escape <- function(x) {
    z <- if_else(is.na(x), "", as.character(x))
    z <- str_replace_all(z, fixed("&"), paste0(bs, "&"))
    z <- str_replace_all(z, fixed("%"), paste0(bs, "%"))
    z <- str_replace_all(z, fixed("_"), paste0(bs, "_"))
    z <- str_replace_all(z, fixed("#"), paste0(bs, "#"))
    z <- str_replace_all(z, fixed("$"), paste0(bs, "$"))
    z
  }

  comparison_for_tex <- comparison %>%
    mutate(
      panel = case_when(
        table == "Table 1" & source == "SCF 2001" ~ "Table 1 SCF",
        table == "Table 1" & str_detect(source, "^CEX") ~ "Table 1 CEX",
        table == "Table 2" & source == "SCF 2001" ~ "Table 2 SCF",
        TRUE ~ paste(table, source)
      ),
      measure = case_when(
        !is.na(characteristic) ~ characteristic,
        statistic == "Puzzle size (%)" ~ "Puzzle size",
        statistic == "Credit-card interest rate (%)" ~
          "Credit-card interest rate",
        statistic == "Share (%)" ~ "Share",
        TRUE ~ statistic
      )
    ) %>%
    select(panel, measure, group, paper, replication, diff)

  table_rows <- comparison_for_tex %>%
    mutate(
      row_id = row_number(),
      previous_panel = lag(panel),
      previous_measure = lag(measure),
      panel_print = if_else(
        row_id == 1L | panel != previous_panel,
        panel,
        ""
      ),
      measure_print = if_else(
        row_id == 1L |
          measure != previous_measure |
          panel != previous_panel,
        measure,
        ""
      ),
      line = paste0(
        latex_escape(panel_print), " & ",
        latex_escape(measure_print), " & ",
        latex_escape(group), " & ",
        sprintf("%.2f", paper), " & ",
        sprintf("%.2f", replication), " & ",
        sprintf("%.2f", diff), " ",
        latex_linebreak
      )
    ) %>%
    pull(line)

  latex_lines <- c(
    paste0(latex_command("begin"), "{longtable}{lllrrr}"),
    paste0(
      latex_command("caption"), "{", caption, "}",
      latex_command("label"), "{", label, "}",
      latex_linebreak
    ),
    latex_command("toprule"),
    paste0(
      "Panel & Measure & Group & Target & Replication & Difference ",
      latex_linebreak
    ),
    latex_command("midrule"),
    latex_command("endfirsthead"),
    latex_command("toprule"),
    paste0(
      "Panel & Measure & Group & Target & Replication & Difference ",
      latex_linebreak
    ),
    latex_command("midrule"),
    latex_command("endhead"),
    table_rows,
    latex_command("bottomrule"),
    paste0(
      latex_command("multicolumn"),
      "{6}{l}{",
      latex_command("footnotesize"),
      " Notes: Entries are percentages or percentage-point differences, ",
      "matching the source statistic.}",
      latex_linebreak
    ),
    paste0(latex_command("end"), "{longtable}")
  )

  writeLines(latex_lines, output_path)
}

read_cex_expense_file_from_zips <- function(file_prefix, zip_dir = cex_zip_dir) {
  specs <- tibble::tibble(
    cex_release_year = 2000:2002,
    zip_file = sprintf("intrvw%02d.zip", 0:2),
    file_regex = sprintf("%s%02d[.]csv$", file_prefix, 0:2)
  )

  bind_rows(lapply(seq_len(nrow(specs)), function(i) {
    zip_path <- file.path(zip_dir, specs$zip_file[[i]])
    if (!file.exists(zip_path)) {
      stop("Missing CEX zip file: ", zip_path, call. = FALSE)
    }

    zip_entries <- unzip(zip_path, list = TRUE)$Name
    file_entry <- zip_entries[
      str_detect(str_to_lower(zip_entries), specs$file_regex[[i]])
    ][1]

    if (is.na(file_entry)) {
      stop(
        "Could not find ", specs$file_regex[[i]], " inside ", zip_path,
        call. = FALSE
      )
    }

    read_csv(
      unz(zip_path, file_entry),
      col_types = cols(.default = col_character()),
      show_col_types = FALSE
    ) %>%
      mutate(
        cex_release_year = specs$cex_release_year[[i]],
        source_zip = specs$zip_file[[i]],
        source_file = file_entry
      ) %>%
      # EXPN files also contain the following Q1, repeated in the next release.
      filter(
        floor(to_num(QYEAR) / 10) == cex_release_year
      )
  }))
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

scf_summary_extract <- read_csv(scf_summary_path, show_col_types = FALSE)

needed_summary_vars <- c(
  "YY1", "Y1", "WGT",
  "RACE", "RACECL",
  "MARRIED",
  "KIDS", "FAMSTRUCT",
  "EDCL",
  "LF", "OCCAT1", "OCCAT2"
)

check_vars(scf_summary_extract, needed_summary_vars, "scf_summary_extract")

scf_summary_vars <- scf_summary_extract %>%
  transmute(
    case_id = to_num(YY1),
    implicate = to_num(Y1) - 10 * to_num(YY1),
    summary_weight = to_num(WGT),
    summary_race = to_num(RACE),
    summary_racecl = to_num(RACECL),
    summary_married = to_num(MARRIED),
    summary_kids = to_num(KIDS),
    summary_famstruct = to_num(FAMSTRUCT),
    summary_edcl = to_num(EDCL),
    summary_lf = to_num(LF),
    summary_occat1 = to_num(OCCAT1),
    summary_occat2 = to_num(OCCAT2),

    race_white_summary = as.integer(summary_race == 1),
    married_summary = as.integer(summary_married == 1),
    has_dependent_children_summary = as.integer(summary_famstruct == 4),
    less_than_hs_summary = as.integer(summary_edcl == 1),
    hs_some_college_summary = as.integer(summary_edcl %in% c(2, 3)),
    college_degree_or_more_summary = as.integer(summary_edcl == 4),

    # Work-status diagnostics. LF is an employment indicator rather than a
    # full-time indicator, so the main full-time row remains raw unless one of
    # these variants demonstrably improves the paper comparison.
    head_full_time_summary_lf = as.integer(summary_lf == 1),
    head_full_time_summary_occat1_one = as.integer(summary_occat1 == 1),
    head_full_time_summary_occat1_broad = as.integer(summary_occat1 %in% c(1, 2)),
    head_full_time_summary_lf_occat1_broad = as.integer(
      summary_lf == 1 & summary_occat1 %in% c(1, 2)
    ),

    head_working_summary = head_full_time_summary_lf,
    head_white_collar_occat1_summary = as.integer(summary_occat1 == 1),
    head_white_collar_occat2_summary = as.integer(summary_occat2 == 1),
    head_white_collar_occat2_broad_summary = as.integer(summary_occat2 %in% c(1, 2))
  )

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
  "X6809", "X7019", "X7020", "X7372", "X8023",
  "X4100", "X4105", "X4106", "X4110", "X4111",
  "X4507", "X4508", "X4511", "X7401",
  "X5901", "X5902", "X5904", "X5905"
)
table2_demog_j_vars <- c(
  "J7019", "J7020", "J7372", "J8023",
  "J4100", "J4105", "J4106", "J4110", "J4111",
  "J4507", "J4508", "J4511", "J7401"
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
spouse_partner_in_peu_x7019_codes <- c(
  1, 2, 5, 6, 7, 10, 13, 14, 15, 16, 19, 22, 25, 29, 30, 31,
  34, 37, 40, 44, 45, 48, 51, 52, 55, 60, 61, 64, 69, 70,
  73, 78, 79, 82
)

needed_scf_vars <- c(
  "Y1", "YY1", "X14", "X42001",
  income_var, income_shadow_var,
  "X432", "J432",
  cc_rate_source_var, cc_rate_shadow_var,
  cc_debt_vars,
  liquid_asset_vars,
  table2_demog_vars,
  table2_demog_j_vars,
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
    marital_arrangement = to_num(X7019),
    spouse_partner_peu_status = to_num(X7020),
    legal_marital_status = to_num(X7372),
    marital_current_status = to_num(X8023),
    married = as.integer(marital_current_status == 1),
    married_or_partnered = as.integer(marital_current_status %in% c(1, 2)),
    married_legal = as.integer(legal_marital_status == 1),
    spouse_partner_in_peu = as.integer(spouse_partner_peu_status == 2),
    spouse_partner_in_peu_x7019 = as.integer(
      marital_arrangement %in% spouse_partner_in_peu_x7019_codes
    ),
    married_legal_and_peu = as.integer(
      legal_marital_status == 1 & spouse_partner_peu_status == 2
    ),
    married_current_and_peu = as.integer(
      marital_current_status == 1 & spouse_partner_peu_status == 2
    ),
    married_current_or_peu = as.integer(
      marital_current_status == 1 | spouse_partner_peu_status == 2
    ),
    married_legal_or_peu = as.integer(
      legal_marital_status == 1 | spouse_partner_peu_status == 2
    ),
    married_legal_or_current_partner = as.integer(
      legal_marital_status == 1 | marital_current_status == 2
    ),
    current_partner_legal_separated = as.integer(
      marital_current_status == 2 & legal_marital_status == 3
    ),
    current_partner_legal_divorced = as.integer(
      marital_current_status == 2 & legal_marital_status == 4
    ),
    current_partner_legal_widowed = as.integer(
      marital_current_status == 2 & legal_marital_status == 5
    ),
    current_partner_legal_never_married = as.integer(
      marital_current_status == 2 & legal_marital_status == 6
    ),
    married_legal_plus_partner_divorced = as.integer(
      legal_marital_status == 1 |
        (marital_current_status == 2 & legal_marital_status == 4)
    ),
    married_legal_plus_partner_divorced_widowed = as.integer(
      legal_marital_status == 1 |
        (marital_current_status == 2 & legal_marital_status %in% c(4, 5))
    ),
    married_legal_plus_partner_separated_divorced = as.integer(
      legal_marital_status == 1 |
        (marital_current_status == 2 & legal_marital_status %in% c(3, 4))
    ),
    married_legal_plus_partner_separated_divorced_widowed = as.integer(
      legal_marital_status == 1 |
        (marital_current_status == 2 & legal_marital_status %in% c(3, 4, 5))
    ),
    married_legal_plus_partner_never_married = as.integer(
      legal_marital_status == 1 |
        (marital_current_status == 2 & legal_marital_status == 6)
    ),
    married_legal_plus_partner_widowed_never_married = as.integer(
      legal_marital_status == 1 |
        (marital_current_status == 2 & legal_marital_status %in% c(5, 6))
    ),
    married_current_reported_only = if_else(
      to_num(J8023) == 0,
      as.integer(marital_current_status == 1),
      NA_integer_
    ),
    married_legal_reported_only = if_else(
      to_num(J7372) == 0,
      as.integer(legal_marital_status == 1),
      NA_integer_
    ),
    head_work_status_recode = to_num(X4100),
    head_work_for_pay_status = to_num(X4105),
    head_main_job_class = to_num(X4106),
    head_main_job_weekly_hours = to_nonnegative_num(X4110),
    head_main_job_annual_weeks = to_nonnegative_num(X4111),
    head_other_jobs_weekly_hours = to_nonnegative_num(X4507),
    head_other_jobs_annual_weeks = to_nonnegative_num(X4508),
    head_total_weekly_hours = head_main_job_weekly_hours +
      head_other_jobs_weekly_hours,
    head_main_job_annual_hours = head_main_job_weekly_hours *
      head_main_job_annual_weeks,
    head_total_annual_hours = head_main_job_annual_hours +
      head_other_jobs_weekly_hours * head_other_jobs_annual_weeks,
    head_full_time = as.integer(to_num(X4511) == 1),
    head_full_time_drop_missing_imputed = if_else(
      to_num(J4511) >= 997,
      NA_integer_,
      as.integer(to_num(X4511) == 1)
    ),
    head_full_time_reported_edited = as.integer(
      to_num(X4511) == 1 & to_num(J4511) < 90
    ),
    head_full_time_reported_only = as.integer(
      to_num(X4511) == 1 & to_num(J4511) == 0
    ),
    head_full_time_worker_recode = as.integer(
      to_num(X4511) == 1 &
        head_work_status_recode %in% c(11, 12, 13, 14, 15, 16, 17)
    ),
    head_full_time_current_worker_recode = as.integer(
      to_num(X4511) == 1 &
        head_work_status_recode %in% c(11, 12, 13, 14, 15, 16, 17, 96)
    ),
    head_full_time_active_worker_recode = as.integer(
      to_num(X4511) == 1 &
        head_work_status_recode %in% c(11, 12, 13, 14, 15, 16, 96)
    ),
    head_full_time_worker_only = as.integer(
      to_num(X4511) == 1 & head_work_status_recode == 11
    ),
    head_full_time_employee = as.integer(
      to_num(X4511) == 1 & head_main_job_class == 1
    ),
    head_full_time_self_employed = as.integer(
      to_num(X4511) == 1 & head_main_job_class == 2
    ),
    head_full_time_main_hours30 = as.integer(head_main_job_weekly_hours >= 30),
    head_full_time_main_hours35 = as.integer(head_main_job_weekly_hours >= 35),
    head_full_time_total_hours30 = as.integer(head_total_weekly_hours >= 30),
    head_full_time_total_hours35 = as.integer(head_total_weekly_hours >= 35),
    head_full_time_self_report_or_total_hours35 = as.integer(
      to_num(X4511) == 1 | head_total_weekly_hours >= 35
    ),
    head_full_time_self_report_and_total_hours35 = as.integer(
      to_num(X4511) == 1 & head_total_weekly_hours >= 35
    ),
    head_full_time_annual_total1750 = as.integer(head_total_annual_hours >= 1750),
    head_occupation_public = to_num(X7401),
    head_white_collar_prof = as.integer(head_occupation_public %in% c(1, 2)),
    head_occ_managerial_professional = as.integer(head_occupation_public == 1),
    head_occ_technical_sales_admin = as.integer(head_occupation_public == 2),
    head_occ_service_protective_military = as.integer(head_occupation_public == 3),
    head_white_collar_public_12 = as.integer(head_occupation_public %in% c(1, 2)),
    head_white_collar_public_13 = as.integer(head_occupation_public %in% c(1, 2, 3)),
    head_white_collar_public_13_drop_missing_imputed = if_else(
      to_num(J7401) >= 997,
      NA_integer_,
      as.integer(head_occupation_public %in% c(1, 2, 3))
    ),
    head_white_collar_public_13_reported_coded = as.integer(
      head_occupation_public %in% c(1, 2, 3) & to_num(J7401) < 90
    ),
    head_white_collar_public_13_full_time = as.integer(
      head_occupation_public %in% c(1, 2, 3) & to_num(X4511) == 1
    ),
    head_white_collar_public_13_current_worker = as.integer(
      head_occupation_public %in% c(1, 2, 3) &
        head_work_status_recode %in% c(11, 12, 13, 14, 15, 16, 17, 96)
    ),
    head_white_collar_public_13_employee = as.integer(
      head_occupation_public %in% c(1, 2, 3) & head_main_job_class == 1
    ),
    head_white_collar_public_12_service_employee = as.integer(
      head_occupation_public %in% c(1, 2) |
        (head_occupation_public == 3 & head_main_job_class == 1)
    ),
    head_white_collar_public_12_service_full_time = as.integer(
      head_occupation_public %in% c(1, 2) |
        (head_occupation_public == 3 & to_num(X4511) == 1)
    ),
    head_white_collar_public_12_service_current_worker = as.integer(
      head_occupation_public %in% c(1, 2) |
        (
          head_occupation_public == 3 &
            head_work_status_recode %in% c(11, 12, 13, 14, 15, 16, 17, 96)
        )
    ),
    head_white_collar_public_14 = as.integer(head_occupation_public %in% c(1, 2, 3, 4)),
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
    college_degree_nonassociate = as.integer(
      has_college_degree & highest_degree %in% c(2, 3, 4, 11)
    ),
    college_degree_years16 = as.integer(educ_years >= 16),
    college_degree_years16_or_strict = as.integer(
      educ_years >= 16 |
        (has_college_degree & highest_degree %in% c(2, 3, 4))
    ),
    college_degree_years16_or_nonassociate = as.integer(
      educ_years >= 16 |
        (has_college_degree & highest_degree %in% c(2, 3, 4, 11))
    ),
    college_degree_include_associate = as.integer(
      has_college_degree & highest_degree %in% c(1, 2, 3, 4, 11)
    ),
    hs_some_college = as.integer(
      less_than_hs == 0 & college_degree_or_more == 0
    )
  ) %>%
  left_join(
    scf_summary_vars,
    by = c("case_id", "implicate")
  ) %>%
  mutate(
    race_white_raw = race_white,
    married_raw = married,
    married_from_raw_famstruct = as.integer(famstruct_raw %in% c(4, 5)),
    married_from_summary_famstruct = as.integer(summary_famstruct %in% c(4, 5)),
    has_dependent_children_raw_famstruct = has_dependent_children,
    head_full_time_raw = head_full_time,
    head_white_collar_prof_raw = head_white_collar_prof,
    head_white_collar_prof_raw_broad = as.integer(to_num(X7401) %in% c(1, 2, 3)),
    less_than_hs_raw = less_than_hs,
    hs_some_college_raw = hs_some_college,
    college_degree_or_more_raw = college_degree_or_more,

    # Main experimental change: Table 2 rows use the Fed summary extract where
    # the summary variable maps cleanly to the paper row.
    race_white = race_white_summary,
    married = married_legal_plus_partner_divorced_widowed,
    has_dependent_children = has_dependent_children_summary,
    head_full_time = head_full_time_raw,
    head_white_collar_prof = head_white_collar_occat2_broad_summary,
    less_than_hs = less_than_hs_summary,
    hs_some_college = hs_some_college_summary,
    college_degree_or_more = college_degree_or_more_summary
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
    marital_arrangement,
    spouse_partner_peu_status,
    legal_marital_status,
    marital_current_status,
    J7019,
    J7020,
    J7372,
    J8023,
    married,
    married_or_partnered,
    married_legal,
    spouse_partner_in_peu,
    spouse_partner_in_peu_x7019,
    married_legal_and_peu,
    married_current_and_peu,
    married_current_or_peu,
    married_legal_or_peu,
    married_legal_or_current_partner,
    current_partner_legal_separated,
    current_partner_legal_divorced,
    current_partner_legal_widowed,
    current_partner_legal_never_married,
    married_legal_plus_partner_divorced,
    married_legal_plus_partner_divorced_widowed,
    married_legal_plus_partner_separated_divorced,
    married_legal_plus_partner_separated_divorced_widowed,
    married_legal_plus_partner_never_married,
    married_legal_plus_partner_widowed_never_married,
    married_current_reported_only,
    married_legal_reported_only,
    head_work_status_recode,
    head_work_for_pay_status,
    head_main_job_class,
    head_main_job_weekly_hours,
    head_main_job_annual_weeks,
    head_other_jobs_weekly_hours,
    head_other_jobs_annual_weeks,
    head_total_weekly_hours,
    head_main_job_annual_hours,
    head_total_annual_hours,
    J4100,
    J4105,
    J4106,
    J4110,
    J4111,
    J4507,
    J4508,
    J4511,
    head_full_time,
    head_full_time_drop_missing_imputed,
    head_full_time_reported_edited,
    head_full_time_reported_only,
    head_full_time_worker_recode,
    head_full_time_current_worker_recode,
    head_full_time_active_worker_recode,
    head_full_time_worker_only,
    head_full_time_employee,
    head_full_time_self_employed,
    head_full_time_main_hours30,
    head_full_time_main_hours35,
    head_full_time_total_hours30,
    head_full_time_total_hours35,
    head_full_time_self_report_or_total_hours35,
    head_full_time_self_report_and_total_hours35,
    head_full_time_annual_total1750,
    head_occupation_public,
    J7401,
    head_white_collar_prof,
    head_occ_managerial_professional,
    head_occ_technical_sales_admin,
    head_occ_service_protective_military,
    head_white_collar_public_12,
    head_white_collar_public_13,
    head_white_collar_public_13_drop_missing_imputed,
    head_white_collar_public_13_reported_coded,
    head_white_collar_public_13_full_time,
    head_white_collar_public_13_current_worker,
    head_white_collar_public_13_employee,
    head_white_collar_public_12_service_employee,
    head_white_collar_public_12_service_full_time,
    head_white_collar_public_12_service_current_worker,
    head_white_collar_public_14,
    educ_years,
    hs_diploma_or_ged,
    has_college_degree,
    highest_degree,
    less_than_hs,
    hs_some_college,
    college_degree_or_more,
    college_degree_nonassociate,
    college_degree_years16,
    college_degree_years16_or_strict,
    college_degree_years16_or_nonassociate,
    college_degree_include_associate,
    kids_famstruct,
    famstruct_raw,
    has_dependent_children,
    has_dependent_children_famstruct4,
    has_dependent_children_roster_under18,
    has_dependent_children_roster_under16,
    summary_weight,
    summary_race,
    summary_racecl,
    summary_married,
    summary_kids,
    summary_famstruct,
    summary_edcl,
    summary_lf,
    summary_occat1,
    summary_occat2,
    race_white_raw,
    race_white_summary,
    married_raw,
    married_summary,
    married_from_raw_famstruct,
    married_from_summary_famstruct,
    has_dependent_children_raw_famstruct,
    has_dependent_children_summary,
    head_full_time_raw,
    head_full_time_summary_lf,
    head_full_time_summary_occat1_one,
    head_full_time_summary_occat1_broad,
    head_full_time_summary_lf_occat1_broad,
    head_white_collar_prof_raw,
    head_white_collar_prof_raw_broad,
    less_than_hs_raw,
    less_than_hs_summary,
    hs_some_college_raw,
    hs_some_college_summary,
    college_degree_or_more_raw,
    college_degree_or_more_summary,
    head_working_summary,
    head_white_collar_occat1_summary,
    head_white_collar_occat2_summary,
    head_white_collar_occat2_broad_summary
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
# 9. CEX Table 1 puzzle-size outputs
# -----------------------------------------------------------------------------

cex_fmli_raw <- readRDS(cex_fmli_path)

needed_cex_fmli_vars <- c(
  "NEWID", "FINLWT21", "AGE_REF", "FINCBTAX", "RESPSTAT",
  "CKBKACTX", "SAVACCTX", "CKBK_CTX", "SAVA_CTX",
  "QINTRVMO", "QINTRVYR", "INC_RANK", "INCLASS", "CUID",
  "cex_year", "cex_quarter"
)
check_vars(cex_fmli_raw, needed_cex_fmli_vars, "cex_fmli_raw")

cex_fna_raw <- read_cex_expense_file_from_zips("fna")
cex_fn2_raw <- read_cex_expense_file_from_zips("fn2")
cex_fnb_raw <- read_cex_expense_file_from_zips("fnb")

saveRDS(
  cex_fna_raw,
  file.path(cex_output_dir, "cex_fna_2000_2002_raw.rds")
)
saveRDS(
  cex_fn2_raw,
  file.path(cex_output_dir, "cex_fn2_2000_2002_raw.rds")
)
saveRDS(
  cex_fnb_raw,
  file.path(cex_output_dir, "cex_fnb_2000_2002_raw.rds")
)
write_csv(
  cex_fna_raw,
  file.path(cex_output_dir, "cex_fna_2000_2002_raw.csv.gz")
)
write_csv(
  cex_fn2_raw,
  file.path(cex_output_dir, "cex_fn2_2000_2002_raw.csv.gz")
)
write_csv(
  cex_fnb_raw,
  file.path(cex_output_dir, "cex_fnb_2000_2002_raw.csv.gz")
)

cex_collection_2001_qyears <- c(20011, 20012, 20013, 20014)
cex_calendar_2001_qyears <- c(20011, 20012, 20013, 20014, 20021)
cex_collection_2002_qyears <- c(20021, 20022, 20023, 20024)
cex_appendix_q2_2000_q1_2001_qyears <- c(20002, 20003, 20004, 20011)
cex_rolling_diagnostic_qyears <- c(20004, 20011, 20012, 20013, 20014,
                                   20021, 20022, 20023, 20024)
cex_diagnostic_qyears <- sort(unique(c(
  cex_collection_2001_qyears,
  cex_calendar_2001_qyears,
  cex_collection_2002_qyears,
  cex_rolling_diagnostic_qyears
)))

cex_fifth_interview_qyears <- cex_collection_2001_qyears
cex_credit_codes <- c(100)

cex_flag_clean <- function(x) {
  str_trim(as.character(x))
}

cex_amount_with_flag <- function(x, flag) {
  z <- to_amount(x)
  f <- cex_flag_clean(flag)

  case_when(
    f %in% c("B", "C") ~ NA_real_,
    f == "A" ~ 0,
    TRUE ~ z
  )
}

cex_qyear_to_index <- function(qyear) {
  floor(qyear / 10) * 4 + (qyear %% 10)
}

cex_index_to_qyear <- function(index) {
  year <- floor((index - 1) / 4)
  quarter <- index - year * 4
  year * 10 + quarter
}

cex_fmli_base <- cex_fmli_raw %>%
  mutate(
    newid = as.character(NEWID),
    cuid = na_if(str_trim(as.character(CUID)), ""),
    case_id = str_sub(newid, 1L, -2L),
    interview = to_num(str_sub(newid, -1L, -1L)),
    cex_year = to_num(cex_year),
    cex_quarter = to_num(cex_quarter),
    qyear = cex_year * 10 + cex_quarter,
    qyear_index = cex_qyear_to_index(qyear),
    qintrv_month = to_num(QINTRVMO),
    qintrv_year = to_num(QINTRVYR),
    qintrv_quarter = ceiling(qintrv_month / 3),
    qintrv_qyear = qintrv_year * 10 + qintrv_quarter,
    weight_raw = to_num(FINLWT21),
    weight = weight_raw / 4,
    age = to_num(AGE_REF),
    income_annual = to_num(FINCBTAX),
    complete_income_reporter = as.character(RESPSTAT) == "1",
    complete_income_from_rank = !is.na(to_num(INC_RANK)),
    complete_income_from_class = to_num(INCLASS) < 10,
    checking_flag = cex_flag_clean(CKBK_CTX),
    savings_flag = cex_flag_clean(SAVA_CTX),
    checking = cex_amount_with_flag(CKBKACTX, CKBK_CTX),
    savings = cex_amount_with_flag(SAVACCTX, SAVA_CTX),
    valid_liquid_assets = !is.na(checking) & !is.na(savings),
    liquid_assets_topcoded = checking_flag == "T" | savings_flag == "T",
    liquid_assets = if_else(
      valid_liquid_assets,
      coalesce(checking, 0) + coalesce(savings, 0),
      NA_real_
    )
  )

cex_panel_presence <- cex_fmli_base %>%
  mutate(
    positive_weight = !is.na(weight_raw) & weight_raw > 0,
    age_25_64 = age >= 25 & age <= 64,
    age_income_complete = age_25_64 &
      !is.na(income_annual) &
      income_annual >= income_annual_floor &
      complete_income_reporter
  ) %>%
  group_by(case_id) %>%
  summarise(
    interviews_observed = paste(sort(unique(interview)), collapse = ""),
    present_interviews_1_5 = all(1:5 %in% interview),
    present_full_12_months = all(2:5 %in% interview),
    present_full_12_months_positive_weight =
      all(2:5 %in% interview[positive_weight]),
    complete_income_all_2_5 =
      all(2:5 %in% interview[complete_income_reporter]),
    complete_income_rank_all_2_5 =
      all(2:5 %in% interview[complete_income_from_rank]),
    complete_income_class_all_2_5 =
      all(2:5 %in% interview[complete_income_from_class]),
    age_25_64_all_2_5 = all(2:5 %in% interview[age_25_64]),
    age_income_complete_all_2_5 =
      all(2:5 %in% interview[age_income_complete]),
    valid_liquid_assets_all_2_5 =
      all(2:5 %in% interview[valid_liquid_assets]),
    first_public_qyear = min(qyear, na.rm = TRUE),
    any_public_in_appendix_window =
      any(qyear %in% cex_appendix_q2_2000_q1_2001_qyears),
    qyear_i2 = first(qyear[interview == 2], default = NA_real_),
    qyear_i5 = first(qyear[interview == 5], default = NA_real_),
    qindex_i2 = first(qyear_index[interview == 2], default = NA_real_),
    qindex_i3 = first(qyear_index[interview == 3], default = NA_real_),
    qindex_i4 = first(qyear_index[interview == 4], default = NA_real_),
    qindex_i5 = first(qyear_index[interview == 5], default = NA_real_),
    .groups = "drop"
  ) %>%
  mutate(
    present_full_12_months_consecutive = present_full_12_months &
      !if_any(
        all_of(c("qindex_i2", "qindex_i3", "qindex_i4", "qindex_i5")),
        is.na
      ) &
      qindex_i3 - qindex_i2 == 1 &
      qindex_i4 - qindex_i3 == 1 &
      qindex_i5 - qindex_i4 == 1
  )

cex_cc_balance_fifth <- cex_fna_raw %>%
  transmute(
    newid = as.character(NEWID),
    qyear = to_num(QYEAR),
    credit_code = to_num(CREDITR5),
    cc_balance_fifth = cex_amount_with_flag(CREDITX5, CRED_TX5),
    balance_fifth_invalid = cex_flag_clean(CRED_TX5) %in% c("B", "C")
  ) %>%
  filter(
    qyear %in% cex_diagnostic_qyears,
    credit_code %in% cex_credit_codes
  ) %>%
  group_by(newid, qyear) %>%
  summarise(
    cc_balance_fifth = sum(cc_balance_fifth, na.rm = TRUE),
    balance_fifth_invalid = any(balance_fifth_invalid),
    n_fifth_balance_records = n(),
    .groups = "drop"
  )

cex_cc_balance_second <- cex_fn2_raw %>%
  transmute(
    newid = as.character(NEWID),
    qyear = to_num(QYEAR),
    credit_code = to_num(CREDITR1),
    cc_balance_second = cex_amount_with_flag(CREDITX1, CRED_TX1)
  ) %>%
  filter(
    qyear %in% cex_diagnostic_qyears,
    credit_code %in% cex_credit_codes
  ) %>%
  group_by(newid, qyear) %>%
  summarise(
    cc_balance_second = sum(cc_balance_second, na.rm = TRUE),
    n_second_balance_records = n(),
    .groups = "drop"
  )

cex_cc_finance_charges <- cex_fnb_raw %>%
  transmute(
    newid = as.character(NEWID),
    qyear = to_num(QYEAR),
    cc_finance_charges = cex_amount_with_flag(CRDCARDX, CRDC_RDX),
    finance_charges_invalid = cex_flag_clean(CRDC_RDX) %in% c("B", "C")
  ) %>%
  filter(qyear %in% cex_diagnostic_qyears) %>%
  group_by(newid, qyear) %>%
  summarise(
    cc_finance_charges = sum(cc_finance_charges, na.rm = TRUE),
    finance_charges_invalid = any(finance_charges_invalid),
    n_finance_charge_records = n(),
    .groups = "drop"
  )

cex_all_base_rows <- cex_fmli_base %>%
  filter(
    interview == 5,
    qyear %in% cex_diagnostic_qyears
  ) %>%
  left_join(cex_panel_presence, by = "case_id") %>%
  left_join(cex_cc_balance_fifth, by = c("newid", "qyear")) %>%
  left_join(cex_cc_balance_second, by = c("newid", "qyear")) %>%
  left_join(cex_cc_finance_charges, by = c("newid", "qyear")) %>%
  mutate(
    cc_balance_fifth = coalesce(cc_balance_fifth, 0),
    cc_balance_second = coalesce(cc_balance_second, 0),
    cc_finance_charges = coalesce(cc_finance_charges, 0),
    balance_fifth_invalid = coalesce(balance_fifth_invalid, FALSE),
    finance_charges_invalid = coalesce(finance_charges_invalid, FALSE),
    n_fifth_balance_records = coalesce(n_fifth_balance_records, 0L),
    n_second_balance_records = coalesce(n_second_balance_records, 0L),
    n_finance_charge_records = coalesce(n_finance_charge_records, 0L),
    valid_cc_debt_info = !balance_fifth_invalid & !finance_charges_invalid,
    inferred_first_interview_qyear =
      cex_index_to_qyear(qyear_index - 4),
    inferred_public_second_interview_qyear =
      cex_index_to_qyear(qyear_index - 3),
    cohort_fifth_interview_2001 =
      qyear %in% cex_collection_2001_qyears,
    cohort_inferred_public_i2_q2_2000_q1_2001 =
      inferred_public_second_interview_qyear %in%
        cex_appendix_q2_2000_q1_2001_qyears,
    cohort_actual_public_i2_q2_2000_q1_2001 =
      qyear_i2 %in% cex_appendix_q2_2000_q1_2001_qyears,
    cohort_observed_public_start_q2_2000_q1_2001 =
      first_public_qyear %in% cex_appendix_q2_2000_q1_2001_qyears,
    cohort_any_public_q2_2000_q1_2001 =
      any_public_in_appendix_window,
    cohort_inferred_first_interview_q2_2000_q1_2001 =
      inferred_first_interview_qyear %in%
        cex_appendix_q2_2000_q1_2001_qyears,
    cc_balance_average_2_5 = case_when(
      cc_balance_second > 0 & cc_balance_fifth > 0 ~
        (cc_balance_second + cc_balance_fifth) / 2,
      cc_balance_fifth > 0 ~ cc_balance_fifth,
      cc_balance_second > 0 ~ cc_balance_second,
      TRUE ~ 0
    ),
    cc_balance_max_2_5 = pmax(cc_balance_second, cc_balance_fifth),
    cc_debt_implied_from_finance_charges =
      cc_finance_charges / cex_finance_charge_apr,
    cc_debt_finance_positive_average_balance = if_else(
      cc_finance_charges > 0,
      cc_balance_average_2_5,
      0
    ),
    cc_debt_finance_positive_fifth_balance = if_else(
      cc_finance_charges > 0,
      cc_balance_fifth,
      0
    ),
    cc_debt_finance_ge_70_average_balance = if_else(
      cc_finance_charges >= cex_finance_charge_apr * cc_debt_group_cutoff,
      cc_balance_average_2_5,
      0
    ),
    cc_debt_apr14_average_balance = if_else(
      cc_balance_average_2_5 > 0 &
        cc_finance_charges >= cex_finance_charge_apr * cc_balance_average_2_5,
      cc_balance_average_2_5,
      0
    ),
    cc_debt_implied_capped_at_average_balance = pmin(
      cc_debt_implied_from_finance_charges,
      cc_balance_average_2_5
    ),
    cc_debt = cc_debt_finance_positive_average_balance
  )

cex_base_rows <- cex_all_base_rows %>%
  filter(qyear %in% cex_fifth_interview_qyears) %>%
  add_telyukova_group()

cex_household_flags <- cex_base_rows %>%
  transmute(
    case_id,
    keep_age_only = age >= 25 & age <= 64,
    keep_after_income_floor = keep_age_only &
      !is.na(income_annual) &
      income_annual >= income_annual_floor,
    keep_after_income_complete = keep_after_income_floor &
      complete_income_reporter,
    keep_final = keep_after_income_complete &
      valid_liquid_assets &
      valid_cc_debt_info &
      !is.na(weight_raw) &
      weight_raw > 0,
    present_full_12_months
  )

write_csv(
  cex_household_flags,
  file.path(table_output_dir, "cex_sample_selection_flags_household.csv")
)

cex_sample_diag <- function(data, label) {
  tibble(
    sample_status = label,
    n_rows = nrow(data),
    n_households = n_distinct(data$case_id),
    n_complete_12_month_households = n_distinct(
      data$case_id[data$present_full_12_months]
    ),
    weighted_population = sum(data$weight, na.rm = TRUE),
    age_mean = weighted_mean_safe(data$age, data$weight),
    income_mean = weighted_mean_safe(data$income_annual, data$weight)
  )
}

cex_rows_with_flags <- cex_base_rows %>%
  left_join(
    cex_household_flags %>%
      select(
        case_id,
        keep_age_only,
        keep_after_income_floor,
        keep_after_income_complete,
        keep_final
      ),
    by = "case_id"
  )

cex_sample_diagnostics_stepwise <- bind_rows(
  cex_sample_diag(cex_rows_with_flags, "00 CEX fifth interviews in 2001"),
  cex_sample_diag(filter(cex_rows_with_flags, keep_age_only), "01 age 25-64"),
  cex_sample_diag(
    filter(cex_rows_with_flags, keep_after_income_floor),
    "02 + income >= $200/month"
  ),
  cex_sample_diag(
    filter(cex_rows_with_flags, keep_after_income_complete),
    "03 + complete income reporter"
  ),
  cex_sample_diag(
    filter(cex_rows_with_flags, keep_final),
    "04 + valid fifth-interview asset/debt info"
  )
) %>%
  mutate(
    target_households = target_cex_households,
    household_gap_vs_target = n_households - target_cex_households,
    target_complete_12_month_households =
      target_cex_complete_12_month_households,
    complete_12_month_gap_vs_target =
      n_complete_12_month_households -
        target_cex_complete_12_month_households
  )

print(cex_sample_diagnostics_stepwise)

write_csv(
  cex_sample_diagnostics_stepwise,
  file.path(table_output_dir, "cex_sample_diagnostics_stepwise.csv")
)

cex_sample_diagnostics_final <- bind_rows(
  cex_sample_diag(filter(cex_rows_with_flags, !keep_final), "Dropped"),
  cex_sample_diag(filter(cex_rows_with_flags, keep_final), "Kept")
) %>%
  mutate(
    target_households = if_else(
      sample_status == "Kept",
      target_cex_households,
      NA_integer_
    ),
    household_gap_vs_target = if_else(
      sample_status == "Kept",
      n_households - target_cex_households,
      NA_integer_
    ),
    target_complete_12_month_households = if_else(
      sample_status == "Kept",
      target_cex_complete_12_month_households,
      NA_integer_
    ),
    complete_12_month_gap_vs_target = if_else(
      sample_status == "Kept",
      n_complete_12_month_households -
        target_cex_complete_12_month_households,
      NA_integer_
    )
  )

write_csv(
  cex_sample_diagnostics_final,
  file.path(table_output_dir, "cex_sample_diagnostics_final.csv")
)

cex_table1_analysis <- cex_rows_with_flags %>%
  filter(keep_final)

saveRDS(
  cex_table1_analysis,
  file.path(table_output_dir, "cex_table1_analysis_sample_selected.rds")
)
write_csv(
  cex_table1_analysis,
  file.path(table_output_dir, "cex_table1_analysis_sample_selected.csv.gz")
)

cex_sample_keep_keys <- cex_table1_analysis %>%
  distinct(case_id, newid, qyear)

write_csv(
  cex_sample_keep_keys,
  file.path(table_output_dir, "cex_sample_keep_keys.csv")
)

cex_table1_denominator <- sum(cex_table1_analysis$weight, na.rm = TRUE)

cex_table1 <- cex_table1_analysis %>%
  filter(!is.na(group)) %>%
  group_by(group) %>%
  summarise(
    source = "CEX",
    n_households = n_distinct(case_id),
    n_complete_12_month_households = n_distinct(
      case_id[present_full_12_months]
    ),
    weighted_population = sum(weight, na.rm = TRUE),
    share_pct = 100 * weighted_population / cex_table1_denominator,
    .groups = "drop"
  ) %>%
  select(
    source, group, share_pct,
    n_households, n_complete_12_month_households,
    weighted_population
  )

print(cex_table1)

write_csv(
  cex_table1,
  file.path(table_output_dir, "table1_cex.csv")
)

cex_table1_targets <- tibble::tribble(
  ~statistic, ~group, ~paper,
  "Puzzle size (%)", "Borrow", 7.0,
  "Puzzle size (%)", "Borrow and save", 29.0,
  "Puzzle size (%)", "Save", 64.0
)

cex_table1_replication <- cex_table1 %>%
  transmute(
    statistic = "Puzzle size (%)",
    group = as.character(group),
    replication = share_pct
  )

cex_table1_target_comparison <- cex_table1_targets %>%
  left_join(cex_table1_replication, by = c("statistic", "group")) %>%
  mutate(
    table = "Table 1",
    source = "CEX 2001",
    diff = replication - paper
  ) %>%
  select(table, source, statistic, group, paper, replication, diff)

print(cex_table1_target_comparison)

write_csv(
  cex_table1_target_comparison,
  file.path(table_output_dir, "table1_cex_paper_target_comparison.csv")
)

table1_target_comparison <- bind_rows(
  scf_table1_target_comparison,
  cex_table1_target_comparison
)

write_csv(
  table1_target_comparison,
  file.path(table_output_dir, "table1_paper_target_comparison.csv")
)

table1_combined_scf_cex <- bind_rows(
  scf_table1 %>%
    mutate(source = "SCF 2001") %>%
    select(source, group, share_pct, cc_rate_pct),
  cex_table1 %>%
    mutate(source = "CEX 2001", cc_rate_pct = NA_real_) %>%
    select(source, group, share_pct, cc_rate_pct)
)

write_csv(
  table1_combined_scf_cex,
  file.path(table_output_dir, "table1_combined_scf_cex.csv")
)

cex_table1_debt_variant_specs <- tibble::tribble(
  ~variant, ~debt_variable,
  "primary: finance charges > 0, average balance 2/5",
  "cc_debt_finance_positive_average_balance",
  "finance charges > 0, fifth-interview balance",
  "cc_debt_finance_positive_fifth_balance",
  "finance charges >= 14% of $500, average balance 2/5",
  "cc_debt_finance_ge_70_average_balance",
  "finance charges / 0.14 implied balance",
  "cc_debt_implied_from_finance_charges",
  "implied balance capped at average balance 2/5",
  "cc_debt_implied_capped_at_average_balance",
  "finance charges >= 14% of average balance 2/5",
  "cc_debt_apr14_average_balance",
  "fifth-interview balance only",
  "cc_balance_fifth",
  "average second/fifth balance only",
  "cc_balance_average_2_5"
)

cex_table1_debt_variant_comparison <- bind_rows(
  lapply(seq_len(nrow(cex_table1_debt_variant_specs)), function(i) {
    debt_variable <- cex_table1_debt_variant_specs$debt_variable[[i]]

    cex_table1_analysis %>%
      mutate(cc_debt = .data[[debt_variable]]) %>%
      add_telyukova_group() %>%
      filter(!is.na(group)) %>%
      group_by(group) %>%
      summarise(
        share_pct = 100 * sum(weight, na.rm = TRUE) /
          sum(cex_table1_analysis$weight, na.rm = TRUE),
        n_households = n_distinct(case_id),
        .groups = "drop"
      ) %>%
      right_join(
        tibble(group = factor(
          c("Borrow", "Borrow and save", "Save"),
          levels = c("Borrow", "Borrow and save", "Save")
        )),
        by = "group"
      ) %>%
      mutate(
        share_pct = coalesce(share_pct, 0),
        n_households = coalesce(n_households, 0L),
        variant = cex_table1_debt_variant_specs$variant[[i]],
        debt_variable = debt_variable
      )
  })
) %>%
  left_join(cex_table1_targets, by = "group") %>%
  mutate(diff = share_pct - paper) %>%
  select(
    variant, debt_variable, statistic, group,
    paper, share_pct, diff, n_households
  )

cex_table1_debt_variant_scores <- cex_table1_debt_variant_comparison %>%
  group_by(variant, debt_variable) %>%
  summarise(
    mean_abs_diff = mean(abs(diff), na.rm = TRUE),
    max_abs_diff = max(abs(diff), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(mean_abs_diff)

print(cex_table1_debt_variant_scores)

write_csv(
  cex_table1_debt_variant_comparison,
  file.path(table_output_dir, "cex_table1_debt_variant_comparison.csv")
)
write_csv(
  cex_table1_debt_variant_scores,
  file.path(table_output_dir, "cex_table1_debt_variant_scores.csv")
)

cex_table1_sample_window_specs <- tibble::tibble(
  sample_window = c(
    "collection_2001_4q",
    "calendar_2001_5q",
    "collection_2002_4q"
  ),
  qyears_label = c(
    paste(cex_collection_2001_qyears, collapse = ";"),
    paste(cex_calendar_2001_qyears, collapse = ";"),
    paste(cex_collection_2002_qyears, collapse = ";")
  ),
  qyears = list(
    cex_collection_2001_qyears,
    cex_calendar_2001_qyears,
    cex_collection_2002_qyears
  )
)

cex_table1_sample_window_comparison <- bind_rows(
  lapply(seq_len(nrow(cex_table1_sample_window_specs)), function(w) {
    qyears <- cex_table1_sample_window_specs$qyears[[w]]

    window_analysis <- cex_all_base_rows %>%
      filter(
        qyear %in% qyears,
        age >= 25,
        age <= 64,
        !is.na(income_annual),
        income_annual >= income_annual_floor,
        complete_income_reporter,
        valid_liquid_assets,
        valid_cc_debt_info,
        !is.na(weight_raw),
        weight_raw > 0
      )

    bind_rows(
      lapply(seq_len(nrow(cex_table1_debt_variant_specs)), function(i) {
        debt_variable <- cex_table1_debt_variant_specs$debt_variable[[i]]

        window_analysis %>%
          mutate(cc_debt = .data[[debt_variable]]) %>%
          add_telyukova_group() %>%
          filter(!is.na(group)) %>%
          group_by(group) %>%
          summarise(
            share_pct = 100 * sum(weight, na.rm = TRUE) /
              sum(window_analysis$weight, na.rm = TRUE),
            n_households_in_group = n_distinct(case_id),
            .groups = "drop"
          ) %>%
          right_join(
            tibble(group = factor(
              c("Borrow", "Borrow and save", "Save"),
              levels = c("Borrow", "Borrow and save", "Save")
            )),
            by = "group"
          ) %>%
          mutate(
            share_pct = coalesce(share_pct, 0),
            n_households_in_group = coalesce(n_households_in_group, 0L),
            sample_window =
              cex_table1_sample_window_specs$sample_window[[w]],
            qyears = cex_table1_sample_window_specs$qyears_label[[w]],
            n_households = n_distinct(window_analysis$case_id),
            n_complete_12_month_households = n_distinct(
              window_analysis$case_id[window_analysis$present_full_12_months]
            ),
            variant = cex_table1_debt_variant_specs$variant[[i]],
            debt_variable = debt_variable
          )
      })
    )
  })
) %>%
  left_join(cex_table1_targets, by = "group") %>%
  mutate(diff = share_pct - paper) %>%
  select(
    sample_window, qyears, variant, debt_variable,
    n_households, n_complete_12_month_households,
    statistic, group, paper, share_pct, diff, n_households_in_group
  )

cex_table1_sample_window_scores <- cex_table1_sample_window_comparison %>%
  group_by(
    sample_window, qyears, variant, debt_variable,
    n_households, n_complete_12_month_households
  ) %>%
  summarise(
    mean_abs_diff = mean(abs(diff), na.rm = TRUE),
    max_abs_diff = max(abs(diff), na.rm = TRUE),
    household_gap_vs_target = first(n_households) - target_cex_households,
    complete_12_month_gap_vs_target =
      first(n_complete_12_month_households) -
        target_cex_complete_12_month_households,
    .groups = "drop"
  ) %>%
  arrange(mean_abs_diff)

print(cex_table1_sample_window_scores)

write_csv(
  cex_table1_sample_window_comparison,
  file.path(table_output_dir, "cex_table1_sample_window_comparison.csv")
)
write_csv(
  cex_table1_sample_window_scores,
  file.path(table_output_dir, "cex_table1_sample_window_scores.csv")
)

cex_format_qyear <- function(qyear) {
  sprintf("%dQ%d", floor(qyear / 10), qyear %% 10)
}

cex_rolling_window_specs <- bind_rows(lapply(
  seq_len(max(length(cex_rolling_diagnostic_qyears) - 3L, 0L)),
  function(i) {
    qyears <- cex_rolling_diagnostic_qyears[i:(i + 3L)]
    if (!all(diff(cex_qyear_to_index(qyears)) == 1)) {
      return(tibble())
    }

    tibble(
      sample_window = paste0(
        "rolling_",
        cex_format_qyear(first(qyears)),
        "_to_",
        cex_format_qyear(last(qyears))
      ),
      qyears_list = list(qyears),
      qyears = paste(qyears, collapse = ";"),
    )
  }
))

cex_truncation_rule_specs <- tibble::tribble(
  ~truncation_rule, ~rule_flag,
  "current valid flags", "rule_current",
  "income complete via INC_RANK", "rule_income_rank_complete",
  "income complete via INCLASS", "rule_income_class_complete",
  "exclude topcoded liquid assets", "rule_exclude_topcoded_liquid_assets",
  "require interviews 2-5 present", "rule_require_present_2_5",
  "require interviews 2-5 consecutive", "rule_require_consecutive_2_5",
  "require positive weights in interviews 2-5", "rule_require_positive_weight_2_5",
  "require age 25-64 in interviews 2-5", "rule_require_age_25_64_all_2_5",
  "require complete income in interviews 2-5", "rule_require_complete_income_all_2_5",
  "require valid liquid assets in interviews 2-5", "rule_require_valid_assets_all_2_5",
  "require fifth balance detail row", "rule_require_fifth_balance_record",
  "require second or fifth balance detail row",
  "rule_require_second_or_fifth_balance_record",
  "exclude fifth-only panels", "rule_exclude_fifth_only_panels",
  "exclude topcoded assets and fifth-only panels",
  "rule_exclude_topcoded_assets_and_fifth_only_panels"
)

cex_table1_share_rows <- function(data) {
  group_levels <- tibble(group = factor(
    c("Borrow", "Borrow and save", "Save"),
    levels = c("Borrow", "Borrow and save", "Save")
  ))

  if (nrow(data) == 0 || sum(data$weight, na.rm = TRUE) <= 0) {
    return(group_levels %>% mutate(share_pct = 0, n_households_in_group = 0L))
  }

  data %>%
    mutate(cc_debt = cc_debt_finance_positive_average_balance) %>%
    add_telyukova_group() %>%
    filter(!is.na(group)) %>%
    group_by(group) %>%
    summarise(
      share_pct = 100 * sum(weight, na.rm = TRUE) /
        sum(data$weight, na.rm = TRUE),
      n_households_in_group = n_distinct(case_id),
      .groups = "drop"
    ) %>%
    right_join(group_levels, by = "group") %>%
    mutate(
      share_pct = coalesce(share_pct, 0),
      n_households_in_group = coalesce(n_households_in_group, 0L)
    )
}

cex_sample_truncation_variant_comparison <- bind_rows(lapply(
  seq_len(nrow(cex_rolling_window_specs)),
  function(w) {
    qyears <- cex_rolling_window_specs$qyears_list[[w]]

    window_base <- cex_all_base_rows %>%
      filter(
        qyear %in% qyears,
        age >= 25,
        age <= 64,
        !is.na(income_annual),
        income_annual >= income_annual_floor,
        valid_liquid_assets,
        valid_cc_debt_info,
        !is.na(weight_raw),
        weight_raw > 0
      ) %>%
      mutate(
        rule_current = complete_income_reporter,
        rule_income_rank_complete = complete_income_from_rank,
        rule_income_class_complete = complete_income_from_class,
        rule_exclude_topcoded_liquid_assets =
          complete_income_reporter & !liquid_assets_topcoded,
        rule_require_present_2_5 =
          complete_income_reporter & present_full_12_months,
        rule_require_consecutive_2_5 =
          complete_income_reporter & present_full_12_months_consecutive,
        rule_require_positive_weight_2_5 =
          complete_income_reporter & present_full_12_months_positive_weight,
        rule_require_age_25_64_all_2_5 =
          complete_income_reporter & age_25_64_all_2_5,
        rule_require_complete_income_all_2_5 =
          complete_income_reporter & complete_income_all_2_5,
        rule_require_valid_assets_all_2_5 =
          complete_income_reporter & valid_liquid_assets_all_2_5,
        rule_require_fifth_balance_record =
          complete_income_reporter & n_fifth_balance_records > 0,
        rule_require_second_or_fifth_balance_record =
          complete_income_reporter &
            (n_second_balance_records > 0 | n_fifth_balance_records > 0),
        rule_exclude_fifth_only_panels =
          complete_income_reporter & interviews_observed != "5",
        rule_exclude_topcoded_assets_and_fifth_only_panels =
          complete_income_reporter &
            !liquid_assets_topcoded &
            interviews_observed != "5"
      )

    bind_rows(lapply(seq_len(nrow(cex_truncation_rule_specs)), function(r) {
      rule_flag <- cex_truncation_rule_specs$rule_flag[[r]]
      variant_data <- window_base %>%
        filter(coalesce(.data[[rule_flag]], FALSE))

      cex_table1_share_rows(variant_data) %>%
        mutate(
          sample_window = cex_rolling_window_specs$sample_window[[w]],
          qyears = cex_rolling_window_specs$qyears[[w]],
          truncation_rule = cex_truncation_rule_specs$truncation_rule[[r]],
          n_households = n_distinct(variant_data$case_id),
          n_cuids = n_distinct(variant_data$cuid[!is.na(variant_data$cuid)]),
          n_complete_12_month_households = n_distinct(
            variant_data$case_id[variant_data$present_full_12_months]
          ),
          n_partial_panel_households =
            n_households - n_complete_12_month_households,
          weighted_population = sum(variant_data$weight, na.rm = TRUE)
        )
    }))
  }
)) %>%
  left_join(cex_table1_targets, by = "group") %>%
  mutate(diff = share_pct - paper) %>%
  select(
    sample_window, qyears, truncation_rule,
    n_households, n_cuids, n_complete_12_month_households,
    n_partial_panel_households, weighted_population,
    statistic, group, paper, share_pct, diff, n_households_in_group
  )

cex_sample_truncation_variant_scores <-
  cex_sample_truncation_variant_comparison %>%
  group_by(
    sample_window, qyears, truncation_rule,
    n_households, n_cuids, n_complete_12_month_households,
    n_partial_panel_households, weighted_population
  ) %>%
  summarise(
    mean_abs_diff = mean(abs(diff), na.rm = TRUE),
    max_abs_diff = max(abs(diff), na.rm = TRUE),
    household_gap_vs_target = first(n_households) - target_cex_households,
    complete_12_month_gap_vs_target =
      first(n_complete_12_month_households) -
        target_cex_complete_12_month_households,
    partial_panel_gap_vs_target =
      first(n_partial_panel_households) -
        (target_cex_households - target_cex_complete_12_month_households),
    borrow_share = share_pct[group == "Borrow"],
    borrow_and_save_share = share_pct[group == "Borrow and save"],
    save_share = share_pct[group == "Save"],
    .groups = "drop"
  ) %>%
  mutate(
    total_count_abs_gap =
      abs(household_gap_vs_target) +
      abs(complete_12_month_gap_vs_target)
  ) %>%
  arrange(total_count_abs_gap, mean_abs_diff)

print(cex_sample_truncation_variant_scores %>% head(20))

cex_partial_panel_pattern_diagnostics <- cex_all_base_rows %>%
  filter(
    qyear %in% cex_collection_2001_qyears,
    age >= 25,
    age <= 64,
    !is.na(income_annual),
    income_annual >= income_annual_floor,
    complete_income_reporter,
    valid_liquid_assets,
    valid_cc_debt_info,
    !is.na(weight_raw),
    weight_raw > 0
  ) %>%
  mutate(
    liquid_assets_not_topcoded = !liquid_assets_topcoded
  ) %>%
  count(
    liquid_assets_not_topcoded,
    qyear,
    present_full_12_months,
    interviews_observed,
    name = "n_households"
  ) %>%
  arrange(
    liquid_assets_not_topcoded,
    qyear,
    present_full_12_months,
    interviews_observed
  )

cex_asset_flag_diagnostics <- cex_all_base_rows %>%
  filter(
    qyear %in% cex_collection_2001_qyears,
    age >= 25,
    age <= 64,
    !is.na(income_annual),
    income_annual >= income_annual_floor,
    complete_income_reporter,
    valid_cc_debt_info,
    !is.na(weight_raw),
    weight_raw > 0
  ) %>%
  count(
    valid_liquid_assets,
    liquid_assets_topcoded,
    checking_flag,
    savings_flag,
    present_full_12_months,
    name = "n_households"
  ) %>%
  arrange(
    valid_liquid_assets,
    liquid_assets_topcoded,
    checking_flag,
    savings_flag,
    present_full_12_months
  )

cex_appendix_timing_cohort_specs <- tibble::tribble(
  ~cohort_rule, ~cohort_flag,
  "fifth interview in 2001Q1-2001Q4",
  "cohort_fifth_interview_2001",
  "appendix as inferred public interview-2 in 2000Q2-2001Q1",
  "cohort_inferred_public_i2_q2_2000_q1_2001",
  "appendix as actual observed public interview-2 in 2000Q2-2001Q1",
  "cohort_actual_public_i2_q2_2000_q1_2001",
  "appendix as earliest observed public interview in 2000Q2-2001Q1",
  "cohort_observed_public_start_q2_2000_q1_2001",
  "appendix as any observed public interview in 2000Q2-2001Q1",
  "cohort_any_public_q2_2000_q1_2001",
  "appendix as inferred first/bounding interview in 2000Q2-2001Q1",
  "cohort_inferred_first_interview_q2_2000_q1_2001"
)

cex_appendix_truncation_rule_specs <- tibble::tribble(
  ~truncation_rule, ~rule_flag,
  "current valid flags", "rule_current",
  "exclude topcoded liquid assets", "rule_exclude_topcoded_liquid_assets",
  "exclude fifth-only panels", "rule_exclude_fifth_only_panels",
  "exclude topcoded assets and fifth-only panels",
  "rule_exclude_topcoded_assets_and_fifth_only_panels"
)

cex_appendix_timing_truncation_comparison <- bind_rows(lapply(
  seq_len(nrow(cex_appendix_timing_cohort_specs)),
  function(c) {
    cohort_flag <- cex_appendix_timing_cohort_specs$cohort_flag[[c]]

    cohort_base <- cex_all_base_rows %>%
      filter(
        coalesce(.data[[cohort_flag]], FALSE),
        age >= 25,
        age <= 64,
        !is.na(income_annual),
        income_annual >= income_annual_floor,
        valid_liquid_assets,
        valid_cc_debt_info,
        !is.na(weight_raw),
        weight_raw > 0
      ) %>%
      mutate(
        rule_current = complete_income_reporter,
        rule_exclude_topcoded_liquid_assets =
          complete_income_reporter & !liquid_assets_topcoded,
        rule_exclude_fifth_only_panels =
          complete_income_reporter & interviews_observed != "5",
        rule_exclude_topcoded_assets_and_fifth_only_panels =
          complete_income_reporter &
            !liquid_assets_topcoded &
            interviews_observed != "5"
      )

    bind_rows(lapply(
      seq_len(nrow(cex_appendix_truncation_rule_specs)),
      function(r) {
        rule_flag <- cex_appendix_truncation_rule_specs$rule_flag[[r]]
        variant_data <- cohort_base %>%
          filter(coalesce(.data[[rule_flag]], FALSE))

        cex_table1_share_rows(variant_data) %>%
          mutate(
            appendix_window = "2000Q2-2001Q1",
            cohort_rule = cex_appendix_timing_cohort_specs$cohort_rule[[c]],
            truncation_rule =
              cex_appendix_truncation_rule_specs$truncation_rule[[r]],
            fifth_interview_qyears = paste(
              sort(unique(variant_data$qyear)),
              collapse = ";"
            ),
            n_households = n_distinct(variant_data$case_id),
            n_complete_12_month_households = n_distinct(
              variant_data$case_id[variant_data$present_full_12_months]
            ),
            n_partial_panel_households =
              n_households - n_complete_12_month_households,
            weighted_population = sum(variant_data$weight, na.rm = TRUE)
          )
      }
    ))
  }
)) %>%
  left_join(cex_table1_targets, by = "group") %>%
  mutate(diff = share_pct - paper) %>%
  select(
    appendix_window, cohort_rule, truncation_rule, fifth_interview_qyears,
    n_households, n_complete_12_month_households,
    n_partial_panel_households, weighted_population,
    statistic, group, paper, share_pct, diff, n_households_in_group
  )

cex_appendix_timing_truncation_scores <-
  cex_appendix_timing_truncation_comparison %>%
  group_by(
    appendix_window, cohort_rule, truncation_rule, fifth_interview_qyears,
    n_households, n_complete_12_month_households,
    n_partial_panel_households, weighted_population
  ) %>%
  summarise(
    mean_abs_diff = mean(abs(diff), na.rm = TRUE),
    max_abs_diff = max(abs(diff), na.rm = TRUE),
    household_gap_vs_target = first(n_households) - target_cex_households,
    complete_12_month_gap_vs_target =
      first(n_complete_12_month_households) -
        target_cex_complete_12_month_households,
    partial_panel_gap_vs_target =
      first(n_partial_panel_households) -
        (target_cex_households - target_cex_complete_12_month_households),
    borrow_share = share_pct[group == "Borrow"],
    borrow_and_save_share = share_pct[group == "Borrow and save"],
    save_share = share_pct[group == "Save"],
    .groups = "drop"
  ) %>%
  mutate(
    total_count_abs_gap =
      abs(household_gap_vs_target) +
      abs(complete_12_month_gap_vs_target)
  ) %>%
  arrange(total_count_abs_gap, mean_abs_diff)

print(cex_appendix_timing_truncation_scores)

write_csv(
  cex_sample_truncation_variant_comparison,
  file.path(table_output_dir, "cex_sample_truncation_variant_comparison.csv")
)
write_csv(
  cex_sample_truncation_variant_scores,
  file.path(table_output_dir, "cex_sample_truncation_variant_scores.csv")
)
write_csv(
  cex_partial_panel_pattern_diagnostics,
  file.path(table_output_dir, "cex_partial_panel_pattern_diagnostics.csv")
)
write_csv(
  cex_asset_flag_diagnostics,
  file.path(table_output_dir, "cex_asset_flag_diagnostics.csv")
)
write_csv(
  cex_appendix_timing_truncation_comparison,
  file.path(table_output_dir, "cex_appendix_timing_truncation_comparison.csv")
)
write_csv(
  cex_appendix_timing_truncation_scores,
  file.path(table_output_dir, "cex_appendix_timing_truncation_scores.csv")
)

# -----------------------------------------------------------------------------
# 10. SCF Table 2 demographics
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
    summary_famstruct4 = 100 * weighted_mean_safe(has_dependent_children_summary, weight),
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

table2_education_variant_specs <- tibble::tribble(
  ~candidate, ~less_than_hs_variable, ~college_variable,
  "primary fork: summary EDCL",
  "less_than_hs_summary", "college_degree_or_more_summary",
  "raw questions: diploma/GED + bachelor degree codes 2/3/4",
  "less_than_hs_raw", "college_degree_or_more_raw",
  "summary less-HS + bachelor degree codes 2/3/4",
  "less_than_hs_summary", "college_degree_or_more_raw",
  "summary less-HS + non-associate degree codes 2/3/4/11",
  "less_than_hs_summary", "college_degree_nonassociate",
  "summary less-HS + four or more completed college years",
  "less_than_hs_summary", "college_degree_years16",
  "summary less-HS + four-plus years or bachelor degree codes",
  "less_than_hs_summary", "college_degree_years16_or_strict",
  "summary less-HS + four-plus years or non-associate degree",
  "less_than_hs_summary", "college_degree_years16_or_nonassociate",
  "raw less-HS + four-plus years or non-associate degree",
  "less_than_hs_raw", "college_degree_years16_or_nonassociate",
  "summary less-HS + any reported college degree including associate",
  "less_than_hs_summary", "college_degree_include_associate"
)

scf_table2_education_variant_comparison <- bind_rows(
  lapply(seq_len(nrow(table2_education_variant_specs)), function(i) {
    less_variable <-
      table2_education_variant_specs$less_than_hs_variable[[i]]
    college_variable <- table2_education_variant_specs$college_variable[[i]]

    variant_rows <- scf_table1_analysis %>%
      transmute(
        group,
        weight,
        less_than_hs_variant = as.integer(.data[[less_variable]]),
        college_degree_or_more_variant =
          as.integer(.data[[college_variable]])
      ) %>%
      mutate(
        hs_some_college_variant = as.integer(
          less_than_hs_variant == 0 &
            college_degree_or_more_variant == 0
        )
      )

    bind_rows(
      variant_rows %>%
        filter(!is.na(group)) %>%
        group_by(group) %>%
        summarise(
          less_than_hs_variant = 100 * weighted_mean_safe(
            less_than_hs_variant, weight
          ),
          hs_some_college_variant = 100 * weighted_mean_safe(
            hs_some_college_variant, weight
          ),
          college_degree_or_more_variant = 100 * weighted_mean_safe(
            college_degree_or_more_variant, weight
          ),
          .groups = "drop"
        ) %>%
        mutate(group = as.character(group)),
      variant_rows %>%
        summarise(
          less_than_hs_variant = 100 * weighted_mean_safe(
            less_than_hs_variant, weight
          ),
          hs_some_college_variant = 100 * weighted_mean_safe(
            hs_some_college_variant, weight
          ),
          college_degree_or_more_variant = 100 * weighted_mean_safe(
            college_degree_or_more_variant, weight
          )
        ) %>%
        mutate(group = "Population")
    ) %>%
      pivot_longer(
        cols = ends_with("_variant"),
        names_to = "education_category",
        values_to = "replication"
      ) %>%
      mutate(
        characteristic = recode(
          education_category,
          less_than_hs_variant = "Education: less than HS",
          hs_some_college_variant = "HS/some college",
          college_degree_or_more_variant = "College degree or more"
        ),
        candidate = table2_education_variant_specs$candidate[[i]],
        less_than_hs_variable = less_variable,
        college_variable = college_variable
      )
  })
) %>%
  left_join(
    paper_table2_targets,
    by = c("characteristic", "group")
  ) %>%
  mutate(diff = replication - paper) %>%
  select(
    candidate, less_than_hs_variable, college_variable,
    characteristic, group, paper, replication, diff
  )

scf_table2_education_variant_scores <-
  scf_table2_education_variant_comparison %>%
  group_by(candidate, less_than_hs_variable, college_variable) %>%
  summarise(
    mean_abs_diff = mean(abs(diff), na.rm = TRUE),
    max_abs_diff = max(abs(diff), na.rm = TRUE),
    college_mean_abs_diff = mean(
      abs(diff[characteristic == "College degree or more"]),
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  arrange(mean_abs_diff)

scf_table2_education_code_distribution <- scf_table1_analysis %>%
  group_by(educ_years, has_college_degree, highest_degree) %>%
  summarise(
    n_implicate_rows = n(),
    weighted_population = sum(weight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    weighted_share_pct = 100 * weighted_population /
      sum(weighted_population)
  ) %>%
  arrange(desc(weighted_population))

print(scf_table2_education_variant_scores)

write_csv(
  scf_table2_education_variant_comparison,
  file.path(table_output_dir, "scf_table2_education_variant_comparison.csv")
)
write_csv(
  scf_table2_education_variant_scores,
  file.path(table_output_dir, "scf_table2_education_variant_scores.csv")
)
write_csv(
  scf_table2_education_code_distribution,
  file.path(table_output_dir, "scf_table2_education_code_distribution.csv")
)

table2_marital_variant_specs <- tibble::tribble(
  ~characteristic, ~candidate, ~variable,
  "Marital status: married", "primary fork: raw X8023 == 1", "married",
  "Marital status: married", "raw X8023 == 1", "married_raw",
  "Marital status: married", "raw X7372 == 1 legal marital status",
  "married_legal",
  "Marital status: married", "hybrid: legal married + partner/divorced",
  "married_legal_plus_partner_divorced",
  "Marital status: married", "hybrid: legal married + partner/divorced/widowed",
  "married_legal_plus_partner_divorced_widowed",
  "Marital status: married", "hybrid: legal married + partner/separated/divorced",
  "married_legal_plus_partner_separated_divorced",
  "Marital status: married", "hybrid: legal married + partner/separated/divorced/widowed",
  "married_legal_plus_partner_separated_divorced_widowed",
  "Marital status: married", "diagnostic: legal married + partner/never married",
  "married_legal_plus_partner_never_married",
  "Marital status: married", "diagnostic: legal married + partner/widowed/never married",
  "married_legal_plus_partner_widowed_never_married",
  "Marital status: married", "diagnostic: partner/separated only",
  "current_partner_legal_separated",
  "Marital status: married", "diagnostic: partner/divorced only",
  "current_partner_legal_divorced",
  "Marital status: married", "diagnostic: partner/widowed only",
  "current_partner_legal_widowed",
  "Marital status: married", "diagnostic: partner/never married only",
  "current_partner_legal_never_married",
  "Marital status: married", "raw X8023 == 1, nonzero J8023 dropped",
  "married_current_reported_only",
  "Marital status: married", "raw X7372 == 1, nonzero J7372 dropped",
  "married_legal_reported_only",
  "Marital status: married", "raw X7020 == 2 spouse/partner in PEU",
  "spouse_partner_in_peu",
  "Marital status: married", "raw X7019 spouse/partner in PEU codes",
  "spouse_partner_in_peu_x7019",
  "Marital status: married", "raw X7372 == 1 & X7020 == 2",
  "married_legal_and_peu",
  "Marital status: married", "raw X8023 == 1 & X7020 == 2",
  "married_current_and_peu",
  "Marital status: married", "raw X8023 == 1 | X7020 == 2",
  "married_current_or_peu",
  "Marital status: married", "raw X7372 == 1 | X7020 == 2",
  "married_legal_or_peu",
  "Marital status: married", "raw X7372 == 1 | X8023 == 2",
  "married_legal_or_current_partner",
  "Marital status: married", "raw X8023 in 1:2",
  "married_or_partnered",
  "Marital status: married", "summary MARRIED == 1", "married_summary",
  "Marital status: married", "raw FAMSTRUCT in 4:5",
  "married_from_raw_famstruct",
  "Marital status: married", "summary FAMSTRUCT in 4:5",
  "married_from_summary_famstruct"
)

scf_table2_marital_variant_comparison <- bind_rows(
  lapply(seq_len(nrow(table2_marital_variant_specs)), function(i) {
    variant_variable <- table2_marital_variant_specs$variable[[i]]

    bind_rows(
      scf_table1_analysis %>%
        filter(!is.na(group)) %>%
        group_by(group) %>%
        summarise(
          replication = 100 * weighted_mean_safe(.data[[variant_variable]], weight),
          .groups = "drop"
        ) %>%
        mutate(group = as.character(group)),
      scf_table1_analysis %>%
        summarise(
          group = "Population",
          replication = 100 * weighted_mean_safe(.data[[variant_variable]], weight)
        )
    ) %>%
      mutate(
        characteristic = table2_marital_variant_specs$characteristic[[i]],
        candidate = table2_marital_variant_specs$candidate[[i]],
        variable = variant_variable
      )
  })
) %>%
  left_join(
    paper_table2_targets,
    by = c("characteristic", "group")
  ) %>%
  mutate(diff = replication - paper) %>%
  select(characteristic, candidate, variable, group, paper, replication, diff)

scf_table2_marital_variant_scores <-
  scf_table2_marital_variant_comparison %>%
  group_by(characteristic, candidate, variable) %>%
  summarise(
    mean_abs_diff = mean(abs(diff), na.rm = TRUE),
    max_abs_diff = max(abs(diff), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(mean_abs_diff)

print(scf_table2_marital_variant_scores)

write_csv(
  scf_table2_marital_variant_comparison,
  file.path(table_output_dir, "scf_table2_marital_variant_comparison.csv")
)
write_csv(
  scf_table2_marital_variant_scores,
  file.path(table_output_dir, "scf_table2_marital_variant_scores.csv")
)

scf_table2_marital_flag_vars <- c("J8023", "J7372", "J7020", "J7019")
scf_table2_marital_flag_normal_codes <- tibble::tribble(
  ~flag, ~normal_code,
  "J8023", 0,
  "J7372", 0,
  "J7020", 8,
  "J7019", 8
)

scf_table2_marital_flag_summary <- bind_rows(
  lapply(scf_table2_marital_flag_vars, function(flag_var) {
    normal_code <- scf_table2_marital_flag_normal_codes$normal_code[
      match(flag_var, scf_table2_marital_flag_normal_codes$flag)
    ]

    scf_table1_analysis %>%
      mutate(
        flag_value = to_num(.data[[flag_var]]),
        non_normal_flag = !is.na(flag_value) & flag_value != normal_code,
        missing_or_imputed_flag = !is.na(flag_value) &
          (flag_value %in% c(15, 17) | flag_value >= 997),
        assumption_flag = !is.na(flag_value) & flag_value == 12
      ) %>%
      summarise(
        flag = flag_var,
        normal_code = normal_code,
        n_non_normal_implicate_rows = sum(non_normal_flag),
        weighted_non_normal_share_pct = 100 *
          sum(if_else(non_normal_flag, weight, 0), na.rm = TRUE) /
          sum(weight, na.rm = TRUE),
        n_missing_or_imputed_implicate_rows = sum(missing_or_imputed_flag),
        weighted_missing_or_imputed_share_pct = 100 *
          sum(if_else(missing_or_imputed_flag, weight, 0), na.rm = TRUE) /
          sum(weight, na.rm = TRUE),
        n_assumption_implicate_rows = sum(assumption_flag),
        weighted_assumption_share_pct = 100 *
          sum(if_else(assumption_flag, weight, 0), na.rm = TRUE) /
          sum(weight, na.rm = TRUE),
        .groups = "drop"
      )
  })
)

scf_table2_marital_flag_by_code <- bind_rows(
  lapply(scf_table2_marital_flag_vars, function(flag_var) {
    scf_table1_analysis %>%
      group_by(code = to_num(.data[[flag_var]])) %>%
      summarise(
        n_implicate_rows = n(),
        weighted_population = sum(weight, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        flag = flag_var,
        weighted_share_pct = 100 * weighted_population / sum(weighted_population)
      ) %>%
      select(flag, code, n_implicate_rows, weighted_population, weighted_share_pct)
  })
) %>%
  arrange(flag, desc(weighted_population))

print(scf_table2_marital_flag_summary)

write_csv(
  scf_table2_marital_flag_summary,
  file.path(table_output_dir, "scf_table2_marital_flag_summary.csv")
)
write_csv(
  scf_table2_marital_flag_by_code,
  file.path(table_output_dir, "scf_table2_marital_flag_by_code.csv")
)

table2_work_variant_specs <- tibble::tribble(
  ~characteristic, ~candidate, ~variable,
  "Head works full-time", "primary fork: raw X4511 == 1", "head_full_time",
  "Head works full-time", "raw X4511 == 1, drop missing/imputed J4511 >= 997",
  "head_full_time_drop_missing_imputed",
  "Head works full-time", "raw X4511 == 1, J4511 < 90",
  "head_full_time_reported_edited",
  "Head works full-time", "raw X4511 == 1, J4511 == 0",
  "head_full_time_reported_only",
  "Head works full-time", "raw X4511 == 1 & X4100 in 11:17",
  "head_full_time_worker_recode",
  "Head works full-time", "raw X4511 == 1 & X4100 in 11:17 or 96",
  "head_full_time_current_worker_recode",
  "Head works full-time", "raw X4511 == 1 & active worker recode",
  "head_full_time_active_worker_recode",
  "Head works full-time", "raw X4511 == 1 & X4100 == 11",
  "head_full_time_worker_only",
  "Head works full-time", "raw X4511 == 1 & employee",
  "head_full_time_employee",
  "Head works full-time", "raw X4511 == 1 & self-employed",
  "head_full_time_self_employed",
  "Head works full-time", "main-job hours >= 30",
  "head_full_time_main_hours30",
  "Head works full-time", "main-job hours >= 35",
  "head_full_time_main_hours35",
  "Head works full-time", "main + other job hours >= 30",
  "head_full_time_total_hours30",
  "Head works full-time", "main + other job hours >= 35",
  "head_full_time_total_hours35",
  "Head works full-time", "raw X4511 == 1 or total hours >= 35",
  "head_full_time_self_report_or_total_hours35",
  "Head works full-time", "raw X4511 == 1 and total hours >= 35",
  "head_full_time_self_report_and_total_hours35",
  "Head works full-time", "annual total hours >= 1750",
  "head_full_time_annual_total1750",
  "Head works full-time", "summary LF == 1", "head_full_time_summary_lf",
  "Head works full-time", "summary OCCAT1 == 1", "head_full_time_summary_occat1_one",
  "Head works full-time", "summary OCCAT1 in 1:2", "head_full_time_summary_occat1_broad",
  "Head works full-time", "summary LF == 1 & OCCAT1 in 1:2",
  "head_full_time_summary_lf_occat1_broad",
  "Head white-collar/prof.", "primary fork: summary OCCAT2 in 1:2",
  "head_white_collar_prof",
  "Head white-collar/prof.", "public X7401 == 1: managerial/professional",
  "head_occ_managerial_professional",
  "Head white-collar/prof.", "public X7401 == 2: technical/sales/admin",
  "head_occ_technical_sales_admin",
  "Head white-collar/prof.", "public X7401 == 3: service/protective/military",
  "head_occ_service_protective_military",
  "Head white-collar/prof.", "raw X7401 in 1:2", "head_white_collar_prof_raw",
  "Head white-collar/prof.", "public X7401 in 1:2",
  "head_white_collar_public_12",
  "Head white-collar/prof.", "raw X7401 in 1:3", "head_white_collar_prof_raw_broad",
  "Head white-collar/prof.", "public X7401 in 1:3",
  "head_white_collar_public_13",
  "Head white-collar/prof.", "public X7401 in 1:3, drop missing/imputed J7401 >= 997",
  "head_white_collar_public_13_drop_missing_imputed",
  "Head white-collar/prof.", "public X7401 in 1:3, J7401 < 90",
  "head_white_collar_public_13_reported_coded",
  "Head white-collar/prof.", "public X7401 in 1:3 & full-time",
  "head_white_collar_public_13_full_time",
  "Head white-collar/prof.", "public X7401 in 1:3 & current worker recode",
  "head_white_collar_public_13_current_worker",
  "Head white-collar/prof.", "public X7401 in 1:3 & employee",
  "head_white_collar_public_13_employee",
  "Head white-collar/prof.", "public X7401 in 1:2 + service employees",
  "head_white_collar_public_12_service_employee",
  "Head white-collar/prof.", "public X7401 in 1:2 + service full-time",
  "head_white_collar_public_12_service_full_time",
  "Head white-collar/prof.", "public X7401 in 1:2 + service current workers",
  "head_white_collar_public_12_service_current_worker",
  "Head white-collar/prof.", "public X7401 in 1:4",
  "head_white_collar_public_14",
  "Head white-collar/prof.", "summary OCCAT1 == 1",
  "head_white_collar_occat1_summary",
  "Head white-collar/prof.", "summary OCCAT2 == 1",
  "head_white_collar_occat2_summary",
  "Head white-collar/prof.", "summary OCCAT2 in 1:2",
  "head_white_collar_occat2_broad_summary"
)

scf_table2_work_summary_variant_comparison <- bind_rows(
  lapply(seq_len(nrow(table2_work_variant_specs)), function(i) {
    variant_variable <- table2_work_variant_specs$variable[[i]]

    bind_rows(
      scf_table1_analysis %>%
        filter(!is.na(group)) %>%
        group_by(group) %>%
        summarise(
          replication = 100 * weighted_mean_safe(.data[[variant_variable]], weight),
          .groups = "drop"
        ) %>%
        mutate(group = as.character(group)),
      scf_table1_analysis %>%
        summarise(
          group = "Population",
          replication = 100 * weighted_mean_safe(.data[[variant_variable]], weight)
        )
    ) %>%
      mutate(
        characteristic = table2_work_variant_specs$characteristic[[i]],
        candidate = table2_work_variant_specs$candidate[[i]],
        variable = variant_variable
      )
  })
) %>%
  left_join(
    paper_table2_targets,
    by = c("characteristic", "group")
  ) %>%
  mutate(diff = replication - paper) %>%
  select(characteristic, candidate, variable, group, paper, replication, diff)

scf_table2_work_summary_variant_scores <-
  scf_table2_work_summary_variant_comparison %>%
  group_by(characteristic, candidate, variable) %>%
  summarise(
    mean_abs_diff = mean(abs(diff), na.rm = TRUE),
    max_abs_diff = max(abs(diff), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(characteristic, mean_abs_diff)

print(scf_table2_work_summary_variant_scores)

write_csv(
  scf_table2_work_summary_variant_comparison,
  file.path(table_output_dir, "scf_table2_work_summary_variant_comparison.csv")
)
write_csv(
  scf_table2_work_summary_variant_scores,
  file.path(table_output_dir, "scf_table2_work_summary_variant_scores.csv")
)

scf_table2_full_time_flag_by_code <- scf_table1_analysis %>%
  group_by(code = to_num(J4511)) %>%
  summarise(
    n_implicate_rows = n(),
    weighted_population = sum(weight, na.rm = TRUE),
    full_time_weighted_population = sum(
      if_else(head_full_time == 1, weight, 0),
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  mutate(
    weighted_share_pct = 100 * weighted_population / sum(weighted_population),
    full_time_weighted_share_pct = 100 *
      full_time_weighted_population / sum(full_time_weighted_population)
  ) %>%
  arrange(desc(weighted_population))

scf_table2_full_time_flag_summary <- scf_table1_analysis %>%
  mutate(
    j4511 = to_num(J4511),
    missing_or_imputed_flag = !is.na(j4511) & (j4511 %in% c(15, 17) | j4511 >= 997)
  ) %>%
  summarise(
    n_missing_or_imputed_implicate_rows = sum(missing_or_imputed_flag),
    weighted_missing_or_imputed_share_pct = 100 *
      sum(if_else(missing_or_imputed_flag, weight, 0), na.rm = TRUE) /
      sum(weight, na.rm = TRUE),
    weighted_full_time_missing_or_imputed_share_pct = 100 *
      sum(if_else(missing_or_imputed_flag & head_full_time == 1, weight, 0), na.rm = TRUE) /
      sum(if_else(head_full_time == 1, weight, 0), na.rm = TRUE),
    .groups = "drop"
  )

scf_table2_full_time_x4100_decomposition <- scf_table1_analysis %>%
  filter(!is.na(group)) %>%
  group_by(group) %>%
  mutate(group_weight = sum(weight, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(head_full_time == 1) %>%
  group_by(group, head_work_status_recode) %>%
  summarise(
    full_time_weighted_population = sum(weight, na.rm = TRUE),
    group_weight = first(group_weight),
    .groups = "drop"
  ) %>%
  mutate(
    share_of_group_weight_pct = 100 * full_time_weighted_population / group_weight
  ) %>%
  arrange(group, desc(share_of_group_weight_pct))

print(scf_table2_full_time_flag_summary)

write_csv(
  scf_table2_full_time_flag_summary,
  file.path(table_output_dir, "scf_table2_full_time_flag_summary.csv")
)
write_csv(
  scf_table2_full_time_flag_by_code,
  file.path(table_output_dir, "scf_table2_full_time_flag_by_code.csv")
)
write_csv(
  scf_table2_full_time_x4100_decomposition,
  file.path(table_output_dir, "scf_table2_full_time_x4100_decomposition.csv")
)

scf_table2_white_collar_x7401_decomposition <- scf_table1_analysis %>%
  filter(!is.na(group)) %>%
  group_by(group) %>%
  mutate(group_weight = sum(weight, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(group, head_occupation_public) %>%
  summarise(
    weighted_population = sum(weight, na.rm = TRUE),
    group_weight = first(group_weight),
    .groups = "drop"
  ) %>%
  mutate(
    share_of_group_weight_pct = 100 * weighted_population / group_weight,
    occupation_public_label = case_when(
      head_occupation_public == 0 ~ "0 no current job/inapplicable",
      head_occupation_public == 1 ~ "1 managerial/professional",
      head_occupation_public == 2 ~ "2 technical/sales/admin",
      head_occupation_public == 3 ~ "3 service/protective/military",
      head_occupation_public == 4 ~ "4 precision/craft/repair",
      head_occupation_public == 5 ~ "5 operators/laborers/transport",
      head_occupation_public == 6 ~ "6 farming/forestry/fishing",
      TRUE ~ "other/missing"
    )
  ) %>%
  arrange(group, head_occupation_public)

scf_table2_white_collar_x7401_summary_crosswalk <- scf_table1_analysis %>%
  group_by(head_occupation_public, summary_occat2) %>%
  summarise(
    weighted_population = sum(weight, na.rm = TRUE),
    n_implicate_rows = n(),
    .groups = "drop"
  ) %>%
  mutate(
    weighted_share_pct = 100 * weighted_population / sum(weighted_population)
  ) %>%
  arrange(head_occupation_public, summary_occat2)

scf_table2_white_collar_flag_by_code <- scf_table1_analysis %>%
  group_by(code = to_num(J7401)) %>%
  summarise(
    n_implicate_rows = n(),
    weighted_population = sum(weight, na.rm = TRUE),
    broad_white_collar_weighted_population = sum(
      if_else(head_white_collar_public_13 == 1, weight, 0),
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  mutate(
    weighted_share_pct = 100 * weighted_population / sum(weighted_population),
    broad_white_collar_weighted_share_pct = 100 *
      broad_white_collar_weighted_population /
      sum(broad_white_collar_weighted_population)
  ) %>%
  arrange(desc(weighted_population))

write_csv(
  scf_table2_white_collar_x7401_decomposition,
  file.path(table_output_dir, "scf_table2_white_collar_x7401_decomposition.csv")
)
write_csv(
  scf_table2_white_collar_x7401_summary_crosswalk,
  file.path(table_output_dir, "scf_table2_white_collar_x7401_summary_crosswalk.csv")
)
write_csv(
  scf_table2_white_collar_flag_by_code,
  file.path(table_output_dir, "scf_table2_white_collar_flag_by_code.csv")
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

raw_table2_comparison_path <- file.path(
  "output/tables",
  "table2_scf_paper_target_comparison.csv"
)

if (file.exists(raw_table2_comparison_path)) {
  raw_table2_target_comparison <- read_csv(
    raw_table2_comparison_path,
    show_col_types = FALSE
  ) %>%
    mutate(version = "raw_variable_constructions")

  table2_raw_vs_summary_long <- bind_rows(
    raw_table2_target_comparison,
    scf_table2_target_comparison %>%
      mutate(version = "summary_variable_constructions")
  ) %>%
    relocate(version, .after = source)

  table2_raw_vs_summary_comparison <- table2_raw_vs_summary_long %>%
    select(version, characteristic, group, paper, replication, diff) %>%
    pivot_wider(
      names_from = version,
      values_from = c(replication, diff),
      names_glue = "{version}_{.value}"
    ) %>%
    mutate(
      raw_abs_diff = abs(raw_variable_constructions_diff),
      summary_abs_diff = abs(summary_variable_constructions_diff),
      abs_diff_improvement = raw_abs_diff - summary_abs_diff
    ) %>%
    arrange(desc(abs_diff_improvement), characteristic, group)

  write_csv(
    table2_raw_vs_summary_long,
    file.path(table_output_dir, "table2_raw_vs_summary_long.csv")
  )
  write_csv(
    table2_raw_vs_summary_comparison,
    file.path(table_output_dir, "table2_raw_vs_summary_comparison.csv")
  )
}

scf_paper_target_comparison <- bind_rows(
  scf_table1_target_comparison %>%
    mutate(characteristic = NA_character_) %>%
    select(table, source, statistic, characteristic, group, paper, replication, diff),
  scf_table2_target_comparison %>%
    select(table, source, statistic, characteristic, group, paper, replication, diff)
)

replication_paper_target_comparison <- bind_rows(
  table1_target_comparison %>%
    mutate(characteristic = NA_character_) %>%
    select(table, source, statistic, characteristic, group, paper, replication, diff),
  scf_table2_target_comparison %>%
    select(table, source, statistic, characteristic, group, paper, replication, diff)
)

cex_appendix_table1_target_comparison <-
  cex_appendix_timing_truncation_comparison %>%
  filter(
    cohort_rule ==
      "appendix as inferred first/bounding interview in 2000Q2-2001Q1",
    truncation_rule == "exclude fifth-only panels"
  ) %>%
  transmute(
    table = "Table 1",
    source = "CEX 2001 appendix timing",
    statistic = "Puzzle size (%)",
    characteristic = NA_character_,
    group = as.character(group),
    paper,
    replication = share_pct,
    diff
  )

table1_appendix_cex_target_comparison <- bind_rows(
  scf_table1_target_comparison %>%
    mutate(characteristic = NA_character_) %>%
    select(table, source, statistic, characteristic, group, paper, replication, diff),
  cex_appendix_table1_target_comparison
)

replication_paper_target_comparison_appendix_cex <- bind_rows(
  table1_appendix_cex_target_comparison,
  scf_table2_target_comparison %>%
    select(table, source, statistic, characteristic, group, paper, replication, diff)
)

write_csv(
  scf_paper_target_comparison,
  file.path(table_output_dir, "scf_paper_target_comparison.csv")
)
write_csv(
  replication_paper_target_comparison,
  file.path(table_output_dir, "replication_paper_target_comparison.csv")
)
write_csv(
  replication_paper_target_comparison,
  file.path(table_output_dir, "paper_target_comparison.csv")
)
write_csv(
  cex_appendix_table1_target_comparison,
  file.path(
    table_output_dir,
    "table1_cex_appendix_timing_paper_target_comparison.csv"
  )
)
write_csv(
  table1_appendix_cex_target_comparison,
  file.path(table_output_dir, "table1_appendix_cex_paper_target_comparison.csv")
)
write_csv(
  replication_paper_target_comparison_appendix_cex,
  file.path(
    table_output_dir,
    "replication_paper_target_comparison_appendix_cex.csv"
  )
)
write_csv(
  replication_paper_target_comparison_appendix_cex,
  file.path(table_output_dir, "paper_target_comparison_appendix_cex.csv")
)

write_replication_comparison_latex(
  replication_paper_target_comparison,
  file.path(table_output_dir, "replication_paper_target_comparison.tex")
)
write_replication_comparison_latex(
  replication_paper_target_comparison_appendix_cex,
  file.path(
    table_output_dir,
    "replication_paper_target_comparison_appendix_cex.tex"
  ),
  caption = paste(
    "Replication comparisons to Telyukova targets",
    "with appendix-timing CEX diagnostic"
  ),
  label = "tab:replication-comparison-appendix-cex"
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

cc_rate_shadow_code_definitions <- tibble::tribble(
  ~cc_rate_shadow, ~cc_rate_shadow_class, ~cc_rate_shadow_definition,
  0, "reported target", "Originally reported value",
  1, "inapplicable target", "Question is inapplicable",
  3, "inapplicable target", "Breakoff or CAPI/questionnaire error rendered target inapplicable",
  5, "coded nonmissing target", "FRB-coded verbatim/comment response",
  13, "coded nonmissing target", "Judgmental override of nonmissing reported value",
  14, "inapplicable target", "Inapplicable code generated by data adjustment",
  2050, "missing/imputed target", "Original response was don't know",
  2053, "missing/imputed target", "Original response was refused",
  2098, "missing/imputed target", "Override of reported/inapplicable/other information with missing value"
)

cc_rate_zero_context <- scf_table1_analysis %>%
  left_join(
    scf_full_selected %>%
      transmute(
        case_id = to_num(YY1),
        implicate = to_num(Y1) - 10 * to_num(YY1),
        has_any_credit_card_raw = to_num(X410),
        has_any_credit_card_shadow = to_num(J410),
        has_visa_mc_discover_raw = to_num(X7973),
        has_visa_mc_discover_shadow = to_num(J7973),
        has_store_card_raw = to_num(X7974),
        has_store_card_shadow = to_num(J7974),
        visa_mc_discover_count_raw = to_num(X411),
        visa_mc_discover_count_shadow = to_num(J411),
        visa_mc_discover_new_charges_raw = to_num(X412),
        visa_mc_discover_new_charges_shadow = to_num(J412),
        visa_mc_discover_balance_raw = to_num(X413),
        visa_mc_discover_balance_shadow = to_num(J413),
        store_card_balance_raw = to_num(X421),
        store_card_balance_shadow = to_num(J421),
        payoff_frequency_raw = to_num(X432),
        payoff_frequency_shadow = to_num(J432)
      ),
    by = c("case_id", "implicate")
  ) %>%
  mutate(
    rate_current = rate_current,
    cc_rate_zero = cc_rate_raw_code == 0,
    has_any_credit_card = has_any_credit_card_raw == 1,
    no_credit_cards = has_any_credit_card_raw == 5,
    has_visa_mc_discover = has_visa_mc_discover_raw == 1,
    no_visa_mc_discover = has_visa_mc_discover_raw == 5,
    has_store_card = has_store_card_raw == 1,
    has_primary_or_store_card_balance =
      coalesce(visa_mc_discover_balance_raw, 0) > 0 |
        coalesce(store_card_balance_raw, 0) > 0,
    cc_rate_inapplicable_target = cc_rate_shadow %in% c(1, 3, 14),
    cc_rate_missing_target = cc_rate_shadow >= 997,
    cc_rate_nonmissing_origin_target = cc_rate_shadow < 90,
    cc_rate_reported_target = cc_rate_shadow == 0,
    cc_rate_resolved_nonmissing_target = cc_rate_shadow %in% c(5, 6, 13, 15, 16, 17),
    cc_rate_inapp_due_no_credit_cards =
      cc_rate_inapplicable_target & no_credit_cards,
    cc_rate_inapp_due_no_visa_mc_discover =
      cc_rate_inapplicable_target & has_any_credit_card & no_visa_mc_discover,
    cc_rate_inapp_reason = case_when(
      cc_rate_inapp_due_no_credit_cards ~ "no credit cards",
      cc_rate_inapp_due_no_visa_mc_discover ~ "has cards, no Visa/MC/Discover/Optima",
      cc_rate_inapplicable_target ~ "other inapplicable path",
      TRUE ~ "not inapplicable"
    ),
    pays_total_balance_always = payoff_frequency_raw == 1,
    pays_total_balance_sometimes = payoff_frequency_raw == 3,
    pays_total_balance_hardly_ever = payoff_frequency_raw == 5,
    zero_rate_has_card_no_primary_store_balance =
      cc_rate_zero & has_any_credit_card & !has_primary_or_store_card_balance,
    card_ownership_status = case_when(
      has_any_credit_card ~ "has credit card",
      no_credit_cards ~ "no credit cards",
      TRUE ~ paste0("other/unknown X410=", has_any_credit_card_raw)
    ),
    card_balance_status = case_when(
      has_primary_or_store_card_balance ~ "positive primary/store balance",
      TRUE ~ "no primary/store balance"
    ),
    payoff_frequency_status = case_when(
      pays_total_balance_always ~ "always/almost always pays total balance",
      pays_total_balance_sometimes ~ "sometimes pays total balance",
      pays_total_balance_hardly_ever ~ "hardly ever pays total balance",
      TRUE ~ paste0("other/unknown X432=", payoff_frequency_raw)
    )
  ) %>%
  left_join(
    cc_rate_shadow_code_definitions,
    by = "cc_rate_shadow"
  ) %>%
  mutate(
    cc_rate_shadow_class = case_when(
      !is.na(cc_rate_shadow_class) ~ cc_rate_shadow_class,
      cc_rate_shadow >= 90 & cc_rate_shadow <= 996 ~ "range target",
      cc_rate_shadow >= 997 ~ "missing/imputed target",
      cc_rate_shadow < 90 ~ "other nonmissing/resolved target",
      TRUE ~ "unknown shadow code"
    ),
    cc_rate_shadow_definition = if_else(
      is.na(cc_rate_shadow_definition),
      "See SCF codebook shadow-variable definitions",
      cc_rate_shadow_definition
    )
  )

cc_rate_zero_context_by_group <- cc_rate_zero_context %>%
  group_by(group) %>%
  summarise(
    group_weight = sum(weight, na.rm = TRUE),
    current_rate = weighted_mean_safe(rate_current, weight),
    zero_rate_share = 100 * weighted_mean_safe(as.numeric(cc_rate_zero), weight),
    zero_rate_no_credit_cards_share =
      100 * weighted_mean_safe(as.numeric(cc_rate_zero & no_credit_cards), weight),
    zero_rate_has_credit_card_share =
      100 * weighted_mean_safe(as.numeric(cc_rate_zero & has_any_credit_card), weight),
    zero_rate_no_visa_mc_discover_share =
      100 * weighted_mean_safe(as.numeric(cc_rate_zero & no_visa_mc_discover), weight),
    zero_rate_positive_balance_share = 100 * weighted_mean_safe(
      as.numeric(cc_rate_zero & has_primary_or_store_card_balance),
      weight
    ),
    zero_rate_no_primary_store_balance_share = 100 * weighted_mean_safe(
      as.numeric(cc_rate_zero & !has_primary_or_store_card_balance),
      weight
    ),
    zero_rate_always_pays_total_balance_share = 100 * weighted_mean_safe(
      as.numeric(cc_rate_zero & pays_total_balance_always),
      weight
    ),
    .groups = "drop"
  )

cc_rate_zero_context_detail <- cc_rate_zero_context %>%
  filter(cc_rate_zero) %>%
  group_by(
    group,
    card_ownership_status,
    card_balance_status,
    payoff_frequency_status
  ) %>%
  summarise(
    n_rows = n(),
    zero_rate_weight = sum(weight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    cc_rate_zero_context_by_group %>%
      select(group, group_weight),
    by = "group"
  ) %>%
  mutate(
    share_of_group_weight = 100 * zero_rate_weight / group_weight
  ) %>%
  arrange(group, desc(share_of_group_weight))

cc_rate_shadow_code_diagnostics <- cc_rate_zero_context %>%
  group_by(
    group,
    cc_rate_shadow,
    cc_rate_shadow_class,
    cc_rate_shadow_definition
  ) %>%
  summarise(
    n_rows = n(),
    shadow_weight = sum(weight, na.rm = TRUE),
    rate = weighted_mean_safe(rate_current, weight),
    zero_rate_share_within_code =
      100 * weighted_mean_safe(as.numeric(cc_rate_zero), weight),
    .groups = "drop"
  ) %>%
  left_join(
    cc_rate_zero_context_by_group %>%
      select(group, group_weight),
    by = "group"
  ) %>%
  mutate(
    share_of_group_weight = 100 * shadow_weight / group_weight
  ) %>%
  arrange(group, cc_rate_shadow)

cc_rate_inapplicable_target_diagnostics <- cc_rate_zero_context %>%
  filter(cc_rate_inapplicable_target) %>%
  group_by(
    group,
    cc_rate_inapp_reason,
    has_any_credit_card_raw,
    has_any_credit_card_shadow,
    has_visa_mc_discover_raw,
    has_visa_mc_discover_shadow,
    has_store_card_raw,
    has_store_card_shadow,
    visa_mc_discover_balance_shadow,
    store_card_balance_shadow,
    payoff_frequency_shadow
  ) %>%
  summarise(
    n_rows = n(),
    inapplicable_weight = sum(weight, na.rm = TRUE),
    primary_balance = weighted_mean_safe(visa_mc_discover_balance_raw, weight),
    store_balance = weighted_mean_safe(store_card_balance_raw, weight),
    .groups = "drop"
  ) %>%
  left_join(
    cc_rate_zero_context_by_group %>%
      select(group, group_weight),
    by = "group"
  ) %>%
  mutate(
    share_of_group_weight = 100 * inapplicable_weight / group_weight
  ) %>%
  arrange(group, desc(share_of_group_weight))

cc_rate_missing_convention_diagnostics_long <- bind_rows(
  score_rate_variant(
    cc_rate_zero_context,
    "Current: X7132 <= 0 as 0",
    cc_rate_zero_context$rate_current
  ),
  score_rate_variant(
    cc_rate_zero_context,
    "Treat all X7132 == 0 as missing",
    if_else(cc_rate_zero_context$cc_rate_zero, NA_real_, cc_rate_zero_context$rate_current)
  ),
  score_rate_variant(
    cc_rate_zero_context,
    "Treat X7132 == 0 and no credit cards as missing",
    if_else(
      cc_rate_zero_context$cc_rate_zero & cc_rate_zero_context$no_credit_cards,
      NA_real_,
      cc_rate_zero_context$rate_current
    )
  ),
  score_rate_variant(
    cc_rate_zero_context,
    "Treat X7132 == 0 and has credit card as missing",
    if_else(
      cc_rate_zero_context$cc_rate_zero & cc_rate_zero_context$has_any_credit_card,
      NA_real_,
      cc_rate_zero_context$rate_current
    )
  ),
  score_rate_variant(
    cc_rate_zero_context,
    "SCF flags: treat original missing APR targets (J>=997) as missing",
    if_else(
      cc_rate_zero_context$cc_rate_missing_target,
      NA_real_,
      cc_rate_zero_context$rate_current
    )
  ),
  score_rate_variant(
    cc_rate_zero_context,
    "SCF flags: treat all inapplicable APR targets (J in 1/3/14) as missing",
    if_else(
      cc_rate_zero_context$cc_rate_inapplicable_target,
      NA_real_,
      cc_rate_zero_context$rate_current
    )
  ),
  score_rate_variant(
    cc_rate_zero_context,
    "SCF flags: treat no-card inapplicable APR targets as missing",
    if_else(
      cc_rate_zero_context$cc_rate_inapp_due_no_credit_cards,
      NA_real_,
      cc_rate_zero_context$rate_current
    )
  ),
  score_rate_variant(
    cc_rate_zero_context,
    "SCF flags: treat no-bank-card inapplicable APR targets as missing",
    if_else(
      cc_rate_zero_context$cc_rate_inapp_due_no_visa_mc_discover,
      NA_real_,
      cc_rate_zero_context$rate_current
    )
  ),
  score_rate_variant(
    cc_rate_zero_context,
    "SCF flags: keep only reported APR targets (J==0)",
    if_else(
      cc_rate_zero_context$cc_rate_reported_target,
      cc_rate_zero_context$rate_current,
      NA_real_
    )
  ),
  score_rate_variant(
    cc_rate_zero_context,
    "SCF flags: keep only nonmissing-origin non-inapplicable APR targets",
    if_else(
      cc_rate_zero_context$cc_rate_nonmissing_origin_target &
        !cc_rate_zero_context$cc_rate_inapplicable_target,
      cc_rate_zero_context$rate_current,
      NA_real_
    )
  ),
  score_rate_variant(
    cc_rate_zero_context,
    "Treat X7132 == 0 and no primary/store balance as missing",
    if_else(
      cc_rate_zero_context$cc_rate_zero &
        !cc_rate_zero_context$has_primary_or_store_card_balance,
      NA_real_,
      cc_rate_zero_context$rate_current
    )
  ),
  score_rate_variant(
    cc_rate_zero_context,
    "Treat X7132 == 0, has card, no primary/store balance as missing",
    if_else(
      cc_rate_zero_context$zero_rate_has_card_no_primary_store_balance,
      NA_real_,
      cc_rate_zero_context$rate_current
    )
  ),
  score_rate_variant(
    cc_rate_zero_context,
    "Cap at 20% plus narrow zero-missing rule",
    if_else(
      cc_rate_zero_context$zero_rate_has_card_no_primary_store_balance,
      NA_real_,
      pmin(cc_rate_zero_context$rate_current, 20)
    )
  ),
  score_rate_variant(
    cc_rate_zero_context,
    "Drop above 22.5% plus narrow zero-missing rule",
    if_else(
      cc_rate_zero_context$rate_current > 22.5 |
        cc_rate_zero_context$zero_rate_has_card_no_primary_store_balance,
      NA_real_,
      cc_rate_zero_context$rate_current
    )
  ),
  score_rate_variant(
    cc_rate_zero_context,
    "Drop above 23.5% plus narrow zero-missing rule",
    if_else(
      cc_rate_zero_context$rate_current > 23.5 |
        cc_rate_zero_context$zero_rate_has_card_no_primary_store_balance,
      NA_real_,
      cc_rate_zero_context$rate_current
    )
  ),
  score_rate_variant(
    cc_rate_zero_context,
    "Drop above 23.5% plus SCF no-bank-card inapplicable target missing",
    if_else(
      cc_rate_zero_context$rate_current > 23.5 |
        cc_rate_zero_context$cc_rate_inapp_due_no_visa_mc_discover,
      NA_real_,
      cc_rate_zero_context$rate_current
    )
  ),
  score_rate_variant(
    cc_rate_zero_context,
    "Drop above 23.5% plus original missing APR target missing",
    if_else(
      cc_rate_zero_context$rate_current > 23.5 |
        cc_rate_zero_context$cc_rate_missing_target,
      NA_real_,
      cc_rate_zero_context$rate_current
    )
  )
) %>%
  left_join(scf_table1_rate_targets, by = "group") %>%
  mutate(
    diff = replication - paper,
    abs_diff = abs(diff)
  )

cc_rate_missing_convention_diagnostics <- cc_rate_missing_convention_diagnostics_long %>%
  group_by(variant) %>%
  summarise(
    mean_abs_diff = mean(abs_diff, na.rm = TRUE),
    max_abs_diff = max(abs_diff, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(mean_abs_diff) %>%
  left_join(
    cc_rate_missing_convention_diagnostics_long %>%
      select(variant, group, replication) %>%
      pivot_wider(names_from = group, values_from = replication, names_prefix = "rate_"),
    by = "variant"
  )

print(cc_rate_zero_context_by_group)
print(cc_rate_shadow_code_diagnostics)
print(cc_rate_inapplicable_target_diagnostics)
print(cc_rate_missing_convention_diagnostics)

write_csv(
  cc_rate_zero_context_by_group,
  file.path(table_output_dir, "scf_cc_interest_rate_zero_context_by_group.csv")
)
write_csv(
  cc_rate_zero_context_detail,
  file.path(table_output_dir, "scf_cc_interest_rate_zero_context_detail.csv")
)
write_csv(
  cc_rate_shadow_code_diagnostics,
  file.path(table_output_dir, "scf_cc_interest_rate_shadow_code_diagnostics.csv")
)
write_csv(
  cc_rate_inapplicable_target_diagnostics,
  file.path(table_output_dir, "scf_cc_interest_rate_inapplicable_target_diagnostics.csv")
)
write_csv(
  cc_rate_missing_convention_diagnostics_long,
  file.path(table_output_dir, "scf_cc_interest_rate_missing_convention_diagnostics_long.csv")
)
write_csv(
  cc_rate_missing_convention_diagnostics,
  file.path(table_output_dir, "scf_cc_interest_rate_missing_convention_diagnostics.csv")
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
selected_cex_households <- n_distinct(cex_table1_analysis$case_id)
selected_cex_complete_12_month_households <- n_distinct(
  cex_table1_analysis$case_id[
    cex_table1_analysis$present_full_12_months
  ]
)

message("Selected SCF households: ", selected_households)
message("Telyukova appendix target: ", target_scf_households)
message("Selected CEX households: ", selected_cex_households)
message("Telyukova CEX appendix target: ", target_cex_households)
message(
  "Selected CEX complete 12-month households: ",
  selected_cex_complete_12_month_households
)
message(
  "Telyukova CEX complete 12-month target: ",
  target_cex_complete_12_month_households
)

if (selected_households != target_scf_households) {
  warning(
    "Selected household count differs from target by ",
    selected_households - target_scf_households,
    ". Inspect output/tables/scf_sample_diagnostics_stepwise.csv and ",
    "output/tables/scf_sample_selection_flags_household.csv. The most likely ",
    "remaining issue is the exact interpretation of incomplete income reporting."
  )
}

if (selected_cex_households != target_cex_households ||
    selected_cex_complete_12_month_households !=
      target_cex_complete_12_month_households) {
  warning(
    "Selected CEX household counts differ from appendix targets by ",
    selected_cex_households - target_cex_households,
    " total households and ",
    selected_cex_complete_12_month_households -
      target_cex_complete_12_month_households,
    " complete 12-month households. Inspect ",
    "output/tables/summary_vars/cex_sample_diagnostics_stepwise.csv."
  )
}
