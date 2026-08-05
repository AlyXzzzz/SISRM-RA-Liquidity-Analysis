 # Final clean replication script for Telyukova (2013) Table 1 and Table 2.
#
# This file intentionally omits diagnostic grids and exploratory variants. It
# rebuilds the current preferred samples and writes only the core replication
# tables and target-comparison tables.
#
# Main inputs expected:
#   output/scf2001_raw_allvars.rds
#   raw/SCFP2001.csv
#   output/cex/cex_fmli_2000_2002_raw.rds
#   raw/cex/intrvw00.zip, raw/cex/intrvw01.zip, raw/cex/intrvw02.zip
#
# Main outputs:
#   output/tables/final_replication/table1_scf.csv
#   output/tables/final_replication/table1_cex.csv
#   output/tables/final_replication/table1_combined_scf_cex.csv
#   output/tables/final_replication/scf_table2.csv
#   output/tables/final_replication/replication_paper_target_comparison.csv
#   output/tables/final_replication/replication_paper_target_comparison.tex

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
# 0. Paths and preferred replication choices
# -----------------------------------------------------------------------------

scf_raw_path <- "output/scf2001_raw_allvars.rds"
scf_summary_path <- "raw/SCFP2001.csv"
cex_fmli_path <- "output/cex/cex_fmli_2000_2002_raw.rds"
cex_zip_dir <- "raw/cex"
table_output_dir <- "output/tables/final_replication"

dir.create(table_output_dir, recursive = TRUE, showWarnings = FALSE)

target_scf_households <- 2878L
target_cex_households <- 2743L
target_cex_complete_12_month_households <- 2164L

income_annual_floor <- 200 * 12

income_var <- "X5729"
income_shadow_var <- "J5729"
income_incomplete_refusal_codes <- c(1094)

cc_debt_group_cutoff <- 500
liquid_asset_group_cutoff <- 500
cc_debt_cutoff_is_strict <- TRUE
liquid_asset_cutoff_in_borrow_group <- FALSE

cc_rate_source_var <- "X7132"
cc_rate_shadow_var <- "J7132"
cc_rate_table1_cap_pct <- NA_real_

cex_finance_charge_apr <- 0.14
cex_credit_codes <- c(100)

# Preferred CEX rule currently used for final Table 1:
# Appendix A.1 timing as inferred first/bounding interview in 2000Q2-2001Q1,
# age 25-64 throughout the observed panel, fifth-only partial panels excluded,
# and topcoded liquid assets retained.
cex_appendix_q2_2000_q1_2001_qyears <- c(20002, 20003, 20004, 20011)
cex_diagnostic_qyears <- c(20004, 20011, 20012, 20013, 20014,
                           20021, 20022, 20023, 20024)

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
  if (strict) x > cutoff else x >= cutoff
}

at_or_below_cutoff <- function(x, cutoff, inclusive = TRUE) {
  if (inclusive) x <= cutoff else x < cutoff
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
  if (length(vars) == 0) return(rep(FALSE, nrow(data)))

  mat <- as.data.frame(lapply(vars, function(v) {
    z <- to_num(data[[v]])
    !is.na(z) & z >= cutoff
  }))
  rowSums(mat, na.rm = TRUE) > 0
}

row_any_j_code_in <- function(data, vars, codes) {
  vars <- intersect(vars, names(data))
  if (length(vars) == 0) return(rep(FALSE, nrow(data)))

  mat <- as.data.frame(lapply(vars, function(v) {
    z <- to_num(data[[v]])
    !is.na(z) & z %in% codes
  }))
  rowSums(mat, na.rm = TRUE) > 0
}

row_count_codes <- function(data, vars, codes) {
  vars <- intersect(vars, names(data))
  if (length(vars) == 0) return(rep(0L, nrow(data)))

  mat <- as.data.frame(lapply(vars, function(v) {
    z <- to_num(data[[v]])
    !is.na(z) & z %in% codes
  }))
  rowSums(mat, na.rm = TRUE)
}

weighted_mean_safe <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
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
      panel_print = if_else(row_id == 1L | panel != previous_panel, panel, ""),
      measure_print = if_else(
        row_id == 1L | measure != previous_measure | panel != previous_panel,
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

# -----------------------------------------------------------------------------
# 2. SCF sample and Table 1/Table 2 analysis frame
# -----------------------------------------------------------------------------

scf_full <- readRDS(scf_raw_path)
scf_summary_extract <- read_csv(scf_summary_path, show_col_types = FALSE)

needed_summary_vars <- c(
  "YY1", "Y1", "WGT", "RACE", "MARRIED", "FAMSTRUCT",
  "EDCL", "LF", "OCCAT1", "OCCAT2"
)
check_vars(scf_summary_extract, needed_summary_vars, "scf_summary_extract")

scf_summary_vars <- scf_summary_extract %>%
  transmute(
    case_id = to_num(YY1),
    implicate = to_num(Y1) - 10 * to_num(YY1),
    summary_race = to_num(RACE),
    summary_married = to_num(MARRIED),
    summary_famstruct = to_num(FAMSTRUCT),
    summary_edcl = to_num(EDCL),
    summary_lf = to_num(LF),
    summary_occat2 = to_num(OCCAT2),
    race_white_summary = as.integer(summary_race == 1),
    married_summary = as.integer(summary_married == 1),
    has_dependent_children_summary = as.integer(summary_famstruct == 4),
    less_than_hs_summary = as.integer(summary_edcl == 1),
    hs_some_college_summary = as.integer(summary_edcl %in% c(2, 3)),
    college_degree_or_more_summary = as.integer(summary_edcl == 4),
    head_full_time_summary_lf = as.integer(summary_lf == 1),
    head_white_collar_occat2_broad_summary =
      as.integer(summary_occat2 %in% c(1, 2))
  )

cc_debt_vars <- c("X413", "X421")

checking_vars <- c(
  "X3506", "X3510", "X3514", "X3518", "X3522", "X3526", "X3529"
)
savings_vars <- c("X3804", "X3807", "X3810", "X3813", "X3816", "X3818")
brokerage_cash_vars <- c("X3930")
liquid_asset_vars <- c(checking_vars, savings_vars, brokerage_cash_vars)
asset_cc_info_vars <- c(liquid_asset_vars, cc_debt_vars, "X432")
asset_cc_j_vars <- paste0("J", sub("^X", "", asset_cc_info_vars))

table2_demog_vars <- c(
  "X6809", "X7372", "X8023", "X4511", "X7401",
  "X5901", "X5902", "X5904", "X5905"
)

roster_rel_vars <- c(
  "X108", "X114", "X120", "X126", "X132",
  "X202", "X208", "X214", "X220"
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
  asset_cc_j_vars
)
check_vars(scf_full, needed_scf_vars, "scf_full")

child_count_famstruct <- row_count_codes(
  scf_full,
  roster_rel_vars,
  family_child_rel_codes
)

scf_base_rows <- scf_full %>%
  mutate(
    case_id = to_num(YY1),
    implicate = to_num(Y1) - 10 * to_num(YY1),
    weight = to_num(X42001) / 5,
    age = to_num(X14),
    income_annual = to_num(.data[[income_var]]),
    income_shadow = to_num(.data[[income_shadow_var]]),
    habitual_revolver = to_num(X432) %in% c(3, 5),
    cc_rate_raw_code = to_num(.data[[cc_rate_source_var]]),
    cc_rate_shadow = to_num(.data[[cc_rate_shadow_var]]),
    cc_rate_pct_table1 = to_rate_pct_table1(
      .data[[cc_rate_source_var]],
      cap_pct = cc_rate_table1_cap_pct
    ),
    income_refusal_no_bound = row_any_j_code_in(
      .,
      income_shadow_var,
      income_incomplete_refusal_codes
    ),
    asset_cc_j_no_numeric_bound = row_any_j_code_at_or_above(
      .,
      asset_cc_j_vars,
      1000
    ),
    has_income_response = !is.na(income_annual),
    has_cc_debt_response = if_any(all_of(cc_debt_vars), ~ !is.na(to_num(.x))),
    has_liquid_asset_response =
      if_any(all_of(liquid_asset_vars), ~ !is.na(to_num(.x))),
    has_payoff_frequency_response = !is.na(to_num(X432)),
    race_white_raw = as.integer(to_num(X6809) == 1),
    legal_marital_status = to_num(X7372),
    marital_current_status = to_num(X8023),
    married_raw = as.integer(marital_current_status == 1),
    married_or_partnered = as.integer(marital_current_status %in% c(1, 2)),
    married_preferred = as.integer(
      legal_marital_status == 1 |
        (marital_current_status == 2 & legal_marital_status %in% c(4, 5))
    ),
    head_full_time_raw = as.integer(to_num(X4511) == 1),
    head_white_collar_prof_raw = as.integer(to_num(X7401) %in% c(1, 2)),
    educ_years = to_num(X5901),
    hs_diploma_or_ged = to_num(X5902) %in% c(1, 2),
    has_college_degree = to_num(X5904) == 1,
    highest_degree = to_num(X5905),
    kids_famstruct = child_count_famstruct,
    famstruct_raw = case_when(
      married_or_partnered != 1 & kids_famstruct >= 1 ~ 1L,
      married_or_partnered != 1 & kids_famstruct == 0 & age < 55 ~ 2L,
      married_or_partnered != 1 & kids_famstruct == 0 & age >= 55 ~ 3L,
      married_or_partnered == 1 & kids_famstruct >= 1 ~ 4L,
      married_or_partnered == 1 & kids_famstruct == 0 ~ 5L,
      TRUE ~ NA_integer_
    ),
    has_dependent_children_famstruct4 = as.integer(famstruct_raw == 4),
    less_than_hs_raw = as.integer(
      educ_years < 12 | (educ_years == 12 & !hs_diploma_or_ged)
    ),
    college_degree_or_more_raw = as.integer(
      has_college_degree & highest_degree %in% c(2, 3, 4)
    ),
    hs_some_college_raw = as.integer(
      less_than_hs_raw == 0 & college_degree_or_more_raw == 0
    )
  ) %>%
  left_join(scf_summary_vars, by = c("case_id", "implicate")) %>%
  mutate(
    race_white = race_white_summary,
    married = married_preferred,
    has_dependent_children = has_dependent_children_summary,
    head_full_time = head_full_time_raw,
    head_white_collar_prof = head_white_collar_occat2_broad_summary,
    less_than_hs = less_than_hs_summary,
    hs_some_college = hs_some_college_summary,
    college_degree_or_more = college_degree_or_more_summary,
    cc_debt_raw = rowSums(across(all_of(cc_debt_vars), to_amount), na.rm = TRUE),
    checking = rowSums(across(all_of(checking_vars), to_amount), na.rm = TRUE),
    savings = rowSums(across(all_of(savings_vars), to_amount), na.rm = TRUE),
    brokerage_cash = rowSums(
      across(all_of(brokerage_cash_vars), to_amount),
      na.rm = TRUE
    ),
    liquid_assets = checking + savings + brokerage_cash,
    cc_debt = if_else(habitual_revolver, cc_debt_raw, 0)
  ) %>%
  add_telyukova_group()

scf_household_flags <- scf_base_rows %>%
  group_by(case_id) %>%
  summarise(
    n_implicates = n_distinct(implicate),
    pass_age_25_64 = all(age >= 25 & age <= 64, na.rm = FALSE),
    pass_income_floor_all_implicates = all(
      !is.na(income_annual) & income_annual >= income_annual_floor,
      na.rm = FALSE
    ),
    pass_complete_income_candidate =
      all(!income_refusal_no_bound, na.rm = FALSE),
    pass_valid_income = all(has_income_response, na.rm = FALSE),
    pass_valid_cc_debt = all(has_cc_debt_response, na.rm = FALSE),
    pass_valid_liquid_assets = all(has_liquid_asset_response, na.rm = FALSE),
    pass_valid_payoff_frequency =
      all(has_payoff_frequency_response, na.rm = FALSE),
    pass_valid_asset_cc_j_codes =
      all(!asset_cc_j_no_numeric_bound, na.rm = FALSE),
    pass_valid_weight = all(!is.na(weight) & weight > 0, na.rm = FALSE),
    pass_complete_implicates = n_implicates == 5L,
    .groups = "drop"
  ) %>%
  mutate(
    keep_final =
      pass_age_25_64 &
      pass_income_floor_all_implicates &
      pass_complete_income_candidate &
      pass_valid_income &
      pass_valid_cc_debt &
      pass_valid_liquid_assets &
      pass_valid_payoff_frequency &
      pass_valid_asset_cc_j_codes &
      pass_valid_weight &
      pass_complete_implicates
  )

scf_table1_analysis <- scf_base_rows %>%
  inner_join(
    scf_household_flags %>% filter(keep_final) %>% select(case_id),
    by = "case_id"
  ) %>%
  select(
    case_id, implicate, weight, age, income_annual,
    cc_debt_raw, habitual_revolver, cc_debt,
    checking, savings, brokerage_cash, liquid_assets,
    cc_rate_raw_code, cc_rate_shadow, cc_rate_pct_table1,
    group,
    race_white, married, has_dependent_children,
    head_full_time, head_white_collar_prof,
    less_than_hs, hs_some_college, college_degree_or_more
  )

# -----------------------------------------------------------------------------
# 3. SCF Table 1
# -----------------------------------------------------------------------------

scf_table1_denominator <- sum(scf_table1_analysis$weight, na.rm = TRUE)

scf_table1 <- scf_table1_analysis %>%
  filter(!is.na(group)) %>%
  group_by(group) %>%
  summarise(
    source = "SCF 2001",
    share_pct = 100 * sum(weight, na.rm = TRUE) / scf_table1_denominator,
    cc_rate_pct = weighted_mean_safe(cc_rate_pct_table1, weight),
    .groups = "drop"
  ) %>%
  select(source, group, share_pct, cc_rate_pct)

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

# -----------------------------------------------------------------------------
# 4. CEX Table 1
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
    checking_flag = cex_flag_clean(CKBK_CTX),
    savings_flag = cex_flag_clean(SAVA_CTX),
    checking = cex_amount_with_flag(CKBKACTX, CKBK_CTX),
    savings = cex_amount_with_flag(SAVACCTX, SAVA_CTX),
    valid_liquid_assets = !is.na(checking) & !is.na(savings),
    liquid_assets = if_else(
      valid_liquid_assets,
      coalesce(checking, 0) + coalesce(savings, 0),
      NA_real_
    )
  )

cex_panel_presence <- cex_fmli_base %>%
  group_by(case_id) %>%
  summarise(
    interviews_observed = paste(sort(unique(interview)), collapse = ""),
    present_full_12_months = all(2:5 %in% interview),
    age_25_64_all_observed =
      all(!is.na(age) & age >= 25 & age <= 64),
    first_public_qyear = min(qyear, na.rm = TRUE),
    qyear_i2 = first(qyear[interview == 2], default = NA_real_),
    qindex_i2 = first(qyear_index[interview == 2], default = NA_real_),
    qindex_i3 = first(qyear_index[interview == 3], default = NA_real_),
    qindex_i4 = first(qyear_index[interview == 4], default = NA_real_),
    qindex_i5 = first(qyear_index[interview == 5], default = NA_real_),
    .groups = "drop"
  )

cex_cc_balance_fifth <- cex_fna_raw %>%
  transmute(
    newid = as.character(NEWID),
    qyear = to_num(QYEAR),
    credit_code = to_num(CREDITR5),
    cc_balance_fifth = cex_amount_with_flag(CREDITX5, CRED_TX5),
    balance_fifth_invalid = cex_flag_clean(CRED_TX5) %in% c("B", "C")
  ) %>%
  filter(qyear %in% cex_diagnostic_qyears, credit_code %in% cex_credit_codes) %>%
  group_by(newid, qyear) %>%
  summarise(
    cc_balance_fifth = sum(cc_balance_fifth, na.rm = TRUE),
    balance_fifth_invalid = any(balance_fifth_invalid),
    .groups = "drop"
  )

cex_cc_balance_second <- cex_fn2_raw %>%
  transmute(
    newid = as.character(NEWID),
    qyear = to_num(QYEAR),
    credit_code = to_num(CREDITR1),
    cc_balance_second = cex_amount_with_flag(CREDITX1, CRED_TX1)
  ) %>%
  filter(qyear %in% cex_diagnostic_qyears, credit_code %in% cex_credit_codes) %>%
  group_by(newid, qyear) %>%
  summarise(
    cc_balance_second = sum(cc_balance_second, na.rm = TRUE),
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
    .groups = "drop"
  )

cex_all_base_rows <- cex_fmli_base %>%
  filter(interview == 5, qyear %in% cex_diagnostic_qyears) %>%
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
    valid_cc_debt_info = !balance_fifth_invalid & !finance_charges_invalid,
    inferred_first_interview_qyear = cex_index_to_qyear(qyear_index - 4),
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
    cc_debt_finance_positive_average_balance = if_else(
      cc_finance_charges > 0,
      cc_balance_average_2_5,
      0
    ),
    cc_debt = cc_debt_finance_positive_average_balance
  )

cex_table1_analysis <- cex_all_base_rows %>%
  filter(
    cohort_inferred_first_interview_q2_2000_q1_2001,
    age_25_64_all_observed,
    !is.na(income_annual),
    income_annual >= income_annual_floor,
    complete_income_reporter,
    valid_liquid_assets,
    valid_cc_debt_info,
    !is.na(weight_raw),
    weight_raw > 0,
    interviews_observed != "5"
  ) %>%
  add_telyukova_group()

cex_table1_denominator <- sum(cex_table1_analysis$weight, na.rm = TRUE)

cex_table1 <- cex_table1_analysis %>%
  filter(!is.na(group)) %>%
  group_by(group) %>%
  summarise(
    source = "CEX 2001 appendix timing",
    share_pct = 100 * sum(weight, na.rm = TRUE) / cex_table1_denominator,
    n_households = n_distinct(case_id),
    n_complete_12_month_households =
      n_distinct(case_id[present_full_12_months]),
    weighted_population = sum(weight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  select(
    source, group, share_pct,
    n_households, n_complete_12_month_households, weighted_population
  )

cex_table1_targets <- tibble::tribble(
  ~statistic, ~group, ~paper,
  "Puzzle size (%)", "Borrow", 7.0,
  "Puzzle size (%)", "Borrow and save", 29.0,
  "Puzzle size (%)", "Save", 64.0
)

cex_table1_target_comparison <- cex_table1_targets %>%
  left_join(
    cex_table1 %>%
      transmute(
        statistic = "Puzzle size (%)",
        group = as.character(group),
        replication = share_pct
      ),
    by = c("statistic", "group")
  ) %>%
  mutate(
    table = "Table 1",
    source = "CEX 2001 appendix timing",
    diff = replication - paper
  ) %>%
  select(table, source, statistic, group, paper, replication, diff)

table1_combined_scf_cex <- bind_rows(
  scf_table1,
  cex_table1 %>%
    mutate(cc_rate_pct = NA_real_) %>%
    select(source, group, share_pct, cc_rate_pct)
)

# -----------------------------------------------------------------------------
# 5. SCF Table 2
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
    across(all_of(table2_vars), ~ weighted_mean_safe(.x, weight)),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = all_of(table2_vars),
    names_to = "characteristic",
    values_to = "share"
  ) %>%
  pivot_wider(names_from = group, values_from = share)

scf_table2_population <- scf_table1_analysis %>%
  summarise(across(all_of(table2_vars), ~ weighted_mean_safe(.x, weight))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "characteristic",
    values_to = "Share in population"
  )

scf_table2 <- scf_table2_by_group %>%
  left_join(scf_table2_population, by = "characteristic") %>%
  mutate(characteristic = recode(characteristic, !!!table2_labels)) %>%
  select(characteristic, Borrow, `Borrow and save`, Save, `Share in population`)

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

scf_table2_target_comparison <- scf_table2 %>%
  pivot_longer(
    cols = c("Borrow", "Borrow and save", "Save", "Share in population"),
    names_to = "group",
    values_to = "replication"
  ) %>%
  mutate(
    group = recode(group, "Share in population" = "Population"),
    replication = replication * 100
  ) %>%
  right_join(paper_table2_targets, by = c("characteristic", "group")) %>%
  mutate(
    table = "Table 2",
    source = "SCF 2001",
    statistic = "Share (%)",
    diff = replication - paper
  ) %>%
  select(table, source, statistic, characteristic, group, paper, replication, diff)

# -----------------------------------------------------------------------------
# 6. Final output tables
# -----------------------------------------------------------------------------

table1_target_comparison <- bind_rows(
  scf_table1_target_comparison,
  cex_table1_target_comparison
)

replication_paper_target_comparison <- bind_rows(
  table1_target_comparison %>%
    mutate(characteristic = NA_character_) %>%
    select(table, source, statistic, characteristic, group, paper, replication, diff),
  scf_table2_target_comparison
)

sample_counts <- tibble::tribble(
  ~source, ~n_households, ~target_households, ~n_complete_12_month_households,
  ~target_complete_12_month_households,
  "SCF 2001", n_distinct(scf_table1_analysis$case_id), target_scf_households,
  NA_integer_, NA_integer_,
  "CEX 2001 appendix timing", n_distinct(cex_table1_analysis$case_id),
  target_cex_households,
  n_distinct(cex_table1_analysis$case_id[
    cex_table1_analysis$present_full_12_months
  ]),
  target_cex_complete_12_month_households
)

write_csv(scf_table1, file.path(table_output_dir, "table1_scf.csv"))
write_csv(cex_table1, file.path(table_output_dir, "table1_cex.csv"))
write_csv(
  table1_combined_scf_cex,
  file.path(table_output_dir, "table1_combined_scf_cex.csv")
)
write_csv(scf_table2, file.path(table_output_dir, "scf_table2.csv"))

write_csv(
  scf_table1_target_comparison,
  file.path(table_output_dir, "table1_scf_paper_target_comparison.csv")
)
write_csv(
  cex_table1_target_comparison,
  file.path(table_output_dir, "table1_cex_paper_target_comparison.csv")
)
write_csv(
  table1_target_comparison,
  file.path(table_output_dir, "table1_paper_target_comparison.csv")
)
write_csv(
  scf_table2_target_comparison,
  file.path(table_output_dir, "table2_scf_paper_target_comparison.csv")
)
write_csv(
  replication_paper_target_comparison,
  file.path(table_output_dir, "replication_paper_target_comparison.csv")
)
write_csv(
  replication_paper_target_comparison,
  file.path(table_output_dir, "paper_target_comparison.csv")
)
write_csv(sample_counts, file.path(table_output_dir, "sample_counts.csv"))

write_replication_comparison_latex(
  replication_paper_target_comparison,
  file.path(table_output_dir, "replication_paper_target_comparison.tex"),
  caption = "Replication comparisons to Telyukova targets",
  label = "tab:telyukova-final-replication"
)

message("Final replication outputs written to ", table_output_dir)
