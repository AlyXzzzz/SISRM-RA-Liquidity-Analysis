# Resolve paths from the repository root even when this script is run from its
# organized subfolder.
script_args <- commandArgs(trailingOnly = FALSE)
script_file <- sub("^--file=", "", script_args[grepl("^--file=", script_args)][1])
if (!is.na(script_file)) {
  script_dir <- dirname(normalizePath(script_file))
  setwd(normalizePath(file.path(script_dir, "..", "..", "..")))
}

# Focused CEX sample-rule diagnostics for Telyukova (2013), Appendix A.1.
#
# This script deliberately leaves the clean replication specification alone.
# It holds the corrected CEX debt construction fixed and varies plausible
# interpretations of cohort timing, age/income timing, debt-data validity, and
# twelve-month panel presence.

source("scripts/telyukova/main/telyukova_replication_final.R")

diagnostic_dir <- "output/tables/cex_sample_rule_diagnostics"
dir.create(diagnostic_dir, recursive = TRUE, showWarnings = FALSE)

appendix_window_qyears <- c(20002, 20003, 20004, 20011)
collection_2001_qyears <- c(20011, 20012, 20013, 20014)

# -----------------------------------------------------------------------------
# 1. Panel-level timing variables
# -----------------------------------------------------------------------------

cex_panel_rule_summary <- cex_fmli_base %>%
  mutate(
    income_class = to_num(INCLASS),
    complete_income_from_class = !is.na(income_class) & income_class %in% 1:9,
    age_in_range = !is.na(age) & age >= 25 & age <= 64,
    income_above_floor =
      !is.na(income_annual) & income_annual >= income_annual_floor,
    income_strictly_above_floor =
      !is.na(income_annual) & income_annual > income_annual_floor
  ) %>%
  arrange(case_id, qyear_index, interview) %>%
  group_by(case_id) %>%
  summarise(
    n_interviews_observed = n_distinct(interview),
    first_public_qyear_rule = first(qyear),
    age_earliest_observed = first(age, default = NA_real_),
    income_earliest_observed = first(income_annual, default = NA_real_),
    complete_income_earliest_observed =
      first(complete_income_reporter, default = FALSE),
    complete_income_class_earliest_observed =
      first(complete_income_from_class, default = FALSE),
    age_in_range_all_observed = all(age_in_range),
    income_eligible_all_observed =
      all(income_above_floor & complete_income_reporter),
    income_eligible_class_all_observed =
      all(income_above_floor & complete_income_from_class),
    present_interviews_2_5 = all(2:5 %in% interview),
    qindex_i2 = first(qyear_index[interview == 2], default = NA_real_),
    qindex_i3 = first(qyear_index[interview == 3], default = NA_real_),
    qindex_i4 = first(qyear_index[interview == 4], default = NA_real_),
    qindex_i5 = first(qyear_index[interview == 5], default = NA_real_),
    .groups = "drop"
  ) %>%
  mutate(
    present_consecutive_interviews_2_5 =
      present_interviews_2_5 &
      !is.na(qindex_i2) & !is.na(qindex_i3) &
      !is.na(qindex_i4) & !is.na(qindex_i5) &
      qindex_i3 - qindex_i2 == 1 &
      qindex_i4 - qindex_i3 == 1 &
      qindex_i5 - qindex_i4 == 1
  )

cex_fnb_presence <- cex_cc_finance_charges %>%
  transmute(newid, qyear, has_finance_charge_record = TRUE)

cex_fna_presence <- cex_cc_balance_fifth %>%
  transmute(newid, qyear, has_fifth_balance_record = TRUE)

cex_rule_base <- cex_all_base_rows %>%
  left_join(cex_panel_rule_summary, by = "case_id") %>%
  left_join(cex_fnb_presence, by = c("newid", "qyear")) %>%
  left_join(cex_fna_presence, by = c("newid", "qyear")) %>%
  mutate(
    has_finance_charge_record =
      coalesce(has_finance_charge_record, FALSE),
    has_fifth_balance_record = coalesce(has_fifth_balance_record, FALSE),
    fifth_income_class = to_num(INCLASS),
    fifth_income_class_complete =
      !is.na(fifth_income_class) & fifth_income_class %in% 1:9,
    inferred_public_i2_qyear =
      cex_index_to_qyear(qyear_index - 3),
    cohort_inferred_public_i2 =
      inferred_public_i2_qyear %in% appendix_window_qyears,
    cohort_fifth_interview_2001 = qyear %in% collection_2001_qyears,
    cohort_earliest_public =
      first_public_qyear_rule %in% appendix_window_qyears,
    fifth_interview_month_index = qintrv_year * 12 + qintrv_month
  )

# -----------------------------------------------------------------------------
# 2. Actual month coverage from MTBI
# -----------------------------------------------------------------------------

index_mtbi_files <- function(zip_path) {
  archive_year <- 2000L + as.integer(
    str_match(basename(zip_path), "intrvw([0-9]{2})[.]zip$")[, 2]
  )

  tibble(
    source_zip = zip_path,
    source_file = unzip(zip_path, list = TRUE)$Name
  ) %>%
    filter(str_detect(
      str_to_lower(source_file),
      "(^|/)mtbi[0-9]{3}x?[.]csv$"
    )) %>%
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
    ) %>%
    arrange(cex_year, cex_quarter)
}

mtbi_index <- bind_rows(lapply(
  file.path(cex_zip_dir, sprintf("intrvw%02d.zip", 0:2)),
  index_mtbi_files
))

if (nrow(mtbi_index) != 12L) {
  stop("Expected 12 calendar-quarter MTBI files; found ", nrow(mtbi_index))
}

candidate_case_ids <- unique(cex_rule_base$case_id)

read_mtbi_months <- function(source_zip, source_file) {
  message("Reading monthly coverage from ", source_file)

  read_csv(
    unz(source_zip, source_file),
    col_select = all_of(c("NEWID", "REF_YR", "REF_MO", "UCC", "COST")),
    col_types = cols(.default = col_character()),
    show_col_types = FALSE,
    progress = FALSE
  ) %>%
    transmute(
      case_id = str_sub(as.character(NEWID), 1L, -2L),
      ref_year = to_num(REF_YR),
      ref_month = to_num(REF_MO),
      ucc = as.character(UCC),
      cost = to_num(COST)
    ) %>%
    filter(
      case_id %in% candidate_case_ids,
      !is.na(ref_year),
      !is.na(ref_month),
      ref_month %in% 1:12,
      !ucc %in% c("006001", "006002")
    ) %>%
    mutate(reference_month_index = ref_year * 12 + ref_month) %>%
    group_by(case_id, reference_month_index) %>%
    summarise(
      has_mtbi_record = TRUE,
      has_nonmissing_cost = any(!is.na(cost)),
      monthly_total_cost = sum(cost, na.rm = TRUE),
      .groups = "drop"
    )
}

cex_mtbi_months <- bind_rows(lapply(seq_len(nrow(mtbi_index)), function(i) {
  read_mtbi_months(
    mtbi_index$source_zip[[i]],
    mtbi_index$source_file[[i]]
  )
})) %>%
  group_by(case_id, reference_month_index) %>%
  summarise(
    has_mtbi_record = any(has_mtbi_record),
    has_nonmissing_cost = any(has_nonmissing_cost),
    monthly_total_cost = sum(monthly_total_cost, na.rm = TRUE),
    .groups = "drop"
  )

cex_fifth_month_anchor <- cex_rule_base %>%
  distinct(case_id, fifth_interview_month_index)

cex_mtbi_coverage <- cex_mtbi_months %>%
  inner_join(cex_fifth_month_anchor, by = "case_id") %>%
  filter(
    reference_month_index >= fifth_interview_month_index - 12,
    reference_month_index <= fifth_interview_month_index - 1
  ) %>%
  group_by(case_id) %>%
  summarise(
    n_expected_months_with_mtbi_records = n_distinct(reference_month_index),
    n_expected_months_with_nonmissing_cost =
      n_distinct(reference_month_index[has_nonmissing_cost]),
    n_expected_months_with_positive_total =
      n_distinct(reference_month_index[monthly_total_cost > 0]),
    .groups = "drop"
  )

cex_rule_base <- cex_rule_base %>%
  left_join(cex_mtbi_coverage, by = "case_id") %>%
  mutate(
    across(
      starts_with("n_expected_months_"),
      ~ coalesce(.x, 0L)
    ),
    present_12_mtbi_record_months =
      n_expected_months_with_mtbi_records == 12,
    present_12_mtbi_nonmissing_months =
      n_expected_months_with_nonmissing_cost == 12,
    present_12_mtbi_positive_months =
      n_expected_months_with_positive_total == 12
  )

cex_monthly_coverage_diagnostics <- cex_rule_base %>%
  count(
    present_full_12_months,
    present_consecutive_interviews_2_5,
    n_expected_months_with_mtbi_records,
    n_expected_months_with_nonmissing_cost,
    n_expected_months_with_positive_total,
    name = "n_households"
  ) %>%
  arrange(
    desc(present_full_12_months),
    desc(n_expected_months_with_mtbi_records)
  )

write_csv(
  cex_monthly_coverage_diagnostics,
  file.path(diagnostic_dir, "cex_monthly_coverage_diagnostics.csv")
)

# -----------------------------------------------------------------------------
# 3. Sample interpretation grid
# -----------------------------------------------------------------------------

cohort_rules <- c(
  "inferred first interview",
  "inferred public interview 2",
  "fifth interview in 2001",
  "earliest observed public interview"
)

age_rules <- c(
  "age 25-64 at interview 5",
  "age 25-64 at earliest observed interview",
  "age 25-64 throughout observed panel",
  "age 26-64 at interview 5",
  "age 25-63 at interview 5",
  "age strictly between 25 and 64 at interview 5"
)

income_rules <- c(
  "income/reporting at interview 5",
  "income/reporting at earliest observed interview",
  "income/reporting throughout observed panel",
  "INCLASS complete at interview 5",
  "income strictly above $200/month at interview 5"
)

debt_validity_rules <- c(
  "valid flags; absent balance record is zero",
  "valid flags plus explicit finance-charge record"
)

partial_panel_rules <- c(
  "exclude fifth-only panels",
  "retain fifth-only panels"
)

full_panel_definitions <- c(
  "FMLI interviews 2-5",
  "consecutive FMLI interviews 2-5",
  "12 MTBI record months",
  "12 MTBI nonmissing-cost months",
  "12 MTBI positive-total months"
)

sample_rule_grid <- tidyr::expand_grid(
  cohort_rule = cohort_rules,
  age_rule = age_rules,
  income_rule = income_rules,
  debt_validity_rule = debt_validity_rules,
  partial_panel_rule = partial_panel_rules
)

evaluate_sample_rule <- function(spec) {
  data <- cex_rule_base

  cohort_keep <- switch(
    spec$cohort_rule,
    "inferred first interview" =
      data$cohort_inferred_first_interview_q2_2000_q1_2001,
    "inferred public interview 2" = data$cohort_inferred_public_i2,
    "fifth interview in 2001" = data$cohort_fifth_interview_2001,
    "earliest observed public interview" = data$cohort_earliest_public
  )

  age_keep <- switch(
    spec$age_rule,
    "age 25-64 at interview 5" =
      !is.na(data$age) & data$age >= 25 & data$age <= 64,
    "age 25-64 at earliest observed interview" =
      !is.na(data$age_earliest_observed) &
        data$age_earliest_observed >= 25 &
        data$age_earliest_observed <= 64,
    "age 25-64 throughout observed panel" =
      data$age_in_range_all_observed,
    "age 26-64 at interview 5" =
      !is.na(data$age) & data$age > 25 & data$age <= 64,
    "age 25-63 at interview 5" =
      !is.na(data$age) & data$age >= 25 & data$age < 64,
    "age strictly between 25 and 64 at interview 5" =
      !is.na(data$age) & data$age > 25 & data$age < 64
  )

  income_keep <- switch(
    spec$income_rule,
    "income/reporting at interview 5" =
      !is.na(data$income_annual) &
        data$income_annual >= income_annual_floor &
        data$complete_income_reporter,
    "income/reporting at earliest observed interview" =
      !is.na(data$income_earliest_observed) &
        data$income_earliest_observed >= income_annual_floor &
        data$complete_income_earliest_observed,
    "income/reporting throughout observed panel" =
      data$income_eligible_all_observed,
    "INCLASS complete at interview 5" =
      !is.na(data$income_annual) &
        data$income_annual >= income_annual_floor &
        data$fifth_income_class_complete,
    "income strictly above $200/month at interview 5" =
      !is.na(data$income_annual) &
        data$income_annual > income_annual_floor &
        data$complete_income_reporter
  )

  debt_keep <- switch(
    spec$debt_validity_rule,
    "valid flags; absent balance record is zero" = data$valid_cc_debt_info,
    "valid flags plus explicit finance-charge record" =
      data$valid_cc_debt_info & data$has_finance_charge_record
  )

  partial_keep <- switch(
    spec$partial_panel_rule,
    "exclude fifth-only panels" = data$interviews_observed != "5",
    "retain fifth-only panels" = rep(TRUE, nrow(data))
  )

  selected <- data %>%
    filter(
      coalesce(cohort_keep, FALSE),
      coalesce(age_keep, FALSE),
      coalesce(income_keep, FALSE),
      valid_liquid_assets,
      coalesce(debt_keep, FALSE),
      !is.na(weight_raw),
      weight_raw > 0,
      coalesce(partial_keep, FALSE)
    ) %>%
    mutate(cc_debt = cc_debt_finance_positive_average_balance) %>%
    add_telyukova_group()

  shares <- selected %>%
    filter(!is.na(group)) %>%
    group_by(group) %>%
    summarise(
      replication = 100 * sum(weight, na.rm = TRUE) /
        sum(selected$weight, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    right_join(
      tibble(
        group = factor(
          c("Borrow", "Borrow and save", "Save"),
          levels = c("Borrow", "Borrow and save", "Save")
        ),
        paper = c(7, 29, 64)
      ),
      by = "group"
    ) %>%
    mutate(
      replication = coalesce(replication, 0),
      diff = replication - paper
    )

  comparison <- bind_rows(lapply(full_panel_definitions, function(definition) {
    full_flag <- switch(
      definition,
      "FMLI interviews 2-5" = selected$present_full_12_months,
      "consecutive FMLI interviews 2-5" =
        selected$present_consecutive_interviews_2_5,
      "12 MTBI record months" = selected$present_12_mtbi_record_months,
      "12 MTBI nonmissing-cost months" =
        selected$present_12_mtbi_nonmissing_months,
      "12 MTBI positive-total months" =
        selected$present_12_mtbi_positive_months
    )

    shares %>%
      mutate(
        cohort_rule = spec$cohort_rule,
        age_rule = spec$age_rule,
        income_rule = spec$income_rule,
        debt_validity_rule = spec$debt_validity_rule,
        partial_panel_rule = spec$partial_panel_rule,
        full_panel_definition = definition,
        n_households = n_distinct(selected$case_id),
        n_complete_12_month_households =
          n_distinct(selected$case_id[coalesce(full_flag, FALSE)]),
        n_partial_panel_households =
          n_households - n_complete_12_month_households
      )
  }))

  comparison
}

cex_sample_rule_comparison <- bind_rows(lapply(
  seq_len(nrow(sample_rule_grid)),
  function(i) evaluate_sample_rule(sample_rule_grid[i, ])
)) %>%
  select(
    cohort_rule, age_rule, income_rule, debt_validity_rule,
    partial_panel_rule, full_panel_definition,
    n_households, n_complete_12_month_households,
    n_partial_panel_households,
    group, paper, replication, diff
  )

target_partial_panels <-
  target_cex_households - target_cex_complete_12_month_households

cex_sample_rule_scores <- cex_sample_rule_comparison %>%
  group_by(
    cohort_rule, age_rule, income_rule, debt_validity_rule,
    partial_panel_rule, full_panel_definition,
    n_households, n_complete_12_month_households,
    n_partial_panel_households
  ) %>%
  summarise(
    household_gap = first(n_households) - target_cex_households,
    complete_12_month_gap =
      first(n_complete_12_month_households) -
        target_cex_complete_12_month_households,
    partial_panel_gap =
      first(n_partial_panel_households) - target_partial_panels,
    mean_abs_share_diff = mean(abs(diff)),
    max_abs_share_diff = max(abs(diff)),
    borrow_share = replication[as.character(group) == "Borrow"],
    borrow_and_save_share =
      replication[as.character(group) == "Borrow and save"],
    save_share = replication[as.character(group) == "Save"],
    .groups = "drop"
  ) %>%
  mutate(
    total_count_abs_gap =
      abs(household_gap) + abs(complete_12_month_gap)
  ) %>%
  arrange(total_count_abs_gap, mean_abs_share_diff)

write_csv(
  cex_sample_rule_comparison,
  file.path(diagnostic_dir, "cex_sample_rule_variant_comparison.csv")
)
write_csv(
  cex_sample_rule_scores,
  file.path(diagnostic_dir, "cex_sample_rule_variant_scores.csv")
)

income_completeness_crosscheck <- cex_fmli_base %>%
  transmute(
    qyear,
    interview,
    respstat_complete = complete_income_reporter,
    inclass_complete = to_num(INCLASS) %in% 1:9
  ) %>%
  count(qyear, interview, respstat_complete, inclass_complete, name = "n_rows")

write_csv(
  income_completeness_crosscheck,
  file.path(diagnostic_dir, "cex_income_completeness_crosscheck.csv")
)

print(cex_sample_rule_scores %>% head(30), n = 30, width = Inf)
message("CEX sample-rule diagnostics written to ", diagnostic_dir)
