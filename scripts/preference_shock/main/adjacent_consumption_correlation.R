# Adjacent-period liquid-consumption correlations for moment matching.
#
# This script uses the same cash-good/liquid-consumption construction as
# preference_shock_replication_compact.R, then computes household-level
# adjacent-period correlations in levels and logs. Adjacent periods are defined
# as consecutive CEX interview quarters: qyear_index_t - qyear_index_{t-1} == 1.
# With the current PQ + CQ construction, each observation is a real three-month
# consumption amount attached to a quarterly interview window.

library(dplyr)
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

output_dir <- file.path("output", "tables", "preference_shock_adjacent_correlation")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Read the long panel dataset from preference shock replication script output
long_panel <- readRDS(file.path(output_dir, "cash_good_consumption_panel_long.rds"))


adjacent_panel <- long_panel %>%
  filter(positive_liquid_consumption) %>%
  arrange(measure, case_id, qyear_index, interview) %>%
  group_by(measure, case_id) %>%
  mutate(
    lag_liquid_consumption = lag(liquid_consumption),
    lag_log_liquid_consumption = lag(log_liquid_consumption),
    lag_qyear_index = lag(qyear_index),
    lag_interview = lag(interview),
    qyear_gap = qyear_index - lag_qyear_index,
    interview_gap = interview - lag_interview,
    consecutive_quarter = qyear_gap == 1
  ) %>%
  ungroup()

adjacent_pairs <- adjacent_panel %>%
  filter(
    consecutive_quarter,
    !is.na(liquid_consumption),
    !is.na(lag_liquid_consumption),
    !is.na(log_liquid_consumption),
    !is.na(lag_log_liquid_consumption)
  )

correlation_results <- adjacent_pairs %>%
  group_by(measure) %>%
  summarise(
    n_pairs = n(),
    n_households = n_distinct(case_id),
    mean_qyear_gap = mean(qyear_gap, na.rm = TRUE),
    share_interview_gap_one = mean(interview_gap == 1, na.rm = TRUE),
    corr_consumption = cor(liquid_consumption, lag_liquid_consumption),
    corr_log_consumption = cor(log_liquid_consumption, lag_log_liquid_consumption),
    .groups = "drop"
  )

frequency_diagnostics <- adjacent_panel %>%
  filter(!is.na(lag_liquid_consumption)) %>%
  count(measure, qyear_gap, interview_gap, name = "n_pairs") %>%
  arrange(measure, qyear_gap, interview_gap)

panel_diagnostics <- long_panel %>%
  group_by(measure) %>%
  summarise(
    n_rows = n(),
    n_households = n_distinct(case_id),
    n_positive_rows = sum(positive_liquid_consumption, na.rm = TRUE),
    n_positive_households = n_distinct(case_id[positive_liquid_consumption]),
    .groups = "drop"
  )

write_csv(
  correlation_results,
  file.path(output_dir, "adjacent_consumption_correlations.csv")
)
write_csv(
  adjacent_pairs,
  file.path(output_dir, "adjacent_consumption_pairs.csv.gz")
)
write_csv(
  frequency_diagnostics,
  file.path(output_dir, "adjacent_period_frequency_diagnostics.csv")
)
write_csv(
  panel_diagnostics,
  file.path(output_dir, "adjacent_correlation_panel_diagnostics.csv")
)

cat("Adjacent-period frequency: consecutive CEX interview quarters.\n")
cat("Constructed panel written to:", output_dir, "\n\n")
print(correlation_results)

