# Compute Adjacent-month consumption correlations for the 2019 liquid-
# consumption panel, for moment matching.

library(tidyverse)

# -----------------------------------------------------------------------------
# 1. Paths
# -----------------------------------------------------------------------------

input_path <- file.path(
  "output", "tables", "preference_shock_volatility_2019_monthly",
  "cash_good_consumption_panel_2019_monthly_long.rds"
)
output_dir <- file.path(
  "output", "tables", "preference_shock_adjacent_correlation_2019_monthly"
)

# -----------------------------------------------------------------------------
# 2. Read Analysis Data Frame and Compute Correlations 
# -----------------------------------------------------------------------------

long_panel <- readRDS(input_path)

# Restrict to adjacent pairs and non-missing values
adjacent_pairs <- long_panel %>%
  filter(positive_liquid_consumption) %>%
  arrange(measure, panel_id, ref_month_index, interview) %>%
  group_by(measure, panel_id) %>%
  mutate(
    lag_liquid_consumption = lag(liquid_consumption),
    lag_log_liquid_consumption = lag(log_liquid_consumption),
    lag_ref_month_index = lag(ref_month_index)
  ) %>%
  ungroup() %>%
  filter(
    ref_month_index - lag_ref_month_index == 1,
    !is.na(liquid_consumption),
    !is.na(lag_liquid_consumption),
    !is.na(log_liquid_consumption),
    !is.na(lag_log_liquid_consumption)
  )

# Compute level and log correlation results
correlation_results <- adjacent_pairs %>%
  group_by(measure) %>%
  summarise(
    frequency = "monthly",
    calendar_year = 2019,
    n_pairs = n(),
    n_households = n_distinct(panel_id),
    corr_consumption = cor(liquid_consumption, lag_liquid_consumption),
    corr_log_consumption = cor(
      log_liquid_consumption,
      lag_log_liquid_consumption
    ),
    .groups = "drop"
  )

# -----------------------------------------------------------------------------
# 3. Output
# -----------------------------------------------------------------------------

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

write_csv(
  correlation_results,
  file.path(output_dir, "adjacent_consumption_correlations_2019_monthly.csv")
)
