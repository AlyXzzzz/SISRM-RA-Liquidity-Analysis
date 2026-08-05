# replication_augmented.R
#
# Recreates the SCF portion of Table 1 and adds a CEX 2000-2002
# Table 1-style analysis. The CEX section uses the FMLI CU-level file
# created by extract_cex_fmli_raw.R and extracts the FNA credit-balance
# files directly from the raw BLS CEX interview zip files.

library(dplyr)
library(readr)
library(stringr)
library(purrr)
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
# 0. Paths
# -----------------------------------------------------------------------------

scf_path <- "output/scf2001_raw_allvars.rds"

cex_fmli_path <- "output/cex/cex_fmli_2000_2002_raw.rds"
cex_zip_dir <- "raw/cex"
cex_output_dir <- "output/cex"
table_output_dir <- "output/tables"

dir.create(cex_output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_output_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# 1. Helper functions
# -----------------------------------------------------------------------------

# Convert the character columns created by fixed-width / CSV raw imports.
# parse_number handles entries like ".", blanks, and numeric strings.
to_num <- function(x) {
  parse_number(as.character(x), na = c("", ".", "NA", "NaN"))
}

# For asset/debt amounts, negative SCF/CEX conventions such as -1 are not
# economically negative balances in this table. Treat them as zero.
to_amount <- function(x) {
  z <- to_num(x)
  pmax(z, 0)
}

# SCF X7132 is coded as percent * 100. For the Table 1-style average,
# inapplicable / no-interest values are treated as zero.
to_rate_pct_table1 <- function(x) {
  z <- to_num(x)
  case_when(
    is.na(z) ~ NA_real_,
    z <= 0  ~ 0,
    TRUE    ~ z / 100
  )
}

# Alternative rate treatment: missing/inapplicable is NA, no interest is zero.
# This is useful diagnostically; the table uses to_rate_pct_table1.
to_rate_pct_conditional <- function(x) {
  z <- to_num(x)
  case_when(
    is.na(z) ~ NA_real_,
    z == 0  ~ NA_real_,
    z == -1 ~ 0,
    TRUE    ~ z / 100
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

add_table1_group <- function(data,
                             debt_var = "cc_debt",
                             liquid_var = "liquid_assets",
                             debt_cutoff = 500,
                             liquid_cutoff = 500,
                             debt_inclusive = FALSE,
                             liquid_inclusive = FALSE) {
  debt <- data[[debt_var]]
  liquid <- data[[liquid_var]]

  debt_high <- if (debt_inclusive) debt >= debt_cutoff else debt > debt_cutoff
  liquid_high <- if (liquid_inclusive) liquid >= liquid_cutoff else liquid > liquid_cutoff

  data %>%
    mutate(
      group = case_when(
        debt_high & !liquid_high ~ "Borrow",
        debt_high & liquid_high  ~ "Borrow and save",
        !debt_high               ~ "Save",
        TRUE                     ~ NA_character_
      ),
      group = factor(group, levels = c("Borrow", "Borrow and save", "Save"))
    )
}

summarise_weighted_shares <- function(data, weight_var = "weight") {
  data %>%
    filter(!is.na(group)) %>%
    group_by(group) %>%
    summarise(
      share_pct = 100 * sum(.data[[weight_var]], na.rm = TRUE) /
        sum(data[[weight_var]], na.rm = TRUE),
      .groups = "drop"
    )
}

summarise_weighted_shares_by_quarter <- function(data,
                                                 year_var = "cex_year",
                                                 quarter_var = "cex_quarter",
                                                 weight_var = "weight_raw") {
  quarterly <- data %>%
    filter(!is.na(group)) %>%
    group_by(.data[[year_var]], .data[[quarter_var]]) %>%
    mutate(total_weight_quarter = sum(.data[[weight_var]], na.rm = TRUE)) %>%
    group_by(.data[[year_var]], .data[[quarter_var]], group) %>%
    summarise(
      share_pct = 100 * sum(.data[[weight_var]], na.rm = TRUE) /
        first(total_weight_quarter),
      .groups = "drop"
    ) %>%
    rename(
      cex_year = all_of(year_var),
      cex_quarter = all_of(quarter_var)
    )

  equally_weighted <- quarterly %>%
    group_by(group) %>%
    summarise(
      share_pct = mean(share_pct, na.rm = TRUE),
      .groups = "drop"
    )

  list(quarterly = quarterly, equally_weighted = equally_weighted)
}

# -----------------------------------------------------------------------------
# 2. SCF Table 1 analysis
# -----------------------------------------------------------------------------

scf_full <- readRDS(scf_path)

# Baseline credit-card debt variables. Other candidate definitions are left here
# for sensitivity checks.
cc_debt_vars <- c("X413", "X421")

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

needed_scf_vars <- c(
  "Y1", "YY1", "X14", "X42001",
  "X432", "X7132",
  cc_debt_vars,
  checking_vars,
  savings_vars,
  brokerage_cash_vars
)

check_vars(scf_full, needed_scf_vars, "scf_full")

scf_t1 <- scf_full %>%
  transmute(
    case_id = to_num(YY1),
    implicate = to_num(Y1) - 10 * to_num(YY1),
    weight = to_num(X42001) / 5,

    age = to_num(X14),

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

    habitual_revolver = to_num(X432) %in% c(3, 5),

    cc_rate_pct_conditional = to_rate_pct_conditional(X7132),
    cc_rate_pct_table1 = to_rate_pct_table1(X7132)
  ) %>%
  mutate(
    liquid_assets = checking + savings + brokerage_cash,
    cc_debt = if_else(habitual_revolver, cc_debt_raw, 0)
  ) %>%
  filter(age >= 25, age <= 64) %>%
  add_table1_group(
    debt_var = "cc_debt",
    liquid_var = "liquid_assets",
    debt_inclusive = FALSE,
    liquid_inclusive = FALSE
  )

scf_table1 <- scf_t1 %>%
  filter(!is.na(group)) %>%
  group_by(group) %>%
  summarise(
    source = "SCF",
    share_pct = 100 * sum(weight, na.rm = TRUE) / sum(scf_t1$weight, na.rm = TRUE),
    cc_rate_pct = weighted.mean(cc_rate_pct_table1, weight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  select(source, group, share_pct, cc_rate_pct)

print(scf_table1)

write_csv(scf_table1, file.path(table_output_dir, "table1_scf.csv"))
saveRDS(scf_t1, file.path(table_output_dir, "scf_table1_analysis.rds"))

# -----------------------------------------------------------------------------
# 3. CEX Table 1 analysis
# -----------------------------------------------------------------------------

# CEX logic:
# - Use the FMLI CU-level data from the 12 calendar interview quarters.
# - Restrict to fifth interviews because the FNA credit-balance file is a fifth-
#   interview file.
# - Join FNA credit balances by NEWID.
# - Use FINLWT21 as the CU weight. For the pooled 12-quarter estimate, divide
#   by 12. This does not change percentages, but makes the pooled denominator
#   interpretable as an average population over 12 quarter-representative samples.
# - Also output quarter-by-quarter shares and the equally weighted 12-quarter
#   average requested in the notes.

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
  "NEWID", "FINLWT21", "AGE_REF", "CKBKACTX", "SAVACCTX"
)
check_vars(cex_fmli_raw, needed_cex_fmli_vars, "cex_fmli_raw")

# cex_year / cex_quarter were added by extract_cex_fmli_raw.R. If they are not
# present, the analysis can still run, but the quarter-by-quarter summaries will
# be unavailable.
if (!all(c("cex_year", "cex_quarter") %in% names(cex_fmli_raw))) {
  warning(
    "cex_year and/or cex_quarter not found in cex_fmli_raw. ",
    "Quarter-level CEX summaries will be skipped."
  )
}

cex_fna_raw <- read_cex_fna_from_zips(cex_zip_dir)
saveRDS(cex_fna_raw, file.path(cex_output_dir, "cex_fna_2000_2002_raw.rds"))
write_csv(cex_fna_raw, file.path(cex_output_dir, "cex_fna_2000_2002_raw.csv.gz"))

# Keep only the 12 calendar quarters: 2000Q1-2002Q4.
cex_calendar_qyears <- c(
  20001, 20002, 20003, 20004,
  20011, 20012, 20013, 20014,
  20021, 20022, 20023, 20024
)

# In the 2000-2002 CEX FNA files, CREDITR5 == 100 is the revolving-credit-card
# category. Keep this as baseline; test c(100, 200) as a robustness check if
# you want to include store installment credit as well.
cex_credit_codes <- c(100)

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
  group_by(case_id) %>%
  summarise(
    cc_debt_raw = sum(cc_balance, na.rm = TRUE),
    cc_debt_prev_year = sum(cc_balance_prev_year, na.rm = TRUE),
    .groups = "drop"
  )

cex_t1 <- cex_fmli_raw %>%
  mutate(
    case_id = as.character(NEWID),
    interview = to_num(str_sub(as.character(NEWID), -1L, -1L))
  ) %>%
  transmute(
    case_id,
    cex_year = if ("cex_year" %in% names(.)) to_num(cex_year) else NA_real_,
    cex_quarter = if ("cex_quarter" %in% names(.)) to_num(cex_quarter) else NA_real_,
    interview,
    weight_raw = to_num(FINLWT21),
    weight = to_num(FINLWT21) / 12,
    age = to_num(AGE_REF),
    checking_brokerage = to_amount(CKBKACTX),
    savings = to_amount(SAVACCTX)
  ) %>%
  filter(
    interview == 5,
    age >= 25,
    age <= 64
  ) %>%
  left_join(cex_cc_debt, by = "case_id") %>%
  mutate(
    cc_debt_raw = coalesce(cc_debt_raw, 0),
    cc_debt_prev_year = coalesce(cc_debt_prev_year, 0),
    liquid_assets = coalesce(checking_brokerage, 0) + coalesce(savings, 0),

    # Baseline: CEX has no exact analogue of SCF's X432 habitual-revolver
    # variable, so Table 1 uses current FNA credit-card debt.
    cc_debt = cc_debt_raw,

    # Robustness proxy for persistence: current debt only counts if the CU also
    # owed money on this credit source one year ago.
    cc_debt_persistent_proxy = if_else(cc_debt_prev_year > 0, cc_debt_raw, 0)
  ) %>%
  add_table1_group(
    debt_var = "cc_debt",
    liquid_var = "liquid_assets",
    debt_inclusive = FALSE,
    liquid_inclusive = FALSE
  )

cex_table1_pooled <- summarise_weighted_shares(cex_t1, weight_var = "weight") %>%
  mutate(source = "CEX pooled 2000-2002") %>%
  select(source, group, share_pct)

print(cex_table1_pooled)

write_csv(cex_table1_pooled, file.path(table_output_dir, "table1_cex_pooled.csv"))
saveRDS(cex_t1, file.path(table_output_dir, "cex_table1_analysis.rds"))
write_csv(cex_t1, file.path(table_output_dir, "cex_table1_analysis.csv.gz"))

# Quarter-by-quarter shares and equally weighted average across 12 quarters.
# This implements the "12 separate cross sections, equally weighted" version.
if (all(!is.na(cex_t1$cex_year)) && all(!is.na(cex_t1$cex_quarter))) {
  cex_quarter_summaries <- summarise_weighted_shares_by_quarter(
    cex_t1,
    year_var = "cex_year",
    quarter_var = "cex_quarter",
    weight_var = "weight_raw"
  )

  cex_table1_quarterly <- cex_quarter_summaries$quarterly
  cex_table1_equal_quarter <- cex_quarter_summaries$equally_weighted %>%
    mutate(source = "CEX equal-weighted quarterly average") %>%
    select(source, group, share_pct)

  print(cex_table1_equal_quarter)

  write_csv(cex_table1_quarterly, file.path(table_output_dir, "table1_cex_quarterly.csv"))
  write_csv(cex_table1_equal_quarter, file.path(table_output_dir, "table1_cex_equal_quarter_average.csv"))
}

# Persistence proxy robustness check.
cex_t1_persistent <- cex_t1 %>%
  select(-group) %>%
  add_table1_group(
    debt_var = "cc_debt_persistent_proxy",
    liquid_var = "liquid_assets",
    debt_inclusive = FALSE,
    liquid_inclusive = FALSE
  )

cex_table1_persistent_proxy <- summarise_weighted_shares(
  cex_t1_persistent,
  weight_var = "weight"
) %>%
  mutate(source = "CEX persistent-debt proxy") %>%
  select(source, group, share_pct)

print(cex_table1_persistent_proxy)
write_csv(cex_table1_persistent_proxy, file.path(table_output_dir, "table1_cex_persistent_proxy.csv"))

# Combined Table 1 output. The CEX table has no credit-card rate column.
table1_combined <- bind_rows(
  scf_table1 %>% mutate(source = "SCF 2001"),
  cex_table1_pooled %>% mutate(cc_rate_pct = NA_real_) %>% select(source, group, share_pct, cc_rate_pct)
)

write_csv(table1_combined, file.path(table_output_dir, "table1_combined_scf_cex.csv"))
print(table1_combined)
