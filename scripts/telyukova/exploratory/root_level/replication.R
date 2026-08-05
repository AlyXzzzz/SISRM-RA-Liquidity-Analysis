library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(purrr)

# Resolve paths from the repository root even when this script is run from its
# organized subfolder.
script_args <- commandArgs(trailingOnly = FALSE)
script_file <- sub("^--file=", "", script_args[grepl("^--file=", script_args)][1])
if (!is.na(script_file)) {
  script_dir <- dirname(normalizePath(script_file))
  setwd(normalizePath(file.path(script_dir, "..", "..", "..")))
}

# -----------------------------
# 0. Paths
# -----------------------------

scf_path <- "output/scf2001_raw_allvars.rds"

cex_fmli_path <- "output/cex/cex_fmli_2000_2002_raw.rds"
cex_zip_dir <- "raw/cex"
cex_output_dir <- "output/cex"
table_output_dir <- "output/tables"

dir.create(cex_output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_output_dir, recursive = TRUE, showWarnings = FALSE)

scf_full <- readRDS(scf_path)

# -----------------------------
# 1. Variable lists
# -----------------------------

cc_debt_vars <- c("X413", "X421")

 # cc_debt_vars <- c(
 #   "X413", "X421", "X7575"
 # )
# 
# cc_debt_vars <- c(
#   "X413", "X421", "X424", "X427", "X430", "X7575"
# )

checking_vars <- c(
  "X3506", "X3510", "X3514",
  "X3518", "X3522", "X3526", "X3529"
)

savings_vars <- c(
  "X3804", "X3807", "X3810",
  "X3813", "X3816", "X3818"
)

brokerage_cash_vars <- c("X3930")

needed_vars <- c(
  "Y1", "YY1", "X14", "X42001",
  "X432", "X7132",
  cc_debt_vars,
  checking_vars,
  savings_vars,
  brokerage_cash_vars
)

# Helper Functions

to_num <- function(x) {
  parse_number(
    as.character(x),
    na = c("", ".", "NA")
  )
}

to_amount <- function(x) {
  z <- to_num(x)
  pmax(z, 0)
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


# -----------------------------
# 2. Construct Debt and Liquidity Variables
# -----------------------------

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
    
    cc_rate_pct = to_rate_pct(X7132),
    cc_rate_pct_table1 = to_rate_pct_table1(X7132)
  ) %>%
  mutate(
    liquid_assets = checking + savings + brokerage_cash,
    cc_debt = if_else(habitual_revolver, cc_debt_raw, 0)
  ) %>%
  filter(age >= 25, age <= 64)

# -----------------------------
# 3. Construct Groups
# -----------------------------

scf_t1 <- scf_t1 %>%
  mutate(
    group = case_when(
      cc_debt > 500 & liquid_assets <= 500 ~ "Borrow",
      cc_debt > 500 & liquid_assets > 500  ~ "Borrow and save",
      cc_debt <= 500                       ~ "Save",
      TRUE                                 ~ NA_character_
    ),
    group = factor(group, levels = c("Borrow", "Borrow and save", "Save"))
  )

# SCF portion of Table 1

scf_t1 %>%
  group_by(group) %>%
  summarise(
    share_pct = 100 * sum(weight, na.rm = TRUE) / sum(scf_t1$weight, na.rm = TRUE),
    cc_rate_pct = weighted.mean(cc_rate_pct_table1, weight, na.rm = TRUE),
    .groups = "drop"
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

# -----------------------------
# 4. CEX
# -----------------------------

# Helper function to extract FNA data (cc debt)
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


cex_fmli_raw <- readRDS(cex_fmli_path) # FMLI data containing liquidity measures 
                                       # processed separately

needed_cex_fmli_vars <- c(
  "NEWID", "FINLWT21", "AGE_REF", "CKBKACTX", "SAVACCTX")

cex_fna_raw <- read_cex_fna_from_zips(cex_zip_dir)
saveRDS(cex_fna_raw, file.path(cex_output_dir, "cex_fna_2000_2002_raw.rds"))
write_csv(cex_fna_raw, file.path(cex_output_dir, "cex_fna_2000_2002_raw.csv.gz"))

cex_calendar_qyears <- c(
  20001, 20002, 20003, 20004,
  20011, 20012, 20013, 20014,
  20021, 20022, 20023, 20024
)

cex_credit_codes <- c(100) # Revolving credit card debt code

# Helper Functions to identify nonresponse flags

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
  filter(!is.na(liquid_assets))

cex_t1 <- cex_t1 %>%
  mutate(
    group = case_when(
      cc_debt > 500 & liquid_assets <= 500 ~ "Borrow",
      cc_debt > 500 & liquid_assets > 500  ~ "Borrow and save",
      cc_debt <= 500                       ~ "Save",
      TRUE                                 ~ NA_character_
    ),
    group = factor(group, levels = c("Borrow", "Borrow and save", "Save"))
  )


cex_t1 %>%
  group_by(group) %>%
  summarise(
    share_pct = 100 * sum(weight, na.rm = TRUE) / sum(cex_t1$weight, na.rm = TRUE),
    .groups = "drop"
  )

cex_table1 <- cex_t1 %>%
  group_by(group) %>%
  summarise(
    source = "CEX",
    share_pct = 100 * sum(weight, na.rm = TRUE) / sum(cex_t1$weight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  select(source, group, share_pct)

print(cex_table1)

table1_combined <- bind_rows(
  scf_table1 %>% mutate(source = "SCF 2001"),
  cex_table1 %>% mutate(cc_rate_pct = NA_real_) %>% select(source, group, share_pct, cc_rate_pct)
)

print(table1_combined)
write_csv(table1_combined, file.path(table_output_dir, "table1_combined_scf_cex.csv"))

# -----------------------------
# 5. SCF Table 2: demographics
# -----------------------------

# Read in the summary statistics SCF File
# scf_2001_sumstats <- read.csv("raw/SCFP2001.csv")

# Helper Function for computing weighted mean, taking into consideration missing values
wt_mean <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  sum(w[ok] * x[ok]) / sum(w[ok])
}

# Household-list variables for positions #3 through #11 
# Relationship vars: child/grandchild/etc.
rel_vars <- c("X108", "X114", "X120", "X126", "X132",
              "X202", "X208", "X214", "X220")

# Financial-dependence vars for those same positions
dep_vars <- c("X113", "X119", "X125", "X131", "X137",
              "X207", "X213", "X219", "X225")

# Age vars for same positions
age_vars <- c("X110", "X116", "X122", "X128", "X134",
              "X204", "X210", "X216", "X222")

# Living vars for same positions
# live_vars <- c("X112", "X118", "X124", "X130", "X136",
#                "X206", "X212", "X218", "X224")

# needed_table2_vars <- c(
#   "Y1", "YY1", "X14",
#   "X6809", "X8023", "X4511", "X7401",
#   "X5901", "X5902", "X5904", "X5905",
#   rel_vars, dep_vars
# )

# Demographic indicators excluding dependent children

scf_demog <- scf_full %>%
  transmute(
    case_id = to_num(YY1),
    implicate = to_num(Y1) - 10 * to_num(YY1),
    age = to_num(X14),
    
    race_white = as.integer(to_num(X6809) == 1),
    
    married = as.integer(to_num(X8023) == 1),
    
    head_full_time = as.integer(to_num(X4511) == 1),
    
    head_white_collar_prof = as.integer(to_num(X7401) %in% c(1, 2)),
    
    educ_years = to_num(X5901),
    hs_diploma_or_ged = to_num(X5902) %in% c(1, 2),
    has_college_degree = to_num(X5904) == 1,
    highest_degree = to_num(X5905)
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
    )
  ) %>% 
  filter(age >= 25, age <= 64)

# Dependent Children Demographics

child_rel_codes <- c(4)

child_matrix_under18 <- sapply(seq_along(rel_vars), function(j) {
  rel <- to_num(scf_full[[rel_vars[j]]])
  age <- to_num(scf_full[[age_vars[j]]])
  
  rel == 4 & age < 18
})

scf_dep_children <- scf_full %>%
  transmute(
    case_id = to_num(YY1),
    implicate = to_num(Y1) - 10 * to_num(YY1),

    has_dependent_children = as.integer(
      rowSums(child_matrix_under18, na.rm = TRUE) > 0
    )
  )

# scf_dep_children <- scf_2001_sumstats %>%
#   transmute(
#     case_id = to_num(YY1),
#     implicate = to_num(Y1) - 10 * to_num(YY1),
#     
#     has_dependent_children = as.integer(FAMSTRUCT %in% c(1, 4))
#   )

scf_t2 <- scf_t1 %>%
  left_join(
    scf_demog %>%
      select(
        case_id, implicate,
        race_white,
        married,
        head_full_time,
        head_white_collar_prof,
        less_than_hs,
        hs_some_college,
        college_degree_or_more
      ),
    by = c("case_id", "implicate")
  ) %>%
  left_join(
    scf_dep_children,
    by = c("case_id", "implicate")
  )

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

scf_table2_by_group <- scf_t2 %>%
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

scf_table2_population <- scf_t2 %>%
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

# ----------
# Testing block
# ----------
child_definition_compare <- scf_t1 %>%
  select(case_id, implicate, group, weight) %>%
  left_join(
    scf_dep_children %>%
      rename(child_under18 = has_dependent_children),
    by = c("case_id", "implicate")
  ) %>%
  left_join(
    scf_2001_sumstats %>%
      transmute(
        case_id = to_num(YY1),
        implicate = to_num(Y1) - 10 * to_num(YY1),
        child_famstruct4 = as.integer(FAMSTRUCT == 4),
        child_kids_any = as.integer(KIDS > 0),
        famstruct = FAMSTRUCT,
        kids = KIDS
      ),
    by = c("case_id", "implicate")
  )

child_definition_compare %>%
  group_by(group) %>%
  summarise(
    under18 = 100 * wt_mean(child_under18, weight),
    famstruct4 = 100 * wt_mean(child_famstruct4, weight),
    kids_any = 100 * wt_mean(child_kids_any, weight),
    .groups = "drop"
  )

child_definition_compare %>%
  summarise(
    under18 = 100 * wt_mean(child_under18, weight),
    famstruct4 = 100 * wt_mean(child_famstruct4, weight),
    kids_any = 100 * wt_mean(child_kids_any, weight)
  )

child_definition_compare %>%
  filter(group == "Borrow") %>%
  count(child_under18, child_famstruct4, wt = weight)

child_definition_compare %>%
  filter(group == "Borrow", child_under18 == 1, child_famstruct4 == 0) %>%
  group_by(famstruct) %>%
  summarise(
    weight_share_within_borrow = 100 * sum(weight, na.rm = TRUE) /
      sum(child_definition_compare$weight[child_definition_compare$group == "Borrow"], na.rm = TRUE),
    avg_kids = wt_mean(kids, weight),
    .groups = "drop"
  )

child_definition_compare %>%
  filter(group == "Borrow") %>%
  mutate(
    category = case_when(
      child_under18 == 1 & child_famstruct4 == 1 ~ "Both under18 and FAMSTRUCT 4",
      child_under18 == 1 & child_famstruct4 == 0 ~ "Under18 only",
      child_under18 == 0 & child_famstruct4 == 1 ~ "FAMSTRUCT 4 only",
      child_under18 == 0 & child_famstruct4 == 0 ~ "Neither"
    )
  ) %>%
  group_by(category, famstruct) %>%
  summarise(
    weight_share_within_borrow = 100 * sum(weight, na.rm = TRUE) /
      sum(child_definition_compare$weight[child_definition_compare$group == "Borrow"], na.rm = TRUE),
    avg_kids = wt_mean(kids, weight),
    .groups = "drop"
  )

# Fed summary-variable style family structure from raw SCF

famstruct_rel_vars <- c(
  "X108", "X114", "X120", "X126", "X132",
  "X202", "X208", "X214", "X220", "X226"
)

# Keep only variables actually present in your raw public file
famstruct_rel_vars <- intersect(famstruct_rel_vars, names(scf_full))

scf_famstruct_raw <- scf_full %>%
  transmute(
    case_id = to_num(YY1),
    implicate = to_num(Y1) - 10 * to_num(YY1),
    
    age = to_num(X14),
    
    # Fed summary macro: 1 = married/living with partner, 2 = neither
    married_sum = if_else(to_num(X8023) %in% c(1, 2), 1L, 2L),
    
    kids_sum = rowSums(
      sapply(famstruct_rel_vars, function(v) {
        rel <- to_num(scf_full[[v]])
        rel %in% c(4, 13, 36)
      }),
      na.rm = TRUE
    ),
    
    famstruct_raw = case_when(
      married_sum != 1 & kids_sum >= 1       ~ 1L,
      married_sum != 1 & kids_sum == 0 & age < 55  ~ 2L,
      married_sum != 1 & kids_sum == 0 & age >= 55 ~ 3L,
      married_sum == 1 & kids_sum >= 1       ~ 4L,
      married_sum == 1 & kids_sum == 0       ~ 5L,
      TRUE ~ NA_integer_
    ),
    
    # Candidate for paper's "Have dependent children" row
    has_dependent_children_famstruct4 = as.integer(famstruct_raw == 4)
  )

famstruct_check <- scf_famstruct_raw %>%
  left_join(
    scf_2001_sumstats %>%
      transmute(
        case_id = to_num(YY1),
        implicate = to_num(Y1) - 10 * to_num(YY1),
        FAMSTRUCT_sumstats = FAMSTRUCT,
        KIDS_sumstats = KIDS
      ),
    by = c("case_id", "implicate")
  )

famstruct_check %>%
  count(famstruct_raw, FAMSTRUCT_sumstats)

famstruct_check %>%
  summarise(
    famstruct_match_rate = mean(famstruct_raw == FAMSTRUCT_sumstats, na.rm = TRUE),
    kids_match_rate = mean(kids_sum == KIDS_sumstats, na.rm = TRUE)
  )
