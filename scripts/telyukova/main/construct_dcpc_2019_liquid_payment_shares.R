library(tidyverse)


# Purpose -----------------------------------------------------------------
#
# Use the 2019 Diary of Consumer Payment Choice (DCPC) to estimate the share
# of payments made with liquid payment instruments for expenditure categories
# related to Telyukova's liquid-consumption construction.
#
# This first version is intentionally explicit. It keeps the data join,
# exclusions, category crosswalk, mixed-payment parsing, and weighted formulas
# visible so that each step can be checked independently.


# Resolve repository paths -------------------------------------------------

script_args <- commandArgs(trailingOnly = FALSE)
script_file_arg <- script_args[grepl("^--file=", script_args)]

if (length(script_file_arg) > 0L) {
  script_file <- sub("^--file=", "", script_file_arg[1])
  # Rscript represents spaces as "~+~" in commandArgs() on some systems.
  script_file <- str_replace_all(script_file, fixed("~+~"), " ")
  script_dir <- dirname(normalizePath(script_file))
  setwd(normalizePath(file.path(script_dir, "..", "..", "..")))
}

transaction_path <- file.path(
  "raw", "dcpc", "2019", "dcpc_2019_tranlevel_public.csv"
)
day_path <- file.path(
  "raw", "dcpc", "2019", "dcpc_2019_daylevel_public.csv"
)
output_dir <- file.path(
  "output", "tables", "dcpc_2019_liquid_payment_shares"
)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)


# Small helper functions ---------------------------------------------------

assert_required_columns <- function(data, required_columns, data_name) {
  missing_columns <- setdiff(required_columns, names(data))

  if (length(missing_columns) > 0L) {
    stop(
      data_name,
      " is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }
}

safe_ratio <- function(numerator, denominator) {
  ifelse(denominator > 0, numerator / denominator, NA_real_)
}


# Read the two files used in the primary analysis -------------------------

transactions_raw <- read_csv(
  transaction_path,
  na = c("", "NA"),
  col_types = cols(
    id = col_character(),
    date = col_date(),
    .default = col_guess()
  ),
  show_col_types = FALSE
)

days_raw <- read_csv(
  day_path,
  na = c("", "NA"),
  col_types = cols(
    id = col_character(),
    date = col_date(),
    .default = col_guess()
  ),
  show_col_types = FALSE
)

required_transaction_columns <- c(
  "id", "date", "diary_day", "tran", "amnt", "payment", "pi",
  "multipi_breakdown", "merch", "pay010", "pay011", "pay016",
  "pay020", "pay030", "pay040", "pay041", "pay042", "pay050"
)

required_day_columns <- c(
  "id", "date", "diary_day", "dow_weight"
)

assert_required_columns(
  transactions_raw,
  required_transaction_columns,
  "Transaction-level file"
)
assert_required_columns(days_raw, required_day_columns, "Day-level file")


# Prepare and validate the many-to-one day-weight join --------------------

day_weights <- days_raw |>
  transmute(
    id,
    diary_day,
    diary_date = date,
    dow_weight
  )

duplicate_day_keys <- day_weights |>
  count(id, diary_day, name = "rows_per_key") |>
  filter(rows_per_key > 1L)

if (nrow(duplicate_day_keys) > 0L) {
  stop(
    "The day-level file is not unique by id + diary_day; the weight join ",
    "would duplicate transaction rows.",
    call. = FALSE
  )
}

n_transactions_before_join <- nrow(transactions_raw)

transactions_joined <- transactions_raw |>
  mutate(transaction_row_id = row_number()) |>
  rename(transaction_date = date) |>
  left_join(
    day_weights,
    by = c("id", "diary_day"),
    relationship = "many-to-one"
  )

if (nrow(transactions_joined) != n_transactions_before_join) {
  stop(
    "The day-weight join changed the number of transaction rows.",
    call. = FALSE
  )
}


# Payment-instrument definitions ------------------------------------------
#
# Narrow liquid: cash, check, and debit card.
# Broad liquid: narrow liquid plus checking-account-backed electronic
# methods (BANP, OBBP, and account-to-account transfers).
#
# Prepaid/EBT cards, money orders, residual mobile-app balances, deductions
# from income, and other methods remain outside both primary liquid sets.
# They remain in the denominator and are flagged as theoretically ambiguous.

narrow_liquid_pi <- c(1L, 2L, 4L)
broad_liquid_pi <- c(1L, 2L, 4L, 6L, 7L, 11L)
valid_pi_codes <- c(0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 10L, 11L, 13L, 14L)
ambiguous_pi_codes <- c(0L, 5L, 8L, 10L, 13L, 14L)

payment_instrument_crosswalk <- tribble(
  ~pi, ~payment_instrument, ~liquid_narrow, ~liquid_broad, ~ambiguous_method, ~classification_note,
  0L, "Multiple payment methods", NA, NA, TRUE, "Parse multipi_breakdown; exclude from count shares",
  1L, "Cash", TRUE, TRUE, FALSE, "Liquid in both definitions",
  2L, "Check", TRUE, TRUE, FALSE, "Liquid in both definitions",
  3L, "Credit card", FALSE, FALSE, FALSE, "Nonliquid credit instrument",
  4L, "Debit card", TRUE, TRUE, FALSE, "Liquid in both definitions",
  5L, "Prepaid/gift/EBT card", FALSE, FALSE, TRUE, "Stored value; retained outside primary liquid sets",
  6L, "Bank account number payment", FALSE, TRUE, FALSE, "Added in broad definition",
  7L, "Online banking bill payment", FALSE, TRUE, FALSE, "Added in broad definition",
  8L, "Money order", FALSE, FALSE, TRUE, "Cash-like but funding source is not observed",
  10L, "Mobile payment app", FALSE, FALSE, TRUE, "Residual app balance after Fed funding-source recodes",
  11L, "Account-to-account transfer", FALSE, TRUE, FALSE, "Added in broad definition for consumption payments",
  13L, "Other payment method", FALSE, FALSE, TRUE, "Unclassified method",
  14L, "Deduction from income", FALSE, FALSE, TRUE, "Not treated as a liquid payment instrument"
)


# Telyukova category crosswalk ---------------------------------------------
#
# The DCPC records merchants, not CEX expenditure items. The rules below use
# merchant codes plus payee-specific follow-up questions whenever possible.
# A transaction is assigned to at most one detailed category by case_when().

category_crosswalk <- tribble(
  ~tely_category, ~tely_group, ~crosswalk_quality, ~definition, ~limitation,
  "food_grocery_proxy", "food_alcohol_tobacco_proxy", "merchant_proxy",
  "Merchant 1: grocery, convenience, or pharmacy",
  "Food, alcohol, tobacco, and pharmacy purchases cannot be separated",
  "food_restaurant_bar_proxy", "food_alcohol_tobacco_proxy", "merchant_proxy",
  "Merchant 3: sit-down restaurant or bar",
  "Food and alcohol cannot be separated",
  "food_fast_service_proxy", "food_alcohol_tobacco_proxy", "merchant_proxy",
  "Merchant 4: fast food, coffee shop, cafeteria, or food truck",
  "Merchant type is observed rather than the purchased good",
  "rent", "rent", "direct_merchant_match",
  "Merchant 14: rent for a home, apartment, or other building",
  "May include nonresidential building rent",
  "mortgage_payment", "mortgage", "followup_identified",
  "Merchant 15, financial purpose loan, loan type mortgage",
  "Amount is the full mortgage payment, not mortgage interest alone",
  "utilities", "utilities", "merchant_or_followup_match",
  "Merchant 8 or a government payment identified as a utility",
  "Government utility follow-up has a small sample",
  "communications_entertainment_proxy", "utilities", "merchant_proxy",
  "Merchant 10: phone, internet, cable, streaming, or movie theater",
  "Communications cannot be separated from streaming and movie theaters",
  "household_repairs", "owned_dwelling_repairs_insurance_other", "direct_merchant_match",
  "Merchant 11: contractor, plumber, electrician, or HVAC provider",
  "Does not capture all household maintenance or operations",
  "homeowners_renters_insurance", "owned_dwelling_repairs_insurance_other", "followup_identified",
  "Financial-services insurance payment identified as homeowners or renters insurance",
  "Insurance payment timing may differ from the consumption period",
  "property_taxes", "property_taxes", "followup_identified",
  "Government tax payment identified as property tax",
  "Small number of diary transactions is expected",
  "public_transport_tolls", "public_transportation", "combined_merchant_proxy",
  "Merchant 21: public transportation and tolls",
  "Public transportation cannot be separated from tolls",
  "taxi_air_delivery_proxy", "public_transportation", "merchant_proxy",
  "Merchant 9: taxi, airplane, or delivery",
  "Delivery cannot be separated from passenger transportation",
  "health_insurance", "health_insurance", "followup_identified",
  "Insurance follow-up identified as health insurance",
  "May include payments through financial, medical, or government payees",
  "childcare", "childcare", "followup_identified",
  "Education or government follow-up identified as childcare/daycare",
  "Informal childcare paid to a person generally cannot be identified",
  "cash_contributions", "cash_contributions", "followup_identified",
  "Charity payment identified as a donation, offering, or tithe",
  "Gifts to individuals are not included"
)

transactions_classified <- transactions_joined |>
  mutate(
    category_rule = case_when(
      merch == 15 & pay010 == 2 & pay011 == 1 ~
        "merch15_pay010_loan_pay011_mortgage",
      merch == 15 & pay010 == 3 & pay016 == 3 ~
        "merch15_pay010_insurance_pay016_health",
      merch == 15 & pay010 == 3 & pay016 %in% c(1, 2) ~
        "merch15_pay010_insurance_pay016_home_or_renters",
      merch == 18 & pay030 == 4 ~
        "merch18_pay030_insurance_company",
      merch == 20 & pay020 == 3 ~
        "merch20_pay020_childcare",
      merch == 19 & pay040 == 2 & pay042 == 4 ~
        "merch19_pay040_tax_pay042_property_tax",
      merch == 19 & pay040 == 1 & pay041 == 1 ~
        "merch19_pay040_goods_pay041_utility",
      merch == 19 & pay040 == 1 & pay041 %in% c(3, 9) ~
        "merch19_pay040_goods_pay041_childcare",
      merch == 19 & pay040 == 1 & pay041 == 8 ~
        "merch19_pay040_goods_pay041_health_insurance",
      merch == 19 & pay040 == 1 & pay041 %in% c(5, 7) ~
        "merch19_pay040_goods_pay041_transit_or_toll",
      merch == 17 & pay050 %in% c(1, 2) ~
        "merch17_pay050_donation_or_offering",
      merch == 1 ~ "merch1_grocery_convenience_pharmacy",
      merch == 3 ~ "merch3_restaurant_bar",
      merch == 4 ~ "merch4_fast_food_coffee",
      merch == 8 ~ "merch8_nongovernment_utility",
      merch == 9 ~ "merch9_taxi_air_delivery",
      merch == 10 ~ "merch10_communications_streaming_movies",
      merch == 11 ~ "merch11_household_contractor",
      merch == 14 ~ "merch14_rent",
      merch == 21 ~ "merch21_public_transport_tolls",
      TRUE ~ NA_character_
    ),
    tely_category = case_when(
      category_rule == "merch15_pay010_loan_pay011_mortgage" ~
        "mortgage_payment",
      category_rule %in% c(
        "merch15_pay010_insurance_pay016_health",
        "merch18_pay030_insurance_company",
        "merch19_pay040_goods_pay041_health_insurance"
      ) ~ "health_insurance",
      category_rule == "merch15_pay010_insurance_pay016_home_or_renters" ~
        "homeowners_renters_insurance",
      category_rule %in% c(
        "merch20_pay020_childcare",
        "merch19_pay040_goods_pay041_childcare"
      ) ~ "childcare",
      category_rule == "merch19_pay040_tax_pay042_property_tax" ~
        "property_taxes",
      category_rule %in% c(
        "merch19_pay040_goods_pay041_utility",
        "merch8_nongovernment_utility"
      ) ~ "utilities",
      category_rule == "merch19_pay040_goods_pay041_transit_or_toll" ~
        "public_transport_tolls",
      category_rule == "merch17_pay050_donation_or_offering" ~
        "cash_contributions",
      category_rule == "merch1_grocery_convenience_pharmacy" ~
        "food_grocery_proxy",
      category_rule == "merch3_restaurant_bar" ~
        "food_restaurant_bar_proxy",
      category_rule == "merch4_fast_food_coffee" ~
        "food_fast_service_proxy",
      category_rule == "merch9_taxi_air_delivery" ~
        "taxi_air_delivery_proxy",
      category_rule == "merch10_communications_streaming_movies" ~
        "communications_entertainment_proxy",
      category_rule == "merch11_household_contractor" ~
        "household_repairs",
      category_rule == "merch14_rent" ~ "rent",
      category_rule == "merch21_public_transport_tolls" ~
        "public_transport_tolls",
      TRUE ~ NA_character_
    )
  ) |>
  left_join(
    category_crosswalk,
    by = "tely_category",
    relationship = "many-to-one"
  )


# Construct the analysis sample -------------------------------------------
#
# first_exclusion_reason records the first failed condition. This prevents
# silent deletion and distinguishes nonpayments/non-Telyukova transactions
# from genuinely incomplete records.

october_start <- as.Date("2019-10-01")
october_end <- as.Date("2019-10-31")

transactions_flagged <- transactions_classified |>
  mutate(
    first_exclusion_reason = case_when(
      is.na(payment) | payment != 1 ~ "not_a_payment",
      is.na(diary_day) | !diary_day %in% 1:3 ~ "not_diary_day_1_to_3",
      is.na(diary_date) |
        diary_date < october_start |
        diary_date > october_end ~ "not_matched_to_october_2019_diary_day",
      is.na(dow_weight) | dow_weight <= 0 ~ "missing_or_invalid_dow_weight",
      is.na(amnt) | amnt <= 0 ~ "missing_or_nonpositive_amount",
      is.na(pi) | !pi %in% valid_pi_codes ~ "missing_or_invalid_payment_instrument",
      is.na(merch) ~ "missing_merchant_category",
      is.na(tely_category) ~ "not_in_selected_telyukova_crosswalk",
      TRUE ~ NA_character_
    )
  )

exclusion_summary <- transactions_flagged |>
  mutate(
    sample_status = coalesce(first_exclusion_reason, "retained_for_analysis")
  ) |>
  count(sample_status, name = "n_transactions") |>
  arrange(desc(n_transactions))

analysis_base <- transactions_flagged |>
  filter(is.na(first_exclusion_reason))


# Parse transactions that used multiple payment instruments ---------------
#
# The character field has entries such as:
#   "Cash: 20, Debit Card: 7.14"
#
# For dollar shares, these amounts are allocated to their instruments. For
# transaction-count shares, multiple-instrument transactions are excluded
# because one transaction does not have a unique payment instrument.

multiple_label_crosswalk <- tribble(
  ~component_label_key, ~component_pi,
  "cash", 1L,
  "check", 2L,
  "credit card", 3L,
  "debit card", 4L,
  "prepaid/gift/ebt card", 5L,
  "bank account number payment", 6L,
  "online banking bill payment", 7L,
  "money order", 8L,
  "paypal", 10L,
  "mobile payment app", 10L,
  "account-to-account transfer", 11L,
  "other payment method", 13L,
  "direct deduction from income", 14L,
  "deduction from income", 14L
)

single_method_components <- analysis_base |>
  filter(pi != 0) |>
  transmute(
    transaction_row_id,
    component_number = 1L,
    component_label = NA_character_,
    component_pi = as.integer(pi),
    component_amount = amnt,
    component_source = "single_method_pi"
  )

multiple_method_components <- analysis_base |>
  filter(pi == 0, !is.na(multipi_breakdown)) |>
  select(transaction_row_id, multipi_breakdown) |>
  mutate(
    payment_piece = str_split(
      multipi_breakdown,
      pattern = ",\\s*(?=[A-Za-z])"
    )
  ) |>
  unnest_longer(payment_piece, indices_to = "component_number") |>
  mutate(
    component_label = str_squish(
      str_remove(
        payment_piece,
        pattern = paste0(
          ":\\s*[-$]?",
          "(?:[0-9][0-9,]*(?:\\.[0-9]+)?|\\.[0-9]+)",
          "\\s*$"
        )
      )
    ),
    component_label_key = str_to_lower(component_label),
    component_amount = suppressWarnings(
      parse_number(
        str_extract(
          payment_piece,
          paste0(
            "[-$]?",
            "(?:[0-9][0-9,]*(?:\\.[0-9]+)?|\\.[0-9]+)",
            "\\s*$"
          )
        )
      )
    )
  ) |>
  left_join(
    multiple_label_crosswalk,
    by = "component_label_key",
    relationship = "many-to-one"
  ) |>
  transmute(
    transaction_row_id,
    component_number,
    component_label,
    component_pi,
    component_amount,
    component_source = "parsed_multipi_breakdown"
  )

payment_components <- bind_rows(
  single_method_components,
  multiple_method_components
) |>
  mutate(
    component_liquid_narrow = component_pi %in% narrow_liquid_pi,
    component_liquid_broad = component_pi %in% broad_liquid_pi
  ) |>
  left_join(
    payment_instrument_crosswalk |>
      select(pi, payment_instrument),
    by = c("component_pi" = "pi"),
    relationship = "many-to-one"
  )

component_summary <- payment_components |>
  group_by(transaction_row_id) |>
  summarise(
    n_payment_components = n(),
    parsed_component_total = sum(component_amount, na.rm = TRUE),
    n_unparsed_components = sum(
      is.na(component_amount) | is.na(component_pi)
    ),
    unparsed_component_amount = sum(
      if_else(is.na(component_pi), component_amount, 0),
      na.rm = TRUE
    ),
    liquid_narrow_amount = sum(
      if_else(component_liquid_narrow, component_amount, 0),
      na.rm = TRUE
    ),
    liquid_broad_amount = sum(
      if_else(component_liquid_broad, component_amount, 0),
      na.rm = TRUE
    ),
    .groups = "drop"
  )

amount_tolerance <- 0.02

analysis_transactions <- analysis_base |>
  left_join(
    component_summary,
    by = "transaction_row_id",
    relationship = "one-to-one"
  ) |>
  left_join(
    payment_instrument_crosswalk |>
      select(pi, payment_instrument, ambiguous_method, classification_note),
    by = "pi",
    relationship = "many-to-one"
  ) |>
  mutate(
    component_total_gap = parsed_component_total - amnt,
    component_total_matches =
      !is.na(component_total_gap) &
      abs(component_total_gap) <= amount_tolerance,
    count_eligible = pi != 0,
    dollar_eligible =
      component_total_matches &
      !is.na(n_unparsed_components) &
      n_unparsed_components == 0,
    liquid_narrow_count = case_when(
      !count_eligible ~ NA_real_,
      pi %in% narrow_liquid_pi ~ 1,
      TRUE ~ 0
    ),
    liquid_broad_count = case_when(
      !count_eligible ~ NA_real_,
      pi %in% broad_liquid_pi ~ 1,
      TRUE ~ 0
    ),
    not_liquid_narrow_count = if_else(
      count_eligible,
      1 - liquid_narrow_count,
      NA_real_
    ),
    not_liquid_broad_count = if_else(
      count_eligible,
      1 - liquid_broad_count,
      NA_real_
    ),
    mixed_payment_method = pi == 0,
    ambiguous_payment_method = pi %in% ambiguous_pi_codes
  )

if (anyDuplicated(analysis_transactions$transaction_row_id)) {
  stop("Analysis transaction identifiers are not unique.", call. = FALSE)
}

if (any(
  analysis_transactions$liquid_narrow_amount >
    analysis_transactions$amnt + amount_tolerance,
  na.rm = TRUE
) || any(
  analysis_transactions$liquid_broad_amount >
    analysis_transactions$amnt + amount_tolerance,
  na.rm = TRUE
)) {
  stop(
    "A parsed liquid amount exceeds its transaction amount.",
    call. = FALSE
  )
}

failed_multiple_parses <- analysis_transactions |>
  filter(pi == 0, !dollar_eligible)

if (nrow(failed_multiple_parses) > 0L) {
  warning(
    nrow(failed_multiple_parses),
    " multiple-method transaction(s) did not parse completely and will be ",
    "excluded from dollar-share denominators.",
    call. = FALSE
  )
}


# Weighted count and dollar shares ----------------------------------------

summarise_liquid_shares <- function(data, grouping_variables) {
  data |>
    group_by(across(all_of(grouping_variables))) |>
    summarise(
      n_transactions = n(),
      n_respondents = n_distinct(id),
      n_mixed_payment_transactions = sum(mixed_payment_method),
      n_ambiguous_method_transactions = sum(ambiguous_payment_method),
      n_count_eligible = sum(count_eligible),
      n_dollar_eligible = sum(dollar_eligible),

      unweighted_count_denominator = sum(count_eligible),
      unweighted_count_narrow_numerator = sum(
        liquid_narrow_count[count_eligible],
        na.rm = TRUE
      ),
      unweighted_count_broad_numerator = sum(
        liquid_broad_count[count_eligible],
        na.rm = TRUE
      ),

      weighted_count_denominator = sum(
        dow_weight[count_eligible],
        na.rm = TRUE
      ),
      weighted_count_narrow_numerator = sum(
        dow_weight[count_eligible] * liquid_narrow_count[count_eligible],
        na.rm = TRUE
      ),
      weighted_count_broad_numerator = sum(
        dow_weight[count_eligible] * liquid_broad_count[count_eligible],
        na.rm = TRUE
      ),

      unweighted_dollar_denominator = sum(
        amnt[dollar_eligible],
        na.rm = TRUE
      ),
      unweighted_dollar_narrow_numerator = sum(
        liquid_narrow_amount[dollar_eligible],
        na.rm = TRUE
      ),
      unweighted_dollar_broad_numerator = sum(
        liquid_broad_amount[dollar_eligible],
        na.rm = TRUE
      ),

      weighted_dollar_denominator = sum(
        dow_weight[dollar_eligible] * amnt[dollar_eligible],
        na.rm = TRUE
      ),
      weighted_dollar_narrow_numerator = sum(
        dow_weight[dollar_eligible] *
          liquid_narrow_amount[dollar_eligible],
        na.rm = TRUE
      ),
      weighted_dollar_broad_numerator = sum(
        dow_weight[dollar_eligible] *
          liquid_broad_amount[dollar_eligible],
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    mutate(
      unweighted_count_share_narrow = safe_ratio(
        unweighted_count_narrow_numerator,
        unweighted_count_denominator
      ),
      unweighted_count_share_broad = safe_ratio(
        unweighted_count_broad_numerator,
        unweighted_count_denominator
      ),
      weighted_count_share_narrow = safe_ratio(
        weighted_count_narrow_numerator,
        weighted_count_denominator
      ),
      weighted_count_share_broad = safe_ratio(
        weighted_count_broad_numerator,
        weighted_count_denominator
      ),
      unweighted_dollar_share_narrow = safe_ratio(
        unweighted_dollar_narrow_numerator,
        unweighted_dollar_denominator
      ),
      unweighted_dollar_share_broad = safe_ratio(
        unweighted_dollar_broad_numerator,
        unweighted_dollar_denominator
      ),
      weighted_dollar_share_narrow = safe_ratio(
        weighted_dollar_narrow_numerator,
        weighted_dollar_denominator
      ),
      weighted_dollar_share_broad = safe_ratio(
        weighted_dollar_broad_numerator,
        weighted_dollar_denominator
      )
    )
}

shares_by_category <- summarise_liquid_shares(
  analysis_transactions,
  c("tely_group", "tely_category", "crosswalk_quality")
) |>
  arrange(tely_group, tely_category)

shares_by_group <- summarise_liquid_shares(
  analysis_transactions,
  "tely_group"
) |>
  arrange(tely_group)


# Validation tables --------------------------------------------------------

join_validation <- tibble(
  metric = c(
    "transaction_rows_before_join",
    "transaction_rows_after_join",
    "transaction_rows_without_matching_diary_day",
    "payment_rows_without_matching_diary_day",
    "retained_analysis_transactions",
    "retained_analysis_respondents"
  ),
  value = c(
    n_transactions_before_join,
    nrow(transactions_joined),
    sum(is.na(transactions_joined$diary_date)),
    sum(
      transactions_joined$payment == 1 &
        is.na(transactions_joined$diary_date),
      na.rm = TRUE
    ),
    nrow(analysis_transactions),
    n_distinct(analysis_transactions$id)
  )
)

multipi_parse_check <- analysis_transactions |>
  filter(pi == 0) |>
  select(
    transaction_row_id,
    id,
    diary_day,
    transaction_date,
    diary_date,
    amnt,
    multipi_breakdown,
    parsed_component_total,
    component_total_gap,
    component_total_matches,
    n_unparsed_components,
    liquid_narrow_amount,
    liquid_broad_amount
  )


# Write analysis products --------------------------------------------------

analysis_transactions_output <- analysis_transactions |>
  select(
    transaction_row_id,
    id,
    diary_day,
    tran,
    transaction_date,
    diary_date,
    dow_weight,
    amnt,
    payment,
    pi,
    payment_instrument,
    multipi_breakdown,
    mixed_payment_method,
    ambiguous_payment_method,
    merch,
    tely_group,
    tely_category,
    category_rule,
    crosswalk_quality,
    pay010,
    pay011,
    pay016,
    pay020,
    pay030,
    pay040,
    pay041,
    pay042,
    pay050,
    count_eligible,
    dollar_eligible,
    liquid_narrow_count,
    liquid_broad_count,
    not_liquid_narrow_count,
    not_liquid_broad_count,
    liquid_narrow_amount,
    liquid_broad_amount,
    parsed_component_total,
    component_total_gap,
    component_total_matches
  )

write_csv(
  analysis_transactions_output,
  file.path(output_dir, "dcpc_2019_telyukova_analysis_transactions.csv"),
  na = ""
)
write_csv(
  payment_components,
  file.path(output_dir, "dcpc_2019_payment_components.csv"),
  na = ""
)
write_csv(
  shares_by_category,
  file.path(output_dir, "liquid_payment_shares_by_category.csv"),
  na = ""
)
write_csv(
  shares_by_group,
  file.path(output_dir, "liquid_payment_shares_by_telyukova_group.csv"),
  na = ""
)
write_csv(
  exclusion_summary,
  file.path(output_dir, "sample_exclusion_summary.csv"),
  na = ""
)
write_csv(
  join_validation,
  file.path(output_dir, "join_validation.csv"),
  na = ""
)
write_csv(
  multipi_parse_check,
  file.path(output_dir, "multipi_parse_check.csv"),
  na = ""
)
write_csv(
  category_crosswalk,
  file.path(output_dir, "telyukova_category_crosswalk.csv"),
  na = ""
)
write_csv(
  payment_instrument_crosswalk,
  file.path(output_dir, "payment_instrument_crosswalk.csv"),
  na = ""
)

message("2019 DCPC liquid-payment construction complete.")
message("Retained transactions: ", nrow(analysis_transactions))
message("Retained respondents: ", n_distinct(analysis_transactions$id))
message("Output directory: ", normalizePath(output_dir))
