# Compute the liquid consumption shares used to weight down CEX liquid goods 
# that were actually paid with liquid payment methods.

library(tidyverse)

# -----------------------------------------------------------------------------
# 1. Paths and analysis choices
# -----------------------------------------------------------------------------

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

# -----------------------------------------------------------------------------
# 2. Read and Join Data for analysis
# -----------------------------------------------------------------------------

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

day_weights <- days_raw %>%
  transmute(id, diary_day, diary_date = date, dow_weight)

transactions_joined <- transactions_raw %>%
  mutate(transaction_row_id = row_number()) %>%
  rename(transaction_date = date) %>%
  left_join(
    day_weights,
    by = c("id", "diary_day"),
    relationship = "many-to-one"
  )

# -----------------------------------------------------------------------------
# 3. Construct Transaction Groups and mappings to Telyukova consumption groups
# -----------------------------------------------------------------------------

# Narrow liquid instruments are cash, check, and debit. The broad definition
# additionally includes bank-account-number payments, online banking bill pay,
# and account-to-account transfers.

narrow_liquid_pi <- c(1, 2, 4)
broad_liquid_pi <- c(1, 2, 4, 6, 7, 11)
valid_pi_codes <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 13, 14)

# Here is a summary of the mapping pipeline: We group transactions by their 
# merchant identification, and their payment purpose identifiers if there
# is ambiguity with the merchant/transaction group. Each individual transaction  
# is mapped to a single Telyukova consumption category. This Telyukova 
# consumption category is then grouped upwards into a broad Telyukova group.
# For example, the food/alcohol/tobacco Telyukova group consists of multiple
# Telyukova categories, such as food purchased as groceries, food purchased as 
# restaurant or bar expenditures, or food purchased from fast food places. We 
# note that mappings are not exact and are only proxies since there is no 1-1 
# correspondence between DCPC groups and CEX consumption categories. 

category_group_crosswalk <- tribble(
  ~tely_category, ~tely_group,
  "food_grocery_proxy", "food_alcohol_tobacco_proxy",
  "food_restaurant_bar_proxy", "food_alcohol_tobacco_proxy",
  "food_fast_service_proxy", "food_alcohol_tobacco_proxy",
  "rent", "rent",
  "mortgage_payment", "mortgage",
  "utilities", "utilities",
  "communications_entertainment_proxy", "utilities",
  "household_operations_bill_proxy", "household_operations",
  "household_repairs", "owned_dwelling_repairs_insurance_other",
  "homeowners_renters_insurance",
    "owned_dwelling_repairs_insurance_other",
  "property_taxes", "property_taxes",
  "public_transport_tolls", "public_transportation",
  "taxi_air_delivery_proxy", "public_transportation",
  "health_insurance", "health_insurance",
  "childcare", "childcare",
  "cash_contributions", "cash_contributions"
)

transactions_classified <- transactions_joined %>%
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
      from_bill_section == 1 & merch %in% c(6, 11) ~
        "bill_reminder_merch6_or_11_household_operations",
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
      category_rule ==
        "merch15_pay010_insurance_pay016_home_or_renters" ~
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
      category_rule ==
        "merch19_pay040_goods_pay041_transit_or_toll" ~
        "public_transport_tolls",
      category_rule == "merch17_pay050_donation_or_offering" ~
        "cash_contributions",
      category_rule ==
        "bill_reminder_merch6_or_11_household_operations" ~
        "household_operations_bill_proxy",
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
  ) %>%
  left_join(
    category_group_crosswalk,
    by = "tely_category",
    relationship = "many-to-one"
  )

# Restrict to valid, weighted October 2019 payment transactions that can be
# assigned to the selected Telyukova consumption categories.
analysis_base <- transactions_classified %>%
  filter(
    payment == 1,
    diary_day %in% 1:3,
    diary_date >= as.Date("2019-10-01"),
    diary_date <= as.Date("2019-10-31"),
    !is.na(dow_weight),
    dow_weight > 0,
    !is.na(amnt),
    amnt > 0,
    pi %in% valid_pi_codes,
    !is.na(tely_category)
  )

# Allocate transactions with multiple payment methods to their component
# instruments for the dollar-share calculation.

multiple_label_crosswalk <- tribble(
  ~component_label_key, ~component_pi,
  "cash", 1,
  "check", 2,
  "credit card", 3,
  "debit card", 4,
  "prepaid/gift/ebt card", 5,
  "bank account number payment", 6,
  "online banking bill payment", 7,
  "money order", 8,
  "paypal", 10,
  "mobile payment app", 10,
  "account-to-account transfer", 11,
  "other payment method", 13,
  "direct deduction from income", 14,
  "deduction from income", 14
)

# Separate the single method components
single_method_components <- analysis_base %>%
  filter(pi != 0) %>%
  transmute(
    transaction_row_id,
    component_pi = as.integer(pi),
    component_amount = amnt
  )

# Parse the multi-method components
multiple_method_components <- analysis_base %>%
  filter(pi == 0, !is.na(multipi_breakdown)) %>%
  select(transaction_row_id, multipi_breakdown) %>%
  mutate(
    payment_piece = str_split(
      multipi_breakdown,
      pattern = ",\\s*(?=[A-Za-z])"
    )
  ) %>%
  unnest_longer(payment_piece) %>%
  mutate(
    component_label_key = str_to_lower(str_squish(str_remove(
      payment_piece,
      pattern = paste0(
        ":\\s*[-$]?",
        "(?:[0-9][0-9,]*(?:\\.[0-9]+)?|\\.[0-9]+)",
        "\\s*$"
      )
    ))),
    component_amount = suppressWarnings(parse_number(str_extract(
      payment_piece,
      paste0(
        "[-$]?",
        "(?:[0-9][0-9,]*(?:\\.[0-9]+)?|\\.[0-9]+)",
        "\\s*$"
      )
    )))
  ) %>%
  left_join(
    multiple_label_crosswalk,
    by = "component_label_key",
    relationship = "many-to-one"
  ) %>%
  select(transaction_row_id, component_pi, component_amount)

# Combine parsed multi-method transactions back to one components data frame 
component_summary <- bind_rows(
  single_method_components,
  multiple_method_components
) %>%
  mutate(
    component_liquid_narrow = component_pi %in% narrow_liquid_pi,
    component_liquid_broad = component_pi %in% broad_liquid_pi
  ) %>%
  group_by(transaction_row_id) %>%
  summarise(
    # Some metadata/summary statistics
    
    parsed_component_total = sum(component_amount, na.rm = TRUE),
    n_unparsed_components = sum(
      is.na(component_amount) | is.na(component_pi)
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

# Join back parsed multi-method components to the main analysis data frame
analysis_transactions <- analysis_base %>%
  left_join(
    component_summary,
    by = "transaction_row_id",
    relationship = "one-to-one"
  ) %>%
  mutate(
    
    # Ensure our parsed multi-payment method amounts match total reported for 
    # that transaction. 
    component_total_matches =
      !is.na(parsed_component_total) &
      abs(parsed_component_total - amnt) <= 0.02,
    count_eligible = pi != 0,
    
    # Check to ensure eligible dollar amounts have matching totals and
    # parsed correctly 
    dollar_eligible =
      component_total_matches &
      !is.na(n_unparsed_components) &
      n_unparsed_components == 0,
    
    # Count-analysis indicators for broad/narrow liquid payment methods.
    liquid_narrow_count = case_when(
      !count_eligible ~ NA_real_,
      pi %in% narrow_liquid_pi ~ 1,
      TRUE ~ 0
    ),
    liquid_broad_count = case_when(
      !count_eligible ~ NA_real_,
      pi %in% broad_liquid_pi ~ 1,
      TRUE ~ 0
    )
  )

# -----------------------------------------------------------------------------
# 4. Compute Weighted Shares
# -----------------------------------------------------------------------------

# Helper function to explicitly deal with potential division by 0
safe_ratio <- function(numerator, denominator) {
  ifelse(denominator > 0, numerator / denominator, NA_real_)
}

# Output data frame: we compute all combinations of weighted, 
# broad/narrow, count/dollar 

shares_by_group_output <- analysis_transactions %>%
  group_by(tely_group) %>%
  summarise(
    weighted_count_denominator = sum(dow_weight[count_eligible]),
    weighted_count_narrow_numerator = sum(
      dow_weight[count_eligible] * liquid_narrow_count[count_eligible]
    ),
    weighted_count_broad_numerator = sum(
      dow_weight[count_eligible] * liquid_broad_count[count_eligible]
    ),
    weighted_dollar_denominator = sum(
      dow_weight[dollar_eligible] * amnt[dollar_eligible]
    ),
    weighted_dollar_narrow_numerator = sum(
      dow_weight[dollar_eligible] * liquid_narrow_amount[dollar_eligible]
    ),
    weighted_dollar_broad_numerator = sum(
      dow_weight[dollar_eligible] * liquid_broad_amount[dollar_eligible]
    ),
    .groups = "drop"
  ) %>%
  transmute(
    tely_group,
    weighted_count_share_narrow = safe_ratio(
      weighted_count_narrow_numerator,
      weighted_count_denominator
    ),
    weighted_count_share_broad = safe_ratio(
      weighted_count_broad_numerator,
      weighted_count_denominator
    ),
    weighted_dollar_share_narrow = safe_ratio(
      weighted_dollar_narrow_numerator,
      weighted_dollar_denominator
    ),
    weighted_dollar_share_broad = safe_ratio(
      weighted_dollar_broad_numerator,
      weighted_dollar_denominator
    )
  ) %>%
  arrange(tely_group)

# -----------------------------------------------------------------------------
# 5. Output
# -----------------------------------------------------------------------------

write_csv(
  shares_by_group_output,
  file.path(output_dir, "shares_by_group_output.csv"),
  na = ""
)
