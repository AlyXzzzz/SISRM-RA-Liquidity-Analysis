# Resolve paths from the repository root even when this script is run from its
# organized subfolder.
script_args <- commandArgs(trailingOnly = FALSE)
script_file <- sub("^--file=", "", script_args[grepl("^--file=", script_args)][1])
if (!is.na(script_file)) {
  script_dir <- dirname(normalizePath(script_file))
  setwd(normalizePath(file.path(script_dir, "..", "..", "..")))
}

# Explore alternative CEX cash-good constructions for Telyukova and Visschers
# (2013), Section 4.2 / Table 2. This is intentionally diagnostic: it compares
# the compact FMLI-summary construction against variants using PQ+CQ collection
# period spending, cleaner household-operation/insurance mappings, and detailed
# MTBI UCC records with and without gift expenditures.

suppressPackageStartupMessages({
  library(dplyr)
  library(fixest)
  library(readr)
  library(stringr)
  library(tidyr)
  library(tibble)
  library(purrr)
})

output_dir <- file.path("output", "tables", "table2_benchmark_tuning_exploration")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

cex_fmli_path <- file.path("output", "cex", "cex_fmli_2000_2002_raw.rds")
cex_zip_dir <- file.path("raw", "cex")
zero_tolerance <- 1e-8
paper_targets <- c(
  benchmark = 19.6,
  excluding_food = 27.5,
  excluding_food_property_taxes = 29.4
)

to_num <- function(x) {
  suppressWarnings(as.numeric(str_replace_all(as.character(x), ",", "")))
}

qyear_to_index <- function(year, quarter) {
  year * 4 + quarter
}

month_add <- function(year, month, k) {
  index <- year * 12 + month + k
  tibble(
    year = (index - 1) %/% 12,
    month = ((index - 1) %% 12) + 1
  )
}

quarter_of_month <- function(month) {
  (as.integer(month) - 1L) %/% 3L + 1L
}

fetch_fred <- function(series_id) {
  url <- paste0("https://fred.stlouisfed.org/graph/fredgraph.csv?id=", series_id)
  tmp <- tempfile(fileext = ".csv")
  status <- system2(
    "curl",
    c("-fsSL", "--connect-timeout", "15", "--max-time", "60", url, "-o", tmp)
  )
  if (!identical(as.integer(status), 0L)) {
    stop("Could not download FRED series: ", series_id, call. = FALSE)
  }

  read_csv(tmp, show_col_types = FALSE) %>%
    rename(date = observation_date, cpi = all_of(series_id)) %>%
    mutate(
      series_id = series_id,
      date = as.Date(date),
      year = as.integer(format(date, "%Y")),
      month = as.integer(format(date, "%m")),
      cpi = to_num(cpi)
    ) %>%
    select(series_id, year, month, cpi)
}

# -----------------------------------------------------------------------------
# 1. FMLI summary component catalog
# -----------------------------------------------------------------------------

fmli_catalog <- tribble(
  ~detail, ~component, ~cq_var, ~pq_var, ~fred_series,
  "food_total", "food", "FOODCQ", "FOODPQ", "CPIUFDNS",
  "food_home", "food", "FDHOMECQ", "FDHOMEPQ", "CPIUFDNS",
  "food_away_no_meals_pay", "food", "FDXMAPCQ", "FDXMAPPQ", "CUUR0000SEFV",
  "food_meals_as_pay", "food", "FDMAPCQ", "FDMAPPQ", "CPIUFDNS",
  "alcohol", "alcohol", "ALCBEVCQ", "ALCBEVPQ", "CUUR0000SAF116",
  "tobacco", "tobacco", "TOBACCCQ", "TOBACCPQ", "CUUR0000SEGA",
  "rent_excluding_as_pay", "rents", "RNTXRPCQ", "RNTXRPPQ", "CUUR0000SEHA",
  "rent_as_pay", "rents", "RNTAPYCQ", "RNTAPYPQ", "CUUR0000SEHA",
  "mortgage_interest", "mortgages", "MRTINTCQ", "MRTINTPQ", "CUUR0000SEHC",
  "vacation_home_mortgage_principal", "mortgages", "MRTPRNOC", "MRTPRNOP", "CUUR0000SEHC",
  "owned_dwelling_repairs_insurance_other", "mortgages", "MRPINSCQ", "MRPINSPQ", "CUUR0000SEHC",
  "utilities", "utilities", "UTILCQ", "UTILPQ", "CUUR0000SAH2",
  "household_furnishings_equipment", "household_furnishings_equipment", "HOUSEQCQ", "HOUSEQPQ", "CUUR0000SAH3",
  "domestic_services", "household_operations", "DOMSRVCQ", "DOMSRVPQ", "CUUR0000SAH3",
  "household_operations_total", "household_operations", "HOUSOPCQ", "HOUSOPPQ", "CUUR0000SAH3",
  "childcare", "household_operations", "BBYDAYCQ", "BBYDAYPQ", "CUUR0000SEEB",
  "property_taxes", "property_taxes", "PROPTXCQ", "PROPTXPQ", "CUUR0000SEHC",
  "personal_insurance_pensions", "insurance", "PERINSCQ", "PERINSPQ", "CUUR0000SEHD",
  "life_other_personal_insurance", "insurance", "LIFINSCQ", "LIFINSPQ", "CPIAUCNS",
  "vehicle_insurance", "insurance", "VEHINSCQ", "VEHINSPQ", "CPIAUCNS",
  "public_transportation", "public_transportation", "PUBTRACQ", "PUBTRAPQ", "CUUR0000SETG",
  "health_insurance", "health_insurance", "HLTHINCQ", "HLTHINPQ", "CUUR0000SAM2"
)

current_code_details <- c(
  "food_total", "alcohol", "tobacco",
  "rent_excluding_as_pay", "rent_as_pay",
  "mortgage_interest", "vacation_home_mortgage_principal",
  "owned_dwelling_repairs_insurance_other",
  "utilities",
  "household_furnishings_equipment", "domestic_services",
  "household_operations_total", "childcare",
  "property_taxes",
  "personal_insurance_pensions", "life_other_personal_insurance",
  "vehicle_insurance", "public_transportation", "health_insurance"
)

no_household_double_details <- setdiff(
  current_code_details,
  c("domestic_services", "childcare")
)

no_houseq_no_household_double_details <- setdiff(
  no_household_double_details,
  "household_furnishings_equipment"
)

no_insurance_double_details <- c(
  setdiff(no_household_double_details, "personal_insurance_pensions"),
  character()
)

no_houseq_no_insurance_double_details <- setdiff(
  no_houseq_no_household_double_details,
  "personal_insurance_pensions"
)

food_no_meals_details <- c(
  setdiff(no_houseq_no_insurance_double_details, "food_total"),
  "food_home", "food_away_no_meals_pay"
)

fmli_specs <- tribble(
  ~spec_id, ~source, ~period, ~details, ~gift_rule, ~note,
  "fmli_cq_current_code", "FMLI summary", "cq_only", list(current_code_details), "not_observed", "Compact-script construction: CQ only, current component list, known household/insurance double counts.",
  "fmli_collection_current_code", "FMLI summary", "pq_plus_cq", list(current_code_details), "not_observed", "Same component list, but uses PQ+CQ to approximate the full three-month collection period.",
  "fmli_collection_no_household_double", "FMLI summary", "pq_plus_cq", list(no_household_double_details), "not_observed", "Removes DOMSRVCQ and BBYDAYCQ because HOUSOPCQ already includes them.",
  "fmli_collection_no_houseq_no_household_double", "FMLI summary", "pq_plus_cq", list(no_houseq_no_household_double_details), "not_observed", "Also removes HOUSEQCQ; paper says household repairs/operations, not all household furnishings/equipment.",
  "fmli_collection_no_insurance_double", "FMLI summary", "pq_plus_cq", list(no_insurance_double_details), "not_observed", "Keeps HOUSEQCQ but removes PERINSCQ to avoid double-counting LIFINSCQ and including pension contributions.",
  "fmli_collection_cleaner_ops_insurance", "FMLI summary", "pq_plus_cq", list(no_houseq_no_insurance_double_details), "not_observed", "Removes HOUSEQCQ, household-operation double counts, and PERINSCQ.",
  "fmli_collection_cleaner_food_no_meals", "FMLI summary", "pq_plus_cq", list(food_no_meals_details), "not_observed", "Cleaner ops/insurance plus food at home and food-away excluding meals as pay."
)

# -----------------------------------------------------------------------------
# 2. Raw MTBI UCC catalog based on csxintvw.pdf summary-variable definitions
# -----------------------------------------------------------------------------

ucc_rows <- function(detail, component, fred_series, codes) {
  tibble(
    detail = detail,
    component = component,
    fred_series = fred_series,
    ucc = as.character(codes)
  )
}

ucc_catalog <- bind_rows(
  ucc_rows("food_total", "food", "CPIUFDNS", c(
    "190904", "790220", "790230", "190901", "190902", "190903",
    "790410", "790430", "800700"
  )),
  ucc_rows("food_home", "food", "CPIUFDNS", c("190904", "790220", "790230")),
  ucc_rows("food_away_no_meals_pay", "food", "CUUR0000SEFV", c(
    "190901", "190902", "190903", "790410", "790430"
  )),
  ucc_rows("food_meals_as_pay", "food", "CPIUFDNS", "800700"),
  ucc_rows("alcohol", "alcohol", "CUUR0000SAF116", c("200900", "790310", "790320", "790420")),
  ucc_rows("tobacco", "tobacco", "CUUR0000SEGA", c("630110", "630210")),
  ucc_rows("rent_excluding_as_pay", "rents", "CUUR0000SEHA", c(
    "210110", "230121", "230141", "230150", "240111", "240121", "240211", "240221",
    "240311", "240321", "320611", "320621", "320631", "350110", "790690", "990920"
  )),
  ucc_rows("rent_as_pay", "rents", "CUUR0000SEHA", "800710"),
  ucc_rows("mortgage_interest", "mortgages", "CUUR0000SEHC", c("220311", "220313", "220321", "880110")),
  ucc_rows("vacation_home_mortgage_principal", "mortgages", "CUUR0000SEHC", c("830102", "830202", "830204", "880320")),
  ucc_rows("owned_dwelling_repairs_insurance_other", "mortgages", "CUUR0000SEHC", c(
    "210901", "220121", "220901", "230112", "230113", "230114", "230115", "230122",
    "230142", "230151", "230901", "240112", "240122", "240212", "240213", "240222",
    "240312", "240322", "320612", "320622", "320632", "340911", "990930"
  )),
  ucc_rows("utilities", "utilities", "CUUR0000SAH2", c(
    "260211", "260212", "260213", "260214", "260111", "260112", "260113", "260114",
    "250111", "250112", "250113", "250114", "250211", "250212", "250213", "250214",
    "250221", "250222", "250223", "250224", "250901", "250902", "250903", "250904",
    "270101", "270102", "270103", "270104", "270211", "270212", "270213", "270214",
    "270411", "270412", "270413", "270414", "270901", "270902", "270903", "270904"
  )),
  ucc_rows("household_furnishings_equipment", "household_furnishings_equipment", "CUUR0000SAH3", c(
    "280110", "280120", "280130", "280210", "280220", "280230", "280900",
    "290110", "290120", "290210", "290310", "290320", "290410", "290420", "290430", "290440",
    "230133", "230134", "320111", "320163",
    "230117", "230118", "300111", "300112", "300211", "300212", "300221", "300222",
    "300311", "300312", "300321", "300322", "300331", "300332", "300411", "300412",
    "320511", "320512",
    "320310", "320320", "320330", "320340", "320350", "320360", "320370", "320521", "320522",
    "320120", "320130", "320150", "320210", "320220", "320231", "320232", "320410",
    "320420", "320901", "320902", "320903", "320904", "340904", "430130", "690111",
    "690112", "690210", "690220", "690230", "690241", "690242", "690243", "690244", "690245"
  )),
  ucc_rows("domestic_services", "household_operations", "CUUR0000SAH3", c(
    "340310", "340410", "340420", "340520", "340530", "340903", "340906", "340910", "340914", "340915",
    "340211", "340212", "670310"
  )),
  ucc_rows("household_operations_total", "household_operations", "CUUR0000SAH3", c(
    "340310", "340410", "340420", "340520", "340530", "340903", "340906", "340910", "340914", "340915",
    "340211", "340212", "670310",
    "330511", "340510", "340620", "340630", "340901", "340907", "340908", "690113", "690114", "990900"
  )),
  ucc_rows("childcare", "household_operations", "CUUR0000SEEB", c("340211", "340212", "670310")),
  ucc_rows("property_taxes", "property_taxes", "CUUR0000SEHC", "220211"),
  ucc_rows("personal_insurance_pensions", "insurance", "CUUR0000SEHD", c(
    "002120", "700110", "800910", "800920", "800931", "800932", "800940"
  )),
  ucc_rows("life_other_personal_insurance", "insurance", "CPIAUCNS", c("002120", "700110")),
  ucc_rows("vehicle_insurance", "insurance", "CPIAUCNS", "500110"),
  ucc_rows("public_transportation", "public_transportation", "CUUR0000SETG", c(
    "530110", "530210", "530312", "530411", "530510", "530901", "530311", "530412", "530902"
  )),
  ucc_rows("health_insurance", "health_insurance", "CUUR0000SAM2", c(
    "580111", "580112", "580113", "580114", "580311", "580312", "580901", "580903", "580904", "580905", "580906"
  ))
)

raw_specs <- tribble(
  ~spec_id, ~source, ~period, ~details, ~gift_rule, ~note,
  "mtbi_collection_current_code_with_gifts", "MTBI UCC", "collection_months", list(current_code_details), "include_gifts", "Raw MTBI UCC version of the compact current component list.",
  "mtbi_collection_current_code_no_gifts", "MTBI UCC", "collection_months", list(current_code_details), "exclude_gift_1", "Same as above, dropping MTBI GIFT == 1 records.",
  "mtbi_collection_no_household_double_no_gifts", "MTBI UCC", "collection_months", list(no_household_double_details), "exclude_gift_1", "Drops household-operation double counts and gift records.",
  "mtbi_collection_no_houseq_no_household_double_no_gifts", "MTBI UCC", "collection_months", list(no_houseq_no_household_double_details), "exclude_gift_1", "Also drops household furnishings/equipment.",
  "mtbi_collection_no_insurance_double_no_gifts", "MTBI UCC", "collection_months", list(no_insurance_double_details), "exclude_gift_1", "Drops household-operation double counts, PERINS double/pension issue, and gift records.",
  "mtbi_collection_cleaner_ops_insurance_no_gifts", "MTBI UCC", "collection_months", list(no_houseq_no_insurance_double_details), "exclude_gift_1", "Drops HOUSEQ, household-operation double counts, PERINS, and gift records.",
  "mtbi_collection_cleaner_food_no_meals_no_gifts", "MTBI UCC", "collection_months", list(food_no_meals_details), "exclude_gift_1", "Cleaner ops/insurance plus food at home and food-away excluding meals as pay."
)

# -----------------------------------------------------------------------------
# 3. Data and CPI setup
# -----------------------------------------------------------------------------

all_fred_series <- unique(c(fmli_catalog$fred_series, ucc_catalog$fred_series))
cpi <- bind_rows(lapply(all_fred_series, fetch_fred)) %>%
  group_by(series_id) %>%
  mutate(cpi_base_2001 = mean(cpi[year == 2001], na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(cpi_norm = cpi / cpi_base_2001)

cex_fmli_raw <- readRDS(cex_fmli_path)
names(cex_fmli_raw) <- toupper(names(cex_fmli_raw))

needed_vars <- unique(c(
  fmli_catalog$cq_var,
  fmli_catalog$pq_var,
  "NEWID", "CEX_YEAR", "CEX_QUARTER", "QINTRVMO", "QINTRVYR",
  "AGE_REF", "EDUC_REF", "MARITAL1", "REF_RACE", "EARNINCX",
  "FINCBTAX", "FAM_SIZE", "CUTENURE"
))
missing_vars <- setdiff(needed_vars, names(cex_fmli_raw))
if (length(missing_vars) > 0) {
  stop("Missing FMLI vars: ", paste(missing_vars, collapse = ", "), call. = FALSE)
}

cex_panel <- cex_fmli_raw %>%
  mutate(
    row_id = row_number(),
    newid = as.character(NEWID),
    case_id = str_sub(newid, 1L, -2L),
    interview = to_num(str_sub(newid, -1L, -1L)),
    cex_year = to_num(CEX_YEAR),
    cex_quarter = to_num(CEX_QUARTER),
    qyear_index = qyear_to_index(cex_year, cex_quarter),
    qintrv_month = to_num(QINTRVMO),
    qintrv_year = to_num(QINTRVYR),
    age = to_num(AGE_REF),
    age2 = age^2,
    age3 = age^3,
    education = as.factor(EDUC_REF),
    marital_status = as.factor(MARITAL1),
    race = as.factor(REF_RACE),
    earnings_annual = to_num(EARNINCX),
    income_before_tax = to_num(FINCBTAX),
    family_size = to_num(FAM_SIZE),
    homeownership = as.factor(CUTENURE)
  )

control_vars <- c(
  "age", "age2", "age3", "education", "marital_status", "race",
  "earnings_annual", "family_size", "homeownership",
  "qintrv_month", "qintrv_year"
)

cpi_deflator_collection <- cex_panel %>%
  distinct(qintrv_year, qintrv_month) %>%
  crossing(fred_series = all_fred_series) %>%
  expand_grid(lag_month = -3:-1) %>%
  rowwise() %>%
  mutate(month_add(qintrv_year, qintrv_month, lag_month)) %>%
  ungroup() %>%
  left_join(cpi, by = c("fred_series" = "series_id", "year" = "year", "month" = "month")) %>%
  group_by(qintrv_year, qintrv_month, fred_series) %>%
  summarise(cpi_norm = mean(cpi_norm, na.rm = TRUE), .groups = "drop")

# -----------------------------------------------------------------------------
# 4. MTBI reader and field scan
# -----------------------------------------------------------------------------

index_zip_csvs <- function(zip_path, pattern = NULL) {
  files <- unzip(zip_path, list = TRUE)$Name
  files <- files[str_detect(str_to_lower(files), "[.]csv$")]
  if (!is.null(pattern)) {
    files <- files[str_detect(str_to_lower(files), pattern)]
  }
  tibble(source_zip = zip_path, source_file = files)
}

mtbi_index <- map_dfr(
  list.files(cex_zip_dir, pattern = "[.]zip$", full.names = TRUE),
  index_zip_csvs,
  pattern = "(^|/)mtbi[0-9]{3}x?[.]csv$"
)

read_mtbi <- function(source_zip, source_file) {
  read_csv(
    unz(source_zip, source_file),
    col_types = cols(.default = col_character()),
    show_col_types = FALSE
  ) %>%
    rename_with(toupper) %>%
    transmute(
      newid = as.character(NEWID),
      ucc = as.character(UCC),
      nominal = to_num(COST),
      gift = as.character(GIFT),
      pubflag = as.character(PUBFLAG),
      ref_month = to_num(REF_MO),
      ref_year = to_num(REF_YR),
      source_zip = basename(source_zip),
      source_file = source_file
    )
}

mtbi <- pmap_dfr(mtbi_index, read_mtbi)

scan_fields <- function() {
  csv_index <- map_dfr(
    list.files(cex_zip_dir, pattern = "[.]zip$", full.names = TRUE),
    index_zip_csvs
  )

  map_dfr(seq_len(nrow(csv_index)), function(i) {
    source_zip <- csv_index$source_zip[[i]]
    source_file <- csv_index$source_file[[i]]
    header <- names(read_csv(
      unz(source_zip, source_file),
      n_max = 0,
      show_col_types = FALSE,
      col_types = cols(.default = col_character())
    ))
    tibble(
      source_zip = basename(source_zip),
      source_file = source_file,
      field = header
    )
  }) %>%
    mutate(field_upper = str_to_upper(field)) %>%
    filter(str_detect(field_upper, "GIFT|GFT|PAY|PMT|PYMT|CARD|CRED|CASH|CHECK|CHCK|DEBIT|PUBFLAG|PUB_FLAG")) %>%
    arrange(source_zip, source_file, field)
}

write_csv(scan_fields(), file.path(output_dir, "cex_payment_gift_field_scan.csv"))
write_csv(mtbi %>% count(gift, pubflag, sort = TRUE), file.path(output_dir, "mtbi_gift_pubflag_counts.csv"))
write_csv(ucc_catalog, file.path(output_dir, "mtbi_ucc_cash_good_crosswalk.csv"))
write_csv(fmli_catalog, file.path(output_dir, "fmli_cash_good_crosswalk.csv"))

# -----------------------------------------------------------------------------
# 5. Panel builders
# -----------------------------------------------------------------------------

summarise_components <- function(component_real) {
  component_real %>%
    group_by(row_id) %>%
    summarise(
      cash_good_benchmark_real = sum(real, na.rm = TRUE),
      cash_good_excluding_food_real = sum(real[component != "food"], na.rm = TRUE),
      cash_good_excluding_food_property_taxes_real =
        sum(real[!component %in% c("food", "property_taxes")], na.rm = TRUE),
      cash_food_real = sum(real[component == "food"], na.rm = TRUE),
      cash_property_taxes_real = sum(real[component == "property_taxes"], na.rm = TRUE),
      .groups = "drop"
    )
}

build_regression_panel <- function(cash_good_real, spec_id, source, period, gift_rule, note) {
  cex_panel %>%
    select(
      row_id, newid, case_id, interview, cex_year, cex_quarter, qyear_index,
      qintrv_month, qintrv_year, all_of(control_vars)
    ) %>%
    left_join(cash_good_real, by = "row_id") %>%
    pivot_longer(
      cols = c(
        cash_good_benchmark_real,
        cash_good_excluding_food_real,
        cash_good_excluding_food_property_taxes_real
      ),
      names_to = "measure",
      values_to = "liquid_consumption"
    ) %>%
    mutate(
      measure = recode(
        measure,
        cash_good_benchmark_real = "benchmark",
        cash_good_excluding_food_real = "excluding_food",
        cash_good_excluding_food_property_taxes_real = "excluding_food_property_taxes"
      ),
      spec_id = spec_id,
      source = source,
      period = period,
      gift_rule = gift_rule,
      note = note,
      positive_liquid_consumption =
        !is.na(liquid_consumption) & liquid_consumption > zero_tolerance,
      log_liquid_consumption = if_else(
        positive_liquid_consumption,
        log(pmax(liquid_consumption, .Machine$double.xmin)),
        NA_real_
      ),
      complete_controls = if_all(all_of(control_vars), ~ !is.na(.x))
    )
}

build_fmli_panel <- function(spec_row) {
  details <- spec_row$details[[1]]
  selected <- fmli_catalog %>% filter(detail %in% details)

  component_real <- map_dfr(seq_len(nrow(selected)), function(i) {
    row <- selected[i, ]
    if (spec_row$period == "cq_only") {
      nominal <- to_num(cex_panel[[row$cq_var]])
    } else if (spec_row$period == "pq_plus_cq") {
      nominal <- to_num(cex_panel[[row$cq_var]]) + to_num(cex_panel[[row$pq_var]])
    } else {
      stop("Unknown FMLI period: ", spec_row$period, call. = FALSE)
    }

    tibble(
      row_id = cex_panel$row_id,
      qintrv_year = cex_panel$qintrv_year,
      qintrv_month = cex_panel$qintrv_month,
      detail = row$detail,
      component = row$component,
      fred_series = row$fred_series,
      nominal = nominal
    )
  }) %>%
    left_join(cpi_deflator_collection, by = c("qintrv_year", "qintrv_month", "fred_series")) %>%
    mutate(real = nominal / cpi_norm)

  component_summary <- component_real %>%
    group_by(detail, component) %>%
    summarise(
      n_positive = sum(nominal > 0, na.rm = TRUE),
      mean_nominal = mean(nominal, na.rm = TRUE),
      p95_nominal = unname(quantile(nominal, 0.95, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    mutate(spec_id = spec_row$spec_id)

  panel <- build_regression_panel(
    summarise_components(component_real),
    spec_row$spec_id,
    spec_row$source,
    spec_row$period,
    spec_row$gift_rule,
    spec_row$note
  )

  list(panel = panel, component_summary = component_summary)
}

build_raw_panel <- function(spec_row) {
  details <- spec_row$details[[1]]
  selected_ucc <- ucc_catalog %>% filter(detail %in% details)

  mtbi_spec <- mtbi %>%
    inner_join(
      cex_panel %>% select(row_id, newid, qintrv_year, qintrv_month, cex_year, cex_quarter),
      by = "newid"
    )

  if (spec_row$gift_rule == "exclude_gift_1") {
    mtbi_spec <- mtbi_spec %>% filter(gift != "1" | is.na(gift))
  }

  if (spec_row$period == "current_calendar_quarter") {
    mtbi_spec <- mtbi_spec %>%
      filter(ref_year == cex_year, quarter_of_month(ref_month) == cex_quarter)
  }

  component_real <- mtbi_spec %>%
    inner_join(selected_ucc, by = "ucc", relationship = "many-to-many") %>%
    left_join(cpi, by = c("fred_series" = "series_id", "ref_year" = "year", "ref_month" = "month")) %>%
    mutate(real = nominal / cpi_norm)

  component_summary <- component_real %>%
    group_by(detail, component) %>%
    summarise(
      n_records = n(),
      n_gift_records = sum(gift == "1", na.rm = TRUE),
      mean_nominal = mean(nominal, na.rm = TRUE),
      p95_nominal = unname(quantile(nominal, 0.95, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    mutate(spec_id = spec_row$spec_id)

  panel <- build_regression_panel(
    summarise_components(component_real),
    spec_row$spec_id,
    spec_row$source,
    spec_row$period,
    spec_row$gift_rule,
    spec_row$note
  )

  list(panel = panel, component_summary = component_summary)
}

# -----------------------------------------------------------------------------
# 6. Estimation and diagnostics
# -----------------------------------------------------------------------------

estimate_measure <- function(panel, measure_name) {
  reg_data <- panel %>%
    filter(measure == measure_name, positive_liquid_consumption, complete_controls) %>%
    group_by(case_id) %>%
    filter(n() >= 2) %>%
    ungroup()

  if (nrow(reg_data) == 0) {
    return(tibble(
      measure = measure_name,
      n_stage1 = 0L,
      n_ar = 0L,
      n_households = 0L,
      rho = NA_real_,
      eta_sd_pct = NA_real_
    ))
  }

  first_stage <- feols(
    log_liquid_consumption ~
      age + age2 + age3 +
      i(education) + i(marital_status) + i(race) +
      earnings_annual + family_size + i(homeownership) +
      i(qintrv_month) + i(qintrv_year) |
      case_id,
    data = reg_data,
    notes = FALSE,
    warn = FALSE
  )

  ar_data <- reg_data %>%
    mutate(epsilon = resid(first_stage)) %>%
    arrange(case_id, qyear_index, interview) %>%
    group_by(case_id) %>%
    mutate(
      epsilon_lag = lag(epsilon),
      qyear_index_lag = lag(qyear_index),
      consecutive_lag = qyear_index - qyear_index_lag == 1
    ) %>%
    ungroup() %>%
    filter(consecutive_lag, !is.na(epsilon), !is.na(epsilon_lag))

  ar_model <- feols(epsilon ~ 0 + epsilon_lag, data = ar_data, notes = FALSE, warn = FALSE)

  tibble(
    measure = measure_name,
    n_stage1 = nrow(reg_data),
    n_ar = nrow(ar_data),
    n_households = n_distinct(reg_data$case_id),
    rho = unname(coef(ar_model)[["epsilon_lag"]]),
    eta_sd_pct = 100 * sd(resid(ar_model))
  )
}

estimate_spec <- function(panel) {
  meta <- panel %>%
    distinct(spec_id, source, period, gift_rule, note)

  bind_rows(lapply(names(paper_targets), function(measure_name) {
    estimate_measure(panel, measure_name)
  })) %>%
    bind_cols(meta[rep(1, nrow(.)), ]) %>%
    relocate(spec_id, source, period, gift_rule, note, measure) %>%
    mutate(
      paper_eta_sd_pct = unname(paper_targets[measure]),
      diff_from_paper_pct = eta_sd_pct - paper_eta_sd_pct,
      abs_diff_from_paper_pct = abs(diff_from_paper_pct)
    )
}

fmli_outputs <- pmap(fmli_specs, function(...) {
  row <- tibble(...)
  message("Building ", row$spec_id)
  build_fmli_panel(row)
})

raw_outputs <- pmap(raw_specs, function(...) {
  row <- tibble(...)
  message("Building ", row$spec_id)
  build_raw_panel(row)
})

all_panels <- c(
  lapply(fmli_outputs, `[[`, "panel"),
  lapply(raw_outputs, `[[`, "panel")
)
all_component_summaries <- bind_rows(
  lapply(fmli_outputs, `[[`, "component_summary"),
  lapply(raw_outputs, `[[`, "component_summary")
)

results <- bind_rows(lapply(all_panels, estimate_spec)) %>%
  arrange(measure, abs_diff_from_paper_pct, spec_id)

panel_diagnostics <- bind_rows(all_panels) %>%
  group_by(spec_id, source, period, gift_rule, measure) %>%
  summarise(
    n_rows = n(),
    n_positive = sum(positive_liquid_consumption, na.rm = TRUE),
    n_positive_complete_controls = sum(positive_liquid_consumption & complete_controls, na.rm = TRUE),
    mean_liquid = mean(liquid_consumption, na.rm = TRUE),
    p50_liquid = median(liquid_consumption, na.rm = TRUE),
    p95_liquid = unname(quantile(liquid_consumption, 0.95, na.rm = TRUE)),
    .groups = "drop"
  )

# Raw-vs-FMLI component checks for the UCC mapping. We compare current calendar
# quarter raw MTBI sums to FMLI CQ and full collection raw MTBI sums to FMLI PQ+CQ.
component_match <- ucc_catalog %>%
  distinct(detail, component, fred_series, ucc) %>%
  inner_join(mtbi, by = "ucc", relationship = "many-to-many") %>%
  inner_join(
    cex_panel %>% select(row_id, newid, cex_year, cex_quarter),
    by = "newid"
  ) %>%
  mutate(raw_period = if_else(
    ref_year == cex_year & quarter_of_month(ref_month) == cex_quarter,
    "cq_only",
    "not_cq"
  )) %>%
  group_by(row_id, detail, raw_period) %>%
  summarise(raw_nominal = sum(nominal, na.rm = TRUE), .groups = "drop") %>%
  filter(raw_period == "cq_only") %>%
  left_join(
    fmli_catalog %>% select(detail, cq_var),
    by = "detail"
  ) %>%
  mutate(fmli_nominal = map2_dbl(row_id, cq_var, ~ to_num(cex_panel[[.y]][.x]))) %>%
  group_by(detail) %>%
  summarise(
    n_matched_rows = n(),
    raw_mean = mean(raw_nominal, na.rm = TRUE),
    fmli_mean = mean(fmli_nominal, na.rm = TRUE),
    mean_ratio_raw_to_fmli = raw_mean / fmli_mean,
    correlation = suppressWarnings(cor(raw_nominal, fmli_nominal, use = "complete.obs")),
    mean_abs_diff = mean(abs(raw_nominal - fmli_nominal), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_abs_diff))

write_csv(results, file.path(output_dir, "table2_benchmark_tuning_results.csv"))
write_csv(panel_diagnostics, file.path(output_dir, "panel_diagnostics.csv"))
write_csv(all_component_summaries, file.path(output_dir, "component_summaries.csv"))
write_csv(component_match, file.path(output_dir, "raw_mtbi_vs_fmli_component_match.csv"))

cat("\nBenchmark-tuning exploration complete.\n")
cat("Results written to:", output_dir, "\n\n")
print(results %>% filter(measure == "benchmark") %>% arrange(abs_diff_from_paper_pct) %>% select(spec_id, source, period, gift_rule, eta_sd_pct, paper_eta_sd_pct, diff_from_paper_pct, n_stage1, n_ar), n = Inf)
cat("\nAll measures, best 20 rows by absolute target miss:\n")
print(results %>% arrange(abs_diff_from_paper_pct) %>% select(spec_id, measure, eta_sd_pct, paper_eta_sd_pct, diff_from_paper_pct, source, period, gift_rule), n = 20)
