# parse_cex_2019_interview_release.R
#
# Purpose:
#   Parse the 2019 Consumer Expenditure Survey Interview PUMD release for a
#   monthly Telyukova-style consumption-volatility analysis.
#
# Input:
#   raw/cex/intrvw19.zip
#
# The 2019 release contains five collection quarters: 2019Q1-2019Q4 and the
# 2020Q1 boundary quarter. All five are retained. Calendar-period restrictions
# should be imposed later with MTBI REF_YR/REF_MO, not by dropping the boundary
# collection file.
#
# Main outputs:
#   output/cex/cex_2019_release_fmli_parsed.rds
#   output/cex/cex_2019_release_mtbi_parsed.rds
#   output/cex/cex_2019_release_file_index.csv
#   output/cex/cex_2019_release_parse_summary.csv
#   output/cex/cex_2019_release_mtbi_flag_counts.csv
#   output/cex/cex_2019_release_panel_coverage.csv
#   output/cex/cex_2019_release_mtbi_outside_reference_window.csv
#
# Parsing principles:
#   - Read all published columns as character so identifiers, UCCs, and raw
#     flags are preserved exactly.
#   - Normalize raw column names to upper case before stacking quarters.
#   - Preserve gift records and negative costs; add flags but do not make
#     analysis-sample decisions in this parser.
#   - Retain the complete modern MTBI primary key and COST_/PUBFLAG fields.
#   - Add standardized panel, interview, numeric cost, and reference-month
#     fields without overwriting the raw fields.

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(readr)
  library(stringr)
  library(tidyr)
})

# Resolve paths from the repository root even when this script is run from its
# organized subfolder.
script_args <- commandArgs(trailingOnly = FALSE)
script_file <- sub("^--file=", "", script_args[grepl("^--file=", script_args)][1])
if (!is.na(script_file)) {
  script_dir <- dirname(normalizePath(script_file))
  setwd(normalizePath(file.path(script_dir, "..", "..", "..")))
}

# -----------------------------------------------------------------------------
# 1. Paths and expected release structure
# -----------------------------------------------------------------------------

archive_year <- 2019L
zip_path <- file.path("raw", "cex", "intrvw19.zip")
output_dir <- file.path("output", "cex")
output_stem <- "cex_2019_release"

if (!file.exists(zip_path)) {
  stop("Missing CEX archive: ", zip_path, call. = FALSE)
}
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

expected_quarters <- tibble(
  collection_year = c(rep(2019L, 4L), 2020L),
  collection_quarter = c(1:4, 1L)
)

required_fmli_fields <- c(
  "NEWID", "CUID", "INTERI", "QINTRVMO", "QINTRVYR",
  "FINLWT21", "AGE_REF", "EDUC_REF", "MARITAL1", "REF_RACE",
  "FINCBTAX", "FAM_SIZE", "CUTENURE"
)

required_mtbi_fields <- c(
  "NEWID", "SEQNO", "ALCNO", "EXPNAME", "COST_", "REF_MO",
  "REF_YR", "RTYPE", "GIFT", "UCCSEQ", "UCC", "COST", "PUBFLAG"
)

mtbi_primary_key <- c(
  "NEWID", "SEQNO", "ALCNO", "EXPNAME", "RTYPE", "UCCSEQ",
  "UCC", "REF_MO", "REF_YR"
)

to_num <- function(x) {
  suppressWarnings(parse_number(
    as.character(x),
    na = c("", ".", "NA", "NaN")
  ))
}

month_index <- function(year, month) {
  as.integer(year) * 12L + as.integer(month)
}

# -----------------------------------------------------------------------------
# 2. Index exactly one FMLI and one MTBI file per release collection quarter
# -----------------------------------------------------------------------------

zip_listing <- utils::unzip(zip_path, list = TRUE)$Name

file_index <- tibble(source_file = zip_listing) %>%
  filter(str_detect(
    str_to_lower(source_file),
    "(^|/)(fmli|mtbi)[0-9]{3}x?[.]csv$"
  )) %>%
  mutate(
    source_zip = basename(zip_path),
    file_base = str_to_lower(basename(source_file)),
    file_type = str_to_upper(str_sub(file_base, 1L, 4L)),
    file_code = str_match(file_base, "^(fmli|mtbi)([0-9]{3})x?[.]csv$")[, 3],
    collection_year = 2000L + as.integer(str_sub(file_code, 1L, 2L)),
    collection_quarter = as.integer(str_sub(file_code, 3L, 3L)),
    archive_year = archive_year,
    is_boundary_quarter = collection_year != archive_year
  ) %>%
  select(
    file_type, source_zip, source_file, archive_year,
    collection_year, collection_quarter, is_boundary_quarter
  ) %>%
  arrange(file_type, collection_year, collection_quarter)

expected_file_index <- crossing(
  file_type = c("FMLI", "MTBI"),
  expected_quarters
)

missing_expected_files <- anti_join(
  expected_file_index,
  file_index,
  by = c("file_type", "collection_year", "collection_quarter")
)
unexpected_files <- anti_join(
  file_index,
  expected_file_index,
  by = c("file_type", "collection_year", "collection_quarter")
)
duplicate_file_cells <- file_index %>%
  count(file_type, collection_year, collection_quarter, name = "n_files") %>%
  filter(n_files != 1L)

if (nrow(missing_expected_files) > 0L ||
    nrow(unexpected_files) > 0L ||
    nrow(duplicate_file_cells) > 0L ||
    nrow(file_index) != 10L) {
  print(file_index)
  if (nrow(missing_expected_files) > 0L) print(missing_expected_files)
  if (nrow(unexpected_files) > 0L) print(unexpected_files)
  if (nrow(duplicate_file_cells) > 0L) print(duplicate_file_cells)
  stop(
    "The 2019 archive does not have the expected five-quarter FMLI/MTBI structure.",
    call. = FALSE
  )
}

# -----------------------------------------------------------------------------
# 3. Read raw files while preserving identifiers and flags
# -----------------------------------------------------------------------------

read_indexed_file <- function(
    file_type, source_zip, source_file, archive_year,
    collection_year, collection_quarter, is_boundary_quarter) {
  message("Reading ", source_file)

  data <- read_csv(
    unz(zip_path, source_file),
    col_types = cols(.default = col_character()),
    na = character(),
    name_repair = "minimal",
    show_col_types = FALSE,
    progress = FALSE
  )

  upper_names <- str_to_upper(names(data))
  if (anyDuplicated(upper_names)) {
    duplicate_names <- unique(upper_names[duplicated(upper_names)])
    stop(
      "Upper-case name normalization creates duplicate field(s) in ",
      source_file, ": ", paste(duplicate_names, collapse = ", "),
      call. = FALSE
    )
  }
  names(data) <- upper_names

  required_fields <- if (file_type == "FMLI") {
    required_fmli_fields
  } else {
    required_mtbi_fields
  }
  missing_fields <- setdiff(required_fields, names(data))
  if (length(missing_fields) > 0L) {
    stop(
      source_file, " is missing required field(s): ",
      paste(missing_fields, collapse = ", "),
      call. = FALSE
    )
  }

  data %>%
    mutate(
      source_zip = source_zip,
      source_file = source_file,
      archive_year = as.integer(archive_year),
      collection_year = as.integer(collection_year),
      collection_quarter = as.integer(collection_quarter),
      is_boundary_quarter = as.logical(is_boundary_quarter),
      .before = 1
    )
}

fmli_raw <- file_index %>%
  filter(file_type == "FMLI") %>%
  pmap_dfr(read_indexed_file)

mtbi_raw <- file_index %>%
  filter(file_type == "MTBI") %>%
  pmap_dfr(read_indexed_file)

# -----------------------------------------------------------------------------
# 4. Standardize FMLI identifiers and validate interview timing
# -----------------------------------------------------------------------------

fmli <- fmli_raw %>%
  mutate(
    newid = as.character(NEWID),
    panel_id = str_sub(newid, 1L, -2L),
    cuid = as.character(CUID),
    interview = as.integer(to_num(INTERI)),
    interview_year = as.integer(to_num(QINTRVYR)),
    interview_month = as.integer(to_num(QINTRVMO)),
    interview_month_index = month_index(interview_year, interview_month),
    collection_quarter_from_month = ((interview_month - 1L) %/% 3L) + 1L
  )

if (anyDuplicated(fmli$newid)) {
  stop("FMLI contains duplicate NEWID values after file selection.", call. = FALSE)
}
if (any(is.na(fmli$interview)) || any(!fmli$interview %in% 1:4)) {
  stop("FMLI INTERI is missing or outside the expected 1-4 range.", call. = FALSE)
}
if (any(str_sub(fmli$newid, -1L, -1L) != as.character(fmli$interview))) {
  stop("FMLI NEWID final digit does not consistently equal INTERI.", call. = FALSE)
}

cuid_match <- to_num(fmli$panel_id) == to_num(fmli$cuid)
if (any(is.na(cuid_match)) || any(!cuid_match)) {
  stop("FMLI CUID does not consistently match NEWID without its final digit.", call. = FALSE)
}
if (any(fmli$interview_year != fmli$collection_year) ||
    any(fmli$collection_quarter_from_month != fmli$collection_quarter)) {
  stop("FMLI interview dates do not match their indexed collection quarters.", call. = FALSE)
}

# -----------------------------------------------------------------------------
# 5. Standardize MTBI fields, link interviews, and flag reference-window rows
# -----------------------------------------------------------------------------

fmli_link <- fmli %>%
  select(
    NEWID, panel_id, cuid, interview, interview_year, interview_month,
    interview_month_index
  )

mtbi <- mtbi_raw %>%
  mutate(
    newid = as.character(NEWID),
    ucc = as.character(UCC),
    ref_year = as.integer(to_num(REF_YR)),
    ref_month = as.integer(to_num(REF_MO)),
    ref_month_index = month_index(ref_year, ref_month),
    cost = to_num(COST),
    is_gift = GIFT == "1",
    is_published_integrated = PUBFLAG == "2",
    is_cost_topcoded = COST_ %in% c("T", "U", "V", "W"),
    is_cost_negative = !is.na(cost) & cost < 0
  ) %>%
  left_join(fmli_link, by = "NEWID", relationship = "many-to-one") %>%
  mutate(
    is_standard_reference_window =
      !is.na(ref_month_index) &
      ref_month_index >= interview_month_index - 3L &
      ref_month_index <= interview_month_index - 1L
  )

if (any(is.na(mtbi$panel_id))) {
  stop("Some MTBI records do not match an indexed FMLI NEWID.", call. = FALSE)
}
if (any(is.na(mtbi$ref_year)) ||
    any(is.na(mtbi$ref_month)) ||
    any(!mtbi$ref_month %in% 1:12)) {
  stop("MTBI has missing or invalid REF_YR/REF_MO values.", call. = FALSE)
}
if (any(nchar(mtbi$ucc) != 6L)) {
  stop("MTBI contains a UCC that is not six characters long.", call. = FALSE)
}

duplicate_mtbi_primary_keys <- sum(duplicated(mtbi[mtbi_primary_key]))
if (duplicate_mtbi_primary_keys > 0L) {
  stop(
    "MTBI contains ", duplicate_mtbi_primary_keys,
    " duplicate modern primary keys after file selection.",
    call. = FALSE
  )
}

# -----------------------------------------------------------------------------
# 6. Panel and reference-month diagnostics
# -----------------------------------------------------------------------------

expected_interview_months <- fmli %>%
  select(panel_id, newid, interview, interview_month_index) %>%
  crossing(reference_lag = 1:3) %>%
  mutate(expected_ref_month_index = interview_month_index - reference_lag)

panel_interview_diagnostics <- fmli %>%
  arrange(panel_id, interview_month_index, interview) %>%
  group_by(panel_id) %>%
  summarise(
    interviews_observed = paste(sort(unique(interview)), collapse = ""),
    n_interviews_observed = n_distinct(interview),
    n_interview_records = n(),
    first_interview_month_index = min(interview_month_index),
    last_interview_month_index = max(interview_month_index),
    has_complete_interviews_1_4 = all(1:4 %in% interview),
    interviews_exactly_three_months_apart =
      n_distinct(interview) <= 1L ||
      all(diff(sort(unique(interview_month_index))) == 3L),
    .groups = "drop"
  )

expected_panel_months <- expected_interview_months %>%
  group_by(panel_id) %>%
  summarise(
    n_distinct_expected_reference_months =
      n_distinct(expected_ref_month_index),
    .groups = "drop"
  )

actual_panel_months <- mtbi %>%
  filter(is_standard_reference_window) %>%
  distinct(panel_id, ref_month_index) %>%
  count(panel_id, name = "n_distinct_mtbi_reference_months")

panel_coverage <- panel_interview_diagnostics %>%
  left_join(expected_panel_months, by = "panel_id") %>%
  left_join(actual_panel_months, by = "panel_id") %>%
  mutate(
    n_distinct_mtbi_reference_months =
      replace_na(n_distinct_mtbi_reference_months, 0L),
    has_complete_12_month_mtbi_panel =
      has_complete_interviews_1_4 &
      n_distinct_expected_reference_months == 12L &
      n_distinct_mtbi_reference_months == 12L
  ) %>%
  arrange(desc(has_complete_12_month_mtbi_panel), panel_id)

# -----------------------------------------------------------------------------
# 7. Write parsed datasets and diagnostics
# -----------------------------------------------------------------------------

file_row_counts <- bind_rows(
  fmli %>% count(source_file, name = "n_rows") %>% mutate(file_type = "FMLI"),
  mtbi %>% count(source_file, name = "n_rows") %>% mutate(file_type = "MTBI")
)

file_index_output <- file_index %>%
  left_join(file_row_counts, by = c("file_type", "source_file"))

parse_summary <- tribble(
  ~metric, ~value,
  "fmli_files", sum(file_index$file_type == "FMLI"),
  "mtbi_files", sum(file_index$file_type == "MTBI"),
  "fmli_rows", nrow(fmli),
  "fmli_unique_newid", n_distinct(fmli$newid),
  "fmli_unique_panels", n_distinct(fmli$panel_id),
  "mtbi_rows", nrow(mtbi),
  "mtbi_duplicate_primary_keys", duplicate_mtbi_primary_keys,
  "mtbi_gift_rows", sum(mtbi$is_gift, na.rm = TRUE),
  "mtbi_published_integrated_rows",
    sum(mtbi$is_published_integrated, na.rm = TRUE),
  "mtbi_topcoded_rows", sum(mtbi$is_cost_topcoded, na.rm = TRUE),
  "mtbi_missing_cost_rows", sum(is.na(mtbi$cost)),
  "mtbi_negative_cost_rows", sum(mtbi$is_cost_negative, na.rm = TRUE),
  "mtbi_standard_reference_window_rows",
    sum(mtbi$is_standard_reference_window, na.rm = TRUE),
  "mtbi_outside_standard_reference_window_rows",
    sum(!mtbi$is_standard_reference_window, na.rm = TRUE),
  "complete_interview_1_4_panels",
    sum(panel_coverage$has_complete_interviews_1_4),
  "complete_12_month_mtbi_panels",
    sum(panel_coverage$has_complete_12_month_mtbi_panel)
)

mtbi_flag_counts <- mtbi %>%
  count(GIFT, PUBFLAG, COST_, name = "n_rows", sort = TRUE)

mtbi_outside_reference_window <- mtbi %>%
  filter(!is_standard_reference_window) %>%
  count(
    source_file, NEWID, UCC, REF_YR, REF_MO,
    interview_year, interview_month,
    name = "n_records",
    sort = TRUE
  )

saveRDS(
  fmli,
  file.path(output_dir, paste0(output_stem, "_fmli_parsed.rds"))
)
saveRDS(
  mtbi,
  file.path(output_dir, paste0(output_stem, "_mtbi_parsed.rds"))
)
write_csv(
  file_index_output,
  file.path(output_dir, paste0(output_stem, "_file_index.csv"))
)
write_csv(
  parse_summary,
  file.path(output_dir, paste0(output_stem, "_parse_summary.csv"))
)
write_csv(
  mtbi_flag_counts,
  file.path(output_dir, paste0(output_stem, "_mtbi_flag_counts.csv"))
)
write_csv(
  panel_coverage,
  file.path(output_dir, paste0(output_stem, "_panel_coverage.csv"))
)
write_csv(
  mtbi_outside_reference_window,
  file.path(
    output_dir,
    paste0(output_stem, "_mtbi_outside_reference_window.csv")
  )
)

message("Done parsing the 2019 CEX Interview release.")
message("Outputs written under: ", output_dir)
print(file_index_output, n = Inf)
print(parse_summary, n = Inf)
