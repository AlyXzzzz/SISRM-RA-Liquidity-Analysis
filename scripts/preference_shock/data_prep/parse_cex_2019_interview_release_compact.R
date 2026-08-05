# Parse the five-quarter 2019 CEX Interview release into one FMLI file and one
# monthly MTBI file. 

library(tidyverse)

archive_year <- 2019
zip_path <- file.path("raw", "cex", "intrvw19.zip")
output_dir <- file.path("output", "cex")
output_stem <- "cex_2019_release"

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

expected_quarters <- tibble(
  collection_year = c(rep(2019, 4), 2020),
  collection_quarter = c(1:4, 1)
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
  as.integer(year) * 12 + as.integer(month)
}

# Index exactly one FMLI and one MTBI file for each of 2019Q1-2019Q4 and the
# 2020Q1 boundary quarter.

zip_listing <- utils::unzip(zip_path, list = TRUE)$Name

file_index <- tibble(source_file = zip_listing) %>%
  filter(str_detect(
    str_to_lower(source_file),
    "(^|/)(fmli|mtbi)[0-9]{3}x?[.]csv$"
  )) %>%
  mutate(
    source_zip = basename(zip_path),
    file_base = str_to_lower(basename(source_file)),
    file_type = str_to_upper(str_sub(file_base, 1, 4)),
    file_code = str_match(
      file_base,
      "^(fmli|mtbi)([0-9]{3})x?[.]csv$"
    )[, 3],
    collection_year = 2000 + as.integer(str_sub(file_code, 1, 2)),
    collection_quarter = as.integer(str_sub(file_code, 3, 3)),
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

file_cells <- file_index %>%
  count(file_type, collection_year, collection_quarter, name = "n_files")

if (
  nrow(file_index) != 10 ||
  nrow(anti_join(
    expected_file_index,
    file_index,
    by = c("file_type", "collection_year", "collection_quarter")
  )) > 0 ||
  nrow(anti_join(
    file_index,
    expected_file_index,
    by = c("file_type", "collection_year", "collection_quarter")
  )) > 0 ||
  any(file_cells$n_files != 1)
) {
  stop(
    "The archive does not have the expected five-quarter FMLI/MTBI structure.",
    call. = FALSE
  )
}

read_indexed_file <- function(
    file_type, source_zip, source_file, archive_year,
    collection_year, collection_quarter, is_boundary_quarter) {
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
    stop(
      "Upper-case name normalization creates duplicate fields in ",
      source_file,
      call. = FALSE
    )
  }
  names(data) <- upper_names

  required_fields <- if (file_type == "FMLI") {
    required_fmli_fields
  } else {
    required_mtbi_fields
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

# Standardize the FMLI interview and panel identifiers.

fmli <- fmli_raw %>%
  mutate(
    newid = as.character(NEWID),
    panel_id = str_sub(newid, 1, -2),
    cuid = as.character(CUID),
    interview = as.integer(to_num(INTERI)),
    interview_year = as.integer(to_num(QINTRVYR)),
    interview_month = as.integer(to_num(QINTRVMO)),
    interview_month_index = month_index(interview_year, interview_month),
    collection_quarter_from_month = ((interview_month - 1) %/% 3) + 1
  )

# Standardize MTBI reference months and link each expenditure record to its
# FMLI interview. Records are flagged, not deleted, when they fall outside the
# three calendar months immediately preceding the interview.

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
      ref_month_index >= interview_month_index - 3 &
      ref_month_index <= interview_month_index - 1
  )

saveRDS(
  fmli,
  file.path(output_dir, paste0(output_stem, "_fmli_parsed.rds"))
)
saveRDS(
  mtbi,
  file.path(output_dir, paste0(output_stem, "_mtbi_parsed.rds"))
)

message("Parsed FMLI and MTBI files written under: ", output_dir)
