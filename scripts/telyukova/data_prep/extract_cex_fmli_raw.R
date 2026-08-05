# extract_cex_fmli_raw.R
#
# Purpose:
#   Starting from the original BLS CEX Interview Survey zip files, extract the
#   Consumer Unit characteristics/income files (FMLI) for calendar-year quarters
#   2000Q1-2000Q4, 2001Q1-2001Q4, and 2002Q1-2002Q4, stack them, and save a
#   raw analysis-ready RDS/CSV.GZ.
#
# Inputs expected:
#   raw/cex/intrvw00.zip
#   raw/cex/intrvw01.zip
#   raw/cex/intrvw02.zip
#
# Outputs:
#   output/cex/cex_fmli_2000_2002_raw.rds
#   output/cex/cex_fmli_2000_2002_raw.csv.gz
#   output/cex/cex_fmli_2000_2002_file_index.csv
#
# Notes:
#   - This script reads all FMLI columns as character to preserve the raw values.
#   - It does not harmonize variable names or construct analysis variables.
#   - It adds only source metadata: source_zip, source_file, cex_year, cex_quarter.
#   - It keeps only the four calendar quarters for each archive year. It excludes
#     the extra Q1 file for the following year that is included in each annual zip.

library(readr)
library(dplyr)
library(purrr)
library(stringr)

# Resolve paths from the repository root even when this script is run from its
# organized subfolder.
script_args <- commandArgs(trailingOnly = FALSE)
script_file <- sub("^--file=", "", script_args[grepl("^--file=", script_args)][1])
if (!is.na(script_file)) {
  script_dir <- dirname(normalizePath(script_file))
  setwd(normalizePath(file.path(script_dir, "..", "..", "..")))
}

# ---------------------------------------------------------------------------
# 1. Set paths
# ---------------------------------------------------------------------------

raw_dir <- "raw/cex"
out_dir <- "output/cex"

zip_paths <- file.path(raw_dir, c("intrvw00.zip", "intrvw01.zip", "intrvw02.zip"))

if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

missing_zips <- zip_paths[!file.exists(zip_paths)]
if (length(missing_zips) > 0) {
  stop(
    "These input zip files were not found:\n",
    paste(missing_zips, collapse = "\n"),
    "\n\nEdit raw_dir or move the zip files into raw/cex/."
  )
}

# ---------------------------------------------------------------------------
# 2. Build an index of FMLI files inside the annual zips
# ---------------------------------------------------------------------------

get_archive_year <- function(zip_path) {
  # intrvw00.zip -> 2000, intrvw01.zip -> 2001, etc.
  yy <- str_match(basename(zip_path), "intrvw([0-9]{2})\\.zip$")[, 2]
  if (is.na(yy)) {
    stop("Could not parse archive year from zip filename: ", basename(zip_path))
  }
  2000L + as.integer(yy)
}

index_zip <- function(zip_path) {
  archive_year <- get_archive_year(zip_path)

  zip_listing <- utils::unzip(zip_path, list = TRUE)

  tibble(
    source_zip = zip_path,
    source_file = zip_listing$Name
  ) %>%
    filter(str_detect(str_to_lower(source_file), "(^|/)fmli[0-9]{3}x?\\.csv$")) %>%
    mutate(
      file_base = basename(source_file),
      file_code = str_match(str_to_lower(file_base), "^fmli([0-9]{3}x?)\\.csv$")[, 2],
      cex_year = 2000L + as.integer(str_sub(file_code, 1, 2)),
      cex_quarter = as.integer(str_sub(file_code, 3, 3)),
      archive_year = archive_year
    )
}

fmli_index_all <- map_dfr(zip_paths, index_zip)

# Keep exactly the four calendar quarters corresponding to each annual archive.
# This excludes, for example, fmli011.csv in intrvw00.zip, which is 2001Q1.
fmli_index <- fmli_index_all %>%
  filter(
    cex_year == archive_year,
    cex_year %in% 2000:2002,
    cex_quarter %in% 1:4
  ) %>%
  arrange(cex_year, cex_quarter, source_file)

if (nrow(fmli_index) != 12) {
  print(fmli_index_all %>% select(source_zip, source_file, cex_year, cex_quarter, archive_year))
  stop("Expected to find 12 FMLI calendar-quarter files, but found ", nrow(fmli_index), ".")
}

# Optional: verify one file per year-quarter.
quarter_counts <- fmli_index %>%
  count(cex_year, cex_quarter, name = "n_files")

bad_quarters <- quarter_counts %>% filter(n_files != 1)
if (nrow(bad_quarters) > 0) {
  print(bad_quarters)
  stop("Some year-quarter cells do not have exactly one FMLI file.")
}

write_csv(
  fmli_index %>% select(source_zip, source_file, cex_year, cex_quarter, archive_year),
  file.path(out_dir, "cex_fmli_2000_2002_file_index.csv")
)

# ---------------------------------------------------------------------------
# 3. Extract and read each FMLI file
# ---------------------------------------------------------------------------

read_fmli_from_zip <- function(source_zip, source_file, cex_year, cex_quarter) {
  tmp_dir <- tempfile(pattern = "cex_fmli_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  extracted_path <- utils::unzip(
    zipfile = source_zip,
    files = source_file,
    exdir = tmp_dir
  )

  read_csv(
    extracted_path,
    col_types = cols(.default = col_character()),
    na = character(),
    show_col_types = FALSE,
    progress = FALSE
  ) %>%
    mutate(
      source_zip = basename(source_zip),
      source_file = source_file,
      cex_year = as.integer(cex_year),
      cex_quarter = as.integer(cex_quarter),
      .before = 1
    )
}

cex_fmli_raw <- pmap_dfr(
  fmli_index %>% select(source_zip, source_file, cex_year, cex_quarter),
  read_fmli_from_zip
)

# ---------------------------------------------------------------------------
# 4. Save stacked raw data
# ---------------------------------------------------------------------------

saveRDS(
  cex_fmli_raw,
  file.path(out_dir, "cex_fmli_2000_2002_raw.rds")
)

write_csv(
  cex_fmli_raw,
  file.path(out_dir, "cex_fmli_2000_2002_raw.csv.gz")
)

# ---------------------------------------------------------------------------
# 5. Basic completion checks printed to console
# ---------------------------------------------------------------------------

message("Done.")
message("Rows: ", nrow(cex_fmli_raw))
message("Columns: ", ncol(cex_fmli_raw))
message("Files read:")
print(fmli_index %>% select(source_file, cex_year, cex_quarter))
