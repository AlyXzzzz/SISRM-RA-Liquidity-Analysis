# Combine and standardize the five-quarter 2019 CEX Interview files, then add
# the identifiers, dates, and flags used by the monthly analysis scripts.

library(tidyverse)

# -----------------------------------------------------------------------------
# 1. Paths and analysis choices
# -----------------------------------------------------------------------------

archive_year <- 2019
input_dir <- file.path("raw", "cex", "intrvw19")
output_dir <- file.path("output", "cex")

fmli_files <- file.path(
  input_dir,
  c("fmli191x.csv", "fmli192.csv", "fmli193.csv", "fmli194.csv", "fmli201.csv")
)

mtbi_files <- file.path(
  input_dir,
  c("mtbi191x.csv", "mtbi192.csv", "mtbi193.csv", "mtbi194.csv", "mtbi201.csv")
)

collection_year <- c(rep(2019, 4), 2020)
collection_quarter <- c(1:4, 1)

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
  as.integer(as.integer(year) * 12 + as.integer(month))
}

# -----------------------------------------------------------------------------
# 2. Read and Combine Data
# -----------------------------------------------------------------------------

# Function to read CEX FMLI and MTBI files
read_cex_file <- function(
    path, required_fields, collection_year, collection_quarter) {
  data <- read_csv(
    path,
    col_types = cols(.default = col_character()),
    na = character(),
    name_repair = "minimal",
    show_col_types = FALSE,
    progress = FALSE
  )

  # Create identifying metadata for all 5 quarters
  data %>%
    mutate(
      source_file = file.path(basename(input_dir), basename(path)),
      archive_year = as.integer(archive_year),
      collection_year = as.integer(collection_year),
      collection_quarter = as.integer(collection_quarter),
      is_boundary_quarter = collection_year != archive_year,
      .before = 1
    )
}

# Combine the rows of the 5 quarters of files in one data frame

read_cex_quarters <- function(files, required_fields) {
  bind_rows(lapply(seq_along(files), function(i) {
    read_cex_file(
      files[i], required_fields, collection_year[i], collection_quarter[i]
    )
  }))
}

fmli_raw <- read_cex_quarters(fmli_files, required_fmli_fields)
mtbi_raw <- read_cex_quarters(mtbi_files, required_mtbi_fields)

# -----------------------------------------------------------------------------
# 3. Standardize Data for Analysis
# -----------------------------------------------------------------------------

fmli <- fmli_raw %>%
  mutate(
    newid = as.character(NEWID),
    panel_id = str_sub(newid, 1, -2),
    cuid = as.character(CUID),
    interview = as.integer(to_num(INTERI)),
    interview_year = as.integer(to_num(QINTRVYR)),
    interview_month = as.integer(to_num(QINTRVMO)),
    interview_month_index = month_index(interview_year, interview_month),
    collection_quarter_from_month = as.integer(
      ((interview_month - 1) %/% 3) + 1
    )
  )

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
  
  # Join the FMLI ID, consumer unit and panel ID, interview, interview year,
  # month, and index variables to the MTBI data frame for downstream analysis
  left_join(fmli_link, by = "NEWID", relationship = "many-to-one") %>%
  mutate(
    is_standard_reference_window =
      !is.na(ref_month_index) &
      ref_month_index >= interview_month_index - 3 &
      ref_month_index <= interview_month_index - 1
  )

# -----------------------------------------------------------------------------
# 4. Outputs
# -----------------------------------------------------------------------------

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

saveRDS(fmli, file.path(output_dir, "cex_2019_release_fmli_parsed.rds"))
saveRDS(mtbi, file.path(output_dir, "cex_2019_release_mtbi_parsed.rds"))
