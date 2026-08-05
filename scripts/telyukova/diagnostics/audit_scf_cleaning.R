library(readxl)
library(readr)
library(dplyr)
library(tibble)

# Resolve paths from the repository root even when this script is run from its
# organized subfolder.
script_args <- commandArgs(trailingOnly = FALSE)
script_file <- sub("^--file=", "", script_args[grepl("^--file=", script_args)][1])
if (!is.na(script_file)) {
  script_dir <- dirname(normalizePath(script_file))
  setwd(normalizePath(file.path(script_dir, "..", "..", "..")))
}

raw_ascii_path <- "raw/scf2001.ascii"
map_path <- "raw/2001map.xls"
parsed_path <- "output/scf2001_raw_allvars.rds"
summary_path <- "raw/SCFP2001.csv"
output_path <- "output/tables/scf_parsing_audit.csv"

map_raw <- read_excel(map_path, sheet = 1, col_names = FALSE)
if (ncol(map_raw) != 4) {
  stop("Expected four columns in the SCF fixed-width map.")
}

names(map_raw) <- c("var_label", "format", "width", "end_pos")
map_raw <- map_raw %>%
  mutate(
    var_name = sub(
      "^[[:space:]]*([^[:space:]]+).*",
      "\\1",
      as.character(var_label)
    ),
    start_pos = end_pos - width + 1
  )

scf <- readRDS(parsed_path)

checking_vars <- c(
  "X3506", "X3510", "X3514", "X3518", "X3522", "X3526", "X3529"
)
savings_vars <- c("X3804", "X3807", "X3810", "X3813", "X3816", "X3818")
cc_and_liquid_vars <- c(checking_vars, savings_vars, "X3930", "X413", "X421", "X432")

critical_vars <- unique(c(
  "Y1", "YY1", "X14", "X42001", "X5729", "J5729",
  "X7132", "J7132",
  cc_and_liquid_vars,
  paste0("J", sub("^X", "", cc_and_liquid_vars)),
  "X6809", "X7372", "X8023", "X4511", "X7401",
  "X5901", "X5902", "X5904", "X5905",
  "X108", "X114", "X120", "X126", "X132",
  "X202", "X208", "X214", "X220"
))

missing_critical_map <- setdiff(critical_vars, map_raw$var_name)
missing_critical_parsed <- setdiff(critical_vars, names(scf))

audit_rows <- list()
add_audit <- function(metric, passed, observed, expected, detail = "") {
  audit_rows[[length(audit_rows) + 1L]] <<- tibble(
    metric = metric,
    status = ifelse(passed, "PASS", "FAIL"),
    observed = as.character(observed),
    expected = as.character(expected),
    detail = detail
  )
}

add_audit("map variable count", nrow(map_raw) == 5307L, nrow(map_raw), 5307L)
add_audit(
  "map variable names unique",
  !anyDuplicated(map_raw$var_name),
  sum(duplicated(map_raw$var_name)),
  0L
)
add_audit(
  "map positions complete and contiguous",
  map_raw$start_pos[1] == 1 &&
    all(map_raw$start_pos[-1] == head(map_raw$end_pos, -1) + 1) &&
    tail(map_raw$end_pos, 1) == 53082,
  paste0(map_raw$start_pos[1], "-", tail(map_raw$end_pos, 1)),
  "1-53082"
)
add_audit(
  "critical variables present in map",
  length(missing_critical_map) == 0,
  length(missing_critical_map),
  0L,
  paste(missing_critical_map, collapse = ", ")
)
add_audit(
  "parsed dimensions",
  nrow(scf) == 22210L && ncol(scf) == 5307L,
  paste(dim(scf), collapse = " x "),
  "22210 x 5307"
)
add_audit(
  "parsed names match map and order",
  identical(names(scf), map_raw$var_name),
  sum(names(scf) != map_raw$var_name),
  0L
)
add_audit(
  "critical variables present in parsed data",
  length(missing_critical_parsed) == 0,
  length(missing_critical_parsed),
  0L,
  paste(missing_critical_parsed, collapse = ", ")
)

critical_map <- map_raw %>%
  filter(var_name %in% critical_vars) %>%
  arrange(match(var_name, critical_vars))

mismatch_counts <- setNames(integer(nrow(critical_map)), critical_map$var_name)
raw_row_count <- 0L
bad_line_length_count <- 0L
chunk_size <- 250L
raw_connection <- file(raw_ascii_path, open = "r")
on.exit(close(raw_connection), add = TRUE)

repeat {
  raw_lines <- readLines(raw_connection, n = chunk_size, warn = FALSE)
  if (length(raw_lines) == 0L) {
    break
  }

  parsed_rows <- raw_row_count + seq_along(raw_lines)
  bad_line_length_count <- bad_line_length_count +
    sum(nchar(raw_lines, type = "bytes") != 53082L)

  for (i in seq_len(nrow(critical_map))) {
    variable <- critical_map$var_name[i]
    expected <- trimws(substr(
      raw_lines,
      critical_map$start_pos[i],
      critical_map$end_pos[i]
    ))
    expected[expected == ""] <- NA_character_
    observed <- as.character(scf[[variable]][parsed_rows])
    mismatch <- xor(is.na(expected), is.na(observed)) |
      (!is.na(expected) & !is.na(observed) & expected != observed)
    mismatch_counts[variable] <- mismatch_counts[variable] + sum(mismatch)
  }

  raw_row_count <- raw_row_count + length(raw_lines)
}

close(raw_connection)
on.exit(NULL, add = FALSE)

add_audit("raw record count", raw_row_count == 22210L, raw_row_count, 22210L)
add_audit(
  "raw record width",
  bad_line_length_count == 0L,
  bad_line_length_count,
  0L,
  "Number of records not exactly 53,082 bytes"
)
add_audit(
  "critical raw fields match parsed RDS",
  sum(mismatch_counts) == 0L,
  sum(mismatch_counts),
  0L,
  paste(
    names(mismatch_counts)[mismatch_counts > 0],
    mismatch_counts[mismatch_counts > 0],
    sep = "=",
    collapse = ", "
  )
)

case_id <- as.numeric(scf$YY1)
implicate_id <- as.numeric(scf$Y1)
implicate_number <- implicate_id - 10 * case_id
rows_per_case <- table(case_id)
implicates_by_case <- split(implicate_number, case_id)
valid_implicate_pattern <- all(vapply(
  implicates_by_case,
  function(x) length(x) == 5L && all(sort(x) == 1:5),
  logical(1)
))

add_audit(
  "SCF household count",
  length(rows_per_case) == 4442L,
  length(rows_per_case),
  4442L
)
add_audit(
  "five implicates per household",
  all(rows_per_case == 5L) && valid_implicate_pattern,
  paste(range(rows_per_case), collapse = "-"),
  "5-5"
)
add_audit(
  "implicate IDs unique",
  !anyDuplicated(implicate_id),
  sum(duplicated(implicate_id)),
  0L
)

summary_extract <- read_csv(
  summary_path,
  col_select = c(YY1, Y1, WGT, AGE),
  show_col_types = FALSE,
  progress = FALSE
)
summary_match <- match(implicate_id, summary_extract$Y1)
summary_keys_complete <- !anyNA(summary_match) &&
  !anyDuplicated(summary_extract$Y1) &&
  nrow(summary_extract) == nrow(scf)

add_audit(
  "summary extract implicate keys",
  summary_keys_complete,
  sum(is.na(summary_match)),
  0L
)

if (summary_keys_complete) {
  matched_summary <- summary_extract[summary_match, ]
  age_matches <- as.numeric(scf$X14) == matched_summary$AGE
  case_matches <- case_id == matched_summary$YY1
  weight_difference <- abs(as.numeric(scf$X42001) / 5 - matched_summary$WGT)

  add_audit(
    "raw case ID matches summary extract",
    all(case_matches),
    sum(!case_matches),
    0L
  )
  add_audit(
    "raw age matches summary extract",
    all(age_matches),
    sum(!age_matches),
    0L
  )
  add_audit(
    "raw weight matches summary extract",
    max(weight_difference, na.rm = TRUE) < 0.0011,
    format(max(weight_difference, na.rm = TRUE), scientific = FALSE),
    "< 0.0011",
    "Raw X42001 is stored to two decimal places; summary WGT retains more precision."
  )
}

audit <- bind_rows(audit_rows)
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
write_csv(audit, output_path)
print(audit, n = Inf)

if (any(audit$status == "FAIL")) {
  stop("SCF parsing audit failed. See ", output_path)
}
