library(readxl)
library(readr)
library(dplyr)

# Resolve paths from the repository root even when this script is run from its
# organized subfolder.
script_args <- commandArgs(trailingOnly = FALSE)
script_file <- sub("^--file=", "", script_args[grepl("^--file=", script_args)][1])
if (!is.na(script_file)) {
  script_dir <- dirname(normalizePath(script_file))
  setwd(normalizePath(file.path(script_dir, "..", "..", "..")))
}

excel_sheets("raw/2001map.xls")

map_raw <- read_excel(
  "raw/2001map.xls",
  sheet = 1,
  col_names = FALSE
)

colnames(map_raw) <- c("var_label", "format", "width", "end_pos")
map_raw$var_name <- sub(
  "^[[:space:]]*([^[:space:]]+).*",
  "\\1",
  as.character(map_raw$var_label)
)
map_raw$start_pos <- map_raw$end_pos - map_raw$width + 1

raw_lines <- readLines("raw/scf2001.ascii", n = 5)

fwf_spec <- fwf_positions(
  start = map_raw$start_pos,
  end = map_raw$end_pos,
  col_names = map_raw$var_name
)

scf_full <- read_fwf(
  file = "raw/scf2001.ascii",
  col_positions = fwf_spec,
  col_types = cols(.default = col_character()),
  progress = TRUE
)

names(scf_full) <- map_raw$var_name

saveRDS(scf_full, "output/scf2001_raw_allvars.rds")
write_csv(scf_full, "output/scf2001_raw_allvars.csv.gz")
