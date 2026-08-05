library(dplyr)
library(readr)

# Resolve paths from the repository root even when this script is run from its
# organized subfolder.
script_args <- commandArgs(trailingOnly = FALSE)
script_file <- sub("^--file=", "", script_args[grepl("^--file=", script_args)][1])
if (!is.na(script_file)) {
  script_dir <- dirname(normalizePath(script_file))
  setwd(normalizePath(file.path(script_dir, "..", "..", "..")))
}

# -----------------------------
# 1. Variable lists
# -----------------------------

cc_debt_vars <- c("X413", "X421")

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

setdiff(needed_vars, names(scf_full))