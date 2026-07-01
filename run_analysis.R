# ============================================================================
# ONE-CLICK RUNNER  (no package install required)
# ----------------------------------------------------------------------------
# This is the easy path for anyone who just wants results:
#   1. Open adrceeg.Rproj in RStudio  (this sets the working directory here,
#      so every path below is relative -- nothing is tied to one computer).
#   2. Put your ADRC CSV exports in the  data/  folder (it is git-ignored, so
#      the HIPAA-protected files never leave your machine).
#   3. Open this file and click "Source" (or Cmd/Ctrl + Shift + S).
#
# It loads the dependencies, sources every function in R/, and runs the
# pipeline. Results (the table, figures, and summary CSVs) land in  outputs/.
#
# Prefer the package route? Instead do:
#   install.packages("devtools"); devtools::install("."); library(adrceeg)
#   run_pipeline(data_dir = "data")
# ============================================================================

options(pkgType = "binary")  # avoid the source-compile prompt on a fresh Mac

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(magrittr, tidyverse, data.table, rlang, here)

# 'export' is optional: it collects all the figures into one editable PowerPoint
# deck (outputs/cohort_figures.pptx). If it can't be installed the pipeline still
# runs and just writes the image files (and logs a note). Comment out to skip.
try(pacman::p_load(export), silent = TRUE)

# Source every function file in R/ -- this includes params.R, which defines
# adrc_params (all the file names, columns, thresholds and plot settings). Edit
# params.R to change any of those. Order doesn't matter: each definition finds
# the others once all are loaded, and params are read when the pipeline runs.
r_files <- list.files(here::here("R"), pattern = "[.][Rr]$", full.names = TRUE)
invisible(lapply(r_files, source))

# Run it. Edit data_dir if your CSVs live somewhere other than ./data
run_pipeline(
  data_dir = here::here("data"),
  out_dir  = here::here("outputs")
)
