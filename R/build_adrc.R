# ----------------------------------------------------------------------------
# Orchestrator for STAGES 1-2: turn the raw ADRC CSVs into the EEG cohort table.
# Each step lives in its own file; this just threads them together in order.
#
# This is the function that CALLS zscore_biomarkers() and trim_outliers() -- both
# now driven by the biomarker list in params, so the whole chain generalises to
# any set of markers without editing code here.
# ----------------------------------------------------------------------------

#' Build the combined ADRC table, filtered to sleep-EEG participants
#'
#' Reproduces Nicole S. McKay's ADRC data-combining pipeline (STAGE 1), folds
#' in brain-age-gap and PACC cognition, and keeps only the people who had a
#' sleep EEG (STAGE 2). The result is longitudinal: several rows (imaging
#' visits) per person. All file names, column names and thresholds come from
#' params (see params.R).
#'
#' @param data_dir Folder that directly contains the ADRC CSV exports.
#' @param params Config list (default `adrc_params`).
#' @param check_dates_first If TRUE (default), warn about un-parseable dates
#'   before loading anything.
#' @return A data.frame: one row per imaging visit for EEG participants.
#' @export
build_adrc <- function(data_dir, params = adrc_params, check_dates_first = TRUE) {
  if (!dir.exists(data_dir))
    stop("data_dir does not exist: ", data_dir, call. = FALSE)
  if (check_dates_first) check_all_dates(data_dir, params)

  # STAGE 1 -- combined ADRC table
  ADRC <- merge_imaging(data_dir, params) %>%
    add_demographics(data_dir, params) %>%
    add_apoe(data_dir, params) %>%
    match_cdr(data_dir, params) %>%
    fill_and_finalize() %>%
    factorise_vars() %>%
    zscore_biomarkers(params) %>%
    trim_outliers(params) %>%
    add_visit_counts()

  # STAGE 2 -- BAG, PACC, then filter to EEG participants
  sleep_ids <- load_sleep_ids(data_dir, params)
  ADRC <- ADRC %>%
    add_bag(data_dir, params) %>%
    add_pacc(data_dir, params)

  filter_to_eeg(ADRC, sleep_ids, params)
}
