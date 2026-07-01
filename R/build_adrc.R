# ----------------------------------------------------------------------------
# Orchestrator for STAGES 1-2: turn the raw ADRC CSVs into the EEG cohort table.
# Each step lives in its own file; this just threads them together in order.
# ----------------------------------------------------------------------------

#' Build the combined ADRC table, filtered to sleep-EEG participants
#'
#' Reproduces Nicole S. McKay's ADRC data-combining pipeline (STAGE 1), folds
#' in brain-age-gap and PACC cognition, and keeps only the people who had a
#' sleep EEG (STAGE 2). The result is longitudinal: several rows (imaging
#' visits) per person.
#'
#' @param data_dir Folder that directly contains the ADRC CSV exports
#'   (pib.csv, fdg.csv, tau.csv, mri_3t.csv, demographics.csv, apoe.csv,
#'   b4_cdr.csv, sleep_IDs.csv, and optionally ADRC_BAG.csv, psychometrics.csv).
#' @param pacc_match "date" (default) or "mean"; see [add_pacc()].
#' @param pacc_window_years Window for date-matched PACC (default 2).
#' @param check_dates_first If TRUE (default), warn about un-parseable dates
#'   before loading anything.
#' @return A data.frame: one row per imaging visit for EEG participants.
#' @export
build_adrc <- function(data_dir,
                       pacc_match = c("date", "mean"),
                       pacc_window_years = 2,
                       check_dates_first = TRUE) {
  pacc_match <- match.arg(pacc_match)
  if (!dir.exists(data_dir))
    stop("data_dir does not exist: ", data_dir, call. = FALSE)
  if (check_dates_first) check_all_dates(data_dir)

  # STAGE 1 -- combined ADRC table
  ADRC <- merge_imaging(data_dir) %>%
    add_demographics(data_dir) %>%
    add_apoe(data_dir) %>%
    match_cdr(data_dir) %>%
    fill_and_finalize() %>%
    factorise_vars() %>%
    zscore_biomarkers() %>%
    trim_outliers() %>%
    add_visit_counts()

  # STAGE 2 -- BAG, PACC, then filter to EEG participants
  sleep_ids <- load_sleep_ids(data_dir)
  ADRC <- ADRC %>%
    add_bag(data_dir) %>%
    add_pacc(data_dir, match = pacc_match, window_years = pacc_window_years)

  filter_to_eeg(ADRC, sleep_ids)
}
