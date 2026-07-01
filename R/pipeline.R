# ----------------------------------------------------------------------------
# Top-level entry point. Build the EEG cohort table, save it, and (optionally)
# write the cohort figures + summaries. This is the single function a user
# calls; everything else is an internal helper.
# ----------------------------------------------------------------------------

#' Run the full ADRC sleep-EEG pipeline
#'
#' Builds the combined ADRC table, filters to sleep-EEG participants, writes the
#' result to `ADRC_eeg.csv`, and (optionally) writes the cohort figures and
#' summary tables. No data is hard-coded: point `data_dir` at a local folder of
#' the ADRC CSV exports.
#'
#' @param data_dir Folder that directly contains the ADRC CSV exports. If NULL,
#'   defaults to a `data/` folder next to your project (via `here::here("data")`
#'   when the \pkg{here} package is installed, otherwise `"data"`).
#' @param out_dir Folder for outputs (table, figures, summaries). Default
#'   "outputs".
#' @param make_plots If TRUE (default), also write the STAGE 3 figures + CSVs.
#' @param pacc_match "date" (default) or "mean"; see [add_pacc()].
#' @param pacc_window_years Window for date-matched PACC (default 2).
#' @return Invisibly, the EEG cohort `data.frame`.
#' @examples
#' \dontrun{
#'   # CSVs live in ~/ADRC_data/ on this machine:
#'   eeg <- run_pipeline("~/ADRC_data")
#' }
#' @export
run_pipeline <- function(data_dir = NULL,
                         out_dir = "outputs",
                         make_plots = TRUE,
                         pacc_match = c("date", "mean"),
                         pacc_window_years = 2) {
  pacc_match <- match.arg(pacc_match)
  if (is.null(data_dir)) {
    data_dir <- if (requireNamespace("here", quietly = TRUE)) here::here("data") else "data"
  }

  ADRC_eeg <- build_adrc(data_dir,
                         pacc_match = pacc_match,
                         pacc_window_years = pacc_window_years)

  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  table_path <- file.path(out_dir, "ADRC_eeg.csv")
  write.csv(ADRC_eeg, table_path, row.names = FALSE)
  cat("\nSaved final table to:", normalizePath(table_path), "\n")

  if (make_plots) make_cohort_plots(ADRC_eeg, out_dir = out_dir)

  invisible(ADRC_eeg)
}
