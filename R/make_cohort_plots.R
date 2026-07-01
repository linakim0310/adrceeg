# ----------------------------------------------------------------------------
# STAGE 3 orchestrator -- build the shared "one row per person" frames once,
# then hand them to each figure/summary writer.
# ----------------------------------------------------------------------------

#' Write the full set of cohort figures and summary tables
#'
#' Describes the EEG cohort: who's in it and how the AD biomarkers look. The
#' FOOOF aperiodic exponent (the E/I marker) isn't in this table yet -- it lives
#' in the separate EEG pipeline; once merged, exponent-vs-biomarker scatters are
#' the natural next figures.
#'
#' @param ADRC_eeg EEG cohort table from [build_adrc()].
#' @param out_dir Folder to write PNGs and CSVs into (created if needed).
#' @return Invisibly, the path to `out_dir`.
#' @export
make_cohort_plots <- function(ADRC_eeg, out_dir = "outputs") {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  theme_set(theme_minimal(base_size = 13))

  bm_labels <- biomarker_labels()
  grp_pal   <- group_palette()

  adrc_base <- build_adrc_base(ADRC_eeg)
  avail     <- build_availability(adrc_base, ADRC_eeg, bm_labels)
  z_long    <- build_z_long(ADRC_eeg, adrc_base, bm_labels)

  plot_demographics(adrc_base, avail, grp_pal, out_dir)
  plot_biomarkers(adrc_base, z_long, bm_labels, grp_pal, out_dir)
  plot_cognition(adrc_base, bm_labels, grp_pal, out_dir)
  write_summaries(adrc_base, avail, bm_labels, out_dir)

  n_figs <- length(list.files(out_dir, pattern = "\\.png$"))
  cat("Saved", n_figs, "figures + 2 summary CSVs to:", normalizePath(out_dir), "\n")
  invisible(out_dir)
}
