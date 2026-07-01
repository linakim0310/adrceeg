# ----------------------------------------------------------------------------
# STAGE 3 orchestrator -- build the shared "one row per person" frames once,
# then hand them to each figure/summary writer. Styling and output options
# (images, PowerPoint deck) come from params$plots.
# ----------------------------------------------------------------------------

#' Write the full set of cohort figures and summary tables
#'
#' Describes the EEG cohort: who's in it and how the AD biomarkers look. The
#' FOOOF aperiodic exponent (the E/I marker) isn't in this table yet -- it lives
#' in the separate EEG pipeline; once merged, exponent-vs-biomarker scatters are
#' the natural next figures.
#'
#' Each figure is written as an image and (optionally) added as a slide to one
#' editable PowerPoint deck via the \pkg{export} package.
#'
#' @param ADRC_eeg EEG cohort table from [build_adrc()].
#' @param out_dir Folder to write figures and CSVs into (created if needed).
#' @param params Config list (default `adrc_params`).
#' @return Invisibly, the path to `out_dir`.
#' @export
make_cohort_plots <- function(ADRC_eeg, out_dir = "outputs", params = adrc_params) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  theme_set(plot_theme(params))

  # PPTX deck: check the export package once, and start a fresh deck.
  if (isTRUE(params$plots$save_pptx) && !requireNamespace("export", quietly = TRUE)) {
    adrc_log(paste("Package 'export' is not installed -- the PowerPoint deck of figures",
                   "will be skipped (image files are still written). Install it with",
                   "install.packages('export')."), params)
    params$plots$save_pptx <- FALSE
  }
  if (isTRUE(params$plots$save_pptx)) {
    pptx_path <- file.path(out_dir, params$plots$pptx_file)
    if (file.exists(pptx_path)) try(file.remove(pptx_path), silent = TRUE)
  }

  bm_labels <- biomarker_labels(params)
  grp_pal   <- group_palette(params)

  adrc_base <- build_adrc_base(ADRC_eeg, params)
  avail     <- build_availability(adrc_base, ADRC_eeg, bm_labels)
  z_long    <- build_z_long(ADRC_eeg, adrc_base, bm_labels)

  plot_demographics(adrc_base, avail, grp_pal, out_dir, params)
  plot_biomarkers(adrc_base, z_long, bm_labels, grp_pal, out_dir, params)
  plot_cognition(adrc_base, bm_labels, grp_pal, out_dir, params)
  write_summaries(adrc_base, avail, bm_labels, out_dir)

  n_figs <- length(list.files(out_dir, pattern = paste0("\\.", params$plots$image_format, "$")))
  cat("Saved", n_figs, "figures + 2 summary CSVs to:", normalizePath(out_dir), "\n")
  if (isTRUE(params$plots$save_pptx))
    cat("Editable figure deck:", file.path(normalizePath(out_dir), params$plots$pptx_file), "\n")
  invisible(out_dir)
}
