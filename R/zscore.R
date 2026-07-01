# ----------------------------------------------------------------------------
# STAGE 1c -- Data clean: factorise key variables, z-score the biomarkers
# against the unimpaired controls, trim extreme outliers, number visits.
#
# z-scoring and trimming are now GENERAL: they loop over params$biomarkers, so
# they work for any number of markers. Add or remove a biomarker in params.R
# and these functions follow automatically -- no need to edit the code here.
# The maths (control mean/SD, +/- N SD trim) is identical to the original.
# ----------------------------------------------------------------------------

#' Factorise ID, sex and the APOE classifications
#' @param ADRC Working data.frame.
#' @keywords internal
factorise_vars <- function(ADRC) {
  ADRC %>%
    mutate(ID = as.factor(ID), apoe_bin = as.factor(apoe_bin), SEX = as.factor(SEX),
           apoe_e4_count = as.factor(apoe_e4_count), apoe_fin = as.factor(apoe_fin))
}

#' z-score each biomarker relative to the unimpaired (CDR 0) controls
#'
#' For every biomarker in params$biomarkers, the mean and SD are taken from the
#' controls' earliest visit and applied to the whole table, creating the marker's
#' `z` column. Generalised from the original (which hard-coded five markers).
#' @param ADRC Working data.frame.
#' @param params Config list (default `adrc_params`).
#' @keywords internal
zscore_biomarkers <- function(ADRC, params = adrc_params) {
  controls <- ADRC %>%
    filter(cdr_bin == 0) %>%
    group_by(ID) %>%
    slice_min(visit) %>%
    ungroup()

  for (bm in params$biomarkers) {
    src <- bm$source; zn <- bm$z
    mu  <- mean(controls[[src]], na.rm = TRUE)
    sig <- sd(controls[[src]],   na.rm = TRUE)
    ADRC[[zn]] <- (ADRC[[src]] - mu) / sig
  }
  ADRC
}

#' Trim biomarker outliers beyond +/- N SD of the z-scored distribution
#'
#' N is params$thresholds$trim_sd (default 5). Any z beyond mean +/- N*SD is set
#' to NA. Loops over params$biomarkers, so it is not tied to a fixed marker set.
#' @param ADRC Working data.frame with the z_* columns.
#' @param params Config list (default `adrc_params`).
#' @keywords internal
trim_outliers <- function(ADRC, params = adrc_params) {
  n_sd <- params$thresholds$trim_sd
  for (bm in params$biomarkers) {
    zn  <- bm$z
    v   <- ADRC[[zn]]
    mu  <- mean(v, na.rm = TRUE)
    sig <- sd(v,   na.rm = TRUE)
    lo  <- mu - n_sd * sig
    hi  <- mu + n_sd * sig
    v[!is.na(v) & (v < lo | v > hi)] <- NA   # blank the extremes, keep the rest
    ADRC[[zn]] <- v
  }
  ADRC
}

#' Add per-person visit number and max-visit count
#' @param ADRC Working data.frame.
#' @keywords internal
add_visit_counts <- function(ADRC) {
  ADRC %>%
    group_by(ID) %>%
    mutate(visit_num = as.numeric(visit), max_visit = max(visit_num)) %>%
    ungroup()
}
