# ----------------------------------------------------------------------------
# STAGE 3 helpers -- shared constants and the "one row per person" data frames
# used by every cohort figure. Because the data is longitudinal, summaries use
# one row per person so nobody is counted twice.
# ----------------------------------------------------------------------------

#' Colour palette for the two impairment groups
#' @keywords internal
group_palette <- function() {
  c("Unimpaired (CDR 0)" = "#4C72B0", "Impaired (CDR > 0)" = "#C44E52")
}

#' Friendly names for the five z-scored biomarkers
#' @keywords internal
biomarker_labels <- function() {
  c(z_pib = "Amyloid (PiB)", z_tau = "Tau", z_FDG = "FDG metabolism",
    z_cort_sig = "Cortical thickness", z_hippvol = "Hippocampal volume")
}

#' One row per person at their BASELINE (earliest) visit, with a group label
#' @param ADRC_eeg EEG cohort table from [build_adrc()].
#' @keywords internal
build_adrc_base <- function(ADRC_eeg) {
  ADRC_eeg %>%
    group_by(ID) %>%
    slice_min(visit_num, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(group = factor(cdr_bin, levels = c(0, 1),
                          labels = c("Unimpaired (CDR 0)", "Impaired (CDR > 0)")))
}

#' Data-availability table: % non-missing, baseline-only vs all visits
#' @param adrc_base Baseline rows from [build_adrc_base()].
#' @param ADRC_eeg Full EEG cohort table.
#' @param bm_labels Named biomarker label vector.
#' @keywords internal
build_availability <- function(adrc_base, ADRC_eeg, bm_labels) {
  vars_check <- c("visitage", "EDUC", "cdrglob", "apoe",
                  "cort_sig", "pib", "hippvol", "FDG", "tau", names(bm_labels))
  avail_fun <- function(df, label) {
    df %>%
      summarise(across(all_of(vars_check), ~ 100 * mean(!is.na(.x)))) %>%
      pivot_longer(everything(), names_to = "variable", values_to = "pct_present") %>%
      mutate(set = label)
  }
  bind_rows(avail_fun(adrc_base, "Baseline only"),
            avail_fun(ADRC_eeg, "All visits"))
}

#' Long biomarker table read from each marker's FIRST-AVAILABLE visit
#'
#' Some biomarkers (FDG-PET especially) aren't collected at baseline, so reading
#' baseline-only would leave them looking empty; first-available surfaces them.
#' @param ADRC_eeg Full EEG cohort table.
#' @param adrc_base Baseline rows (supplies each person's group label).
#' @param bm_labels Named biomarker label vector.
#' @keywords internal
build_z_long <- function(ADRC_eeg, adrc_base, bm_labels) {
  group_lookup <- adrc_base %>% select(ID, group)
  map_dfr(names(bm_labels), function(v) {
    ADRC_eeg %>%
      filter(!is.na(.data[[v]])) %>%
      group_by(ID) %>%
      slice_min(visit_num, with_ties = FALSE) %>%
      ungroup() %>%
      transmute(ID, visitage, biomarker = v, z = .data[[v]])
  }) %>%
    left_join(group_lookup, by = "ID") %>%
    mutate(biomarker = factor(recode(biomarker, !!!bm_labels), levels = unname(bm_labels)))
}
