# ----------------------------------------------------------------------------
# STAGE 1c -- Data clean: factorise key variables, z-score the biomarkers
# against the unimpaired controls, trim extreme outliers, number visits.
# Logic unchanged from the original DATA CLEAN section.
# ----------------------------------------------------------------------------

#' Factorise ID, sex and the APOE classifications
#' @param ADRC Working data.frame.
#' @keywords internal
factorise_vars <- function(ADRC) {
  ADRC %>%
    mutate(ID = as.factor(ID), apoe_bin = as.factor(apoe_bin), SEX = as.factor(SEX),
           apoe_3 = as.factor(apoe_3), apoe_fin = as.factor(apoe_fin))
}

#' z-score each biomarker relative to the unimpaired (CDR 0) controls
#'
#' Means/SDs are taken from each control's earliest visit, then applied to the
#' whole table, exactly as in the original script.
#' @param ADRC Working data.frame.
#' @keywords internal
zscore_biomarkers <- function(ADRC) {
  ADRC_cons <- ADRC %>%
    filter(cdr_bin == 0) %>%
    group_by(ID) %>%
    slice_min(visit)

  m_cortsig <- mean(ADRC_cons$cort_sig, na.rm = TRUE); sd_cortsig <- sd(ADRC_cons$cort_sig, na.rm = TRUE)
  ADRC$z_cort_sig <- (ADRC$cort_sig - m_cortsig) / sd_cortsig
  m_pib <- mean(ADRC_cons$pib, na.rm = TRUE); sd_pib <- sd(ADRC_cons$pib, na.rm = TRUE)
  ADRC$z_pib <- (ADRC$pib - m_pib) / sd_pib
  m_fdg <- mean(ADRC_cons$FDG, na.rm = TRUE); sd_fdg <- sd(ADRC_cons$FDG, na.rm = TRUE)
  ADRC$z_FDG <- (ADRC$FDG - m_fdg) / sd_fdg
  m_tau <- mean(ADRC_cons$tau, na.rm = TRUE); sd_tau <- sd(ADRC_cons$tau, na.rm = TRUE)
  ADRC$z_tau <- (ADRC$tau - m_tau) / sd_tau
  m_hippvol <- mean(ADRC_cons$hippvol, na.rm = TRUE); sd_hippvol <- sd(ADRC_cons$hippvol, na.rm = TRUE)
  ADRC$z_hippvol <- (ADRC$hippvol - m_hippvol) / sd_hippvol
  ADRC
}

#' Trim biomarker outliers beyond +/- 5 SD of the z-scored distribution
#' @param ADRC Working data.frame with z_* columns.
#' @keywords internal
trim_outliers <- function(ADRC) {
  desc <- data.frame(t(describe(ADRC[c("z_pib", "z_hippvol", "z_tau", "z_cort_sig", "z_FDG")])))
  ADRC %>%
    mutate(z_cort_sig = case_when(z_cort_sig <= (desc$z_cort_sig[3] + 5 * desc$z_cort_sig[4]) & z_cort_sig >= (desc$z_cort_sig[3] - 5 * desc$z_cort_sig[4]) ~ z_cort_sig)) %>%
    mutate(z_pib = case_when(z_pib <= (desc$z_pib[3] + 5 * desc$z_pib[4]) & z_pib >= (desc$z_pib[3] - 5 * desc$z_pib[4]) ~ z_pib)) %>%
    mutate(z_FDG = case_when(z_FDG <= (desc$z_FDG[3] + 5 * desc$z_FDG[4]) & z_FDG >= (desc$z_FDG[3] - 5 * desc$z_FDG[4]) ~ z_FDG)) %>%
    mutate(z_hippvol = case_when(z_hippvol <= (desc$z_hippvol[3] + 5 * desc$z_hippvol[4]) & z_hippvol >= (desc$z_hippvol[3] - 5 * desc$z_hippvol[4]) ~ z_hippvol)) %>%
    mutate(z_tau = case_when(z_tau <= (desc$z_tau[3] + 5 * desc$z_tau[4]) & z_tau >= (desc$z_tau[3] - 5 * desc$z_tau[4]) ~ z_tau))
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
