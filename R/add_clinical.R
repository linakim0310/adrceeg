# ----------------------------------------------------------------------------
# STAGE 1b -- Fold demographics, APOE genetics and CDR clinical scores onto the
# imaging visits, then date-match the nearest CDR to each MRI. Logic unchanged.
# ----------------------------------------------------------------------------

#' Add demographics (birth date, education, sex)
#' @param imaging Imaging-visit data.frame from [merge_imaging()].
#' @param data_dir Folder containing the ADRC CSV exports.
#' @keywords internal
add_demographics <- function(imaging, data_dir) {
  read.csv(adrc_path(data_dir, "demographics.csv")) %>%
    drop_na(BIRTH, EDUC, sex) %>%
    mutate(BIRTH = to_date(BIRTH), SEX = sex) %>%
    select(ID, BIRTH, EDUC, SEX) %>%
    merge(imaging, by = c("ID"))
}

#' Add APOE genotype and its recoded classifications
#' @param df Working data.frame.
#' @param data_dir Folder containing the ADRC CSV exports.
#' @keywords internal
add_apoe <- function(df, data_dir) {
  read.csv(adrc_path(data_dir, "apoe.csv")) %>%
    drop_na(apoe) %>%
    mutate(ID = id,
           apoe_bin = str_replace_all(apoe, c("33" = "0", "34" = "1", "23" = "0", "24" = "1", "44" = "1", "22" = "0")),
           apoe_3   = str_replace_all(apoe, c("33" = "0", "34" = "1", "23" = "0", "24" = "1", "44" = "2", "22" = "0")),
           apoe_fin = str_replace_all(apoe, c("33" = "3", "34" = "4", "23" = "2", "24" = "4", "44" = "4", "22" = "2"))) %>%
    select(ID, apoe, apoe_bin, apoe_3, apoe_fin) %>%
    merge(df, by = "ID")
}

#' Roll the nearest CDR visit onto each imaging visit (within 2 years)
#' @param df Working data.frame (imaging + demographics + APOE).
#' @param data_dir Folder containing the ADRC CSV exports.
#' @keywords internal
match_cdr <- function(df, data_dir) {
  ADRC_clin <- read.csv(adrc_path(data_dir, "b4_cdr.csv")) %>%
    drop_na(cdr) %>%
    mutate(cdr_date = to_date(TESTDATE),
           cdr_bin = case_when(cdr == 0 ~ 0, cdr != 0 ~ 1), cdrglob = cdr) %>%
    select(ID, cdr_date, cdrglob, cdr_bin)

  # match the closest CDR values to the imaging values using exact dates
  ADRC_clin    <- data.table(ADRC_clin, dates = ADRC_clin$cdr_date, key = c("ID", "dates"))
  ADRC_imaging <- data.table(df,        dates = df$mri_date,        key = c("ID", "dates"))
  ADRC <- ADRC_clin[ADRC_imaging, roll = "nearest"]

  # restrict to imaging visits within two years of a clinical visit
  ADRC %>%
    mutate(datediff = as.numeric(round(abs(difftime(mri_date, cdr_date, units = "days")) / 365))) %>%
    filter(datediff <= 2) %>%
    group_by(ID, mri_date) %>%
    slice_min(datediff) %>%
    distinct() %>%
    ungroup()
}

#' Fill demographic gaps, number the visits, compute visit age, trim columns
#'
#' NOTE: mri_date is KEPT (Nicole's original dropped it) because the date-
#' matched PACC join in STAGE 2 needs each imaging visit's date.
#' @param df Working data.frame from [match_cdr()].
#' @keywords internal
fill_and_finalize <- function(df) {
  df %>%
    group_by(ID) %>%
    arrange(mri_date) %>%
    fill(apoe, .direction = "downup") %>%
    fill(apoe_bin, .direction = "downup") %>%
    fill(apoe_3, .direction = "downup") %>%
    fill(BIRTH, .direction = "downup") %>%
    fill(EDUC, .direction = "downup") %>%
    fill(SEX, .direction = "downup") %>%
    mutate(count = "1", visit = cumsum(count),
           visitage = as.numeric(round(mri_date - BIRTH) / 365),
           tau = Tauopathy) %>%
    ungroup() %>%
    select(ID, mri_date, visit, EDUC, SEX, visitage, cdrglob, cdr_bin,
           apoe, apoe_bin, apoe_3, apoe_fin, cort_sig, pib, hippvol, FDG, tau)
}
