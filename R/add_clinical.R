# ----------------------------------------------------------------------------
# STAGE 1b -- Fold demographics, APOE genetics and CDR clinical scores onto the
# imaging visits, then date-match the nearest CDR to each MRI.
#
# As in the loaders, source column names come from params$cols and are renamed
# to stable internal names. The CDR matching window is params$thresholds$
# cdr_window_years. The APOE recoding maps live in params$apoe (with comments
# explaining what each classification means).
# ----------------------------------------------------------------------------

#' Add demographics (birth date, education, sex)
#' @param imaging Imaging-visit data.frame from [merge_imaging()].
#' @param data_dir Folder containing the ADRC CSV exports.
#' @param params Config list (default `adrc_params`).
#' @keywords internal
add_demographics <- function(imaging, data_dir, params = adrc_params) {
  cfg <- params$cols$demographics
  raw <- read_adrc(data_dir, params$files$demographics)
  raw <- raw[!(is_blank(raw[[cfg$birth]]) | is_blank(raw[[cfg$educ]]) |
                 is_blank(raw[[cfg$sex]])), , drop = FALSE]
  demo <- data.frame(
    ID    = norm_id(raw[[cfg$id]]),
    BIRTH = to_date(raw[[cfg$birth]]),
    EDUC  = as.numeric(raw[[cfg$educ]]),
    SEX   = as.character(raw[[cfg$sex]]),
    stringsAsFactors = FALSE
  )
  merge(demo, imaging, by = "ID")
}

#' Add APOE genotype and its recoded classifications
#'
#' The three recoded variables (apoe_bin, apoe_e4_count, apoe_fin) are produced by
#' str_replace_all() using the maps in params$apoe. See params.R for a full
#' explanation of what each one means (e4 carrier flag, e4 dosage, 3-level risk).
#' @param df Working data.frame.
#' @param data_dir Folder containing the ADRC CSV exports.
#' @param params Config list (default `adrc_params`).
#' @keywords internal
add_apoe <- function(df, data_dir, params = adrc_params) {
  cfg <- params$cols$apoe
  m   <- params$apoe
  raw <- read_adrc(data_dir, params$files$apoe)
  raw <- raw[!is_blank(raw[[cfg$genotype]]), , drop = FALSE]     # drop_na(apoe)
  apoe_chr <- as.character(raw[[cfg$genotype]])
  out <- data.frame(
    ID       = norm_id(raw[[cfg$id]]),
    apoe     = apoe_chr,
    apoe_bin = str_replace_all(apoe_chr, m$carrier_binary),   # e4 carrier: 0/1
    apoe_e4_count   = str_replace_all(apoe_chr, m$e4_allele_count),  # e4 dosage: 0/1/2
    apoe_fin = str_replace_all(apoe_chr, m$dominant_allele),  # 3-level risk
    stringsAsFactors = FALSE
  )
  merge(out, df, by = "ID")
}

#' Roll the nearest CDR visit onto each imaging visit
#'
#' Scans and clinical visits aren't same-day, so each MRI is matched to the
#' nearest CDR, then kept only if that CDR is within
#' params$thresholds$cdr_window_years of the scan.
#' @param df Working data.frame (imaging + demographics + APOE).
#' @param data_dir Folder containing the ADRC CSV exports.
#' @param params Config list (default `adrc_params`).
#' @keywords internal
match_cdr <- function(df, data_dir, params = adrc_params) {
  cfg <- params$cols$cdr
  win <- params$thresholds$cdr_window_years
  raw <- read_adrc(data_dir, params$files$cdr)
  raw <- raw[!is_blank(raw[[cfg$cdr]]), , drop = FALSE]         # drop_na(cdr)

  ADRC_clin <- data.frame(
    ID       = norm_id(raw[[cfg$id]]),
    cdr_date = to_date(raw[[cfg$testdate]]),
    cdrglob  = as.numeric(raw[[cfg$cdr]]),
    stringsAsFactors = FALSE
  )
  ADRC_clin$cdr_bin <- ifelse(ADRC_clin$cdrglob == 0, 0, 1)

  # match the closest CDR to each imaging visit using exact dates
  ADRC_clin    <- data.table(ADRC_clin, dates = ADRC_clin$cdr_date, key = c("ID", "dates"))
  ADRC_imaging <- data.table(df,        dates = df$mri_date,        key = c("ID", "dates"))
  ADRC <- ADRC_clin[ADRC_imaging, roll = "nearest"]

  # restrict to imaging visits within the CDR window of a clinical visit
  ADRC %>%
    mutate(datediff = as.numeric(round(abs(difftime(mri_date, cdr_date, units = "days")) / 365))) %>%
    filter(datediff <= win) %>%
    group_by(ID, mri_date) %>%
    slice_min(datediff) %>%
    distinct() %>%
    ungroup()
}

#' Fill demographic gaps, number the visits, compute visit age, trim columns
#'
#' NOTE: mri_date is KEPT (Nicole's original dropped it) because the date-
#' matched PACC join in STAGE 2 needs each imaging visit's date.
#' Logic is unchanged from the original.
#' @param df Working data.frame from [match_cdr()].
#' @keywords internal
fill_and_finalize <- function(df) {
  df %>%
    group_by(ID) %>%
    arrange(mri_date) %>%
    fill(apoe, .direction = "downup") %>%
    fill(apoe_bin, .direction = "downup") %>%
    fill(apoe_e4_count, .direction = "downup") %>%
    fill(BIRTH, .direction = "downup") %>%
    fill(EDUC, .direction = "downup") %>%
    fill(SEX, .direction = "downup") %>%
    mutate(count = 1, visit = cumsum(count),   # count is numeric 1 so cumsum gives 1,2,3... visits per person
           visitage = as.numeric(round(mri_date - BIRTH) / 365),
           tau = Tauopathy) %>%
    ungroup() %>%
    select(ID, mri_date, visit, EDUC, SEX, visitage, cdrglob, cdr_bin,
           apoe, apoe_bin, apoe_e4_count, apoe_fin, cort_sig, pib, hippvol, FDG, tau)
}
