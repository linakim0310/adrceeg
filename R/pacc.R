# ----------------------------------------------------------------------------
# STAGE 2b -- Add PACC cognition (psychometrics file).
#
# PACC (Preclinical Alzheimer Cognitive Composite, Donohue et al. 2014): a
# memory + executive + global-cognition score where LOWER = worse cognition.
# The value lives in the column named params$cols$pacc$score (default "K1") and
# is ALREADY a z-score, so it is used as-is. Each imaging visit is date-matched
# to the nearest cognitive session within params$thresholds$pacc_window_years.
#
# The old per-person "mean" fallback has been removed (per code review): PACC is
# always date-matched. File/column names come from params; a missing optional
# file is logged loudly via adrc_log() rather than skipped silently.
# ----------------------------------------------------------------------------

#' Attach PACC cognition to the imaging visits (date-matched)
#'
#' @param ADRC Working data.frame (must carry ID and mri_date).
#' @param data_dir Folder containing the ADRC CSV exports.
#' @param params Config list (default `adrc_params`).
#' @param window_years Drop matches farther apart than this (default from params).
#' @return ADRC with a PACC column (unchanged if the file/columns are absent).
#' @keywords internal
add_pacc <- function(ADRC, data_dir, params = adrc_params,
                     window_years = params$thresholds$pacc_window_years) {
  cfg  <- params$cols$pacc
  path <- adrc_path(data_dir, params$files$pacc, required = FALSE)

  if (!file.exists(path)) {
    adrc_log(sprintf("PACC skipped: file '%s' not found in the data folder.",
                     params$files$pacc), params)
    return(ADRC)
  }

  psy_raw <- read.csv(path, stringsAsFactors = FALSE)
  if (!cfg$score %in% names(psy_raw))
    stop(sprintf("psychometrics file has no column named '%s'.", cfg$score))
  if (!cfg$id %in% names(psy_raw))
    stop(sprintf("psychometrics file has no column named '%s'.", cfg$id))
  if (!cfg$date %in% names(psy_raw)) {
    adrc_log(sprintf("PACC skipped: no date column '%s' in the psychometrics file (date matching requires it).",
                     cfg$date), params)
    return(ADRC)
  }

  k1_chr <- trimws(as.character(psy_raw[[cfg$score]]))
  k1_num <- suppressWarnings(as.numeric(k1_chr))      # non-numeric junk -> NA
  bad_k1 <- !(is.na(k1_chr) | k1_chr == "" | toupper(k1_chr) == "NA") & is.na(k1_num)
  if (any(bad_k1)) {
    adrc_log(sprintf("psychometrics [%s]: %d non-empty values didn't parse as numbers (e.g. %s) -> set to NA.",
                     cfg$score, sum(bad_k1),
                     paste(head(unique(k1_chr[bad_k1]), 3), collapse = ", ")), params)
  }

  pacc_sess <- data.frame(ID = norm_id(psy_raw[[cfg$id]]),
                          psy_date = to_date(psy_raw[[cfg$date]]),
                          PACC = k1_num, stringsAsFactors = FALSE)
  n_score   <- sum(!is.na(pacc_sess$PACC))
  pacc_sess <- pacc_sess %>% filter(!is.na(PACC), !is.na(psy_date))
  cat(sprintf("PACC: %d sessions have a score; %d of those also have a usable date (date-matching on those).\n",
              n_score, nrow(pacc_sess)))

  pacc_dt <- data.table(pacc_sess, dates = pacc_sess$psy_date, key = c("ID", "dates"))
  adrc_dt <- data.table(ADRC,      dates = ADRC$mri_date,      key = c("ID", "dates"))
  matched <- pacc_dt[adrc_dt, roll = "nearest"]
  matched[, pacc_gap_yrs := abs(as.numeric(difftime(dates, psy_date, units = "days"))) / 365]
  matched[is.na(pacc_gap_yrs) | pacc_gap_yrs > window_years, PACC := NA]
  cat(sprintf("PACC: %d imaging visits got a PACC within %g years.\n",
              sum(!is.na(matched$PACC)), window_years))
  as.data.frame(matched) %>% select(-psy_date, -dates, -pacc_gap_yrs)
}
