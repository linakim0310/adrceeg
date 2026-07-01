# ----------------------------------------------------------------------------
# STAGE 2b -- Add PACC cognition (psychometrics.csv).
#
# PACC (Preclinical Alzheimer Cognitive Composite, Donohue et al. 2014): a
# memory + executive + global-cognition score where LOWER = worse cognition.
# The value lives in column "K1" and is ALREADY a z-score, so it is used as-is.
# Most people have several sessions, so by default we date-match each imaging
# visit to the nearest cognitive session (mirrors the CDR join).
# ----------------------------------------------------------------------------

#' Attach PACC cognition to the imaging visits
#'
#' @param ADRC Working data.frame (must carry ID and mri_date).
#' @param data_dir Folder containing the ADRC CSV exports.
#' @param match "date" rolls each MRI to the nearest cognitive session within
#'   `window_years`; "mean" collapses each person to their mean K1.
#' @param window_years In "date" mode, drop matches farther apart than this.
#' @return ADRC with a PACC column (unchanged if psychometrics.csv is absent).
#' @keywords internal
add_pacc <- function(ADRC, data_dir, match = c("date", "mean"), window_years = 2) {
  match <- match.arg(match)
  path  <- adrc_path(data_dir, "psychometrics.csv", required = FALSE)
  if (!file.exists(path)) {
    cat("NOTE: psychometrics.csv not found -- skipping PACC merge.\n")
    return(ADRC)
  }

  psy_raw <- read.csv(path, stringsAsFactors = FALSE)
  if (!"K1" %in% names(psy_raw)) stop("psychometrics.csv has no column named 'K1'.")
  if (!"ID" %in% names(psy_raw)) stop("psychometrics.csv has no column named 'ID'.")

  k1_chr <- trimws(as.character(psy_raw$K1))
  k1_num <- suppressWarnings(as.numeric(k1_chr))      # non-numeric junk -> NA
  bad_k1 <- !(is.na(k1_chr) | k1_chr == "" | toupper(k1_chr) == "NA") & is.na(k1_num)
  if (any(bad_k1)) {
    cat(sprintf("  WARNING  psychometrics.csv [K1]: %d non-empty values didn't parse as numbers (e.g. %s) -> set to NA.\n",
                sum(bad_k1), paste(head(unique(k1_chr[bad_k1]), 3), collapse = ", ")))
  }

  if (match == "date" && "psy_date" %in% names(psy_raw)) {
    pacc_sess <- data.frame(ID = norm_id(psy_raw$ID),
                            psy_date = to_date(psy_raw$psy_date), PACC = k1_num)
    n_score   <- sum(!is.na(pacc_sess$PACC))
    pacc_sess <- pacc_sess %>% filter(!is.na(PACC), !is.na(psy_date))
    cat(sprintf("PACC: %d sessions have a score; %d of those also have a usable psy_date (date-matching on those).\n",
                n_score, nrow(pacc_sess)))

    pacc_dt <- data.table(pacc_sess, dates = pacc_sess$psy_date, key = c("ID", "dates"))
    adrc_dt <- data.table(ADRC,      dates = ADRC$mri_date,      key = c("ID", "dates"))
    matched <- pacc_dt[adrc_dt, roll = "nearest"]
    matched[, pacc_gap_yrs := abs(as.numeric(difftime(dates, psy_date, units = "days"))) / 365]
    matched[is.na(pacc_gap_yrs) | pacc_gap_yrs > window_years, PACC := NA]
    cat(sprintf("PACC: %d imaging visits got a PACC within %g years.\n",
                sum(!is.na(matched$PACC)), window_years))
    return(as.data.frame(matched) %>% select(-psy_date, -dates, -pacc_gap_yrs))
  }

  # ---- MEAN fallback (merge by ID) -----------------------------------------
  if (match == "date")
    cat("NOTE: match='date' but no 'psy_date' column found -- using per-person mean instead.\n")
  ADRC_pacc <- data.frame(ID = norm_id(psy_raw$ID), PACC = k1_num) %>%
    filter(!is.na(PACC)) %>%
    group_by(ID) %>%
    summarise(PACC = mean(PACC), n_pacc = n(), .groups = "drop")
  cat(sprintf("PACC: %d people have a score; %d had >1 session (values averaged).\n",
              nrow(ADRC_pacc), sum(ADRC_pacc$n_pacc > 1)))
  merge(ADRC, ADRC_pacc %>% select(ID, PACC), by = "ID", all.x = TRUE)
}
