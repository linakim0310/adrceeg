# ----------------------------------------------------------------------------
# STAGE 2 -- Fold in brain-age-gap (ADRC_BAG) and filter to the participants
# who had a sleep EEG. Logic unchanged from the original STAGE 2.
# ----------------------------------------------------------------------------

#' Load the sleep-EEG participant list
#'
#' sleep_IDs.csv lists every participant who had a sleep EEG. The identifier
#' lives in column "mapid" (lowercase), treated as the same value as "ID".
#' @param data_dir Folder containing the ADRC CSV exports.
#' @keywords internal
load_sleep_ids <- function(data_dir) {
  read.csv(adrc_path(data_dir, "sleep_IDs.csv")) %>%
    mutate(ID = norm_id(mapid)) %>%   # rename + normalise so the join is type-safe
    distinct(ID)                      # one entry per participant
}

#' Merge brain-age-gap data when present (ADRC_BAG.csv uses "MAPID")
#'
#' Merged by ID alone; BAG values may repeat across a person's visits. The
#' file.exists() guard lets the pipeline still run on datasets without BAG.
#' @param ADRC Working data.frame.
#' @param data_dir Folder containing the ADRC CSV exports.
#' @keywords internal
add_bag <- function(ADRC, data_dir) {
  ADRC <- ADRC %>% mutate(ID = norm_id(ID))   # ID is a factor from the clean step
  path <- adrc_path(data_dir, "ADRC_BAG.csv", required = FALSE)
  if (file.exists(path)) {
    ADRC_BAG <- read.csv(path) %>% mutate(ID = norm_id(MAPID))
    return(merge(ADRC, ADRC_BAG, by = "ID", all.x = TRUE))
  }
  cat("NOTE: ADRC_BAG.csv not found -- skipping brain-age-gap merge.\n")
  ADRC
}

#' Filter the combined table to EEG participants and report the match rate
#'
#' Keeps ALL imaging visits for anyone who had an EEG. A pile of "unmatched"
#' IDs almost always means an ID FORMAT mismatch, not truly missing people.
#' @param ADRC Combined ADRC table (ID already normalised to character).
#' @param sleep_ids Output of [load_sleep_ids()].
#' @return The EEG-only subset (data.frame).
#' @keywords internal
filter_to_eeg <- function(ADRC, sleep_ids) {
  ADRC_eeg <- ADRC %>% filter(ID %in% sleep_ids$ID)

  n_listed  <- nrow(sleep_ids)
  n_matched <- ADRC_eeg %>% distinct(ID) %>% nrow()
  cat("EEG participants listed in sleep_IDs.csv:", n_listed, "\n")
  cat("...of those, found in the ADRC table:    ", n_matched, "\n")
  cat("...NOT found (check ID formatting!):      ", n_listed - n_matched, "\n")
  cat("Rows kept in ADRC_eeg (imaging visits):   ", nrow(ADRC_eeg), "\n")

  unmatched <- setdiff(sleep_ids$ID, unique(ADRC$ID))
  if (length(unmatched) > 0) {
    cat("\nEEG IDs with no match in ADRC:\n")
    print(unmatched)
  }
  ADRC_eeg
}
