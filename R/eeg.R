# ----------------------------------------------------------------------------
# STAGE 2 -- Fold in brain-age-gap (ADRC_BAG) and filter to the participants
# who had a sleep EEG.
#
# File/column names come from params. A missing OPTIONAL brain-age-gap file is
# now logged loudly via adrc_log() (console warning + run log) instead of a
# silent skip. Filtering logic is unchanged from the original STAGE 2.
# ----------------------------------------------------------------------------

#' Load the sleep-EEG participant list
#'
#' The participant ID lives in the column named params$cols$sleep$id
#' (default "mapid"), normalised to match the ADRC ID.
#' @param data_dir Folder containing the ADRC CSV exports.
#' @param params Config list (default `adrc_params`).
#' @keywords internal
load_sleep_ids <- function(data_dir, params = adrc_params) {
  cfg <- params$cols$sleep
  raw <- read_adrc(data_dir, params$files$sleep)
  data.frame(ID = norm_id(raw[[cfg$id]]), stringsAsFactors = FALSE) %>%
    distinct(ID)                      # one entry per participant
}

#' Merge brain-age-gap data when present
#'
#' The BAG file's ID column is params$cols$bag$id (default "MAPID"). Merged by
#' ID alone; BAG values may repeat across a person's visits. If the OPTIONAL
#' file is absent the pipeline keeps running and logs it loudly.
#' @param ADRC Working data.frame.
#' @param data_dir Folder containing the ADRC CSV exports.
#' @param params Config list (default `adrc_params`).
#' @keywords internal
add_bag <- function(ADRC, data_dir, params = adrc_params) {
  cfg  <- params$cols$bag
  ADRC <- ADRC %>% mutate(ID = norm_id(ID))   # ID is a factor from the clean step
  path <- adrc_path(data_dir, params$files$bag, required = FALSE)
  if (file.exists(path)) {
    bag <- read.csv(path, stringsAsFactors = FALSE)
    bag$ID <- norm_id(bag[[cfg$id]])
    if (!identical(cfg$id, "ID")) bag[[cfg$id]] <- NULL   # drop the raw id column
    return(merge(ADRC, bag, by = "ID", all.x = TRUE))
  }
  adrc_log(sprintf("Brain-age-gap skipped: file '%s' not found in the data folder.",
                   params$files$bag), params)
  ADRC
}

#' Filter the combined table to EEG participants and report the match rate
#'
#' Keeps ALL imaging visits for anyone who had an EEG. A pile of "unmatched"
#' IDs almost always means an ID FORMAT mismatch, not truly missing people.
#' @param ADRC Combined ADRC table (ID already normalised to character).
#' @param sleep_ids Output of [load_sleep_ids()].
#' @param params Config list (default `adrc_params`).
#' @return The EEG-only subset (data.frame).
#' @keywords internal
filter_to_eeg <- function(ADRC, sleep_ids, params = adrc_params) {
  ADRC_eeg <- ADRC %>% filter(ID %in% sleep_ids$ID)

  n_listed  <- nrow(sleep_ids)
  n_matched <- ADRC_eeg %>% distinct(ID) %>% nrow()
  cat("EEG participants listed in the sleep-ID file:", n_listed, "\n")
  cat("...of those, found in the ADRC table:        ", n_matched, "\n")
  cat("...NOT found (check ID formatting!):          ", n_listed - n_matched, "\n")
  cat("Rows kept in ADRC_eeg (imaging visits):       ", nrow(ADRC_eeg), "\n")

  unmatched <- setdiff(sleep_ids$ID, unique(as.character(ADRC$ID)))
  if (length(unmatched) > 0) {
    adrc_log(sprintf("%d EEG IDs had no match in the ADRC table (likely ID formatting).",
                     length(unmatched)), params)
    cat("\nEEG IDs with no match in ADRC:\n")
    print(unmatched)
  }
  ADRC_eeg
}
