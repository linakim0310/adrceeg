# ----------------------------------------------------------------------------
# Small shared helpers used across the pipeline. These carry LOGIC only; the
# names/paths/settings they act on come from params.R (adrc_params).
# ----------------------------------------------------------------------------

#' Normalise an ID column to a clean character string
#'
#' R may read "95136" as a number in one file and as text in another, and a
#' number won't match a string even when they look identical. Forcing every ID
#' to a trimmed character value makes the joins type-safe regardless of source.
#'
#' @param x A vector of IDs (numeric or character).
#' @return A trimmed character vector.
#' @keywords internal
norm_id <- function(x) trimws(as.character(x))

#' TRUE where a value is missing/blank ("" , NA, or the text "NA")
#'
#' Used at the read boundary to drop rows the way the original `drop_na()` did,
#' but by column NAME (from params) rather than a hard-coded symbol.
#' @param x A vector.
#' @keywords internal
is_blank <- function(x) {
  x <- trimws(as.character(x))
  is.na(x) | x == "" | toupper(x) == "NA"
}

#' Build the full path to an ADRC CSV and check it exists
#'
#' Keeps every read.csv() call relative to the user-supplied data folder
#' instead of a hard-coded machine path.
#'
#' @param data_dir Folder that directly contains the ADRC CSV exports.
#' @param file File name, e.g. "pib.csv".
#' @param required If TRUE, stop with a clear message when the file is absent.
#' @return The full path.
#' @keywords internal
adrc_path <- function(data_dir, file, required = TRUE) {
  path <- file.path(data_dir, file)
  if (!file.exists(path) && required) {
    stop(sprintf(
      "Could not find '%s' in the data folder:\n  %s\nPut the ADRC CSV exports there (or pass data_dir=).",
      file, normalizePath(data_dir, mustWork = FALSE)
    ), call. = FALSE)
  }
  path
}

#' Read one ADRC CSV by its params file-name, as plain character-friendly data
#'
#' Reads with stringsAsFactors = FALSE so every column starts as text; loaders
#' then coerce the specific columns they need. Returns NULL for an absent
#' optional file (required = FALSE).
#' @param data_dir Folder containing the ADRC CSV exports.
#' @param file File name (usually `params$files$...`).
#' @param required Passed to [adrc_path()].
#' @keywords internal
read_adrc <- function(data_dir, file, required = TRUE) {
  path <- adrc_path(data_dir, file, required = required)
  if (!file.exists(path)) return(NULL)
  read.csv(path, stringsAsFactors = FALSE)
}

#' Record a non-fatal problem LOUDLY (console warning + log file)
#'
#' Replaces the old silent `cat("NOTE: ...")` skips. The pipeline keeps running
#' so a missing OPTIONAL file doesn't halt everything, but the problem is
#' impossible to miss: it prints an immediate warning AND appends a timestamped
#' line to the run log (params$logging$file).
#' @param msg The message to record.
#' @param params The params list (default `adrc_params`).
#' @keywords internal
adrc_log <- function(msg, params = adrc_params) {
  warning(msg, call. = FALSE, immediate. = TRUE)          # loud, prints now
  if (isTRUE(params$logging$enabled)) {
    line <- sprintf("[%s] %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), msg)
    try(cat(line, file = params$logging$file, append = TRUE), silent = TRUE)
  }
  invisible()
}

# ---- Biomarker accessors (read the flexible list in params) ----------------

#' z-scored column names for every biomarker in params
#' @keywords internal
bm_z_names <- function(params = adrc_params) {
  vapply(params$biomarkers, function(b) b$z, character(1))
}

#' source (raw) column names for every biomarker in params
#' @keywords internal
bm_sources <- function(params = adrc_params) {
  vapply(params$biomarkers, function(b) b$source, character(1))
}

#' Named vector mapping each z-column -> its pretty label (for plots)
#' @keywords internal
bm_label_vec <- function(params = adrc_params) {
  stats::setNames(vapply(params$biomarkers, function(b) b$label, character(1)),
                  bm_z_names(params))
}
