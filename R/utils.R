# ----------------------------------------------------------------------------
# Small shared helpers used across the pipeline.
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

#' Build the full path to an ADRC CSV and check it exists
#'
#' Keeps every read.csv() call relative to the user-supplied data folder
#' instead of a hard-coded machine path.
#'
#' @param data_dir Folder that directly contains the ADRC CSV exports.
#' @param file File name, e.g. "pib.csv".
#' @param required If TRUE, stop with a clear message when the file is absent.
#' @return The full path (invisibly NULL-friendly when not required).
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
