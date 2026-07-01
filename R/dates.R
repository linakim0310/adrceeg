# ----------------------------------------------------------------------------
# Robust date parsing for the ADRC CSVs.
#
# Nicole's pipeline needs US-style M/D/YYYY dates (e.g. "3/14/2012"). If a
# file's dates are in any other format, a plain as.Date() silently turns them
# into NA, which then breaks the date-based merges and makes ages and CDR
# matching wrong. `to_date()` accepts canonical M/D/YYYY, ISO (YYYY-MM-DD), raw
# Excel serial numbers, AND 2-digit-year M/D/YY. `check_dates()` reports
# anything that STILL didn't parse, so you know to apply the Excel fix.
# ----------------------------------------------------------------------------

#' Parse mixed ADRC date formats into Date
#'
#' 2-digit years: the century is resolved by assuming the date is in the PAST
#' (20YY if that year is <= the current year, otherwise 19YY). Safe for this
#' cohort because patients are old (no birth year in the 2000s) and visit/scan
#' dates are never in the future. e.g. "10/17/39" -> 1939; "2/13/05" -> 2005.
#'
#' @param x A character/numeric vector of dates in any supported format.
#' @return A `Date` vector, NA where parsing failed.
#' @keywords internal
to_date <- function(x) {
  x     <- trimws(as.character(x))
  out   <- rep(as.Date(NA), length(x))
  blank <- is.na(x) | x == "" | toupper(x) == "NA"

  serial <- !blank & grepl("^[0-9]+$", x)                        # Excel serial number
  out[serial] <- as.Date(as.numeric(x[serial]), origin = "1899-12-30")

  iso <- !blank & grepl("^[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}$", x)   # ISO YYYY-MM-DD
  out[iso] <- as.Date(x[iso], format = "%Y-%m-%d")

  us4 <- !blank & grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}$", x)   # M/D/YYYY (4-digit year)
  out[us4] <- as.Date(x[us4], format = "%m/%d/%Y")

  us2 <- !blank & grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{2}$", x)   # M/D/YY (2-digit year)
  if (any(us2)) {
    d  <- as.Date(x[us2], format = "%m/%d/%y")   # R's default split; we re-pin below
    yr <- as.numeric(format(d, "%Y"))
    future <- !is.na(yr) & yr > as.numeric(format(Sys.Date(), "%Y"))
    if (any(future)) {                           # 20YY in the future -> use 19YY
      d[future] <- as.Date(paste0(yr[future] - 100, format(d[future], "-%m-%d")))
    }
    out[us2] <- d
  }

  out
}

#' Warn (loudly) about un-parseable dates in a CSV before loading
#'
#' @param path Path to a CSV file.
#' @param date_cols Character vector of column names to validate.
#' @return Invisibly NULL; prints a WARNING per problem column.
#' @keywords internal
check_dates <- function(path, date_cols) {
  if (!file.exists(path)) return(invisible())
  raw <- read.csv(path, stringsAsFactors = FALSE)
  for (col in date_cols) {
    if (!col %in% names(raw)) next
    vals     <- trimws(as.character(raw[[col]]))
    nonblank <- !(is.na(vals) | vals == "" | toupper(vals) == "NA")
    bad      <- nonblank & is.na(to_date(vals))
    if (any(bad)) {
      cat(sprintf("  WARNING  %s [%s]: %d of %d dates failed to parse (e.g. %s)\n",
                  basename(path), col, sum(bad), sum(nonblank),
                  paste(head(unique(vals[bad]), 3), collapse = ", ")))
      cat("           Fix: open this CSV, format those date cells as 3/14/2012 (M/D/YYYY), re-save.\n")
    }
  }
  invisible()
}

#' Run the pre-load date sanity check across all ADRC files
#'
#' File names and the date columns to validate are read from params, so this
#' stays correct if a file or column gets renamed in params.R.
#' @param data_dir Folder containing the ADRC CSV exports.
#' @param params Config list (default `adrc_params`).
#' @return Invisibly NULL.
#' @keywords internal
check_all_dates <- function(data_dir, params = adrc_params) {
  cat("Checking date formats in the CSV files...\n")
  f <- params$files; co <- params$cols
  check_dates(file.path(data_dir, f$pib),          c(co$pib$pet_date, co$pib$mri_date))
  check_dates(file.path(data_dir, f$fdg),          c(co$fdg$pet_date, co$fdg$mri_date))
  check_dates(file.path(data_dir, f$tau),          c(co$tau$pet_date, co$tau$mri_date))
  check_dates(file.path(data_dir, f$mri),          c(co$mri$mr_date))
  check_dates(file.path(data_dir, f$demographics), c(co$demographics$birth))
  check_dates(file.path(data_dir, f$cdr),          c(co$cdr$testdate))
  check_dates(file.path(data_dir, f$pacc),         c(co$pacc$date))
  cat("Date check done.\n\n")
  invisible()
}
