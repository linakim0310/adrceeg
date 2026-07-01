#' adrceeg: build and filter the Knight ADRC sleep-EEG cohort table
#'
#' One exported entry point, [run_pipeline()], reproduces Nicole S. McKay's
#' ADRC data-combining pipeline, folds in brain-age-gap and PACC cognition,
#' filters to sleep-EEG participants, and (optionally) writes cohort figures
#' and summary tables. Every step lives in its own small file under R/ so the
#' code stays readable and portable; nothing is hard-coded to one machine.
#'
#' No participant data ships here. Point the pipeline at a folder of the
#' (HIPAA-protected) ADRC CSV exports and it runs end to end.
#'
#' @keywords internal
#'
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @import purrr
#' @import ggplot2
#' @import data.table
#' @importFrom psych describe
#' @importFrom rlang .data
#' @importFrom stats lm coef sd cor
#' @importFrom utils read.csv write.csv head
"_PACKAGE"

# Re-export the magrittr pipe so %>% works for users of the package too.
#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`
