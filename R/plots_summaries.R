# ----------------------------------------------------------------------------
# STAGE 3 numeric summaries -- the per-group "analytics" table and the wide
# data-availability table. Written as CSVs. Logic unchanged.
# ----------------------------------------------------------------------------

#' Write the per-group summary and the wide availability table
#' @param adrc_base Baseline rows from [build_adrc_base()].
#' @param avail Availability table (long) from [build_availability()].
#' @param bm_labels Named biomarker label vector.
#' @param out_dir Output folder for the CSVs.
#' @keywords internal
write_summaries <- function(adrc_base, avail, bm_labels, out_dir) {
  summary_tbl <- adrc_base %>%
    group_by(group) %>%
    summarise(
      n          = n(),
      age_mean   = mean(visitage, na.rm = TRUE),
      age_sd     = sd(visitage, na.rm = TRUE),
      educ_mean  = mean(EDUC, na.rm = TRUE),
      educ_sd    = sd(EDUC, na.rm = TRUE),
      pct_female = mean(SEX == "F", na.rm = TRUE) * 100,
      pct_apoe4  = mean(apoe_bin == "1", na.rm = TRUE) * 100,
      across(all_of(names(bm_labels)),
             list(mean = ~mean(.x, na.rm = TRUE), sd = ~sd(.x, na.rm = TRUE)),
             .names = "{.col}_{.fn}"),
      .groups = "drop"
    )

  if ("PACC" %in% names(adrc_base)) {
    pacc_summary <- adrc_base %>%
      group_by(group) %>%
      summarise(PACC_mean = mean(PACC, na.rm = TRUE),
                PACC_sd   = sd(PACC, na.rm = TRUE),
                PACC_n    = sum(!is.na(PACC)), .groups = "drop")
    summary_tbl <- left_join(summary_tbl, pacc_summary, by = "group")
  }
  write.csv(summary_tbl, file.path(out_dir, "cohort_summary.csv"), row.names = FALSE)

  avail_wide <- avail %>% pivot_wider(names_from = set, values_from = pct_present)
  write.csv(avail_wide, file.path(out_dir, "data_availability.csv"), row.names = FALSE)
  invisible()
}
