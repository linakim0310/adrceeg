# ----------------------------------------------------------------------------
# STAGE 3 figures 12-14: PACC cognition. Guarded so the pipeline still runs on
# datasets without psychometrics.csv. Lower PACC = worse cognition.
# ----------------------------------------------------------------------------

#' Write the PACC cognition figures (12-14) when PACC is present
#' @param adrc_base Baseline rows from [build_adrc_base()].
#' @param bm_labels Named biomarker label vector.
#' @param grp_pal Group colour palette.
#' @param out_dir Output folder for PNGs.
#' @keywords internal
plot_cognition <- function(adrc_base, bm_labels, grp_pal, out_dir) {
  if (!("PACC" %in% names(adrc_base) && any(!is.na(adrc_base$PACC)))) return(invisible())

  p_pacc_grp <- adrc_base %>%
    filter(!is.na(PACC)) %>%
    ggplot(aes(group, PACC, fill = group)) +
    geom_boxplot(width = 0.5, outlier.size = 0.6) +
    scale_fill_manual(values = grp_pal, guide = "none") +
    labs(title = "Cognition (PACC) by impairment group",
         subtitle = "lower PACC = worse cognition", x = NULL, y = "PACC")
  ggsave(file.path(out_dir, "12_pacc_by_group.png"), p_pacc_grp, width = 6, height = 5, dpi = 150)

  p_pacc_age <- adrc_base %>%
    filter(!is.na(PACC)) %>%
    ggplot(aes(visitage, PACC, color = group)) +
    geom_point(alpha = 0.7, size = 1.8) +
    geom_smooth(method = "lm", se = FALSE) +
    scale_color_manual(values = grp_pal, name = NULL) +
    labs(title = "Cognition (PACC) vs age", x = "Age (years)", y = "PACC")
  ggsave(file.path(out_dir, "13_pacc_vs_age.png"), p_pacc_age, width = 7, height = 5, dpi = 150)

  pacc_long <- adrc_base %>%
    select(group, PACC, all_of(names(bm_labels))) %>%
    pivot_longer(all_of(names(bm_labels)), names_to = "biomarker", values_to = "z") %>%
    mutate(biomarker = factor(recode(biomarker, !!!bm_labels), levels = unname(bm_labels))) %>%
    filter(!is.na(PACC), !is.na(z))
  p_pacc_bm <- ggplot(pacc_long, aes(z, PACC, color = group)) +
    geom_point(alpha = 0.6, size = 1.6) +
    geom_smooth(method = "lm", se = FALSE) +
    scale_color_manual(values = grp_pal, name = NULL) +
    facet_wrap(~ biomarker, scales = "free_x") +
    labs(title = "Cognition (PACC) vs AD biomarkers", x = "Biomarker (z-score)", y = "PACC")
  ggsave(file.path(out_dir, "14_pacc_vs_biomarkers.png"), p_pacc_bm, width = 9, height = 6, dpi = 150)

  cat("PACC figures (12-14) written.\n")
  invisible()
}
