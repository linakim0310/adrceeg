# ----------------------------------------------------------------------------
# STAGE 3 figures 12-14: PACC cognition. Guarded so the pipeline still runs on
# datasets without the psychometrics file. Lower PACC = worse cognition.
# Titles come from params$plots$titles; figures written via save_figure().
# ----------------------------------------------------------------------------

#' Write the PACC cognition figures (12-14) when PACC is present
#' @param adrc_base Baseline rows from [build_adrc_base()].
#' @param bm_labels Named biomarker label vector.
#' @param grp_pal Group colour palette.
#' @param out_dir Output folder.
#' @param params Config list (default `adrc_params`).
#' @keywords internal
plot_cognition <- function(adrc_base, bm_labels, grp_pal, out_dir, params = adrc_params) {
  if (!("PACC" %in% names(adrc_base) && any(!is.na(adrc_base$PACC)))) return(invisible())
  ttl <- params$plots$titles

  p_pacc_grp <- adrc_base %>%
    filter(!is.na(PACC)) %>%
    ggplot(aes(group, PACC, fill = group)) +
    geom_boxplot(width = 0.5, outlier.size = 0.6) +
    scale_fill_manual(values = grp_pal, guide = "none") +
    labs(title = ttl$pacc_group, subtitle = ttl$pacc_group_sub, x = NULL, y = "PACC")
  save_figure(p_pacc_grp, "12_pacc_by_group", out_dir, 6, 5, params)

  p_pacc_age <- adrc_base %>%
    filter(!is.na(PACC)) %>%
    ggplot(aes(visitage, PACC, color = group)) +
    geom_point(alpha = 0.7, size = 1.8) +
    geom_smooth(method = "lm", se = FALSE) +
    scale_color_manual(values = grp_pal, name = NULL) +
    labs(title = ttl$pacc_age, x = "Age (years)", y = "PACC")
  save_figure(p_pacc_age, "13_pacc_vs_age", out_dir, 7, 5, params)

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
    labs(title = ttl$pacc_biomarkers, x = "Biomarker (z-score)", y = "PACC")
  save_figure(p_pacc_bm, "14_pacc_vs_biomarkers", out_dir, 9, 6, params)

  cat("PACC figures (12-14) written.\n")
  invisible()
}
