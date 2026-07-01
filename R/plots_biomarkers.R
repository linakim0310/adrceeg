# ----------------------------------------------------------------------------
# STAGE 3 figures 08-11: the AD biomarker figures. Plotting code is unchanged.
# ----------------------------------------------------------------------------

#' Write the biomarker figures (08-11)
#' @param adrc_base Baseline rows from [build_adrc_base()].
#' @param z_long Long biomarker table from [build_z_long()].
#' @param bm_labels Named biomarker label vector.
#' @param grp_pal Group colour palette.
#' @param out_dir Output folder for PNGs.
#' @keywords internal
plot_biomarkers <- function(adrc_base, z_long, bm_labels, grp_pal, out_dir) {
  p_bm <- ggplot(z_long, aes(biomarker, z, fill = group)) +
    geom_boxplot(outlier.size = 0.6) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    scale_fill_manual(values = grp_pal, name = NULL) +
    scale_x_discrete(drop = FALSE) +
    labs(title = "AD biomarkers by impairment group",
         subtitle = "z vs unimpaired controls (0 = control mean); each marker from its first-available visit",
         x = NULL, y = "z-score") +
    theme(axis.text.x = element_text(angle = 20, hjust = 1))
  ggsave(file.path(out_dir, "08_biomarkers_by_group.png"), p_bm, width = 8, height = 5, dpi = 150)

  cor_mat <- adrc_base %>%
    select(all_of(names(bm_labels))) %>%
    cor(use = "pairwise.complete.obs")
  cor_df <- as.data.frame(as.table(cor_mat))
  names(cor_df) <- c("Var1", "Var2", "r")
  cor_df <- cor_df %>%
    mutate(Var1 = recode(as.character(Var1), !!!bm_labels),
           Var2 = recode(as.character(Var2), !!!bm_labels))
  p_cor <- ggplot(cor_df, aes(Var1, Var2, fill = r)) +
    geom_tile(color = "white") +
    geom_text(aes(label = ifelse(is.na(r), "", sprintf("%.2f", r))), size = 3.5) +
    scale_fill_gradient2(low = "#3B4CC0", mid = "white", high = "#B40426",
                         midpoint = 0, limits = c(-1, 1)) +
    labs(title = "Biomarker correlations", x = NULL, y = NULL) +
    theme(axis.text.x = element_text(angle = 25, hjust = 1))
  ggsave(file.path(out_dir, "09_biomarker_correlations.png"), p_cor, width = 6.5, height = 5.5, dpi = 150)

  p_scatter <- adrc_base %>%
    filter(!is.na(z_pib), !is.na(z_tau)) %>%
    ggplot(aes(z_pib, z_tau, color = group)) +
    geom_point(alpha = 0.7, size = 2) +
    geom_smooth(method = "lm", se = FALSE) +
    scale_color_manual(values = grp_pal, name = NULL) +
    labs(title = "Amyloid vs tau", x = "Amyloid (PiB), z", y = "Tau, z")
  ggsave(file.path(out_dir, "10_amyloid_vs_tau.png"), p_scatter, width = 7, height = 5, dpi = 150)

  p_age_bm <- ggplot(z_long, aes(visitage, z, color = group)) +
    geom_point(alpha = 0.6, size = 1.6) +
    geom_smooth(method = "lm", se = FALSE) +
    scale_color_manual(values = grp_pal, name = NULL) +
    facet_wrap(~ biomarker, scales = "free_y") +
    labs(title = "Biomarkers vs age", x = "Age (years)", y = "z-score")
  ggsave(file.path(out_dir, "11_biomarkers_vs_age.png"), p_age_bm, width = 9, height = 6, dpi = 150)

  invisible()
}
