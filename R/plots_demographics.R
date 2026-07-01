# ----------------------------------------------------------------------------
# STAGE 3 figures 01-07: data availability, demographics, genetics, clinical,
# and longitudinal structure. Plotting code is unchanged from the original.
# ----------------------------------------------------------------------------

#' Write the demographic / availability / structure figures (01-07)
#' @param adrc_base Baseline rows from [build_adrc_base()].
#' @param avail Availability table from [build_availability()].
#' @param grp_pal Group colour palette.
#' @param out_dir Output folder for PNGs.
#' @keywords internal
plot_demographics <- function(adrc_base, avail, grp_pal, out_dir) {
  p_avail <- ggplot(avail, aes(reorder(variable, pct_present), pct_present, fill = set)) +
    geom_col(position = "dodge") + coord_flip() +
    labs(title = "Data availability per variable", x = NULL, y = "% non-missing", fill = NULL)
  ggsave(file.path(out_dir, "01_data_availability.png"), p_avail, width = 7.5, height = 6, dpi = 150)

  p_counts <- ggplot(adrc_base, aes(group, fill = SEX)) +
    geom_bar(position = "dodge") +
    labs(title = "Cohort composition", x = NULL, y = "Participants", fill = "Sex")
  ggsave(file.path(out_dir, "02_cohort_composition.png"), p_counts, width = 7, height = 5, dpi = 150)

  p_age <- ggplot(adrc_base, aes(visitage, fill = group)) +
    geom_histogram(binwidth = 5, position = "identity", alpha = 0.6, color = "white") +
    scale_fill_manual(values = grp_pal, name = NULL) +
    labs(title = "Age at baseline visit", x = "Age (years)", y = "Participants")
  ggsave(file.path(out_dir, "03_age_distribution.png"), p_age, width = 7, height = 5, dpi = 150)

  p_educ <- ggplot(adrc_base, aes(group, EDUC, fill = group)) +
    geom_boxplot(width = 0.5, outlier.size = 0.6) +
    scale_fill_manual(values = grp_pal, guide = "none") +
    labs(title = "Years of education by group", x = NULL, y = "Education (years)")
  ggsave(file.path(out_dir, "04_education.png"), p_educ, width = 6, height = 5, dpi = 150)

  p_apoe <- ggplot(adrc_base, aes(factor(apoe), fill = group)) +
    geom_bar(position = "dodge") +
    scale_fill_manual(values = grp_pal, name = NULL) +
    labs(title = "APOE genotype distribution", x = "APOE genotype", y = "Participants")
  ggsave(file.path(out_dir, "05_apoe_genotype.png"), p_apoe, width = 7, height = 5, dpi = 150)

  p_cdr <- ggplot(adrc_base, aes(factor(cdrglob))) +
    geom_bar(fill = "#4C72B0") +
    labs(title = "Clinical Dementia Rating (global)", x = "CDR global score", y = "Participants")
  ggsave(file.path(out_dir, "06_cdr_distribution.png"), p_cdr, width = 6, height = 5, dpi = 150)

  p_visits <- ggplot(adrc_base, aes(factor(max_visit))) +
    geom_bar(fill = "#55A868") +
    labs(title = "Imaging visits per person", x = "Number of visits", y = "Participants")
  ggsave(file.path(out_dir, "07_visits_per_person.png"), p_visits, width = 6, height = 5, dpi = 150)

  invisible()
}
