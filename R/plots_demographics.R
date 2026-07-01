# ----------------------------------------------------------------------------
# STAGE 3 figures 01-07: data availability, demographics, genetics, clinical,
# and longitudinal structure. Titles come from params$plots$titles; each figure
# is written via save_figure() (image + optional PPTX slide).
# ----------------------------------------------------------------------------

#' Write the demographic / availability / structure figures (01-07)
#' @param adrc_base Baseline rows from [build_adrc_base()].
#' @param avail Availability table from [build_availability()].
#' @param grp_pal Group colour palette.
#' @param out_dir Output folder.
#' @param params Config list (default `adrc_params`).
#' @keywords internal
plot_demographics <- function(adrc_base, avail, grp_pal, out_dir, params = adrc_params) {
  ttl <- params$plots$titles

  p_avail <- ggplot(avail, aes(reorder(variable, pct_present), pct_present, fill = set)) +
    geom_col(position = "dodge") + coord_flip() +
    labs(title = ttl$data_availability, x = NULL, y = "% non-missing", fill = NULL)
  save_figure(p_avail, "01_data_availability", out_dir, 7.5, 6, params)

  p_counts <- ggplot(adrc_base, aes(group, fill = SEX)) +
    geom_bar(position = "dodge") +
    labs(title = ttl$cohort_composition, x = NULL, y = "Participants", fill = "Sex")
  save_figure(p_counts, "02_cohort_composition", out_dir, 7, 5, params)

  p_age <- ggplot(adrc_base, aes(visitage, fill = group)) +
    geom_histogram(binwidth = 5, position = "identity", alpha = 0.6, color = "white") +
    scale_fill_manual(values = grp_pal, name = NULL) +
    labs(title = ttl$age, x = "Age (years)", y = "Participants")
  save_figure(p_age, "03_age_distribution", out_dir, 7, 5, params)

  p_educ <- ggplot(adrc_base, aes(group, EDUC, fill = group)) +
    geom_boxplot(width = 0.5, outlier.size = 0.6) +
    scale_fill_manual(values = grp_pal, guide = "none") +
    labs(title = ttl$education, x = NULL, y = "Education (years)")
  save_figure(p_educ, "04_education", out_dir, 6, 5, params)

  p_apoe <- ggplot(adrc_base, aes(factor(apoe), fill = group)) +
    geom_bar(position = "dodge") +
    scale_fill_manual(values = grp_pal, name = NULL) +
    labs(title = ttl$apoe, x = "APOE genotype", y = "Participants")
  save_figure(p_apoe, "05_apoe_genotype", out_dir, 7, 5, params)

  p_cdr <- ggplot(adrc_base, aes(factor(cdrglob))) +
    geom_bar(fill = "#4C72B0") +
    labs(title = ttl$cdr, x = "CDR global score", y = "Participants")
  save_figure(p_cdr, "06_cdr_distribution", out_dir, 6, 5, params)

  p_visits <- ggplot(adrc_base, aes(factor(max_visit))) +
    geom_bar(fill = "#55A868") +
    labs(title = ttl$visits, x = "Number of visits", y = "Participants")
  save_figure(p_visits, "07_visits_per_person", out_dir, 6, 5, params)

  invisible()
}
