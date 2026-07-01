# ----------------------------------------------------------------------------
# STAGE 3 helpers -- shared styling, figure-saving, and the "one row per person"
# data frames used by every cohort figure. Because the data is longitudinal,
# summaries use one row per person so nobody is counted twice.
#
# Colours, biomarker labels, the theme, image format/dpi, and the PowerPoint
# option all come from params$plots -- so the look of every figure is controlled
# from params.R, not scattered through the plotting code.
# ----------------------------------------------------------------------------

#' Colour palette for the impairment groups (from params)
#' @param params Config list (default `adrc_params`).
#' @keywords internal
group_palette <- function(params = adrc_params) params$plots$palette

#' Friendly names for the z-scored biomarkers (from params)
#' @param params Config list (default `adrc_params`).
#' @keywords internal
biomarker_labels <- function(params = adrc_params) bm_label_vec(params)

#' A polished ggplot theme (see r-graph-gallery.com for style ideas)
#'
#' A cleaned-up theme_minimal: bold left-aligned title, muted subtitle, no
#' minor grid lines, light panel border. Base font size comes from params.
#' @param params Config list (default `adrc_params`).
#' @keywords internal
plot_theme <- function(params = adrc_params) {
  theme_minimal(base_size = params$plots$base_size) +
    theme(
      plot.title       = element_text(face = "bold", hjust = 0),
      plot.subtitle    = element_text(colour = "grey35"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "grey92"),
      legend.position  = "top",
      plot.margin      = margin(10, 14, 10, 10)
    )
}

#' Save one figure: image file and/or a slide in the shared PPTX deck
#'
#' Controlled by params$plots: `save_images` writes a standalone image (format
#' and dpi from params); `save_pptx` appends the figure to one editable
#' PowerPoint via the \pkg{export} package (each call adds a slide).
#' @param plot A ggplot object.
#' @param base File name without extension, e.g. "03_age_distribution".
#' @param out_dir Output folder.
#' @param width,height Size in inches for the image file.
#' @param params Config list (default `adrc_params`).
#' @keywords internal
save_figure <- function(plot, base, out_dir, width, height, params = adrc_params) {
  if (isTRUE(params$plots$save_images)) {
    ggsave(file.path(out_dir, paste0(base, ".", params$plots$image_format)),
           plot, width = width, height = height, dpi = params$plots$dpi)
  }
  if (isTRUE(params$plots$save_pptx)) add_plot_to_pptx(plot, out_dir, params)
  invisible()
}

#' Append one ggplot to the shared PPTX deck (via the export package)
#'
#' Uses export::graph2ppt with append = TRUE so every figure becomes its own
#' editable slide. Silently no-ops if `export` isn't installed (the caller,
#' make_cohort_plots, checks once up front and logs it loudly).
#' @param plot A ggplot object.
#' @param out_dir Output folder.
#' @param params Config list (default `adrc_params`).
#' @keywords internal
add_plot_to_pptx <- function(plot, out_dir, params = adrc_params) {
  if (!requireNamespace("export", quietly = TRUE)) return(invisible())
  f <- file.path(out_dir, params$plots$pptx_file)
  try(export::graph2ppt(x = plot, file = f, append = file.exists(f),
                        width = params$plots$pptx_width,
                        height = params$plots$pptx_height), silent = TRUE)
  invisible()
}

#' One row per person at their BASELINE (earliest) visit, with a group label
#' @param ADRC_eeg EEG cohort table from [build_adrc()].
#' @param params Config list (default `adrc_params`).
#' @keywords internal
build_adrc_base <- function(ADRC_eeg, params = adrc_params) {
  ADRC_eeg %>%
    group_by(ID) %>%
    slice_min(visit_num, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(group = factor(cdr_bin, levels = c(0, 1),
                          labels = params$plots$group_labels))
}

#' Data-availability table: % non-missing, baseline-only vs all visits
#' @param adrc_base Baseline rows from [build_adrc_base()].
#' @param ADRC_eeg Full EEG cohort table.
#' @param bm_labels Named biomarker label vector.
#' @keywords internal
build_availability <- function(adrc_base, ADRC_eeg, bm_labels) {
  vars_check <- c("visitage", "EDUC", "cdrglob", "apoe",
                  "cort_sig", "pib", "hippvol", "FDG", "tau", names(bm_labels))
  vars_check <- intersect(vars_check, names(ADRC_eeg))   # tolerate a changed marker set
  avail_fun <- function(df, label) {
    df %>%
      summarise(across(all_of(vars_check), ~ 100 * mean(!is.na(.x)))) %>%
      pivot_longer(everything(), names_to = "variable", values_to = "pct_present") %>%
      mutate(set = label)
  }
  bind_rows(avail_fun(adrc_base, "Baseline only"),
            avail_fun(ADRC_eeg, "All visits"))
}

#' Long biomarker table read from each marker's FIRST-AVAILABLE visit
#'
#' Some biomarkers (FDG-PET especially) aren't collected at baseline, so reading
#' baseline-only would leave them looking empty; first-available surfaces them.
#' @param ADRC_eeg Full EEG cohort table.
#' @param adrc_base Baseline rows (supplies each person's group label).
#' @param bm_labels Named biomarker label vector.
#' @keywords internal
build_z_long <- function(ADRC_eeg, adrc_base, bm_labels) {
  group_lookup <- adrc_base %>% select(ID, group)
  map_dfr(names(bm_labels), function(v) {
    ADRC_eeg %>%
      filter(!is.na(.data[[v]])) %>%
      group_by(ID) %>%
      slice_min(visit_num, with_ties = FALSE) %>%
      ungroup() %>%
      transmute(ID, visitage, biomarker = v, z = .data[[v]])
  }) %>%
    left_join(group_lookup, by = "ID") %>%
    mutate(biomarker = factor(recode(biomarker, !!!bm_labels), levels = unname(bm_labels)))
}
