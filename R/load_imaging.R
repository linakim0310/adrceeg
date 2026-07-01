# ----------------------------------------------------------------------------
# STAGE 1a -- Load each imaging modality and merge them onto the MRI visits.
#
# Design: each loader reads the raw CSV, then pulls the columns it needs BY
# NAME from params (params$cols$<file>) and renames them to stable internal
# names (pib, FDG, tau, cort_sig, hippvol, mri_date, year, ID). Everything
# downstream uses those internal names, so if the ADRC renames a source column
# you only edit params.R -- no pipeline code changes. The numeric logic
# (head-size adjustment, the four-way merge) is unchanged from Nicole's script.
# ----------------------------------------------------------------------------

#' Load the amyloid (PiB) summary measure
#' @param data_dir Folder containing the ADRC CSV exports.
#' @param params Config list (default `adrc_params`).
#' @keywords internal
load_pib <- function(data_dir, params = adrc_params) {
  cfg <- params$cols$pib
  raw <- read_adrc(data_dir, params$files$pib)
  raw <- raw[!is_blank(raw[[cfg$value]]), , drop = FALSE]        # drop_na(value)
  data.frame(
    ID       = norm_id(raw[[cfg$id]]),
    pib_date = to_date(raw[[cfg$pet_date]]),
    pib      = as.numeric(raw[[cfg$value]]),
    year     = format(to_date(raw[[cfg$mri_date]]), "%Y"),
    stringsAsFactors = FALSE
  )
}

#' Load the FDG metabolism summary measure (sum of two ROIs)
#' @inheritParams load_pib
#' @keywords internal
load_fdg <- function(data_dir, params = adrc_params) {
  cfg <- params$cols$fdg
  raw <- read_adrc(data_dir, params$files$fdg)
  raw <- raw[!is_blank(raw[[cfg$value1]]), , drop = FALSE]       # drop_na(value1)
  data.frame(
    ID       = norm_id(raw[[cfg$id]]),
    fdg_date = to_date(raw[[cfg$pet_date]]),
    FDG      = as.numeric(raw[[cfg$value1]]) + as.numeric(raw[[cfg$value2]]),
    year     = format(to_date(raw[[cfg$mri_date]]), "%Y"),
    stringsAsFactors = FALSE
  )
}

#' Load the tau (Tauopathy) summary measure
#' @inheritParams load_pib
#' @keywords internal
load_tau <- function(data_dir, params = adrc_params) {
  cfg <- params$cols$tau
  raw <- read_adrc(data_dir, params$files$tau)
  raw <- raw[!is_blank(raw[[cfg$value]]), , drop = FALSE]        # drop_na(value)
  data.frame(
    ID        = norm_id(raw[[cfg$id]]),
    tau_date  = to_date(raw[[cfg$pet_date]]),
    Tauopathy = as.numeric(raw[[cfg$value]]),
    year      = format(to_date(raw[[cfg$mri_date]]), "%Y"),
    stringsAsFactors = FALSE
  )
}

#' Load MRI cortical thickness + hippocampal volume, head-size adjusted
#'
#' Runs the same regression as the original to remove the influence of
#' intracranial volume on hippocampal volume, then drops the helper columns.
#' @inheritParams load_pib
#' @keywords internal
load_mri <- function(data_dir, params = adrc_params) {
  cfg <- params$cols$mri
  raw <- read_adrc(data_dir, params$files$mri)
  raw <- raw[!is_blank(raw[[cfg$cort_sig]]), , drop = FALSE]     # drop_na(cort_sig)

  mri <- data.frame(
    ID                      = norm_id(raw[[cfg$id]]),
    mri_date                = to_date(raw[[cfg$mr_date]]),
    cort_sig                = as.numeric(raw[[cfg$cort_sig]]),
    mr_vol_tot_intracranial = as.numeric(raw[[cfg$icv]]),
    MR_TOTV_HIPPOCAMPUS     = as.numeric(raw[[cfg$hipp_r]]) + as.numeric(raw[[cfg$hipp_l]]),
    stringsAsFactors = FALSE
  )
  mri$year <- format(mri$mri_date, "%Y")

  # estimate influence of headsize specifically on hippocampal volume
  hipp  <- lm(MR_TOTV_HIPPOCAMPUS ~ mr_vol_tot_intracranial, data = mri)
  Bw    <- as.numeric(coef(hipp)[2])
  meICV <- mean(mri$mr_vol_tot_intracranial, na.rm = TRUE)

  mri$hippvol <- mri$MR_TOTV_HIPPOCAMPUS - (Bw * (mri$mr_vol_tot_intracranial - meICV))
  mri[, c("ID", "mri_date", "cort_sig", "hippvol", "year")]
}

#' Merge the four imaging modalities by ID + MRI year
#' @param data_dir Folder containing the ADRC CSV exports.
#' @param params Config list (default `adrc_params`).
#' @return A data.frame of imaging visits with pib/FDG/tau joined onto MRI.
#' @keywords internal
merge_imaging <- function(data_dir, params = adrc_params) {
  load_mri(data_dir, params) %>%
    merge(load_pib(data_dir, params), by = c("ID", "year"), all.x = TRUE) %>%
    merge(load_fdg(data_dir, params), by = c("ID", "year"), all.x = TRUE) %>%
    merge(load_tau(data_dir, params), by = c("ID", "year"), all.x = TRUE)
}
