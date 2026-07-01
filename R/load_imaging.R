# ----------------------------------------------------------------------------
# STAGE 1a -- Load each imaging modality and merge them onto the MRI visits.
# Logic is unchanged from Nicole's pipeline; only the file paths are now
# relative to a user-supplied data folder.
# ----------------------------------------------------------------------------

#' Load the amyloid (PiB) summary measure
#' @param data_dir Folder containing the ADRC CSV exports.
#' @keywords internal
load_pib <- function(data_dir) {
  read.csv(adrc_path(data_dir, "pib.csv")) %>%
    drop_na(pet_fsuvr_rsf_tot_cortmean) %>%
    mutate(pib_date = to_date(PET_Date), pib = pet_fsuvr_rsf_tot_cortmean,
           mri_date = to_date(Processed_with_MR_Date),
           year = format(mri_date, format = "%Y")) %>%
    select(ID, pib_date, pib, year)
}

#' Load the FDG metabolism summary measure
#' @inheritParams load_pib
#' @keywords internal
load_fdg <- function(data_dir) {
  read.csv(adrc_path(data_dir, "fdg.csv")) %>%
    drop_na(pet_fsuvr_rsf_tot_ctx_inferprtl) %>%
    mutate(fdg_date = to_date(PET_Date),
           FDG = pet_fsuvr_rsf_tot_ctx_inferprtl + pet_fsuvr_rsf_tot_ctx_isthmuscng,
           mri_date = to_date(Processed_with_MR_Date),
           year = format(mri_date, format = "%Y")) %>%
    select(ID, fdg_date, FDG, year)
}

#' Load the tau (Tauopathy) summary measure
#' @inheritParams load_pib
#' @keywords internal
load_tau <- function(data_dir) {
  read.csv(adrc_path(data_dir, "tau.csv")) %>%
    drop_na(Tauopathy) %>%
    mutate(tau_date = to_date(PET_Date), mri_date = to_date(Processed_with_MR_Date),
           year = format(mri_date, format = "%Y")) %>%
    select(ID, tau_date, Tauopathy, year)
}

#' Load MRI cortical thickness + hippocampal volume, head-size adjusted
#'
#' Runs the same regression as the original to remove the influence of
#' intracranial volume on hippocampal volume, then drops the helper columns.
#' @inheritParams load_pib
#' @keywords internal
load_mri <- function(data_dir) {
  ADRC_mri <- read.csv(adrc_path(data_dir, "mri_3t.csv")) %>%
    drop_na(LOAD_CorticalSignature_Thickness) %>%
    mutate(mri_date = to_date(MR_Date), cort_sig = LOAD_CorticalSignature_Thickness,
           MR_TOTV_HIPPOCAMPUS = mr_vol_r_hippocampus + mr_vol_l_hippocampus,
           year = format(mri_date, format = "%Y")) %>%
    select(ID, mri_date, cort_sig, mr_vol_tot_intracranial, MR_TOTV_HIPPOCAMPUS, year)

  # estimate influence of headsize specifically on hippocampal volume
  hipp <- lm(ADRC_mri$MR_TOTV_HIPPOCAMPUS ~ ADRC_mri$mr_vol_tot_intracranial)
  Bw   <- as.numeric(hipp$coefficients[2])
  meICV <- mean(ADRC_mri$mr_vol_tot_intracranial, na.rm = TRUE)

  ADRC_mri %>%
    mutate(hippvol = MR_TOTV_HIPPOCAMPUS - (Bw * (mr_vol_tot_intracranial - meICV))) %>%
    select(-mr_vol_tot_intracranial, -MR_TOTV_HIPPOCAMPUS)
}

#' Merge the four imaging modalities by ID + MRI year
#' @param data_dir Folder containing the ADRC CSV exports.
#' @return A data.frame of imaging visits with pib/FDG/tau joined onto MRI.
#' @keywords internal
merge_imaging <- function(data_dir) {
  ADRC_mri <- load_mri(data_dir)
  ADRC_mri %>%
    merge(load_pib(data_dir), by = c("ID", "year"), all.x = TRUE) %>%
    merge(load_fdg(data_dir), by = c("ID", "year"), all.x = TRUE) %>%
    merge(load_tau(data_dir), by = c("ID", "year"), all.x = TRUE)
}
