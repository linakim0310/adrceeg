# ----------------------------------------------------------------------------
# Declare the data-column names used inside dplyr/ggplot2 non-standard
# evaluation so `R CMD check` doesn't flag them as undefined globals. These are
# column names in the ADRC CSVs, not real objects -- this block is cosmetic.
# ----------------------------------------------------------------------------
# tryCatch keeps this harmless when the file is sourced directly (no package
# context) in the install-free run_analysis.R path.
tryCatch(utils::globalVariables(c(
  # imaging
  "pet_fsuvr_rsf_tot_cortmean", "PET_Date", "pib", "Processed_with_MR_Date",
  "mri_date", "year", "ID", "pib_date", "pet_fsuvr_rsf_tot_ctx_inferprtl",
  "pet_fsuvr_rsf_tot_ctx_isthmuscng", "FDG", "fdg_date", "Tauopathy", "tau_date",
  "LOAD_CorticalSignature_Thickness", "MR_Date", "cort_sig",
  "mr_vol_r_hippocampus", "mr_vol_l_hippocampus", "MR_TOTV_HIPPOCAMPUS",
  "mr_vol_tot_intracranial", "hippvol",
  # demographics / genetics / clinical
  "BIRTH", "EDUC", "sex", "SEX", "id", "apoe", "apoe_bin", "apoe_3", "apoe_fin",
  "cdr", "TESTDATE", "cdr_date", "cdr_bin", "cdrglob", "dates", "datediff",
  "count", "visit", "visitage", "tau",
  # z-scores / visit meta
  "visit_num", "max_visit", "z_pib", "z_hippvol", "z_tau", "z_cort_sig", "z_FDG",
  # PACC / BAG
  "K1", "psy_date", "PACC", "pacc_gap_yrs", "n_pacc", "mapid", "MAPID",
  # plotting
  "group", "pct_present", "variable", "set", "biomarker", "z", "Var1", "Var2", "r"
)), error = function(e) invisible(NULL))
