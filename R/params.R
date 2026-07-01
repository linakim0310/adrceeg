# ============================================================================
# params.R  --  THE ONE FILE YOU EDIT
# ----------------------------------------------------------------------------
# Everything that might change between data batches lives here, defined ONCE:
#   * the CSV file names                     (params$files)
#   * the column names inside each CSV       (params$cols)
#   * the thresholds / windows               (params$thresholds)
#   * which biomarkers get z-scored          (params$biomarkers)
#   * how APOE genotypes are recoded         (params$apoe)
#   * plot labels / colours / output options (params$plots)
#   * the run log settings                   (params$logging)
#
# If the ADRC renames a column, adds a biomarker, or you want a different
# matching window, you change it HERE and nowhere else. The pipeline functions
# never hard-code these values -- they read them from `adrc_params`.
#
# HOW IT WORKS: every pipeline function takes `params = adrc_params` as its last
# argument, so by default it uses the settings below. Nothing else to wire up.
# ============================================================================

adrc_params <- list(

  # --------------------------------------------------------------------------
  # 1. FILE NAMES  --  what each CSV export is called inside your data folder.
  #    Change only the right-hand strings if a file gets renamed.
  # --------------------------------------------------------------------------
  files = list(
    pib          = "pib.csv",            # amyloid PET
    fdg          = "fdg.csv",            # glucose-metabolism PET
    tau          = "tau.csv",            # tau PET
    mri          = "mri_3t.csv",         # cortical thickness + hippocampus
    demographics = "demographics.csv",   # birth date, education, sex
    apoe         = "apoe.csv",           # APOE genotype
    cdr          = "b4_cdr.csv",         # clinical dementia rating
    sleep        = "sleep_IDs.csv",      # who had a sleep EEG
    bag          = "ADRC_BAG.csv",       # brain-age-gap (optional)
    pacc         = "psychometrics.csv"   # PACC cognition (optional)
  ),

  # --------------------------------------------------------------------------
  # 2. COLUMN NAMES  --  the exact header text inside each CSV.
  #    e.g. birth date is column "BIRTH" in demographics.csv. If a future batch
  #    calls it "Birth" instead, change ONLY the string here.
  #    The pipeline renames these to stable internal names, so downstream code
  #    never needs touching.
  # --------------------------------------------------------------------------
  cols = list(
    pib = list(
      id       = "ID",
      pet_date = "PET_Date",
      mri_date = "Processed_with_MR_Date",
      value    = "pet_fsuvr_rsf_tot_cortmean"      # -> internal "pib"
    ),
    fdg = list(
      id       = "ID",
      pet_date = "PET_Date",
      mri_date = "Processed_with_MR_Date",
      value1   = "pet_fsuvr_rsf_tot_ctx_inferprtl",  # FDG = value1 + value2
      value2   = "pet_fsuvr_rsf_tot_ctx_isthmuscng"
    ),
    tau = list(
      id       = "ID",
      pet_date = "PET_Date",
      mri_date = "Processed_with_MR_Date",
      value    = "Tauopathy"                        # -> internal "tau"
    ),
    mri = list(
      id       = "ID",
      mr_date  = "MR_Date",
      cort_sig = "LOAD_CorticalSignature_Thickness", # -> internal "cort_sig"
      hipp_r   = "mr_vol_r_hippocampus",             # right + left = hippvol
      hipp_l   = "mr_vol_l_hippocampus",
      icv      = "mr_vol_tot_intracranial"           # head size, for adjustment
    ),
    demographics = list(
      id    = "ID",
      birth = "BIRTH",
      educ  = "EDUC",
      sex   = "sex"
    ),
    apoe = list(
      id       = "id",
      genotype = "apoe"
    ),
    cdr = list(
      id       = "ID",
      cdr      = "cdr",
      testdate = "TESTDATE"
    ),
    sleep = list(
      id = "mapid"        # sleep_IDs.csv stores the participant ID as "mapid"
    ),
    bag = list(
      id = "MAPID"        # ADRC_BAG.csv stores the participant ID as "MAPID"
    ),
    pacc = list(
      id    = "ID",
      score = "K1",       # PACC value, already a z-score (lower = worse)
      date  = "psy_date"  # session date, used to match to imaging visits
    )
  ),

  # --------------------------------------------------------------------------
  # 3. THRESHOLDS  --  the tunable numbers, named so they are easy to change.
  # --------------------------------------------------------------------------
  thresholds = list(
    cdr_window_years  = 2,   # keep an imaging visit only if a CDR is within N yrs
    pacc_window_years = 2,   # same idea for the PACC cognitive session
    trim_sd           = 5    # blank z-scores farther than N SD from the mean
  ),

  # --------------------------------------------------------------------------
  # 4. BIOMARKERS  --  the list that makes z-scoring / trimming / plotting
  #    flexible for ANY number of markers. To add one, copy a line; to drop
  #    one, delete a line. `source` is the internal column that holds the raw
  #    value; `z` is the z-scored column name that gets created; `label` is the
  #    pretty name used in figures.
  # --------------------------------------------------------------------------
  biomarkers = list(
    list(z = "z_pib",      source = "pib",      label = "Amyloid (PiB)"),
    list(z = "z_FDG",      source = "FDG",      label = "FDG metabolism"),
    list(z = "z_tau",      source = "tau",      label = "Tau"),
    list(z = "z_cort_sig", source = "cort_sig", label = "Cortical thickness"),
    list(z = "z_hippvol",  source = "hippvol",  label = "Hippocampal volume")
  ),

  # --------------------------------------------------------------------------
  # 5. APOE RECODING  --  how the two-letter genotype (e.g. "34") becomes the
  #    three numeric risk variables. APOE has three alleles: e2 (protective),
  #    e3 (neutral), e4 (risk). A genotype is the two alleles a person carries,
  #    written as digits, e.g. "34" = one e3 + one e4.
  #
  #    Each map below is passed to stringr::str_replace_all(genotype, map), so
  #    the LEFT side is the genotype and the RIGHT side is the recoded value.
  # --------------------------------------------------------------------------
  apoe = list(
    # apoe_bin: e4-CARRIER flag. 1 if the genotype contains an e4, else 0.
    #   carriers: 34, 24, 44  ->  1      non-carriers: 33, 23, 22  ->  0
    carrier_binary = c("33" = "0", "34" = "1", "23" = "0",
                       "24" = "1", "44" = "1", "22" = "0"),

    # apoe_3: e4 DOSAGE (how many e4 alleles). 0, 1, or 2.
    #   33/23/22 -> 0 copies    34/24 -> 1 copy    44 -> 2 copies
    e4_allele_count = c("33" = "0", "34" = "1", "23" = "0",
                        "24" = "1", "44" = "2", "22" = "0"),

    # apoe_fin: a 3-level risk summary.
    #   4 = carries an e4 (highest risk): 34, 24, 44
    #   2 = carries an e2 but no e4 (protective): 23, 22
    #   3 = e3/e3 (neutral reference): 33
    dominant_allele = c("33" = "3", "34" = "4", "23" = "2",
                        "24" = "4", "44" = "4", "22" = "2")
  ),

  # --------------------------------------------------------------------------
  # 6. PLOTS  --  everything cosmetic, so figures can be relabelled/recoloured
  #    without touching plotting code. `labels` is derived automatically from
  #    the biomarker list above (kept in sync). See the R Graph Gallery
  #    (https://r-graph-gallery.com) for palette / style ideas.
  # --------------------------------------------------------------------------
  plots = list(
    # colours for the two impairment groups
    palette = c("Unimpaired (CDR 0)" = "#4C72B0",
                "Impaired (CDR > 0)" = "#C44E52"),

    # group display names (change if you prefer different wording)
    group_labels = c("Unimpaired (CDR 0)", "Impaired (CDR > 0)"),

    base_size    = 13,        # base font size for the theme
    dpi          = 150,       # resolution for the image files
    image_format = "png",     # "png", "pdf", "tiff", ...
    save_images  = TRUE,      # write standalone image files
    save_pptx    = TRUE,      # ALSO collect every figure into one editable deck
    pptx_file    = "cohort_figures.pptx",
    pptx_width   = 8,         # slide graphic size (inches)
    pptx_height  = 5,

    # figure titles / subtitles, all in one place
    titles = list(
      data_availability  = "Data availability per variable",
      cohort_composition = "Cohort composition",
      age                = "Age at baseline visit",
      education          = "Years of education by group",
      apoe               = "APOE genotype distribution",
      cdr                = "Clinical Dementia Rating (global)",
      visits             = "Imaging visits per person",
      biomarkers_group   = "AD biomarkers by impairment group",
      biomarkers_group_sub = "z vs unimpaired controls (0 = control mean); each marker from its first-available visit",
      correlations       = "Biomarker correlations",
      amyloid_vs_tau     = "Amyloid vs tau",
      biomarkers_vs_age  = "Biomarkers vs age",
      pacc_group         = "Cognition (PACC) by impairment group",
      pacc_group_sub     = "lower PACC = worse cognition",
      pacc_age           = "Cognition (PACC) vs age",
      pacc_biomarkers    = "Cognition (PACC) vs AD biomarkers"
    )
  ),

  # --------------------------------------------------------------------------
  # 7. LOGGING  --  when an OPTIONAL file is missing (or a value won't parse),
  #    the pipeline keeps running but records it LOUDLY: a console warning plus
  #    a line appended to this log file. Nothing fails silently.
  # --------------------------------------------------------------------------
  logging = list(
    enabled = TRUE,
    file    = "adrceeg_run.log"   # written in the working directory
  )
)
