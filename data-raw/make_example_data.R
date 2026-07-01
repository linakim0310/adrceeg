# ============================================================================
# Generate SYNTHETIC, schema-correct ADRC CSVs so anyone can prove the pipeline
# runs end to end WITHOUT the real (HIPAA-protected) data.
# ----------------------------------------------------------------------------
# This is FAKE data drawn from random numbers -- it is NOT real participants and
# the values are not scientifically meaningful. Its only job is to have the exact
# columns the pipeline reads, with join keys that line up, so `run_analysis.R`
# produces the table + all 14 figures.
#
# Uses BASE R only (no packages), so it runs on a clean install.
#   1. Open adrceeg.Rproj
#   2. source("data-raw/make_example_data.R")   # fills data/ with fake CSVs
#   3. source("run_analysis.R")                 # runs the real pipeline on them
# ============================================================================

set.seed(42)                                  # reproducible fake data
out <- "data"; dir.create(out, showWarnings = FALSE)
mdy <- function(d) format(d, "%m/%d/%Y")      # ADRC-style M/D/YYYY dates

n_people <- 40
ids      <- sprintf("EX%04d", seq_len(n_people))
apoe_opts <- c("22", "23", "24", "33", "34", "44")

# Per-person fixed attributes -------------------------------------------------
people <- data.frame(
  ID    = ids,
  BIRTH = mdy(as.Date("1940-01-01") + sample(0:5000, n_people, TRUE)),
  EDUC  = sample(10:20, n_people, TRUE),
  sex   = sample(c("M", "F"), n_people, TRUE),
  apoe  = sample(apoe_opts, n_people, TRUE),
  # first half are unimpaired controls (CDR 0); rest are impaired (CDR > 0)
  base_cdr = c(rep(0, n_people / 2),
               sample(c(0.5, 1, 2), n_people / 2, TRUE)),
  stringsAsFactors = FALSE
)

# Per-visit rows (1-3 imaging visits per person) -----------------------------
visit_rows <- do.call(rbind, lapply(seq_len(n_people), function(i) {
  nv   <- sample(1:3, 1)
  yrs  <- sort(sample(2008:2020, nv))
  mri  <- as.Date(paste0(yrs, "-0", sample(1:9, nv, TRUE), "-15"))
  data.frame(ID = ids[i], mri_date = mri, stringsAsFactors = FALSE)
}))
nrw <- nrow(visit_rows)
md  <- mdy(visit_rows$mri_date)               # MRI date string, reused as PET MR date

# Helper to bolt an ID + the matching MR date onto a modality frame
imaging_base <- data.frame(ID = visit_rows$ID, MR = md, stringsAsFactors = FALSE)

# MRI: cortical thickness + hippocampal + intracranial volumes
write.csv(data.frame(
  ID = imaging_base$ID, MR_Date = imaging_base$MR,
  LOAD_CorticalSignature_Thickness = round(rnorm(nrw, 2.6, 0.2), 3),
  mr_vol_r_hippocampus = round(rnorm(nrw, 3500, 400)),
  mr_vol_l_hippocampus = round(rnorm(nrw, 3500, 400)),
  mr_vol_tot_intracranial = round(rnorm(nrw, 1.45e6, 1.4e5))
), file.path(out, "mri_3t.csv"), row.names = FALSE)

# Amyloid PET (pib). Processed_with_MR_Date shares the MRI's YEAR so it joins.
write.csv(data.frame(
  ID = imaging_base$ID, PET_Date = imaging_base$MR, Processed_with_MR_Date = imaging_base$MR,
  pet_fsuvr_rsf_tot_cortmean = round(rnorm(nrw, 1.3, 0.3), 3)
), file.path(out, "pib.csv"), row.names = FALSE)

# FDG PET (two regions, summed in the pipeline)
write.csv(data.frame(
  ID = imaging_base$ID, PET_Date = imaging_base$MR, Processed_with_MR_Date = imaging_base$MR,
  pet_fsuvr_rsf_tot_ctx_inferprtl = round(rnorm(nrw, 1.2, 0.15), 3),
  pet_fsuvr_rsf_tot_ctx_isthmuscng = round(rnorm(nrw, 1.1, 0.15), 3)
), file.path(out, "fdg.csv"), row.names = FALSE)

# Tau PET
write.csv(data.frame(
  ID = imaging_base$ID, PET_Date = imaging_base$MR, Processed_with_MR_Date = imaging_base$MR,
  Tauopathy = round(rnorm(nrw, 1.25, 0.25), 3)
), file.path(out, "tau.csv"), row.names = FALSE)

# Demographics / APOE (one row per person; note apoe uses lowercase "id")
write.csv(people[, c("ID", "BIRTH", "EDUC", "sex")], file.path(out, "demographics.csv"), row.names = FALSE)
write.csv(data.frame(id = people$ID, apoe = people$apoe), file.path(out, "apoe.csv"), row.names = FALSE)

# CDR clinical: one test per imaging visit, dated in the same month (within 2 yrs)
cdr_by_id <- setNames(people$base_cdr, people$ID)
write.csv(data.frame(
  ID = visit_rows$ID, TESTDATE = md,
  cdr = pmax(0, cdr_by_id[visit_rows$ID] + sample(c(0, 0, 0.5), nrw, TRUE))
), file.path(out, "b4_cdr.csv"), row.names = FALSE)

# Sleep-EEG participant list: ~25 of the 40 people had an EEG (column "mapid")
write.csv(data.frame(mapid = sample(ids, 25)), file.path(out, "sleep_IDs.csv"), row.names = FALSE)

# PACC cognition (optional): a couple of sessions per person near their visits
pacc <- do.call(rbind, lapply(seq_len(nrw), function(i) data.frame(
  ID = visit_rows$ID[i],
  psy_date = mdy(visit_rows$mri_date[i] + sample(-200:200, 1)),
  K1 = round(rnorm(1, ifelse(cdr_by_id[visit_rows$ID[i]] > 0, -0.8, 0.2), 0.5), 3)
)))
write.csv(pacc, file.path(out, "psychometrics.csv"), row.names = FALSE)

# Brain-age-gap (optional): one value per person (column "MAPID")
write.csv(data.frame(MAPID = ids, BAG = round(rnorm(n_people, 0, 4), 2)),
          file.path(out, "ADRC_BAG.csv"), row.names = FALSE)

cat("Wrote", length(list.files(out, "\\.csv$")), "synthetic CSVs to", normalizePath(out), "\n")
cat("Now run:  source('run_analysis.R')\n")
