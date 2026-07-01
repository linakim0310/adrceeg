# Put your ADRC CSV exports here

This folder is **git-ignored** so the HIPAA-protected participant data never
gets committed or uploaded. Only this README is tracked.

Drop the following CSV files directly into this folder (the names must match):

Required:

- `pib.csv` — amyloid PET
- `fdg.csv` — FDG-PET metabolism
- `tau.csv` — tau PET
- `mri_3t.csv` — MRI cortical thickness + hippocampal volume
- `demographics.csv` — birth date, education, sex
- `apoe.csv` — APOE genotype
- `b4_cdr.csv` — Clinical Dementia Rating
- `sleep_IDs.csv` — list of sleep-EEG participants (column `mapid`)

Optional (the pipeline runs without them and just skips the relevant step):

- `ADRC_BAG.csv` — brain-age-gap (column `MAPID`)
- `psychometrics.csv` — PACC cognition (columns `K1`, `ID`, `psy_date`)

Then run `run_analysis.R`, or `run_pipeline(data_dir = "data")`.

## No real data handy?

Run `source("data-raw/make_example_data.R")` first. It fills this folder with
synthetic, schema-correct CSVs (random numbers, correct columns) so you can
confirm the pipeline runs end to end. It is a smoke test only — not real data.
