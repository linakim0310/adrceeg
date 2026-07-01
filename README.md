# adrceeg

Build the Knight ADRC **sleep-EEG cohort table** from the raw ADRC CSV exports,
in one reproducible step. The pipeline combines imaging (amyloid, FDG, tau, MRI),
genetics (APOE), demographics and clinical (CDR) data into a single longitudinal
table, folds in brain-age-gap and PACC cognition, filters to the people who had a
sleep EEG, and writes a set of cohort figures and summary tables.

Originally adapted from Nicole S. McKay's ADRC data-combining script, then
refactored into a small, portable, well-documented package so anyone can run it
on their own machine without editing a single path.

## No data ships with this code

The ADRC data is HIPAA-protected and is **never** part of this repository. You
point the pipeline at a local folder of CSVs; `.gitignore` makes sure nothing in
`data/` (or any `.csv`) is ever committed or pushed.

## Two ways to run

**A. The easy way (no install).** Open `adrceeg.Rproj` in RStudio, drop your CSV
exports into `data/`, open `run_analysis.R`, and click **Source**. It loads the
dependencies, sources everything in `R/`, and runs the pipeline. Results land in
`outputs/`.

**B. As an installed package.**

```r
install.packages("devtools")
devtools::install_github("Harshu-Pande/adrceeg")
library(adrceeg)
run_pipeline(data_dir = "path/to/synthetic_data",
             out_dir  = "path/to/outputs")
```

Either way the entry point is the same function, `run_pipeline()`.

## Prove it runs without the real data

The real ADRC data is HIPAA-protected and never ships here, but you can still
confirm the whole pipeline works on **synthetic, schema-correct** CSVs:

```r
source("data-raw/make_example_data.R")   # base R only; fills data/ with fake CSVs
source("run_analysis.R")                  # runs the real pipeline on them
```

This writes the cohort table and all 14 figures to `outputs/`. The fake data is
random numbers with the correct columns and join keys — useful for a smoke test
or a demo, **not** for any real analysis. The synthetic CSVs are git-ignored.

## What you need in the data folder

Required: `pib.csv`, `fdg.csv`, `tau.csv`, `mri_3t.csv`, `demographics.csv`,
`apoe.csv`, `b4_cdr.csv`, `sleep_IDs.csv`.
Optional (auto-skipped if absent): `ADRC_BAG.csv`, `psychometrics.csv`.
See `data/README.md` for the column details.

## What you get

`outputs/ADRC_eeg.csv` (the cohort table, one row per imaging visit),
14 figures (`01_*.png` … `14_*.png`), and two summary tables
(`cohort_summary.csv`, `data_availability.csv`).

## How it's organised

Every stage is its own small file under `R/`, and one orchestrator threads them
together — so the code is easy to read, test, and hand off.

```
R/
  dates.R            robust date parsing + pre-load date checks
  utils.R            id normalisation + path helper
  load_imaging.R     load & merge amyloid / FDG / tau / MRI
  add_clinical.R     demographics, APOE, CDR date-matching
  zscore.R           factorise, z-score vs controls, trim outliers, visit counts
  pacc.R             attach PACC cognition (date- or mean-matched)
  eeg.R              brain-age-gap + filter to EEG participants
  build_adrc.R       build_adrc()  -> the combined, EEG-filtered table
  plots_*.R          cohort figures + summary tables
  make_cohort_plots.R / pipeline.R   the orchestrators
run_analysis.R       install-free one-click runner
```

## Configuration

`run_pipeline()` takes a few arguments: `data_dir`, `out_dir`,
`make_plots = TRUE`, `pacc_match = "date"` (or `"mean"`), and
`pacc_window_years = 2`.

## Putting it on GitHub

The repo is already set up so the data can't leak. To publish, run this from
inside the `adrceeg` folder for a clean first commit:

```bash
cd adrceeg
rm -rf .git                      # start fresh (clears any half-initialised repo)
git init
git add .
git commit -m "Initial commit: adrceeg pipeline"
git branch -M main
git remote add origin https://github.com/<you>/adrceeg.git
git push -u origin main
```

Because of `.gitignore`, `git add .` will **not** stage anything in `data/`
(your CSVs) or `outputs/`. Always confirm with `git status` before pushing —
you should see the `R/` files, `DESCRIPTION`, `NAMESPACE`, the two READMEs and
`data-raw/`, but **no** `.csv` files.

## Credit

Pipeline logic: Nicole S. McKay. 
