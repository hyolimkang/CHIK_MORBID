# Harmonised primary models: hospitalisation H2, mortality H0

Every main manuscript estimate (Figures 1–3, Table 2) comes from **one fitted model per outcome**:

| Model ID | Outcome | Specification | Cohort |
|---|---|---|---|
| `hospitalisation_H2` | Hospitalisation | profile + sex + year + s(state, re) + s(age, k = 10, ts) + s(age, profile, fs, k = 10, m = 1) | 776,072 cases; 25,363 hospitalisations |
| `mortality_H0` | Chikungunya-attributed death | profile + sex + year + s(state, re) + s(age, k = 10, ts) | 983,947 cases; 1,050 deaths |

**Fitting and standardisation:**
- Models were fitted with `mgcv::bam(binomial, fREML, discrete = TRUE)`.
- Each outcome has 4,000 shared draws from `vcov(unconditional = TRUE)` (seeds 6201 and 6202).
- Risks are g-computation standardised over the outcome cohort's exact age × sex × year × state distribution.
- Overall and age-specific risks, RRs and RDs are all computed from the same predictions and draws.

## Pipeline (run from `CHIK_MORBID/`)

| Script | Role | Approx. runtime |
|---|---|---|
| `02_Script/primary_models_functions.R` | Shared cohort, standardisation and summary functions | — |
| `02_Script/62_primary_models_fit_standardise.R` | Cohorts and checks, fits, verification, draws, estimates, identities, old-vs-new | 15 min |
| `02_Script/63_primary_models_sensitivity.R` | Sensitivity refits | 60 min |
| `02_Script/64_primary_models_figures_tables.R` | Figures 1–3, Table 2, legends, supplementary RR figures, cross-figure checks | 1 min |
| `02_Script/65_primary_models_verification.R` | Independent verification (57 checks) | 2 min |

Run logs are in `run_62.log` to `run_65.log`.

## Contents

| Folder | Files |
|---|---|
| `models/` | `hospitalisation_H2_primary.rds` and `mortality_H0_primary.rds` (**contain the individual-level model frame; git-ignored, do not share**); `model_provenance.csv` |
| `estimates/` | Overall risks, RRs, age-specific risks and RRs, within-count RDs, profile event support; coefficient draws; draw-level standardised risks |
| `figures/` | Figure 1 (overall RR), Figure 2 (age-specific risks), Figure 3 (within-count RD), each as PDF, PNG (600 dpi) and SVG; plotted-value CSVs |
| `tables/` | `Table2_Joint_Profiles_FINAL.docx` and `.png`; `Table2_Joint_Profiles_Validated.csv` |
| `legends/` | Title and legend `.docx` for Figures 1–3 |
| `supplementary/` | All age × profile estimates and support counts; sensitivity estimates; age-specific RR figures (H2, H0) |
| `validation/` | Cohort reconciliation, model verification and comparison, identities, cross-figure consistency, Monte Carlo check, old-vs-new estimates, `sensitivity_summary.md`, `statistical_verification.md`, final checks |

The report is in `PRIMARY_MODEL_HARMONISATION_REPORT.md`.

## Not changed

- Table 1.
- `01_Data/`.
- All earlier figures, tables, caches and the archive.
- The individual-condition supplementary table, which uses a different exposure specification.

The earlier H0 and factor-smooth outputs remain available as sensitivity and historical results.
