# CHIK_MORBID analysis pipeline manual

This document records **what order, and why**, the scripts in `02_Script/` should be run in.
The goal is that reopening this repo much later, reading this one file should be enough to
reconstruct the whole workflow.

Last updated: 2026-09 (repo restructuring session)

---

## 0. Big picture

This repo contains **two independent branches** of analysis.

1. **Main pipeline** (sections 1-8 below) — computes relative risk (RR) of severe outcomes
   by comorbidity count and by individual comorbidity from SINAN individual-level data,
   then combines it with GBD/IHME background comorbidity prevalence to estimate
   country-level population morbidity burden.
2. **`descriptive_appendix/descriptive.R`** — a fully independent descriptive-analysis
   script. It re-reads the raw SINAN CSVs itself and builds its own cache
   (`01_Data/descriptive_appendix/chik_analytic.rds`). It does not depend on, and is not
   depended on by, the main pipeline's `analysis_df` or RR results. Run separately, only
   when needed.

Scripts under `archive/` are superseded drafts / scratch code no longer executed (see
section 7). Scripts under `side_analyses/` are one-off tables/checks, not part of the
main linear sequence (see section 5).

---

## 1. Execution order (Quick Start)

⚠️ **Important**: these scripts cannot be run independently via `Rscript` each. They pass
objects such as `analysis_df`, `ind`, `corr_by_age_bridge` between each other within a
single live R session, so they **must be `source()`d in order, in one session**.

The sequence below is **strictly linear as of the 2026-09 restructuring** — no script
needs to be run twice, unlike in earlier versions of this pipeline.

```r
source("02_Script/00_setup.R")                    # load packages (once per session)

# ---- Raw data preparation (skip if already fetched) ----
source("02_Script/01_fetch_chik_sinan_brazil.R")   # download SINAN raw csv.zip files
source("02_Script/02_clean_chik_sinan_brazil.R")   # -> 01_Data/chik_sinan_individual_2015_2024.rds

# ---- Main analysis ----
source("02_Script/03_relative_risk.R")             # core: builds analysis_df, RR models, RR results

# ---- IHME/GBD combination ----
source("02_Script/04_background_prevalence.R")     # loads GBD + hypertension + population -> bg_prev_country
source("02_Script/05_corr_matrix.R")                # Brazil-derived comorbidity correlation matrix -> corr_by_age_bridge
source("02_Script/06_corr_heatmap.R")               # visualises 05's correlation matrix
source("02_Script/07_gbd_age_alignment.R")          # harmonises 04's GBD data onto a shared age grid
source("02_Script/08_copula_simulation.R")          # copula simulation -> bg_count_dist_wide, bg_exclusive_dist
source("02_Script/09_copula_result_graphs.R")       # visualises 08's simulation results
source("02_Script/10_background_burden_graphs.R")   # visualises 04/07/08's background prevalence + burden

# ---- Side analyses, any time after 03, not part of the sequence above ----
source("02_Script/side_analyses/table_basic.R")             # Table 1 (currently broken - see section 6)
source("02_Script/side_analyses/hosp_rate_validation.R")    # observed hospitalisation rate vs model prediction
```

---

## 2. Raw data collection / cleaning

| Order | Script | Input | Output |
|---|---|---|---|
| 1 | `01_fetch_chik_sinan_brazil.R` | (none, downloads from the internet) | `01_Data/sinan_chik_csv/CHIKBR*.csv.zip`, `01_Data/sinan_chik_docs/` |
| 2 | `02_clean_chik_sinan_brazil.R` | the zip files above | **`01_Data/chik_sinan_individual_2015_2024.rds`** <- starting point for every downstream analysis |

`02_clean_chik_sinan_brazil.R` also builds a muni x month panel, a muni x week panel, and a
state x year comorbidity summary, but **no analysis script re-reads any of the three**
(municipality-level analysis appears to have been dropped). The already-generated outputs
are kept in `01_Data/archive/`; the script itself is left as-is (fully reusable if
municipality-level analysis is revived later).

---

## 3. Main RR calculation — `03_relative_risk.R`

The core script. What it does:

1. Loads `01_Data/chik_sinan_individual_2015_2024.rds` -> `ind`
2. **Builds the analysis cohort `analysis_df`**: confirmed CHIK, 2017 onward (the year
   comorbidity reporting started), valid age (0-100)/sex/hospitalisation status
3. **Exposure variables**: `comorb_count_group` = number of comorbidities, `"0" / "1" / "2+"`,
   plus the 7 individual condition flags (diabetes, hypertension, hepatic, renal,
   haematological, peptic ulcer, autoimmune disease)
4. **Outcome models**: for hospitalisation (`hosp_only`) and death (`death_only`)
   separately, fits `ns(age, df=4) + comorb_count_group + sex` GLMs and
   `s(age, by=comorb_count_group)` GAMs (mgcv, REML), plus one single-condition GLM per
   comorbidity (no age interaction) for section 3-1/3-3 below
5. **RR by comorbidity count**: draws 2000 coefficient vectors via `MASS::mvrnorm`,
   averages by sex ratio (age-sex standardisation), computes risk by 10-year age band,
   takes the ratio against the 0-comorbidity group as RR, with 95% CI from draw quantiles
6. **RR by individual comorbidity** (section 3-3, `make_rr_comorb()`): the same
   age-sex-standardised bootstrap approach applied to each single-condition GLM in turn —
   condition present vs absent, faceted by condition. These GLMs have no age x condition
   interaction, so each curve is expected to be flatter than the comorbidity-count RR
   above (by design, not a bug)
7. Final save: `01_Data/rr_hosp_model.RData`, `01_Data/rr_death_model.RData` (comorbidity-count
   RR only; the individual-comorbidity RR from step 6 is not saved to disk, only plotted)

**Objects this script leaves in the session for downstream scripts to pick up**: `ind`,
`analysis_df`, `comorb_cols`, `comorb_count_group` (3 groups), `age_band_levels` /
`age_band_labels`, `theme_lancet_clean()`, `fit_hosp_gam`, `fit_death_gam`, etc.

As of the 2026-09 cleanup, the comorbidity-count RR bootstrap (step 5) is the **only**
comorbidity-count RR calculation in the file: a second, GLM-based RR pass used to live at
the end of the script (continuous-age and age-band versions, plus a GAM "interaction"
re-implementation that used `exp()` instead of `plogis()` on a logit-link model, so it did
not produce valid probabilities) and its final step silently overwrote
`rr_hosp_model.RData`/`rr_death_model.RData` with that buggy result. It has been removed.
The file shrank from ~3000 to ~1950 lines in the process (redundant point-estimate-only
standardisation passes, one always-overwritten prediction call, an unused disease-specific
GAM model, and a literal copy-pasted duplicate block were also removed). Comments are now
English-only, and package loading was moved out to `00_setup.R`. Six plots that were built
but never written to disk (`p_hosp_absolute_risk`, `p_death_absolute_risk`, `p_rr_hosp_gam`,
`p_rr_death_gam`, and the two individual-comorbidity RR facets) now also get `ggsave()`d,
to `03_Output/figures/fig_absolute_risk_hosp.jpg`, `fig_absolute_risk_death.jpg`,
`fig_rr_hosp_gam.jpg`, `fig_rr_death_gam.jpg`, `fig_rr_hosp_by_comorb.jpg`, and
`fig_rr_death_by_comorb.jpg`.

---

## 4. IHME/GBD combination — `04` through `10`

This is where **IHME/GBD data enters the pipeline**. As of the 2026-09 restructuring this
is one clean linear chain of 7 small, single-purpose scripts. It used to be 2 files
(`06_corr_matrix.R`, `07_morbid_prob_calc.R`) mixing several unrelated jobs each, with a
circular dependency between them — see section 6 for that history.

| Script | Job | Needs | Produces |
|---|---|---|---|
| `04_background_prevalence.R` | Load GBD prevalence (6 of 7 conditions), hypertension (separate source), and population; combine into one country x age-band table | (external CSVs only) | `bg_prev_country`, `bg_prev_country_pop`, `pop_country_age` |
| `05_corr_matrix.R` | Compute the correlation matrix between the 7 comorbidities by age band, from the **Brazil SINAN cohort** (not GBD) | `analysis_df` (03), `bg_prev_country` (04, Brazil rows only) | `corr_by_age_bridge` |
| `06_corr_heatmap.R` | Visualise `corr_by_age_bridge` | `corr_by_age_bridge` (05) | `fig_corr_heatmap.jpg` |
| `07_gbd_age_alignment.R` | Re-express 04's GBD age bands (a mix of 5-year bands and open-ended groups) onto one shared age grid, so a 7-condition marginal-prevalence vector can be pulled for any country/age | `bg_prev_country` (04) | `bg_prev_country_wide`, `bg_prev_country_complete` |
| `08_copula_simulation.R` | **The core computation.** Gaussian copula: for each country x age-band row, simulate 10,000 people whose 7 conditions have 04's country-specific marginal prevalence *and* 05's Brazil-derived correlation structure; tabulate comorbidity count (0/1/2+) and mutually-exclusive condition category | `corr_by_age_bridge` (05), `bg_prev_country_complete` (07), `pop_country_age` (04) | `bg_count_dist_wide` (saved), `bg_exclusive_dist` (saved), `bg_count_dist_pop` |
| `09_copula_result_graphs.R` | Visualise 08's simulation output: selected-country comorbidity-count distributions, a 2+ comorbidity heatmap, the population-weighted global average, and the exclusive-category breakdown | `bg_count_dist_wide`, `bg_count_dist_pop`, `bg_exclusive_dist` (08) | `fig_heatmap_2plus.jpg`, `fig_global_stack.jpg`, `fig_global_stack_cat.jpg` |
| `10_background_burden_graphs.R` | Visualise between-country prevalence distribution by condition/age, and population-by-comorbidity-count by world region | `bg_prev_country_wide` (07), `bg_count_dist_wide` (08), `theme_lancet_gbd()`/`count_palette` (09) | `fig_comorbidity_age_by_country.jpg`, `fig_region_comorb_share.jpg`, `fig_region_comorb_stack.jpg` |

**Note**: the code that multiplies 08's background-prevalence distribution by 03's RR to
produce a final "population-level excess burden" estimate does not exist yet anywhere in
this chain. That combination step is unfinished.

`theme_lancet_gbd()` (defined in `09_copula_result_graphs.R`) is deliberately named
differently from `03_relative_risk.R`'s own `theme_lancet_clean()` — they are different
functions, and giving them different names means they can never silently shadow each
other in the shared session.

---

## 5. Side-branch scripts after the main RR script (`side_analyses/`)

| Script | Objects it needs | What it does |
|---|---|---|
| `side_analyses/table_basic.R` | `analysis_df`, `ind`, `comorb_cols` | Table 1 (baseline characteristics), missingness table, condition composition table -> `03_Output/tables/chik_sinan_descriptive_tables.xlsx`. **Currently does not run** (see section 6) |
| `side_analyses/hosp_rate_validation.R` | `analysis_df` | Computes the observed SINAN hospitalisation rate (10-year bands, Wilson CI) as a validation check against 03's model predictions -> `01_Data/brazil_observed_hosp_by_age.rds` |

These live in their own subfolder (no numeric prefix) because they are not part of the
main sequential chain — either one can be run any time after `03_relative_risk.R`, in
either order, and neither is needed by any other script.

---

## 6. Known issues (checklist for the next round of per-script fixes)

~~1. `comorb_count_group` level mismatch in the GLM-based RR code.~~
~~2. `rr_hosp_model.RData`/`rr_death_model.RData` saved twice, buggy GLM version winning.~~
**Fixed 2026-09**: the entire GLM-based RR pass was removed from `03_relative_risk.R`
during cleanup (see section 3) rather than patched, since the GAM-based RR already in
the file made it fully redundant. The same `"2"` vs `"2+"` typo was also found and
fixed in `get_crude_rr()`'s call site at the bottom of the script.

~~3. `06_corr_matrix.R` <-> `07_morbid_prob_calc.R` circular dependency.~~
**Fixed 2026-09**: what was one 570-line file (`06_corr_matrix.R`, mixing GBD/hypertension
loading, the Brazil correlation matrix, and population plots that needed the *next*
script's output) and one 1300-line file (`07_morbid_prob_calc.R`, mixing a correlation
heatmap, GBD age-band alignment, the copula simulation *duplicated as two near-identical
implementations run back to back*, and several rounds of result plots) have both been
split apart into the 7 single-purpose scripts in section 4. The chain is now strictly
linear (04 -> 05 -> ... -> 10), nothing needs to be re-run, and the copula simulation now
runs once per country/age-band row instead of twice.

~~4. `07_morbid_prob_calc.R` referenced an undefined object (`p_heatmap_3plus`).~~
**Fixed 2026-09**: this bug turned out to be two separate typos next to each other. (a)
`ggsave()` saved a different, undefined object (`p_heatmap_3plus`) instead of the one
actually built (`p_heatmap_2plus`) — now in `09_copula_result_graphs.R`, fixed. (b) That
`p_heatmap_2plus` object itself referenced a non-existent column, `prev_comorb_3plus` —
`bg_count_dist_wide` only ever had `prev_comorb_0/1/2plus` (the pipeline moved from a
4-group to a 3-group comorbidity-count scheme at some point and this plot wasn't updated)
— also fixed, to `prev_comorb_2plus`. A third instance of the same 3-vs-4-group drift was
found and fixed in the same pass: `08_copula_simulation.R`'s two duplicate
`simulate_count_distribution()` implementations disagreed with each other (one used
3 groups, one used 4) — the merged version now uses 3 groups throughout, matching
`comorb_count_group` everywhere else in the pipeline.

~~5. `06_morbid_prob_calc.R` had a stray `selected_iso3_heatmap` typo.~~
**Fixed 2026-09**: `selected_iso3_heatmap` was used but never defined; the actually-defined
object a few lines above was `selected_iso3`. Now in `09_copula_result_graphs.R`, fixed.

6. **`side_analyses/table_basic.R` does not run** — it needs a `"3+"` column and a
   `comorbidity_labels` object, but `03_relative_risk.R` only produces 3 groups and
   never defines `comorbidity_labels` anywhere. Not yet fixed.

---

## 7. Why `archive/`, `descriptive_appendix/`, and `side_analyses/` are separated out

- **`archive/`**: files no longer executed.
  - `02_Relative_risk.R` — superseded draft of `03_relative_risk.R` (GLM only, old
    4-group scheme)
  - `01_Descriptive.R` — exploratory scratch code that was pasted into the console
    (produces no saved output)
  - `fetch_ibge_population.R` — downloads IBGE municipality population. Kept together
    with its outputs (`ibge_pop_muni_year_*`) because no analysis script reads them.
    Pull it back out if a municipality-level denominator is needed again.
- **`descriptive_appendix/`**: `descriptive.R` — a standalone descriptive analysis
  unrelated to the main RR pipeline. Its own cache lives at
  `01_Data/descriptive_appendix/chik_analytic.rds`.
- **`side_analyses/`**: `table_basic.R`, `hosp_rate_validation.R` — not superseded, not
  independent of the main pipeline (both need `analysis_df`/`ind` from 03), but not part
  of its linear chain either. See section 5.

---

## 8. Data file summary (`01_Data/`)

| File | Produced by | Consumed by |
|---|---|---|
| `chik_sinan_individual_2015_2024.rds` | 02 | 03 (and, via 03, side_analyses/ and 05) |
| `gbd_pop.csv`, `gbd_prevalence.csv`, `ncd_hypertension.csv` | (external download, no script) | 04 |
| `rr_hosp_model.RData`, `rr_death_model.RData` | 03 | (not yet re-read by anything — intended to be combined with 08's output later) |
| `brazil_observed_hosp_by_age.rds` | `side_analyses/hosp_rate_validation.R` | (none, validation output only) |
| `bg_count_dist_wide.RData`, `bg_exclusive_dist.RData` | 08 | 09, 10 |
| `01_Data/archive/*`, `01_Data/descriptive_appendix/*` | see section 7 | — |

Raw data (`sinan_chik_csv/`) and most derived data (`*.RData`,
`chik_sinan_individual_*`, `chik_brazil_muni_*`, `ibge_pop_muni_year_*`,
`01_Data/archive/`, `01_Data/descriptive_appendix/*.rds|csv`) are excluded from git via
`.gitignore`, since all of it is regenerable from scripts. `gbd_*.csv`,
`ncd_hypertension.csv`, and `brazil_observed_hosp_by_age.rds` are currently not covered
by a gitignore rule, so `git add` can pick them up — worth deciding whether to commit
them given they're slow-to-regenerate external GBD source data.
