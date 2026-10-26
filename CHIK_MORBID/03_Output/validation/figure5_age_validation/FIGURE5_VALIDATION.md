# Figure 5 validation: age-specific multimorbidity–mortality associations

Brazil SINAN confirmed chikungunya, 2017–2025; core-3 joint profiles (DM, HTN, CKD). Produced by `02_Script/46_figure5_age_profile_validation.R` (analysis) and `02_Script/47_figure5_age_profile_figures.R` (figures). This is an audit and figure-refinement exercise. No original analysis, cache, figure or the manuscript was modified.

**Original Figure 5** = death row of `03_Output/figures/fig3_profile_age_specific_relative_risk.pdf/.png`, produced by `12_core3_condition_specific_rr.R` section 12 and cached in `01_Data/core_profile_rr_age_curve.RData`.

---

## 0. Cohort reconciliation and reproduction

| | Value |
|---|---:|
| Eligible core-3 population (before outcome filter) | 1,074,796 |
| **Mortality cohort actually used by Figure 5 (outcome known)** | **983,947** |
| Chikungunya-attributed deaths | 1,050 |
| Crude mortality risk | 0.107% |

The brief quotes N = 1,074,796. That is the eligible population *before* the outcome filter. Since the Gate 3.1 outcome correction (EVOLUCAO 4/9/blank recoded to unknown rather than survived), script 12's death cohort is 983,947 records with the same 1,050 deaths. This audit uses that current cohort; the script stops if either number drifts.

The original fs model was refitted with an identical formula. It reproduces the cached Figure 5 curve to a maximum relative difference of 4 × 10⁻¹¹ across all 760 profile × age points (`original_model_reproduction_check.csv`). Every result below therefore refers to the exact published model.

---

## 1. Empirical support (Task 1)

Deaths / cases per age group × profile (`age_profile_support.csv`; full counts, crude risk with Clopper–Pearson 95% CI, share of cohort, and observed vs model-expected deaths):

| Profile | 0–14 | 15–39 | 40–59 | 60–74 | ≥75 | Total deaths |
|---|---|---|---|---|---|---:|
| None | 83 / 107,764 | 113 / 374,825 | 98 / 257,921 | 91 / 89,825 | 137 / 26,007 | 522 |
| DM | 0 / 373 | 5 / 2,679 | 7 / 6,895 | 9 / 4,207 | **32** / 1,148 | 53 |
| HTN | 0 / 557 | **10** / 8,456 | **34** / 35,152 | **52** / 25,723 | **132** / 9,624 | 228 |
| CKD | 1 / 167 | 1 / 536 | 0 / 559 | 0 / 269 | **10** / 109 | 12 |
| DM+HTN | 0 / 137 | 5 / 1,163 | **23** / 10,113 | **67** / 12,671 | **86** / 4,295 | 181 |
| DM+CKD | 0 / 10 | 0 / 50 | 0 / 120 | 0 / 77 | 4 / 56 | 4 |
| HTN+CKD | 0 / 18 | 1 / 107 | 3 / 332 | 6 / 252 | **12** / 215 | 22 |
| DM+HTN+CKD | 0 / 97 | 0 / 414 | 3 / 452 | 8 / 383 | **17** / 189 | 28 |

Bold marks cells with ≥10 deaths. This is a screening heuristic, not a validity threshold.

**Support classification of the 35 non-reference cells:**
- 11 adequately supported (≥10 deaths)
- 12 sparse (1–9 deaths)
- 12 with no observed deaths

HTN at 15–39 sits exactly on the threshold (10 deaths) and should be read as borderline.

### Key sparsity findings
- **Ages 0–14:** one death in total across all 7 non-None profiles (CKD). No profile contrast is empirically supported in children.
- **DM+CKD:** 4 deaths in the entire cohort, all at age ≥78 (`profile_age_and_death_age_distribution.csv`). No age-specific DM+CKD estimate is supported anywhere.
- **CKD alone:** 12 deaths; 10 are at ≥75 years (median death age 83.5). Below 75, CKD has 2 deaths in 1,531 cases.
- **HTN+CKD and DM+HTN+CKD:** reach ≥10 deaths only at ≥75 years. They have 1–8 deaths per group at 40–74 and none below 40 (DM+HTN+CKD).
- **DM alone:** reaches ≥10 deaths only at ≥75; it has 5–9 deaths in each of 15–39, 40–59 and 60–74.

### Age-boundary effects
Moving the upper boundary from 75 to 80 (script 12's `0–19 … 80+` bands) moves DM (24 deaths) and DM+HTN+CKD (15 deaths) into an adequately supported 60–79 group. However, it pushes CKD (9) and HTN+CKD (9) at 80+ below the threshold. The number of adequate cells is 11 under both schemes, but the schemes support different profiles (6 profiles have ≥1 adequate cell under the primary scheme vs 4 under script 12's bands; `age_scheme_support_summary.csv`). Merging to `0–59 / 60–74 / ≥75` also gives 11 adequate cells, adding DM 0–59 (12 deaths) while losing HTN 15–39 and the separate 40–59 cells. No grouping rescues CKD-containing profiles below 75 years.

### Data-quality flag (not resolved here)
97 children aged 0–14 are recorded as DM+HTN+CKD, versus 10 as DM+CKD and 18 as HTN+CKD. Such triple-morbidity counts in children are implausible. They are not concentrated in one state-year (the largest single state-year holds 13%; `child_multimorbid_top_state_years.csv`), which points to a form-completion artefact rather than a local outbreak cluster. None of these children died, so the issue does not affect any supported estimate. It does feed the model-based childhood curves.

---

## 2. Audit of the original age-interaction GAM (Task 2)

**Exact specification** (script 12 §12):

```r
death_only ~ core_profile + sex + year + s(uf_residence, bs = "re") +
             s(age_years, core_profile, bs = "fs", k = 10)
# mgcv::bam, binomial logit, method = "fREML", discrete = TRUE
```

- **Age basis:** one thin-plate regression spline curve per profile (`k = 10`, 8 curves, 31.1 edf for the age terms in total). All curves share one set of smoothing parameters; `fs` penalises the null space, so each curve is shrunk as a random-effect-like term.
- **No global age smooth.** The `fs` penalty therefore shrinks each profile's own log-odds-by-age curve toward a *flat line*, not toward the common (None) age shape. For a profile with few deaths, the fitted curve is pulled toward flat while None's curve follows the steep, U-shaped age gradient of mortality. The resulting RR (profile / None) is then roughly the inverse of None's age shape, which predicts exactly the artefacts seen:
  - CKD: RR 16.5 at age 10, falling to 1.3 at age 45
  - DM+CKD: RR 19.6 at age 10, falling to 2.4 at age 45
  - Both rest on 0–1 deaths at those ages
- **Uncertainty:** `MASS::mvrnorm` draws (1,000) from `vcov(fit, unconditional = TRUE)`. Percentile intervals, with the profile/None ratio taken within each draw, so covariance is preserved. This is correct.
- **Standardisation:** the original curve standardises over the *whole-cohort* sex × year × state distribution at every age. It does not use the age-specific covariate distribution, so the curve is not an age-group-specific standardised estimate.
- **Extrapolation** (`original_curve_local_support_summary.csv`, `Supplement_original_curve_support_overlay.png`), ages 1–95 of the published curve lying outside the profile's observed death-age range or with zero deaths within ±5 years:

  | Profile | Ages with no local deaths / outside death-age range |
  |---|---:|
  | DM+CKD | 77 / 95 |
  | CKD | 54 / 95 |
  | DM+HTN+CKD | 53 / 95 |
  | HTN+CKD | 38 / 95 |
  | DM+HTN | 23 / 95 |
  | DM | 19 / 95 |
  | HTN | 19 / 95 |

  Only DM (ages 71–89), HTN (36–95), DM+HTN (50–94) and DM+HTN+CKD (74–86) have any curve segment with ≥10 deaths within ±5 years.

### Interaction evidence (`age_interaction_diagnostics.csv`, `age_interaction_wald_tests_by_profile.csv`)

| Model | AIC | Total edf | ΔAIC vs no interaction | Approx. LRT p | Wald p, age-varying log-OR, ages 20–90 | Same, ages 1–95 |
|---|---:|---:|---:|---:|---:|---:|
| No interaction, s(age, k=10) | 13,739 | 43.6 | 0 | – | – | – |
| No interaction, s(age, k=20) | 13,735 | 47.5 | −3.2 | 0.07 | – | – |
| **Original fs (k=10)** | 13,724 | 66.4 | −14.8 | 0.0007 | 0.47 | 0.09 |
| Global s(age) + fs deviations (m=1) | 13,706 | 66.7 | −32.7 | <0.0001 | 0.67 | 0.12 |
| fs, k = 5 | 13,759 | 56.4 | +19.9 | 0.99 | 0.001 | 0.001 |
| fs, k = 15 | 13,723 | 69.0 | −16.1 | 0.0006 | 0.51 | 0.59 |
| fs, smoothing ×¼ (fixed) | 13,716 | 75.0 | −22.9 | – | 0.20 | 0.05 |
| fs, smoothing ×4 (fixed) | 13,739 | 57.6 | 0.0 | – | 0.62 | 0.41 |
| Linear age × profile slopes | 13,738 | 50.6 | −1.0 | 0.04 | 0.07 (7 df) | – |
| Age-group × profile random effect | 13,735 | 56.5 | −3.3 | 0.03 | – | – |

Notes on the tests:
- The Wald test asks whether each profile's log-OR vs None is constant across age. It uses the Bayesian covariance `Vp`, which accounts for penalisation, and contrasts ages 20, 30, …, 90 against age 60.
- LRT p-values are `anova.gam` approximations and are anticonservative for penalised fits.
- The random-effect model's SD for profile × age-group deviations is 0.33 on the log-odds scale (95% CI 0.15–0.73).

### Interpretation
- **AIC and LRT favour the interaction models, but the gain does not localise to adult ages.** No penalisation-aware test detects departure from a constant OR across ages 20–90, for any profile or jointly (p = 0.47 original; 0.67 GS). Over ages 1–95 the joint test is borderline (p ≈ 0.09–0.12).
- **Most of the AIC gain is plausibly from childhood.** The no-interaction model predicts about 8 deaths among comorbid children aged 0–14 against 1 observed; the fs model predicts about 4 (`age_profile_support.csv`, `expected_deaths_*`). In other words, the interaction models mainly learn that comorbid children did not share None's elevated childhood mortality. That contrast rests on a very small number of expected events in a group with a recording-quality concern (§1).
- **Interaction evidence is specification-dependent.** k = 5 gives a *worse* AIC than no interaction but a "significant" Wald test, because an inadequate age basis leaks misfit into the profile contrasts. A 4× smoother fit removes the AIC advantage entirely. The coarser random-effect and linear-slope interactions improve AIC by only 1–3 points, which is the same size as simply giving the shared age smooth more flexibility (k = 20: −3.2).
- **Lower AIC is not evidence of clinically meaningful effect modification here.** There is no robust evidence that the profile–mortality RR changes with adult age.

### Convergence and uncertainty diagnostics
All 11 fits converged with no warnings. The only exception is mgcv's expected informational message for the GS model ("repeated 1-d smooths of same variable"), which is intrinsic to that specification. `Vp` is positive definite for every model (minimum eigenvalue ≥ 5 × 10⁻⁵). `k.check` returns k-index ≈ 0.89 (p = 0.005) for every model. For Bernoulli outcomes with ~0.1% prevalence this residual-based check is not informative; the k = 10 vs k = 15 comparison is the more useful basis check, and RRs change by ≤2% between them.

---

## 3. Age-group standardised estimates (Task 3)

`age_profile_standardised_estimates.csv` contains the original fs model with 2,000 coefficient draws. Within each age group, every profile is standardised over the observed joint (exact age × sex × year × state) distribution of all cohort members in that group. The same target applies to all profiles, and risk, RR and RD are computed within the same draw.

Estimates for every cell are retained for transparency, with a status column:

| Status | Count |
|---|---:|
| Reported (profile cell ≥10 deaths) | 11 |
| Flagged: sparse (1–9 deaths) | 12 |
| Suppressed: no observed deaths | 12 |

None reference cells have 83–137 deaths in every group. Every exclusion and suppression is listed in `suppression_and_exclusion_log.csv`.

**Reported cells** (standardised risk, RR and RD vs None, 95% CI):

| Age | Profile | Deaths / cases | Standardised risk, % | RR | RD, percentage points |
|---|---|---|---|---|---|
| 15–39 | HTN (borderline) | 10 / 8,456 | 0.13 (0.07–0.26) | 4.6 (2.5–9.5) | 0.10 (0.05–0.23) |
| 40–59 | HTN | 34 / 35,152 | 0.10 (0.08–0.14) | 2.6 (1.8–3.7) | 0.06 (0.03–0.10) |
| 40–59 | DM+HTN | 23 / 10,113 | 0.25 (0.18–0.40) | 6.4 (4.2–10.2) | 0.21 (0.14–0.36) |
| 60–74 | HTN | 52 / 25,723 | 0.20 (0.16–0.26) | 2.0 (1.5–2.7) | 0.10 (0.05–0.16) |
| 60–74 | DM+HTN | 67 / 12,671 | 0.52 (0.42–0.66) | 5.2 (3.9–6.9) | 0.42 (0.32–0.55) |
| ≥75 | DM | 32 / 1,148 | 2.4 (1.8–3.5) | 4.7 (3.3–7.0) | 1.9 (1.2–3.0) |
| ≥75 | HTN | 132 / 9,624 | 1.4 (1.2–1.7) | 2.8 (2.2–3.6) | 0.9 (0.7–1.2) |
| ≥75 | CKD | 10 / 109 | 6.4 (3.6–11.1) | 12.5 (6.8–22.1) | 5.8 (3.1–10.5) |
| ≥75 | DM+HTN | 86 / 4,295 | 2.3 (2.0–2.9) | 4.6 (3.6–5.9) | 1.8 (1.4–2.4) |
| ≥75 | HTN+CKD | 12 / 215 | 4.6 (2.9–7.9) | 9.1 (5.5–15.8) | 4.1 (2.4–7.4) |
| ≥75 | DM+HTN+CKD | 17 / 189 | 8.4 (5.6–12.9) | 16.6 (10.5–25.8) | 7.9 (5.1–12.4) |

None reference risk: 0.084%, 0.028%, 0.039%, 0.099% and 0.51% for 0–14 through ≥75.

These are **model-based standardised risks**, not observed mortality rates. Observed crude risks are given alongside in `age_profile_support.csv`.

Sparse-cell estimates can look striking but rest on a handful of deaths, for example HTN+CKD 40–59 with RR 27 based on 3 deaths. Suppressed-cell estimates are pure model output: DM+HTN at 0–14 has an RR of 5.7 with 0 deaths among 137 cases. Neither kind should be reported as findings.

---

## 4. Robustness (Task 5)

`age_profile_robustness_estimates.csv`, `robustness_vs_primary_cellwise.csv`, `robustness_summary.csv`, `age_profile_rr_ratio_contrasts.csv`, `Supplement_age_profile_robustness.png`.

### Spread of RRs in the 11 reported cells
| Alternative | Range of RR (alternative / primary) |
|---|---|
| fs k = 15 | 0.99–1.02 |
| fs, smoothing ×¼ | 0.97–1.06 |
| fs, smoothing ×4 | 0.87–1.07 |
| GS model | 0.88–1.06 |
| Refit excluding ages 0–14 | 0.94–1.04 |
| Age-group random effect | 0.80–1.21 |
| fs k = 5 | 0.88–1.20 |
| Linear interaction | 0.46–1.27 |
| No interaction | 0.56–1.28 |

The linear-interaction extreme is HTN at 15–39. In every specification, every reported RR has a 95% CI excluding 1 (0 significance changes). Conclusions for reported cells are therefore not materially altered.

### Sparse and suppressed cells
These are highly specification-dependent: the median |log RR| shift is 0.25–0.49 for most alternatives (fs k = 15 is the only stable one), with up to 3–7 significance changes per specification. They are not robust.

### Merging age groups
Merging (`merged_4`, `merged_3`, `merged_2`, script 12 bands) leaves the reported-cell RRs unchanged, since they come from the same fitted model. It adds a few adequate cells (DM 0–59: RR 3.9; DM+HTN 0–59: 9.6). However, wide groups such as 0–59 average over a 50-year age range in which the None baseline varies roughly threefold.

### Is there age-varying association?
RR at ≥75 relative to RR at 60–74 (the two best-supported groups), within draw:

| Profile | fs | GS | Random effect | Linear |
|---|---|---|---|---|
| HTN | 1.42 (0.98–2.03) | 1.43 (0.98–2.06) | 1.33 (0.89–1.94) | 1.06 (0.91–1.24) |
| DM+HTN | 0.88 (0.62–1.28) | 0.87 (0.60–1.27) | 0.87 (0.57–1.32) | 0.87 (0.73–1.04) |

Neither profile shows age modification. The ratios of HTN+CKD (0.37–0.70) and DM (1.1–2.0) depend on the specification, and their 60–74 comparison cells are sparse (6 and 9 deaths).

### Absolute risk
The robust age pattern is absolute: risk differences versus None rise about 9–15-fold from 40–59 to ≥75 in the profiles supported at both ages (DM+HTN: 0.21 → 0.42 → 1.84 percentage points; HTN: 0.06 → 0.10 → 0.92), because baseline risk rises steeply with age while RRs stay broadly similar.

---

## 5. Conclusions and recommendation

### Remain supported
- HTN and DM+HTN are associated with higher chikungunya-attributed mortality in every adult age group with adequate data (HTN RR ≈ 2–3 at 40+; DM+HTN RR ≈ 5–6). These are robust to all specifications.
- At ≥75 years every profile with adequate data has elevated mortality risk. CKD-containing profiles have the largest RRs (CKD 12.5, HTN+CKD 9.1, DM+HTN+CKD 16.6) and absolute risks of 4.6–8.4%.
- Absolute excess mortality is concentrated in older adults.

### Unreliable
- **All profile contrasts at 0–14 years.**
- **All DM+CKD age-specific estimates.**
- **CKD, HTN+CKD and DM+HTN+CKD below 75 years.**
- **DM below 75 years.** Each age group has 5–9 deaths; DM is only interpretable if 0–59 is merged.
- **Specific curve features in the original figure:**
  - the childhood peaks for CKD and DM+CKD
  - the CKD and DM+CKD trough at 40–60
  - the HTN+CKD peak at ~60
  - the early-life rises for DM, HTN and DM+HTN

  These sit in regions with zero or 1–9 local deaths, and are at least partly an artefact of shrinking each profile's curve toward flat rather than toward the common age shape (§2).

### Robustness of the age-varying associations
Not robust. AIC favours flexible interaction models, but penalisation-aware tests find no evidence that RRs change across adult ages. The evidence that does exist is concentrated in childhood, depends on the basis and smoothing choices, and is not reproduced by simpler interaction specifications. The data support "RRs broadly similar across adult ages; absolute risk differences increase sharply with age". They do not support "the association is modified by age".

### Recommended for the manuscript
- **Main text:** Figure 5A (age-group forest plot), showing reported cells, sparse cells de-emphasised, and "0 deaths" cells labelled rather than estimated. Optionally combine it with Figure 5B as a second panel to show the absolute-risk gradient, which is the clinically important message.
- **Supplement:** the support heatmap.
- **Methods:** state that age-group estimates come from the age-interaction model but that interaction evidence was weak and specification-dependent.

### Original continuous curves
Retain them **only in the Supplement**, if at all, and then:
- (a) show them with local-support shading, or truncated to supported age ranges, as in `Supplement_original_curve_support_overlay`; and
- (b) preferably refit them with a global age smooth plus fs deviations (`s(age) + s(age, profile, bs = "fs", m = 1)`), which fits better (AIC −18 vs the original) and removes most of the unsupported childhood peaks.

DM+CKD should not be presented as a curve at all.

---

## 6. Files

| File | Content |
|---|---|
| `age_profile_support.csv` | Task 1 support table (primary scheme first, plus 4 alternative schemes), with exact CIs and observed vs expected deaths |
| `age_profile_standardised_estimates.csv` | Task 3 estimates with status flags |
| `age_interaction_diagnostics.csv` | Task 2: formulas, AIC, edf, LRT, Wald, convergence, `Vp`, `k.check` |
| `age_interaction_wald_tests_by_profile.csv`, `linear_age_interaction_coefficients.csv` | Per-profile interaction tests |
| `age_profile_robustness_estimates.csv`, `robustness_vs_primary_cellwise.csv`, `robustness_summary.csv`, `age_profile_rr_ratio_contrasts.csv` | Task 5 |
| `profile_age_and_death_age_distribution.csv`, `age_profile_support_5yr_bins.csv`, `age_scheme_support_summary.csv` | Age distributions and boundary diagnostics |
| `original_curve_local_support_audit.csv` / `_summary.csv`, `fs_global_curve_points.csv` | Extrapolation audit of the original curves; GS alternative |
| `original_model_reproduction_check.csv`, `mortality_cohort_check.csv` | Reproduction checks |
| `suppression_and_exclusion_log.csv`, `child_multimorbid_top_state_years.csv` | Suppressions; data-quality probe |
| `Figure_5A_age_stratified_RR.*`, `Figure_5B_age_stratified_absolute_risk.*` | PDF, SVG, 600 dpi PNG |
| `Supplement_age_profile_support_heatmap.*`, `Supplement_original_curve_support_overlay.*`, `Supplement_age_profile_robustness.*` | PDF, PNG |
| `primary_standardisation_draws.rds` | Primary draw-level risks (for any further contrasts) and `sessionInfo` |

**Reproduce:** from the repo root, `Rscript 02_Script/46_figure5_age_profile_validation.R` then `Rscript 02_Script/47_figure5_age_profile_figures.R`. This takes about 8 minutes, and all seeds are fixed.
