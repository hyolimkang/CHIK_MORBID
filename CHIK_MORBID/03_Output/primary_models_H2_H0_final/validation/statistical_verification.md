# Statistical verification: harmonised primary models

**Scripts:**
- `62_primary_models_fit_standardise.R`: fits, draws, estimates and identities.
- `64_primary_models_figures_tables.R`: figures, table and cross-figure checks.
- `65_primary_models_verification.R`: independent re-derivation.

**Result:** 57 of 57 checks in `final_verification_checks.csv` passed.

## Cohorts (`cohort_reconciliation.csv`: 19 of 19 checks passed)

| | N | Events |
|---|---:|---:|
| Eligible core-3 population | 1,074,796 | |
| Hospitalisation cohort | 776,072 | 25,363 |
| Mortality cohort | 983,947 | 1,050 |

- The `EVOLUCAO` mapping is verified (`evolucao_mapping_check.csv`): code 2 is death, codes 1 and 3 are non-death, and all other codes are missing.
- There are eight mutually exclusive profiles.
- Profile case counts equal validated Table 1. Profile event counts equal the validated profile caches.

## Models (`primary_model_verification.csv`)

| | hospitalisation_H2 | mortality_H0 |
|---|---|---|
| Converged | Yes | Yes |
| AIC | 204,302.1 (same as the script 52 H2 model) | 13,738.6 |
| Total EDF | 81.9 | 43.6 |
| Deviance explained | 8.6% | 17.1% |
| Covariance (`vcov`, unconditional) | Positive definite; condition number 7.8 × 10⁵ | Positive definite; condition number 1.3 × 10⁵ |
| k-check, age smooths | k-index 0.90 (p = 0.17 / 0.10); age EDF 6.8 / 9; deviations EDF 32.3 / 80 | k-index 0.94 (p = 0.50); age EDF 8.4 / 9 |
| Warnings | One: "repeated 1-d smooths of same variable". This is expected for a common smooth plus factor-smooth deviations (Pedersen et al. 2019, model GS). | None |

### Reproduction of historical models

- **Hospitalisation H2:** age-specific point estimates equal script 52's H2 to within 5 × 10⁻¹⁵ (relative). CI limits differ by up to 5.5% because these are new draws (4,000 vs 2,000, different seed).
- **Mortality H0:** overall risks equal the script 28 cache to within 1 × 10⁻⁴ (relative), even though that cache was fitted with `gam(REML)` and this pipeline uses `bam(fREML, discrete)`.
- **Mortality RDs:** point estimates equal script 56 exactly.

## Standardisation identities

- **Overall vs age-specific (D).** The overall risk equals the target-weighted mean of the five age-specific risks for every profile and outcome. Maximum absolute difference: 1.4 × 10⁻¹⁶ at the point estimates and 6.1 × 10⁻¹⁶ across all 4,000 draws (`draw_level_standardisation_checks.csv`).
- **Weighted vs individual-level averaging.** Frequency-weighted g-computation equals individual-level averaging over all records. Maximum relative difference: 3.3 × 10⁻¹⁶.
- **RR (E).** RR = risk / risk(None) on unrounded values. RR CIs are quantiles of within-draw ratios, and RR(None) = 1 in every draw.
- **RD (F).** RD = 100 × [risk(a) − risk(b)] on unrounded values. RD CIs come from paired draws.
- **Figures and table (G).**
  - Figure 1 equals Table 2: maximum difference ≤ 1.8 × 10⁻¹⁵.
  - Figure 3 RDs equal differences of Table 2 risks.
  - Figure 2 cells equal the canonical age-specific estimates, from the same models and draws.
- **Numerical checks (H).**
  - All draw probabilities lie in (0, 1) and are finite.
  - Every CI contains its point estimate.
  - There are no missing or infinite values.

## Monte Carlo error (`monte_carlo_check_4000_vs_1000_draws.csv`)

- Using the first 1,000 instead of all 4,000 draws changes CI limits by at most 3.0% for profiles with ≥10 events.
- For DM+CKD mortality (4 deaths), the upper limit changes by 5.3%.

## Within-count contrasts

The six contrasts in Figure 3 use the following, from shared draws:
- **Main figure:** pointwise 95% intervals.
- **CSV only:** Bonferroni-simultaneous intervals (1 − 0.05/6) and the proportion of draws with RD > 0.

The historical Holm-adjusted Wald tests (script 56) tested profile main-effect coefficients of the no-interaction model. Under H2 the profile effect varies with age, so those tests do not apply to the H2 standardised RDs. They were **not reused**, and no new p-values are reported.

## What the intervals capture

The intervals capture within-model coefficient uncertainty, including smoothing-parameter uncertainty through `vcov(unconditional = TRUE)`. They do **not** capture:
- model-selection uncertainty (see `sensitivity_summary.md`);
- bias from missing outcome status;
- comorbidity misclassification.

Standardised associations are descriptive and not causal effects.
