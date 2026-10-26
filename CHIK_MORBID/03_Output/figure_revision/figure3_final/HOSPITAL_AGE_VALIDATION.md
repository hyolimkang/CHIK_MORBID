# Hospitalisation age-specific validation

**Script:** `02_Script/52_hospital_age_validation.R` (~20 min). All outputs are in this folder.

**Design:** the hospitalisation counterpart of the mortality audit (`03_Output/validation/figure5_age_validation/FIGURE5_VALIDATION.md`). It uses the same age groups, profiles, g-computation and coefficient-draw uncertainty. No earlier analysis or figure was modified.

## 1. Cohort reconciliation (`cohort_reconciliation.csv`)

All counts were verified directly from `01_Data/chik_sinan_individual_2015_2025.rds` using script 12's cohort rules. The script stops if any count drifts.

| | Expected | Observed |
|---|---:|---:|
| Core-3 eligible population | 1,074,796 | 1,074,796 |
| Hospitalisation outcome-known cohort | 776,072 | 776,072 |
| Hospitalisations | 25,363 | 25,363 |
| Hospitalisation outcome missing | – | 27.79% of eligible |
| Mortality outcome-known cohort | 983,947 | 983,947 |
| Chikungunya-attributed deaths | 1,050 | 1,050 |

The figure 1,074,796 is used only as the eligible population. It is never used as a mortality denominator.

## 2. Empirical support (`hospital_age_profile_support.csv`, `hospital_age_profile_support_heatmap.*`)
Each cell reports cases, hospitalisations and non-hospitalisations, crude risk with a Clopper–Pearson 95% CI, age distribution within the cell, number of state-years represented, the share of the age-group standardisation target in state-years where the profile is observed (overlap), and model-expected hospitalisations under H0–H3.

**Support at alternative thresholds (40 cells):**

| Threshold | Cells meeting it |
|---|---:|
| ≥5 events | 37 |
| ≥10 events | 34 |
| ≥20 events | 28 |
| ≥50 events | 19 |

The smallest non-event count in any cell is 7.

- **"Adequate" (≥10 hospitalisations and ≥10 non-hospitalisations): 34/40.** Like the mortality rule, this is a screening heuristic, not a validity threshold.
- **Sparse (1–9 events): 6 cells, and no cell has zero events.** The sparse cells are DM+CKD in every age group (1, 1, 8, 8 and 9 events) and HTN+CKD at 0–14 (1 event). The exact CIs on DM+CKD's crude risk span roughly 3–4-fold, e.g. 9.6–34.6% at ≥75.
- **Overlap:** every adequate cell has ≥66% of its standardisation target in state-years where that profile is observed. The lowest values are in children: DM+HTN+CKD 0.66 and DM+HTN 0.72 at 0–14. Sparse DM+CKD cells fall to 0.29–0.78.

### Children (0–14)
The 0–14 group is **well supported for hospitalisation, unlike mortality.** It contains 6,492 hospitalisations in the None profile, and 10–32 events per cell for DM, HTN, CKD, DM+HTN and DM+HTN+CKD.

The None baseline is strongly age-graded within childhood (`hospital_childhood_detail.csv`):

| Age | <1 | 1–4 | 5–9 | 10–14 |
|---|---:|---:|---:|---:|
| Crude hospitalisation risk | 19.8% | 12.4% | 7.1% | 4.6% |

**Data-quality warning for comorbidity in children.**
- Hypertension is recorded in 216 infants (3.4% of all infant cases) and diabetes in 74 (1.2%). Both are far above any plausible prevalence in infancy.
- Comorbid infants have *lower* crude hospitalisation (HTN 5.1%, DM 9.5%) than infants without comorbidity (19.8%).
- Together with the 85 children coded DM+HTN+CKD, this strongly suggests that recorded comorbidity in young children is substantially misclassified. A misclassified "comorbid" infant is likely an ordinary infant, and ordinary infants are hospitalised at high rates regardless of comorbidity.

**Conclusion:** childhood *baseline* hospitalisation risk is a robust, clinically important result and is shown in the main figure. Childhood *profile contrasts* are not interpretable as multimorbidity effects and are shown only in the Supplement.

## 3. Models and validation

All models include sex, year and `s(state, bs = "re")`, and are fitted with `bam(fREML, discrete)` as in script 12.

| Model | Specification | AIC | ΔAIC vs H0 | edf | Wald p, adult 20–90 | Wald p, childhood vs young adult |
|---|---|---:|---:|---:|---:|---:|
| H0 | profile + s(age, k=10) | 204,543 | 0 | 51.3 | – | – |
| H0, k=20 | richer common age smooth | 204,510 | −33 | 57.4 | – | – |
| **H1** (original) | profile + s(age, profile, fs, k=10) | 204,332 | −211 | 79.0 | 4 × 10⁻¹¹ | 3 × 10⁻²¹ |
| **H2** (selected) | profile + s(age, k=10) + s(age, profile, fs, k=10, m=1) | **204,302** | **−241** | 81.9 | 5 × 10⁻¹⁰ | 1 × 10⁻¹⁴ |
| H3 | profile + age group + s(age) + s(profile × age group, re) | 204,365 | −178 | 74.6 | RE SD 0.29 (0.20–0.42) | – |
| H3, fixed | unpenalised profile × age group | 204,371 | −172 | 83.2 | joint p = 1 × 10⁻²⁶ (28 df) | – |
| Linear | profile-specific linear age slopes | 204,467 | −76 | 58.3 | joint p = 1 × 10⁻¹⁵ (7 df) | – |
| H1, k = 5 / k = 15 | | | −68 / −253 | | 4 × 10⁻⁵² / 3 × 10⁻¹¹ | |
| H2, k = 5 / k = 15 | | | −214 / −259 | | 5 × 10⁻³¹ / 3 × 10⁻⁹ | |
| H2, smoothing ×¼ / ×4 (fixed) | | | −242 / −224 | | 2 × 10⁻¹² / 6 × 10⁻⁷ | |

Notes:
- The Wald tests use the Bayesian covariance Vp, which accounts for penalisation. They test whether each profile's log-OR vs None is constant across ages, i.e. the same tests as the mortality audit.
- All 15 fits converged with no warnings, apart from mgcv's expected informational note for the GS (H2) specification ("repeated 1-d smooths of same variable").
- Vp is positive definite in every fit. `k.check` (seeded) shows k-index ≈ 0.91 with p ≥ 0.64 for all models, so there is no evidence of an inadequate basis.
- H1 reproduces the cached hospitalisation curve in `01_Data/core_profile_rr_age_curve.RData` to a relative difference of 2 × 10⁻¹² (`hospital_H1_reproduction_check.csv`).

### Three distinct age effects, kept separate
1. **Baseline risk varies with age, strongly and non-linearly.**
   - For None the curve is U-shaped: 7.6% at 0–14, 2.4% at 15–39, 2.0% at 40–59, 2.5% at 60–74 and 4.9% at ≥75 (H2).
   - Even a no-interaction model needs a flexible age term for this (k = 20 improves AIC by 33).
2. **Absolute excess risk associated with each profile varies with age, mostly following the baseline.**
   - Example: DM+HTN's risk difference vs None is 1.8 percentage points at 40–59 and 7.6 at ≥75.
3. **Genuine profile-by-age effect modification of the relative association is present and robust.** This is unlike mortality.
   - Every interaction specification improves AIC by 76–259 points.
   - Every penalisation-aware adult-age Wald test gives p ≤ 6 × 10⁻⁷.
   - The partially pooled H3 variance component is clearly non-zero (SD 0.29 on the log-odds scale, 95% CI 0.20–0.42).
   - The pattern, as ratios of RRs relative to 40–59 (H2, same-draw, `hospital_age_rr_ratio_contrasts.csv`):

     | Age group | Ratio of RRs vs 40–59 |
     |---|---|
     | 15–39 | 1.3–1.5 for DM, HTN and DM+HTN (CIs exclude 1) |
     | 60–74 | ~1.05–1.15 (CIs include or touch 1) |
     | ≥75 | 1.37 (HTN, 1.23–1.52), 1.54 (DM, 1.25–1.91), 1.37 (DM+HTN, 1.19–1.59), 1.94 (CKD, 1.28–2.94) |
     | 0–14 | 0.70–0.75 for HTN and DM, i.e. RR ≈ 1 |

   - **Relative associations are therefore weakest in middle age (40–74), higher in young adults and older adults, and approximately null in children.**
   - The child-specific attenuation is the part most likely to reflect comorbidity misclassification (§2) rather than biology.

### Are there artefactual peaks or troughs, as found for mortality?
**Largely no.** In `Supp_hospital_RR_curve_validation.png`, H1 and H2 curves almost coincide (cells shift ≤9%), and for DM, HTN, CKD and DM+HTN almost the whole age range has ≥10 hospitalisations within ±5 years. The young-adult RR peak (ages ~20–25) and the mid-life trough are event-supported, and they reproduce under H3, which pools by age group and has no smoothness assumption.

Exceptions:
- **DM+CKD:** sparse over its whole age range, so its curve is uninformative.
- **HTN+CKD and DM+HTN+CKD below age ~40:** these segments rest on 1–9 events.
- **Infancy:** RR < 1 at age 0–2 under H1/H2 for DM and HTN. This is where the coding problem in §2 is concentrated.

### Model selected: H2 (global age smooth + penalised profile deviations)
- **Interaction is real, so H0 is rejected.** Its constant-OR assumption visibly misfits the data. HTN at 0–14 has 73 expected hospitalisations under H0 against 32 observed; None at ≥75 has 1,101 expected against 940 observed; CKD at ≥75 has 16 expected against 28 observed. The H2 equivalents are 34, 950 and 23 (`expected_hosp_*` columns).
- **H2 has the best AIC among the k = 10 specifications,** and it shrinks profile deviations toward a common age curve rather than toward a flat line. That fixes the mechanism behind the mortality artefacts, even though for hospitalisation H1 and H2 happen to agree.
- **H2 is stable.** Against H2, the adequate cells (`hospital_age_model_robustness_summary.csv`) shift by:

  | Alternative | Maximum shift in RR |
  |---|---:|
  | H1 | 9% |
  | k = 15 | 8% |
  | Smoothing ×¼ | 8% |
  | Smoothing ×4 | 17% |
  | k = 5 | 19% |
  | H3 | median 4%, max 42% |

  The large H3 and H3-fixed differences occur only in the 0–14 profile cells.
- H2 with k = 15 has a marginally lower AIC (−18 vs H2) but adds 7.5 edf and changes adequate-cell RRs by ≤8%. The k = 10 basis is kept for consistency with the mortality and original models.

## 4. Standardised risks, RRs and RDs (selected model H2, 2,000 draws)

**Files:**
- `hospital_age_profile_standardised_risks.csv`
- `hospital_age_profile_standardised_RRs.csv`
- `hospital_age_profile_risk_differences.csv`
- `hospital_age_profile_all_model_estimates.csv` (every model)

**Method:** within each age group, all eight profiles are standardised to the same observed joint distribution of exact age × sex × year × state in the hospitalisation cohort. Risk, RR and RD are computed within shared draws from `vcov(unconditional = TRUE)`. This is identical to the mortality method.

**Retention and flagging:** all 40 cells are kept, with `estimate_status` set to Reference, Reported (adequate), or Flagged: sparse (1–9 events). There are no suppressed cells.

## 5. Missing-outcome sensitivity (`hospital_age_missingness_sensitivity*.csv`)
- **Recording completeness is flat across age** for None (68.8–72.1%). It is higher for comorbid profiles (typically 75–84%), and the gap is widest at ≥75 (None 68.8% vs comorbid 78–84%; `hospital_recording_completeness_by_age_profile.csv`).
- **Restricted refits:** H0, H2 and H3 were refitted on the Gate 3.2B high-completeness state-years:
  - C1: ≥70% recorded; 510,051 cases, 15,971 events, 66% of the cohort.
  - C2: ≥80% recorded; 337,021 cases, 10,909 events, 43% of the cohort.
  Each restricted model was compared with the full model on the restricted cohort's age-group targets (common target).
- **None, HTN, DM+HTN, and CKD below 75:** risks shift by ≤14% and RRs by ≤21%. The U-shaped baseline and the age pattern of the relative associations are preserved in both scenarios.
- **CKD-containing profiles at ≥75 are more sensitive:**

  | Profile (≥75) | Risk change, C1 | Risk change, C2 |
  |---|---:|---:|
  | CKD | −13% | −22% |
  | DM+HTN+CKD | −18% | −30% |
  | DM+CKD | −19% | −35% |

- **Limits:** this addresses selection by state/year recording completeness only. No IPW or MNAR analysis has been done (both were deferred in Gate 3.2A). Model-specification robustness (§3) and missing-outcome robustness are therefore separate claims, and the second is only partially established.
