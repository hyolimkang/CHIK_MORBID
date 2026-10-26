# Figure 4: does the specific comorbidity profile add information beyond condition count?

## Files and scripts

**Scripts:**
- `02_Script/50_figure4_count_vs_profile.R`: models, cross-validation and standardisation (~20 min)
- `02_Script/51_figure4_count_vs_profile_plot.R`: figure only, drawn from the CSVs

**Figure:** `Figure4_count_vs_profile_main.pdf/.png/.svg`

**Main tables (this folder):**
- `model_comparison_metrics.csv`: pooled out-of-fold metrics, plus paired improvements with 95% CI
- `model_comparison_fold_metrics.csv`: every metric for each fold × model × analysis
- `model_standardised_risks.csv`: models A, B and C
- `within_count_profile_contrasts.csv`

**Detail tables (`figure4/`):**
- `information_criteria.csv`
- `nested_model_tests.csv`
- `fold_consistency.csv`
- `profile_contribution_to_C_vs_B_logloss.csv`
- `model_C_vs_script12_cache_check.csv`
- `cv_out_of_fold_predictions.rds`

## Design
- **Outcomes and cohorts:** identical to the manuscript.
  - Hospitalisation: N = 776,072 (25,363 events).
  - Chikungunya-attributed death: N = 983,947 (1,050 events).
- **Adjustment set:** sex + notification year + state random effect + penalised age smooth (k = 10). This is identical to the published profile model (script 12 §8) and has no profile-by-age interaction.
- **The three representations:**
  - Model A: any of DM, HTN, CKD.
  - Model B: number of conditions (0–3).
  - Model C: the 8 profiles.
  - A ⊂ B ⊂ C are nested, so C vs B asks precisely whether *which* conditions matter once *how many* is known.
- **Fitting:** `mgcv::bam` (fREML, discrete). Full-data model C reproduces script 12's cached `gam()` estimates to within 0.01% (`model_C_vs_script12_cache_check.csv`), so Figure 4 Panel A is numerically consistent with Figure 2.
- **Cross-validation:** 5-fold, with folds dealt at random within every outcome × state × year stratum. Each fold therefore has the same event rate and state/year mix. The folds are shared by A, B and C. All models are refitted within each training fold and predict only the held-out fold, so there is no leakage.
- **Uncertainty:**
  - Log loss and Brier improvements: per-person paired differences on pooled out-of-fold predictions, normal 95% CI.
  - AUROC and AUPRC differences: stratified bootstrap, 200 replicates.
  - Fold-by-fold consistency is reported separately.
- **Standardised risks:** g-computation over each cohort's observed age × sex × year × state distribution, with 2,000 coefficient draws. Within-count contrasts are computed within draw.
- **Sensitivity:** the cross-validation is repeated after excluding every CKD-containing record (hospitalisation n = 772,333; death n = 979,535).

## Results

### In-sample fit and nested tests (`information_criteria.csv`, `nested_model_tests.csv`)
| Outcome | ΔAIC B vs A | ΔAIC C vs B | Wald: count 1 = 2 = 3 | Wald: within-count homogeneity (C vs B, 4 df) |
|---|---:|---:|---|---|
| Hospitalisation | −187 | −119 | χ² = 208, p < 10⁻⁴⁵ | χ² = 147, p < 10⁻³⁰ |
| Death | −89 | −23 | χ² = 112, p < 10⁻²⁴ | χ² = 38, p = 1 × 10⁻⁷ |

Within-count homogeneity is rejected both among the one-condition profiles (hospitalisation p < 10⁻¹²; death p = 3 × 10⁻⁵) and among the two-condition profiles (p < 10⁻¹⁹ and p = 2 × 10⁻⁴).

### Cross-validated performance (pooled out of fold)
| Outcome | Model | Log loss | Brier | AUROC | AUPRC | Calibration slope |
|---|---|---:|---:|---:|---:|---:|
| Hospitalisation | A | 0.13194 | 0.030346 | 0.7173 | 0.1160 | 1.001 |
| | B | 0.13182 | 0.030331 | 0.7182 | 0.1168 | 1.001 |
| | C | 0.13174 | 0.030322 | 0.7186 | 0.1172 | 1.001 |
| Death | A | 0.007022 | 0.0010535 | 0.8461 | 0.0222 | 1.003 |
| | B | 0.006976 | 0.0010516 | 0.8474 | 0.0260 | 1.002 |
| | C | 0.006964 | 0.0010509 | 0.8478 | 0.0274 | 1.001 |

All models are well calibrated out of sample (calibration-in-the-large ≈ 0, slope 1.001–1.003).

### Paired improvements, more detailed model minus less detailed (positive = better)
| Outcome | Comparison | Log loss reduction | Brier reduction | AUROC gain | AUPRC gain | Folds improved (log loss / Brier) |
|---|---|---|---|---|---|---|
| Hosp. | B vs A | 0.090% (0.062–0.118) | 0.047% (0.024–0.069) | +0.0009 (0.0006–0.0012) | +0.0007 (0.0004–0.0011) | 5/5, 5/5 |
| Hosp. | **C vs B** | **0.059% (0.035–0.083)** | **0.030% (0.007–0.053)** | **+0.0005 (0.0003–0.0007)** | +0.0005 (0.0000–0.0009) | 5/5, 4/5 |
| Death | B vs A | 0.66% (0.35–0.96) | 0.18% (0.01–0.35) | +0.0013 (0.0003–0.0026) | +0.0037 (0.0015–0.0069) | 5/5, 5/5 |
| Death | **C vs B** | **0.18% (0.004–0.35)** | 0.06% (−0.06 to 0.19) | +0.0004 (−0.0005 to 0.0014) | +0.0014 (−0.0012 to 0.0042) | 5/5, 4/5 |
| Hosp., no CKD | C vs B | 0.004% (−0.002 to 0.010) | 0.002% (−0.002 to 0.006) | +0.00006 (−0.00003 to 0.00015) | +0.00003 (−0.00003 to 0.0001) | 5/5, 3/5 |
| Death, no CKD | C vs B | 0.04% (−0.04 to 0.13) | 0.004% (−0.02 to 0.03) | +0.00002 (−0.0006 to 0.0006) | −0.0001 (−0.0011 to 0.0004) | 3/5, 3/5 |

Log loss and Brier reductions are relative to the comparator model.

### Where the C vs B gain comes from (`profile_contribution_to_C_vs_B_logloss.csv`)
| Outcome | CKD + HTN+CKD: share of C-vs-B log-loss gain | CKD + HTN+CKD: share of cohort |
|---|---:|---:|
| Hospitalisation | 85% | 0.28% |
| Death | 79% | 0.26% |

DM alone contributes 18% of the death gain. DM+CKD contributes −8% for death, meaning its separate estimate from 4 deaths slightly *worsens* out-of-sample prediction.

### Within-count heterogeneity (`within_count_profile_contrasts.csv`; model C, same-draw RRs)
| Contrast | Hospitalisation RR | Death RR |
|---|---|---|
| DM vs HTN | 1.14 (1.05–1.23) | 1.53 (1.14–2.07) |
| CKD vs HTN | 1.90 (1.60–2.24) | 3.17 (1.78–5.50) |
| CKD vs DM | 1.67 (1.40–1.99) | 2.08 (1.15–3.76) |
| DM+CKD vs DM+HTN | 1.76 (1.25–2.45) | 1.96 (0.71–5.05), 4 deaths |
| HTN+CKD vs DM+HTN | 2.19 (1.85–2.58) | 2.50 (1.59–3.90) |
| HTN+CKD vs DM+CKD | 1.24 (0.87–1.83) | 1.27 (0.45–3.91) |
| CKD vs average of one-condition profiles | 1.83 (1.55–2.15) | 2.83 (1.63–4.77) |
| HTN+CKD vs average of two-condition profiles | 2.09 (1.78–2.43) | 2.36 (1.56–3.56) |

In absolute terms, CKD alone carries a hospitalisation risk of 8.6%, against 4.7% for the one-condition average implied by model B, and a death risk of 0.54% against 0.19%. DM+HTN, by far the most common two-condition profile, sits slightly below its count-group average (RR 0.95–0.96).

## Answers to the six questions

**1. Does comorbidity count improve on any-comorbidity?** Yes, for both outcomes and on every metric.
- In sample: ΔAIC −187 and −89; the Wald test rejects equal risk across 1, 2 and 3 conditions (p < 10⁻²⁴).
- Out of sample: log loss improves in 5/5 folds for both outcomes, AUROC improves (hospitalisation +0.0009, death +0.0013), and every paired interval excludes 0.

Risk rises monotonically with count. Standardised RR vs none:

| | 1 condition | 2 conditions | 3 conditions |
|---|---:|---:|---:|
| Hospitalisation | 1.59 | 2.19 | 3.63 |
| Death | 2.79 | 5.48 | 16.1 |

**2. Do specific profiles improve on comorbidity count?** Yes.
- **Hospitalisation:** unambiguous. ΔAIC −119; log loss, Brier and AUROC all improve with CIs excluding 0; log loss improves in 5/5 folds.
- **Death:** weaker. ΔAIC −23 and the in-sample Wald p = 10⁻⁷, but out of sample only log loss improves (0.18%, 95% CI 0.004–0.35, lower bound near 0). The Brier, AUROC and AUPRC intervals include 0.

**3. Is the improvement statistically supported?**
- **Hospitalisation:** yes, both in-sample and out-of-sample.
- **Death:** strongly in-sample, but only marginally on held-out data. With 1,050 deaths and the information concentrated in about 4,400 CKD-containing cases (66 deaths), out-of-sample power is limited.

**4. Is it clinically meaningful?** It depends on the scale.
- **Population-level discrimination: no.** Profile detail adds less than 0.001 AUROC and under 0.2% relative log loss. A severity score would gain essentially nothing from replacing count with profile, and all models remain modest discriminators for hospitalisation (AUROC ≈ 0.72).
- **Individual-level risk assessment for CKD patients: yes.** The count model assigns a CKD-only patient the risk of an average one-condition patient, who is mostly hypertensive. That understates their hospitalisation risk by about 1.8-fold and their death risk by about 2.8-fold. HTN+CKD is likewise understated about 2-fold relative to DM+HTN. Misclassification on this scale matters for triage. Its small effect on aggregate metrics simply reflects that CKD-containing cases are under 0.6% of the cohort.

**5. Are conclusions driven mainly by rare CKD-containing profiles?** Yes.
- CKD alone and HTN+CKD supply 79–85% of the C-vs-B cross-validated gain while making up about 0.3% of each cohort.
- With all CKD-containing records excluded, the C-vs-B improvement disappears for both outcomes (CIs include 0).
- The DM-vs-HTN difference is real in the fitted models (death RR 1.53, 1.14–2.07) but does not translate into detectable out-of-sample gain.

Put simply, comorbidity count is an adequate summary for DM and HTN, but it is a poor summary whenever CKD is present.

**6. Is Figure 4 strong enough for a main manuscript figure?** Yes, but the framing should match the findings above.
- **Panel A** is the strongest element. It shows directly that DM and HTN sit on or near the count-model band while CKD-containing profiles sit far above it, and that this holds for both outcomes. It earns a main-figure place.
- **Panel B** is honest and necessary context. It also shows that the effect sizes are very small, and it includes the CKD-excluded sensitivity row, which makes the CKD-driven nature of the result visible at a glance.
- If space is tight, keep Panel A as Figure 4 and move Panel B (with `model_comparison_metrics.csv` as a table) to the Supplement.
- The manuscript should **not** claim that profiles materially improve prediction overall. The defensible claim is: *"count captures most of the gradient; CKD-containing profiles carry substantially more risk than their count implies."*

## Caveats
- **Data quality:** the CKD coding issues documented in Gate 3.3A, and the implausible triple-morbidity records among children found in the Figure 5 validation, affect exactly the profiles that drive this result. Both point toward misclassification, which would more likely dilute than create the CKD signal, but this cannot be shown with these data.
- **Different cohorts:** hospitalisation analyses use a different cohort from mortality, and about 28% of eligible records lack hospitalisation status (Gate 3.2A). That limitation applies to Figure 4 Panel A's hospitalisation row exactly as it does to Figure 2.
- **No age interaction:** the models deliberately omit profile-by-age interaction, consistent with the Figure 5 validation finding of no robust adult-age effect modification.
