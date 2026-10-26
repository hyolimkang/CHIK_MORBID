# Within-count profile contrasts: main figure or supplement?

**Figures (this folder; PDF, SVG and 600 dpi PNG):**
- `Figure4_candidate_A_count_vs_profile_risks.*`: profile-specific standardised risks against the comorbidity-count model.
- `Figure4_candidate_B_within_count_contrasts.*`: direct within-count risk differences.

**Methods and validation:** `within_count_model_validation.md`.

**Data:**
- `within_count_standardised_risks.csv`: all eight profiles plus the count model, both outcomes.
- `within_count_direct_contrasts.csv`: six contrasts per outcome; RD and RR with nominal, simultaneous and bootstrap intervals; event counts; Holm-adjusted p-values.
- `within_count_omnibus_tests.csv`.

## Key results

Standardised to each outcome cohort's age × sex × year × state distribution. Bold rows have Holm-adjusted p < 0.05.

| Contrast | Hospitalisation RD, pp (95% CI) | Hospitalisation RR (95% CI) | Death RD, pp (95% CI) | Death RR (95% CI) | Events, first / second (death) |
|---|---|---|---|---|---|
| **DM vs HTN** | **0.6 (0.2 to 1.1)** | 1.14 (1.05–1.24) | **0.09 (0.02 to 0.17)** | 1.53 (1.13–2.04) | 53 / 228 |
| **CKD vs HTN** | **4.1 (2.8 to 5.7)** | 1.90 (1.61–2.25) | **0.37 (0.14 to 0.80)** | 3.17 (1.76–5.68) | 12 / 228 |
| CKD vs DM | **3.4 (2.1 to 5.0)** | 1.67 (1.40–2.00) | 0.28 (0.02 to 0.71)\* | 2.08 (1.08–3.86) | 12 / 53 |
| DM+CKD vs DM+HTN | **4.7 (1.5 to 8.8)** | 1.76 (1.24–2.45) | 0.33 (−0.09 to 1.36)† | 1.96 (0.73–4.96) | 4 / 181 |
| **HTN+CKD vs DM+HTN** | **7.3 (5.3 to 9.5)** | 2.19 (1.85–2.56) | **0.51 (0.22 to 0.97)** | 2.50 (1.62–3.87) | 22 / 181 |
| HTN+CKD vs DM+CKD | 2.6 (−2.0 to 6.6) | 1.24 (0.86–1.82) | 0.18 (−0.90 to 0.81)† | 1.27 (0.45–3.59) | 22 / 4 |

\* Borderline: Holm p = 0.066, and the simultaneous and bootstrap intervals just include 0.
† Involves DM+CKD (4 deaths); no death had occurred in DM+CKD in 5/200 bootstrap replicates.

**Hospitalisation (bold contrasts above):** 5 of 6 contrasts have Holm p < 0.05. HTN+CKD vs DM+CKD (Holm p = 0.25) is the exception.

**Within-count omnibus tests (Wald, penalisation-aware Vp):**
- Hospitalisation: one-condition p = 1 × 10⁻¹³; two-condition p = 1 × 10⁻¹⁹.
- Death: one-condition p = 3 × 10⁻⁵; two-condition p = 2 × 10⁻⁴.

## Comparing the candidates

| Criterion | A: profile risks vs count model | B: direct risk differences |
|---|---|---|
| 1. Distinct from Figure 2? | Partly. Absolute risks of six of the eight Figure 2 profiles, re-grouped, plus the count reference. Largely the same content as the earlier `Figure4_count_vs_profile_main` Panel A. | **Yes.** Figure 2 compares each profile with None. Same-count differences, on the absolute scale and with their own uncertainty, cannot be read from Figure 2; overlapping CIs there would mislead. |
| 2. Shows within-count heterogeneity? | Visually, as departures from the count line. Points sitting off the line do not, however, test profile-vs-profile differences. | Directly: each row *is* a within-count difference. |
| 3. Clinically interpretable? | Moderately: the reader must compute differences on a log scale. | **High:** "percentage points more patients hospitalised or dying" for a given comorbidity combination at the same count. |
| 4. Uncertainty and sparse estimates clear? | Clear for each profile; DM+CKD for death is open and dashed. | **Clear for each contrast**; the two DM+CKD death contrasts are muted and dashed, and every interval is drawn in full. |
| 5. Avoids repeating the eight-profile results? | No. | Yes. |
| 6. Precise and meaningful enough for a main figure? | – | Hospitalisation: 5/6 contrasts precise, with clinically sizeable CKD-related differences (+4 to +7 pp on baselines of 4.5–6%). Death: 3/6 clearly supported, 1 borderline, 2 uncertain (DM+CKD). |
| 7. Better suited to the Supplement? | **Yes.** | Main text is defensible (see below). |

## Answers

**1. Are hospitalisation risks meaningfully different between profiles with the same number of conditions?**
Yes.
- Among single conditions, CKD carries 4.1 (2.8 to 5.7) percentage points higher standardised risk than HTN (8.6% vs 4.5%), and 3.4 pp more than DM. DM vs HTN differs only slightly (+0.6 pp).
- Among two-condition profiles, HTN+CKD exceeds DM+HTN by 7.3 (5.3 to 9.5) pp (13.4% vs 6.1%), and DM+CKD exceeds DM+HTN by 4.7 (1.5 to 8.8) pp.
- The two CKD-containing pairs (HTN+CKD vs DM+CKD) do not differ detectably.

**2. Are mortality risks meaningfully different between profiles with the same number of conditions?**
In relative terms, yes for CKD versus non-CKD profiles: RR 3.2 for CKD vs HTN and 2.5 for HTN+CKD vs DM+HTN. In absolute terms the differences are small, because death is rare: 0.4–0.5 percentage points, i.e. about 4–5 more deaths per 1,000 cases. DM vs HTN differs by 0.09 pp (about 1 per 1,000). All contrasts involving DM+CKD are uncertain.

**3. Which direct comparisons are reasonably precise?**
- All hospitalisation contrasts except HTN+CKD vs DM+CKD.
- For death: HTN+CKD vs DM+HTN, CKD vs HTN, and DM vs HTN.
- CKD vs DM (death) is borderline.

**4. Which comparisons rely heavily on rare CKD profiles?**
Every contrast involving CKD, DM+CKD or HTN+CKD. These profiles hold 0.5% of each cohort; for death they contribute 12, 4 and 22 deaths respectively.
- Both DM+CKD death contrasts are effectively uninformative.
- DM+CKD hospitalisation contrasts rest on 27 events.
- Only DM vs HTN involves no CKD, and it shows the smallest differences.

**5. Does the evidence support "comorbidity composition provides additional severity information beyond comorbidity count"?**
Yes, with a specific qualification: the additional information is almost entirely **whether CKD is present**. The count model sits close to the common profiles (HTN, DM+HTN) and substantially under-represents CKD-containing profiles. This agrees with the earlier cross-validation, where excluding CKD records removed the out-of-sample gain from profile detail.

**6. Is the contribution clinically meaningful beyond model fit?**
For hospitalisation, yes. Knowing that a patient's single condition is CKD rather than hypertension roughly doubles their standardised risk (+4 pp), and for HTN+CKD versus DM+HTN it more than doubles (+7 pp). Differences of this size matter for triage, even though they barely change population-level prediction metrics, because CKD profiles are rare. For death the relative contrasts are large, but the absolute differences are below 1 percentage point and partly imprecise.

**7. Which candidate is most distinct from Figure 2?**
Candidate B.

**8. Is a separate Main Figure 4 justified?**
Yes, conditionally, as **Candidate B**, provided that:
- (a) it replaces, rather than adds to, the earlier prediction-performance Figure 4, which moves to the Supplement;
- (b) the text frames the finding as CKD-driven within-count heterogeneity, not as a general "profiles beat counts" claim;
- (c) the death panel is described as supportive and partly imprecise.

If the main-figure budget is tight, the result fits instead in one sentence of the Results plus a Supplementary figure (Candidate B) and Supplementary table (`within_count_direct_contrasts.csv`). Figure 2's events column already signals the sparse CKD profiles.

**9. Strongest supplementary presentation, if not main:**
- Candidate B as a Supplementary figure.
- A Supplementary table of all eight profile risks, the six contrasts (RD and RR with nominal and simultaneous CIs, bootstrap intervals and event counts) and the omnibus tests.
- The cross-validated performance comparison (`model_comparison_metrics.csv`, CKD-exclusion sensitivity) as a separate methodological supplement, clearly labelled as prediction rather than risk heterogeneity.
- Candidate A as an optional additional Supplementary figure.

**10. Most defensible conclusion for CID:**
> Among patients with the same number of comorbidities, standardised risks of hospitalisation, and to a lesser and less certain extent death, differed according to which conditions were present, chiefly reflecting higher risk when chronic kidney disease was one of them. Comorbidity count therefore understates the risk of patients with CKD, whereas it summarises diabetes and hypertension reasonably well.

This is an association, not evidence of interaction or causation.

## Figure 4 title (Candidate B)
**Figure 4.** Differences in standardised risks of hospitalisation and chikungunya-attributed death between comorbidity profiles with the same number of conditions

## Figure 4 legend (draft)
> Points show standardised risk differences (percentage points) between pairs of comorbidity profiles with the same number of conditions, for (A) hospitalisation (n = 776,072; 25,363 hospitalisations) and (B) chikungunya-attributed death (n = 983,947; 1,050 deaths) among confirmed chikungunya cases, Brazil, 2017–2025. Positive values indicate higher risk in the first-named profile. Risks were estimated from penalised logistic generalised additive models including profile, age (smooth), sex, notification year and state of residence (random effect), and standardised by g-computation to the observed age, sex, year and state distribution of each outcome cohort. Each risk difference was computed within shared draws from the coefficients' sampling distribution (4,000 draws); bars denote 95% confidence intervals and values are given on the right. Open markers with dashed intervals denote contrasts involving a profile with fewer than 10 events (diabetes with chronic kidney disease, 4 deaths). All six pre-specified contrasts are shown. Profile-specific risks, risk ratios, simultaneous confidence intervals, bootstrap intervals and omnibus tests are provided in Supplementary Table S[X]. DM, diabetes mellitus; HTN, hypertension; CKD, chronic kidney disease.

## Results-ready paragraphs (draft)
**Hospitalisation.**
> Among cases with the same number of conditions, standardised hospitalisation risk differed by which conditions were present (one condition: Wald χ² = 59.4, 2 df; two conditions: χ² = 87.5, 2 df; both p < 0.001). Among single conditions, hospitalisation risk was 8.6% (95% CI 7.3–10.2) with chronic kidney disease, compared with 5.2% (4.8–5.6) with diabetes and 4.5% (4.4–4.7) with hypertension, a difference of 4.1 percentage points (2.8–5.7) between chronic kidney disease and hypertension. Among two-condition profiles, risk was 13.4% (11.5–15.7) with hypertension and chronic kidney disease and 10.8% (7.7–14.9) with diabetes and chronic kidney disease, compared with 6.1% (5.9–6.5) with diabetes and hypertension: differences of 7.3 (5.3–9.5) and 4.7 (1.5–8.8) percentage points. Risks of the two profiles containing chronic kidney disease did not differ detectably (2.6 percentage points, −2.0 to 6.6), and diabetes and hypertension alone differed by only 0.6 percentage points (0.2–1.1).

**Mortality.**
> Within-count differences in chikungunya-attributed mortality were in the same direction but smaller in absolute terms and less precise. Mortality was 0.54% (0.31–0.97) with chronic kidney disease alone compared with 0.17% (0.15–0.20) with hypertension alone (difference 0.37 percentage points, 0.14–0.80; risk ratio 3.2), and 0.85% (0.57–1.32) with hypertension and chronic kidney disease compared with 0.34% (0.30–0.41) with diabetes and hypertension (difference 0.51 percentage points, 0.22–0.97). Diabetes alone was associated with slightly higher mortality than hypertension alone (difference 0.09 percentage points, 0.02–0.17). Contrasts involving diabetes with chronic kidney disease, a profile with four deaths, were imprecise and are not interpreted.
