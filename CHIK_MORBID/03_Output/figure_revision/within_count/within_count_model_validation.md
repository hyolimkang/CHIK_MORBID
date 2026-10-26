# Within-count contrast analysis: model and method validation

**Scripts:**
- `02_Script/56_within_count_contrasts.R`: models, standardisation, contrasts, tests and bootstrap.
- `02_Script/57_within_count_figures.R`: figures, drawn from the CSVs only.

**Outputs (this folder):**
- `within_count_standardised_risks.csv`
- `within_count_direct_contrasts.csv`
- `within_count_omnibus_tests.csv`
- `consistency_with_figure2.csv`
- `bootstrap_contrasts.rds`

No previous analysis or figure was modified.

## Verification checklist

| Item | Finding |
|---|---|
| 1. Cohorts and outcomes | Hospitalisation: N = 776,072 with 25,363 events; `hospitalised` recorded as yes/no. Death: N = 983,947 with 1,050 events; `died_from_chik` known (EVOLUCAO 2 = death from chikungunya; 1 and 3 = not; other codes excluded after Gate 3.1). The script stops if any count differs. |
| 2. Profiles | Eight mutually exclusive DM/HTN/CKD combinations, using script 12's coding (`renal_disease` as CKD). Condition count = DM + HTN + CKD. |
| 3. Adjustment variables | Sex, notification year (factor), state of residence (random-effect smooth), and age (penalised thin-plate smooth, k = 10, `bs = "ts"`). This is identical to Figure 2 and to the count model. |
| 4. Formulas | Profile model: `y ~ core_profile + sex + year + s(uf_residence, bs = "re") + s(age_years, k = 10, bs = "ts")`, the Figure 2 / script 12 §8 specification. Count model: identical, with `n_conditions` (0–3) in place of `core_profile`. Both fitted with `mgcv::bam` (fREML, discrete), as in script 50. |
| 5. Standardisation | One common target per outcome: the observed joint distribution of exact age × sex × year × state in the **whole outcome cohort**. It is applied identically to every profile *and* to the count model, so the count reference in Candidate A is on exactly the same target as the profile points. This is the same target as Figure 2 and as script 50's count and profile risks; verified by inspecting `standardise_levels()` in script 50, which uses `count(d, age_years, sex, year, uf_residence)` for both models. |
| 6. Confidence intervals | 4,000 draws from the coefficients' multivariate normal (`vcov(unconditional = TRUE)`), with percentile intervals. Every RD and RR is computed **within the same draw**, so the covariance between profiles is kept. Bonferroni-simultaneous intervals (α = 0.05/6 per outcome) are also reported. |
| 7. Consistency with Figure 2 | Profile-model point risks match script 12's cached `gam(REML)` estimates to within 0.01% (`consistency_with_figure2.csv`). CI bounds differ by ≤1% (hospitalisation) and ≤5.5% (death), which is Monte Carlo variation from different draw counts and seeds (2,000 vs 4,000). |

## Nesting and omnibus tests
- **Nesting:** the count model is nested in the profile model. Each count indicator is an exact linear combination of profile indicators (maximum residual of the projection ≤ 1.2 × 10⁻⁸, i.e. floating-point zero), and the smooth and random-effect terms are identical.
- **Why the LRT is only approximate:** each model's smoothing parameters are estimated separately, so the `anova.gam` likelihood-ratio test is approximate.
- **Primary omnibus tests:** Wald tests on the adjusted log-odds contrasts, using the Bayesian covariance Vp, which accounts for penalisation. These test equal adjusted odds within a count group, not equal standardised risks. Standardised risk contrasts are estimated directly from the draws.

| Outcome | One-condition (DM = HTN = CKD) | Two-condition (DM+HTN = DM+CKD = HTN+CKD) | Joint (4 df) | Approximate LRT | ΔAIC, profile − count |
|---|---|---|---|---|---|
| Hospitalisation | χ² = 59.4, 2 df, p = 1 × 10⁻¹³ | χ² = 87.5, 2 df, p = 1 × 10⁻¹⁹ | χ² = 146.7, p = 1 × 10⁻³⁰ | 126.6 on 4.0 df, p = 2 × 10⁻²⁶ | −118.6 |
| Death | χ² = 20.8, 2 df, p = 3 × 10⁻⁵ | χ² = 17.1, 2 df, p = 2 × 10⁻⁴ | χ² = 38.0, p = 1 × 10⁻⁷ | 31.1 on 4.0 df, p = 3 × 10⁻⁶ | −23.0 |

## Multiplicity
- The six pre-specified contrasts per outcome are all reported regardless of their results.
- Wald p-values on the log-odds contrasts are Holm-adjusted within each outcome.
- RD and RR intervals are also given at the Bonferroni-simultaneous level (99.17%).
- Interpretation rests on the magnitude and precision of the risk differences, not on p-values.

## Rare-profile check: nonparametric bootstrap
- **Method:** 200 case-resampling replicates per outcome. Records were resampled within profile, so profile sizes are fixed but event counts vary. The profile model was refitted each time and standardised to the original target. Percentile intervals are reported alongside the parametric ones in `within_count_direct_contrasts.csv`.
- **Agreement:** bootstrap and parametric RD intervals agree closely for every hospitalisation contrast and for the death contrasts not involving DM+CKD.
- **DM+CKD for death (4 deaths):** in 5 of 200 replicates DM+CKD had *no* deaths. Its contrasts are therefore fragile: the bootstrap RR interval for HTN+CKD vs DM+CKD has an upper bound near 1,400.
- **CKD vs DM for death:** the bootstrap RD interval (−0.01 to 0.56 pp) and the simultaneous interval (−0.02 to 0.92 pp) just include zero, whereas the nominal parametric interval (0.02 to 0.71) does not. The contrast is borderline.

## Assumptions and caveats
- **Associations, not effects:** profile contrasts are associations standardised for age, sex, year and state. They are not causal effects, and evidence that composition matters beyond count does not imply biological interaction or synergy.
- **Measurement quality:** CKD recording quality (Gate 3.3A) and implausible paediatric comorbidity records apply here. CKD-containing profiles are 0.5% of each cohort.
- **Missing outcomes:** 27.8% of eligible cases lack hospitalisation status, and recording is more complete for comorbid profiles (Gate 3.2A). The overall profile RRs were robust in restricted high-completeness samples (Gate 3.2B).
- **Model engine:** profile risks come from `bam(fREML)`; Figure 2 used `gam(REML)`. The difference in estimates is ≤0.01%.
