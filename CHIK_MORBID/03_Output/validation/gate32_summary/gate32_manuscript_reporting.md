# Gate 3.2 manuscript reporting summary

Synthesises the completed Gate 3.2A (hospitalisation missingness audit) and Gate 3.2B (restricted-sample sensitivity analysis) into publication-ready supplementary tables and figures. This is a reporting/synthesis document only - no new analysis was performed; every number is read directly from the validated Gate 3.2A/3.2B CSVs.

**Source reports:** `03_Output/validation/gate32a/VALIDATION_3_2A.md`, `03_Output/validation/gate32b/VALIDATION_3_2B.md`.

## Outputs produced

| File | Content |
|---|---|
| `table_S1_recording_completeness.xlsx` | Recording completeness by age/sex/condition/profile/year/state/lab-confirmation, + state detail, year x state cross-tab, and SMD sheets |
| `table_S2_rr_robustness.xlsx` | Condition-specific (Panel A) and joint-profile (Panel B) RR, original vs restricted, common-target standardisation |
| `table_S3_profile_event_counts.xlsx` | Profile x scenario patient/event counts, observed proportions, sparse-cell flags, event-loss-vs-RR-change concordance |
| `figure_S1_state_year_completeness.png` | State x year recording-completeness heatmap |
| `figure_S2_rr_robustness.png` | RR forest plot, common-target standardisation, original vs restricted per scenario |

## Methods (suggested manuscript text)

> Among confirmed chikungunya cases meeting core-three eligibility criteria (N=1,074,796), hospitalisation outcome was analysed as a complete case among the 776,072 (72.21%) with an explicit yes/no hospitalisation-status response. Recording completeness was assessed across demographic, clinical, temporal and geographic strata, using the full eligible cohort (not the independently defined, Gate 3.1-corrected mortality cohort) as the denominator throughout. Standardised mean differences compared recorded and unrecorded cases; multi-category variables used a generalised multinomial SMD (Yang & Dalton, 2012), not directly comparable to binary/continuous SMDs. To assess whether geographic and temporal heterogeneity in recording completeness could materially affect the primary hospitalisation RR estimates, four prespecified restricted-sample sensitivity analyses were conducted: (B) excluding Distrito Federal during 2020-2025, its period of most severe recording collapse; (C1/C2) restricting to state-calendar-year strata with >=70%/>=80% recording completeness (requiring >=100 eligible cases per stratum); and (D) restricting to laboratory-confirmed cases only. For each restricted sample, the original (full-sample) model and a model refit on the restricted sample were both standardised, via g-computation with a 2000-draw parametric coefficient bootstrap, to the IDENTICAL target population (the full eligible population for that scenario, including hospitalisation-status-unknown cases) - this common-target standardisation isolates genuine change in the fitted association from change in the standardisation population.

## Results (suggested manuscript text)

**1. Overall missingness.** Hospitalisation status was unrecorded for 27.79% of the eligible cohort (72.21% recorded).

**2. Year/state vs age/comorbidity.** Recording completeness varied little by age, sex or individual comorbidity (SMD 0.008-0.094, all <0.1) and only modestly by multimorbidity profile (generalised SMD 0.101). In contrast, completeness varied substantially by calendar year (58.5%-86.5% across 2017-2025; generalised SMD 0.466) and by state (24.4%-96.2% across states; generalised SMD 0.485), with Distrito Federal the most extreme single case, falling to 24.39% recorded overall and remaining below 20% every year from 2020 onward (Figure S1).

**3. Stability of condition-specific RRs.** Under common-target standardisation, the restricted-sample condition-specific RRs (DM, HTN, CKD) changed by a maximum of 7.12% and a median of 1.45% relative to the original model on the same target; 0 of the condition-scenario comparisons exceeded the prespecified 10% descriptive flag.

**4. Persistence of elevated CKD-profile risk.** CKD remained the single strongest individual condition, and CKD-containing joint profiles remained among the highest-risk multimorbidity groups, in every restricted scenario (Table S2).

**5. Sensitivity of rare profiles.** 5 of the joint-profile common-target comparisons exceeded the 10% flag, concentrated in the rarest profiles - most notably DM+CKD (largest single change: 35.03%, D: Laboratory-confirmed only, see Table S3). 1 profile x scenario cell(s) had fewer than 10 observed hospitalisation events.

**6. Sparse-event limitations.** The event-loss-vs-RR-change comparison in Table S3 shows that in the state/year-restricted scenarios (C1/C2), large RR changes for DM+CKD and DM+HTN+CKD coincided with substantial (>35%) loss of observed events - consistent with, though not proof of, sparsity-driven instability. In the laboratory-confirmed scenario (D), however, the RR for those same profiles shifted despite comparatively modest event loss (<20%), suggesting a genuine compositional difference in laboratory-confirmed cases rather than small-sample noise alone (Table S3, "Event loss vs RR change" sheet).

## What these results do NOT establish

- They do **not** establish that hospitalisation-status missingness is random (MAR) - geographic/temporal concentration of missingness is consistent with, but does not prove, a MAR-conditional-on-state-and-year mechanism.
- They do **not** eliminate the possibility of residual selection bias - restricted-sample stability shows the primary estimates are not an artefact of the specific poorly-recorded areas/periods tested, which is a narrower claim than "unbiased".
- Stability of the dominant conditions/profiles does **not** extend uniformly to the rarest profiles (DM+CKD, DM+HTN+CKD), which should be reported with appropriately cautious language.
- These results do **not** by themselves make IPW or imputation unnecessary in every circumstance - see VALIDATION_3_2A.md and VALIDATION_3_2B.md for the specific, narrower recommendation on where a formal IPW sensitivity analysis could still add value (the DM+CKD / DM+HTN+CKD cells specifically).

## Validation note

All displayed values were read programmatically from `03_Output/validation/gate32a/*.csv` and `03_Output/validation/gate32b/*.csv`; none were hand-typed or copied from the Markdown reports. Column structures were verified against expected names before processing (see script header). No model was refit, no new sensitivity scenario was run, and no primary result, cache, or manuscript file was modified.

