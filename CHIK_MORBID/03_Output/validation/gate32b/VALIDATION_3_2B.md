# Gate 3.2B - Restricted-sample sensitivity analysis for hospitalisation RR

Brazil, confirmed chikungunya cases, 2017-2025. Produced by `02_Script/40_hospitalisation_restricted_sensitivity.R`, following the Gate 3.2A missingness audit (`03_Output/validation/gate32a/VALIDATION_3_2A.md`). This is a validation exercise: it does not change the primary hospitalisation model, and does not implement IPW, multiple imputation, or MNAR modelling.

## 1. Baseline reproduction (Scenario A)

| | N | Events |
|---|---:|---:|
| Eligible core-three population | 1,074,796 | - |
| Hospitalisation complete-case cohort | 776,072 | 25,363 |

Matches the expected baseline (1,074,796 / 776,072 / 25,363) exactly - **PASS**.

## 2. Scenario definitions and cohort sizes

| scenario | n_eligible | n_known | pct_known | n_hosp_events | pct_of_original_eligible_retained |
|---|---|---|---|---|---|
| A | 1074796 | 776072 | 72.21 | 25363 | 100.00 |
| B | 1072989 | 775678 | 72.29 | 25284 |  99.83 |
| C1 |  606120 | 510051 | 84.15 | 15971 |  56.39 |
| C2 |  379875 | 337021 | 88.72 | 10909 |  35.34 |
| D |  440596 | 320945 | 72.84 | 15134 |  40.99 |

Full detail, including age/profile composition per scenario: `scenario_cohort_summary.csv`. State-year inclusion detail for Scenario C (both thresholds, with and without the minimum-denominator rule): `scenario_state_year_inclusion.csv` and `scenario_state_year_min_denom_sensitivity.csv`.

**Distrito Federal (DF) verification**: confirmed as `uf_residence` code "53" (IBGE standard), not the string "DF" - 1,898 eligible DF records identified and correctly excluded for 2020-2025 under Scenario B.

## 3. Model convergence and sparse cells

Model fit failures: **0**. Non-converged fits: **0**. Scenario x model combinations with at least one absent or sparse (<10 events) profile cell: **2** (see `model_convergence_and_sparse_cells.csv` for which).

## 4. Condition-specific RR: original vs restricted (common-target standardisation)

| scenario | condition | analysis_type | rr | ci | n |
|---|---|---|---|---|---|
| A | Chronic kidney disease | original_manuscript | 2.42 | 2.20-2.67 | NA |
| B | Chronic kidney disease | common_target_full_model | 2.42 | 2.20-2.67 | 1072989 |
| B | Chronic kidney disease | common_target_restricted_model | 2.43 | 2.21-2.68 | 1072989 |
| C1 | Chronic kidney disease | common_target_full_model | 2.42 | 2.20-2.66 |  606120 |
| C1 | Chronic kidney disease | common_target_restricted_model | 2.40 | 2.11-2.72 |  606120 |
| C2 | Chronic kidney disease | common_target_full_model | 2.40 | 2.18-2.64 |  379875 |
| C2 | Chronic kidney disease | common_target_restricted_model | 2.23 | 1.90-2.59 |  379875 |
| D | Chronic kidney disease | common_target_full_model | 2.40 | 2.18-2.65 |  440596 |
| D | Chronic kidney disease | common_target_restricted_model | 2.53 | 2.27-2.81 |  440596 |
| A | Diabetes | original_manuscript | 1.45 | 1.38-1.51 | NA |
| B | Diabetes | common_target_full_model | 1.45 | 1.38-1.52 | 1072989 |
| B | Diabetes | common_target_restricted_model | 1.45 | 1.38-1.52 | 1072989 |
| C1 | Diabetes | common_target_full_model | 1.45 | 1.38-1.51 |  606120 |
| C1 | Diabetes | common_target_restricted_model | 1.41 | 1.33-1.50 |  606120 |
| C2 | Diabetes | common_target_full_model | 1.44 | 1.38-1.51 |  379875 |
| C2 | Diabetes | common_target_restricted_model | 1.42 | 1.32-1.53 |  379875 |
| D | Diabetes | common_target_full_model | 1.44 | 1.38-1.51 |  440596 |
| D | Diabetes | common_target_restricted_model | 1.51 | 1.42-1.59 |  440596 |
| A | Hypertension | original_manuscript | 1.47 | 1.41-1.52 | NA |
| B | Hypertension | common_target_full_model | 1.47 | 1.41-1.53 | 1072989 |
| B | Hypertension | common_target_restricted_model | 1.47 | 1.41-1.53 | 1072989 |
| C1 | Hypertension | common_target_full_model | 1.47 | 1.41-1.52 |  606120 |
| C1 | Hypertension | common_target_restricted_model | 1.47 | 1.40-1.54 |  606120 |
| C2 | Hypertension | common_target_full_model | 1.46 | 1.41-1.52 |  379875 |
| C2 | Hypertension | common_target_restricted_model | 1.45 | 1.37-1.53 |  379875 |
| D | Hypertension | common_target_full_model | 1.46 | 1.41-1.52 |  440596 |
| D | Hypertension | common_target_restricted_model | 1.51 | 1.44-1.58 |  440596 |

Full table with all analysis types (scenario-specific AND common-target, both full and restricted models) and 95% UIs: `hospitalisation_condition_rr_comparison.csv`.

## 5. Joint-profile RR: original vs restricted (common-target standardisation)

| scenario | core_profile | analysis_type | rr | ci | n |
|---|---|---|---|---|---|
| A | DM | original_manuscript | 1.74 | 1.59-1.87 | NA |
| B | DM | common_target_full_model | 1.74 | 1.59-1.88 | 1072989 |
| B | DM | common_target_restricted_model | 1.74 | 1.60-1.88 | 1072989 |
| C1 | DM | common_target_full_model | 1.74 | 1.59-1.87 |  606120 |
| C1 | DM | common_target_restricted_model | 1.72 | 1.56-1.90 |  606120 |
| C2 | DM | common_target_full_model | 1.73 | 1.58-1.86 |  379875 |
| C2 | DM | common_target_restricted_model | 1.72 | 1.53-1.93 |  379875 |
| D | DM | common_target_full_model | 1.73 | 1.58-1.87 |  440596 |
| D | DM | common_target_restricted_model | 1.81 | 1.65-1.99 |  440596 |
| A | HTN | original_manuscript | 1.52 | 1.46-1.58 | NA |
| B | HTN | common_target_full_model | 1.53 | 1.46-1.59 | 1072989 |
| B | HTN | common_target_restricted_model | 1.53 | 1.47-1.59 | 1072989 |
| C1 | HTN | common_target_full_model | 1.52 | 1.46-1.58 |  606120 |
| C1 | HTN | common_target_restricted_model | 1.53 | 1.46-1.61 |  606120 |
| C2 | HTN | common_target_full_model | 1.52 | 1.46-1.58 |  379875 |
| C2 | HTN | common_target_restricted_model | 1.51 | 1.42-1.60 |  379875 |
| D | HTN | common_target_full_model | 1.52 | 1.46-1.58 |  440596 |
| D | HTN | common_target_restricted_model | 1.58 | 1.50-1.66 |  440596 |
| A | CKD | original_manuscript | 2.90 | 2.46-3.42 | NA |
| B | CKD | common_target_full_model | 2.90 | 2.46-3.43 | 1072989 |
| B | CKD | common_target_restricted_model | 2.91 | 2.47-3.45 | 1072989 |
| C1 | CKD | common_target_full_model | 2.90 | 2.45-3.42 |  606120 |
| C1 | CKD | common_target_restricted_model | 3.05 | 2.49-3.75 |  606120 |
| C2 | CKD | common_target_full_model | 2.87 | 2.44-3.38 |  379875 |
| C2 | CKD | common_target_restricted_model | 3.09 | 2.40-3.95 |  379875 |
| D | CKD | common_target_full_model | 2.88 | 2.44-3.40 |  440596 |
| D | CKD | common_target_restricted_model | 2.92 | 2.39-3.48 |  440596 |
| A | DM+HTN | original_manuscript | 2.07 | 1.96-2.18 | NA |
| B | DM+HTN | common_target_full_model | 2.07 | 1.96-2.18 | 1072989 |
| B | DM+HTN | common_target_restricted_model | 2.07 | 1.96-2.18 | 1072989 |
| C1 | DM+HTN | common_target_full_model | 2.07 | 1.96-2.18 |  606120 |
| C1 | DM+HTN | common_target_restricted_model | 2.02 | 1.89-2.16 |  606120 |
| C2 | DM+HTN | common_target_full_model | 2.06 | 1.95-2.16 |  379875 |
| C2 | DM+HTN | common_target_restricted_model | 2.01 | 1.85-2.17 |  379875 |
| D | DM+HTN | common_target_full_model | 2.06 | 1.95-2.17 |  440596 |
| D | DM+HTN | common_target_restricted_model | 2.19 | 2.06-2.33 |  440596 |
| A | DM+CKD | original_manuscript | 3.64 | 2.59-5.04 | NA |
| B | DM+CKD | common_target_full_model | 3.65 | 2.59-5.06 | 1072989 |
| B | DM+CKD | common_target_restricted_model | 3.65 | 2.54-5.14 | 1072989 |
| C1 | DM+CKD | common_target_full_model | 3.64 | 2.59-5.05 |  606120 |
| C1 | DM+CKD | common_target_restricted_model | 3.24 | 2.08-4.93 |  606120 |
| C2 | DM+CKD | common_target_full_model | 3.60 | 2.57-4.97 |  379875 |
| C2 | DM+CKD | common_target_restricted_model | 2.77 | 1.52-4.86 |  379875 |
| D | DM+CKD | common_target_full_model | 3.61 | 2.57-4.97 |  440596 |
| D | DM+CKD | common_target_restricted_model | 4.87 | 3.46-6.66 |  440596 |
| A | HTN+CKD | original_manuscript | 4.52 | 3.82-5.26 | NA |
| B | HTN+CKD | common_target_full_model | 4.54 | 3.83-5.28 | 1072989 |
| B | HTN+CKD | common_target_restricted_model | 4.54 | 3.84-5.28 | 1072989 |
| C1 | HTN+CKD | common_target_full_model | 4.53 | 3.82-5.27 |  606120 |
| C1 | HTN+CKD | common_target_restricted_model | 4.39 | 3.59-5.32 |  606120 |
| C2 | HTN+CKD | common_target_full_model | 4.46 | 3.77-5.19 |  379875 |
| C2 | HTN+CKD | common_target_restricted_model | 4.02 | 3.10-5.15 |  379875 |
| D | HTN+CKD | common_target_full_model | 4.47 | 3.79-5.19 |  440596 |
| D | HTN+CKD | common_target_restricted_model | 4.43 | 3.63-5.23 |  440596 |
| A | DM+HTN+CKD | original_manuscript | 3.62 | 3.10-4.18 | NA |
| B | DM+HTN+CKD | common_target_full_model | 3.63 | 3.11-4.19 | 1072989 |
| B | DM+HTN+CKD | common_target_restricted_model | 3.65 | 3.13-4.21 | 1072989 |
| C1 | DM+HTN+CKD | common_target_full_model | 3.62 | 3.10-4.18 |  606120 |
| C1 | DM+HTN+CKD | common_target_restricted_model | 3.40 | 2.78-4.15 |  606120 |
| C2 | DM+HTN+CKD | common_target_full_model | 3.58 | 3.07-4.13 |  379875 |
| C2 | DM+HTN+CKD | common_target_restricted_model | 2.91 | 2.20-3.73 |  379875 |
| D | DM+HTN+CKD | common_target_full_model | 3.59 | 3.08-4.13 |  440596 |
| D | DM+HTN+CKD | common_target_restricted_model | 4.13 | 3.51-4.84 |  440596 |

Full table: `hospitalisation_profile_rr_comparison.csv`. Standardised absolute risks for all scenarios/analyses: `hospitalisation_absolute_risk_comparison.csv`.

## 6. Magnitude of change

- **Maximum |% change|** in common-target RR (restricted model vs original model, same target population): **35.03%** (DM+CKD, profile, Scenario D).
- **Median |% change|** across all condition + profile common-target comparisons: **1.98%**.
- **5 of 40** comparisons exceeded the prespecified +/-10% descriptive-flag threshold (not automatic evidence of invalidity - see below for which, and why).
- **Median CI-widening ratio** (restricted-model common-target CI width on the log scale, divided by the original model's CI width on the SAME target): **1.25x**.

Flagged comparisons (>10% change):

| scenario | group | model_type | pct_change_rr |
|---|---|---|---|
| D | DM+CKD | profile |  35.03 |
| C2 | DM+CKD | profile | -23.03 |
| C2 | DM+HTN+CKD | profile | -18.71 |
| D | DM+HTN+CKD | profile |  15.12 |
| C1 | DM+CKD | profile | -10.99 |

## 7. Does the main clinical pattern hold?

| Question | Answer |
|---|---|
| Does CKD remain the strongest individual condition in every scenario? | **YES** |
| Do CKD-containing profiles remain among the top-3 ranked profiles in every scenario? | **YES** |

| scenario | strongest | ckd_rr | max_rr | ckd_is_strongest |
|---|---|---|---|---|
| A | ckd | 2.42 | 2.42 | TRUE |
| B | ckd | 2.42 | 2.42 | TRUE |
| C1 | ckd | 2.40 | 2.40 | TRUE |
| C2 | ckd | 2.23 | 2.23 | TRUE |
| D | ckd | 2.53 | 2.53 | TRUE |

## 8. Common-target support (Analysis 2 positivity check)

| scenario | n_target_total | n_supported | n_unsupported | pct_supported | unsupported_states |
|---|---|---|---|---|---|
| B | 1072989 | 1072989 | 0 | 100 |  |
| C1 |  606120 |  606120 | 0 | 100 |  |
| C2 |  379875 |  379875 | 0 | 100 |  |
| D |  440596 |  440596 | 0 | 100 |  |

No scenario required falling back to a different overlap target - the restricted model's own (state, year) support, used directly as the common target's inclusion criterion, already guarantees full predictive support for both the full and the restricted model by construction (see script Phase 6 for the reasoning). Full detail: `common_target_standardisation_diagnostics.csv`.

## 9. Interpretation

### A. Stability of clinical associations
See Sections 6-7 above: the largest common-target RR change was 35.0% (DM+CKD, profile, Scenario D), above the prespecified 10% descriptive flag.

### B. Geographical and temporal reporting bias
Scenario B (excluding DF 2020-2025, the most extreme single-area/period recording gap found in Gate 3.2A) and Scenarios C1/C2 (restricting to higher-completeness state-year strata generally) directly test whether the poorest-recorded areas/periods are driving the primary estimates. See Section 6 for the quantitative answer; in short, some comparisons moved by more than the prespecified 10% threshold; Section 6 and `hospitalisation_condition_rr_comparison.csv`/`hospitalisation_profile_rr_comparison.csv` identify exactly which, for closer review rather than as proof of bias.

### C. Differential profile representation
Scenario-specific standardisation (Analysis 1, `hospitalisation_profile_rr_comparison.csv` with `analysis_type=="scenario_specific"`) shows each restricted sample's own profile composition has shifted modestly (`scenario_cohort_summary.csv` `pct_profile_*` columns) in the direction already flagged by Gate 3.2A (comorbid profiles slightly over-represented among recorded cases) - the common-target analysis in Section 4-6 is what isolates whether this compositional shift, rather than a genuine change in the fitted association, explains any observed RR movement.

### D. Remaining limitations
- A stable RR across these restricted analyses does **not** prove hospitalisation-status missingness is ignorable (MAR) - it shows the primary estimates are not an artefact of the specific poorly-recorded areas/periods tested here, which is a narrower and more defensible claim.
- Conversely, any RR change after restriction may reflect genuine epidemiological heterogeneity (e.g. real regional differences in chikungunya severity or healthcare access) or reduced precision, not necessarily bias - this report does not assume restriction-driven change equals bias.
- Scenario D (laboratory-confirmed only) changes case-definition composition as well as recording completeness and is interpreted only as a diagnostic-ascertainment sensitivity check, not a missingness correction.
- No MNAR mechanism (hospitalisation-status recording depending on the hospitalisation outcome itself, conditional on observed covariates) can be excluded by any analysis in this report.
- Formal IPW/positivity diagnostics (Gate 3.2A Phase 5) were deferred and have still not been computed; this report's "common support" check (Section 8) is a necessary but not sufficient condition for IPW to be well-behaved if attempted later.

### E. Recommendation for the manuscript
Review the flagged comparisons in Section 6 before deciding; a formal IPW sensitivity analysis (Gate 3.2A's deferred Phase 5) would help quantify whether the specific flagged changes track overall recording-probability rather than one of the restricted scenarios' own idiosyncrasies. MNAR analysis is a larger undertaking and should only follow if IPW itself cannot resolve the flagged discrepancies.

## 10. Gate 3.2B status: **CONDITIONAL PASS**

the largest common-target RR change was 35.0% (DM+CKD, profile, Scenario D), above the prespecified 10% descriptive flag.

---
*Fixed bootstrap seed: 1. Bootstrap draws per standardisation: 2000. Minimum state-year stratum denominator (Scenario C): 100 eligible cases.*

