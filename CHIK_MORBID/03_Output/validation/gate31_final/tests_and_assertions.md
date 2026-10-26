# Gate 3.1 implementation — tests and assertions

All tests run against the **actual regenerated production data** (not the standalone Gate 3.1 validation script), i.e. `01_Data/chik_sinan_individual_2015_2025.rds` and the RData caches produced by rerunning `02_clean_chik_sinan_brazil.R` → `12_core3_condition_specific_rr.R` → `19_`/`27_`/`28_` this session. All 12 checks **PASS**.

## A. Outcome coding
Synthetic end-to-end test: raw code → `map_evolucao` label → `died_from_chik`.

| raw EVOLUCAO | expected | actual | pass |
|---|---|---|---|
| "1" | FALSE | FALSE | ✓ |
| "2" | TRUE | TRUE | ✓ |
| "3" | FALSE | FALSE | ✓ |
| "4" | NA | NA | ✓ |
| "9" | NA | NA | ✓ |
| "" (blank) | NA | NA | ✓ |
| "0" (unmapped) | NA | NA | ✓ |
| actual R `NA` | NA | NA | ✓ |
| "7" (unexpected) | NA | NA | ✓ |

**PASS** — identical to the 9-case table already validated in Gate 3.1's `38_mortality_outcome_validation.R`, re-run here against the production `case_when()` logic actually inserted into `02_clean_chik_sinan_brazil.R`.

## B. Cohort construction
| Assertion | Result | Status |
|---|---|---|
| Mortality cohort N == 983,947 | 983,947 | **PASS** |
| Chikungunya deaths == 1,050 | 1,050 | **PASS** |
| `died_from_chik` has 0 NA after cohort filtering | 0 | **PASS** |
| Records excluded (unresolved) == 90,849 | 90,849 | **PASS** |

## C. Hospitalisation preservation
| Assertion | Result | Status |
|---|---|---|
| Hospitalisation cohort N == 776,072 | 776,072 | **PASS** |
| Hospitalisations == 25,363 | 25,363 | **PASS** |

No hospitalisation code (`hosp_only`/`hospitalised`/`hosp_cohort` construction in `12_`) was touched this gate; these counts are the same ones Gate 3.1 asked us to verify as "expected baseline", confirmed unchanged post-correction.

## D. RR reproducibility
| Assertion | Result | Status |
|---|---|---|
| Regenerated core3 death RR vs Gate 3.1's independent "A_corrected" scenario | max abs diff = **0.00%** | **PASS** |
| Regenerated 8-profile death RR vs Gate 3.1's "A_corrected" scenario | max abs diff = **0.00%** | **PASS** |
| 8-profile ranking stable (DM+HTN+CKD highest) | DM+HTN+CKD > HTN+CKD > DM+CKD > CKD > DM+HTN > DM > HTN | **PASS** |

The production pipeline (`12_`, full model refit, independent code path) and the Gate 3.1 validation script (`38_`, a self-contained, separately-written re-implementation) agree to the displayed decimal precision. This is strong evidence the correction was applied and propagated correctly, not just that one script's numbers "look plausible" in isolation.

## E. Cache validity
All six mortality-relevant RData caches that could have been silently reused (`core3_rr_death.RData`, `core_profile_rr_death.RData`, `core_profile_rr_death_draws.RData`, `core_profile_rr_death_by_age.RData`, `fig3_age_interaction_comparison_data.RData`, `core_profile_rr_age_curve.RData`) were deleted before rerunning `12_`, and all six now carry a modification timestamp after the corrected RDS was built (2026-10-08 16:09). **PASS** — confirmed no `file.exists()`-guarded cache silently survived the correction.

## F. Reporting consistency
| Assertion | Result | Status |
|---|---|---|
| `figure_cohort_flow_attrition.csv`'s mortality-cohort N matches the actual model-input N | 983,947 == 983,947 | **PASS** |

Table 1 (`18_`), the standardised-risk table (`17_`), the risk-difference table (`20_`), and the joint-profile table (`27_`) were all regenerated from the same corrected cache files in the same session (see `changed_files_and_outputs.csv`), so they share one consistent mortality N by construction — not independently re-verified cell-by-cell here, but produced from the identical, already-verified `core3_rr_death.RData`/`core_profile_rr_death.RData` objects.

## Commands executed (abbreviated; full logs available in this session)
```
Rscript 02_Script/02_clean_chik_sinan_brazil.R
Rscript -e 'source("02_Script/00_setup.R"); source("02_Script/12_core3_condition_specific_rr.R")'
Rscript -e 'source("02_Script/00_setup.R"); source("02_Script/13_core3_descriptive.R")'
Rscript -e 'source("02_Script/00_setup.R"); source("02_Script/14_condition_recording_patterns.R")'
Rscript -e 'source("02_Script/00_setup.R"); source("02_Script/17_table_core3_standardised_risks.R")'
Rscript -e 'source("02_Script/00_setup.R"); source("02_Script/18_table1_analytic_cohorts.R")'
Rscript -e 'source("02_Script/00_setup.R"); source("02_Script/19_refresh_core3_risk_cis.R")'
Rscript -e 'source("02_Script/00_setup.R"); source("02_Script/20_table_core3_risk_differences.R")'
Rscript 02_Script/22_figure_cohort_flow.R
Rscript -e 'source("02_Script/00_setup.R"); source("02_Script/26_table_clinical_outcomes_by_condition.R")'
Rscript -e 'source("02_Script/00_setup.R"); source("02_Script/27_table_joint_profile_standardised_risks.R")'
Rscript -e 'source("02_Script/00_setup.R"); source("02_Script/28_refresh_joint_profile_risk_cis.R")'
```
All exited with code 0. No script required a manual retry due to a code error (two transient OneDrive file-lock warnings on earlier, unrelated figure saves in this project are a known, documented non-issue and did not recur here).
