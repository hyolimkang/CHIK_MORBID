# Manuscript output manifest

These are the final manuscript figures and tables and the selected supplementary materials (paths relative to `CHIK_MORBID/03_Output/`). Superseded and prototype outputs are in `archive/`; see `archive/README.md` and `archive/manifest/archive_manifest.csv`.

## Main figures

| Manuscript | Files | Formats | Historical name | Source estimates |
|---|---|---|---|---|
| **Figure 1.** Overall profile-specific standardised RRs (hospitalisation, death) | `figure_revision/Figure2_profile_RR_refined` | PDF, PNG, SVG | Figure 2 | `figure_revision/figure2_plotted_values.csv` ← `01_Data/core_profile_rr_*.RData` (script 12 §8; plot script 48) |
| **Figure 2.** Age-specific standardised absolute risks: A, hospitalisation; B, death | `figure_revision/figure3_final/panels/Figure3_combined_AB_final` with `Figure3_title_and_legend.docx`. Production assets: `Figure3_panelA_hospitalisation`, `Figure3_panelB_death` | PDF, PNG, SVG | Figure 3 | `panels/Figure3_panelsAB_cells_and_display_status.csv`; hospitalisation: `figure3_final/hospital_age_profile_all_model_estimates.csv` (H2); death: `validation/figure5_age_validation/age_profile_standardised_estimates.csv` (scripts 52, 46; plot 54; legend 55) |
| **Figure 3.** Within-count standardised risk differences | `figure_revision/figure4_final/Figure4_within_count_RD_FINAL` with `Figure4_Title_and_Legend.docx` | PDF, PNG, SVG | Figure 4 | `figure4_final/Figure4_within_count_RD_data.csv` ← `figure_revision/within_count/within_count_direct_contrasts.csv` (script 56; plot 58) |

## Main tables

| Manuscript | File | Preview | Source |
|---|---|---|---|
| **Table 1.** Study population characteristics and outcome completeness | `tables/table1_updated/Table1_Study_Population_FINAL.docx` | `.png` | Script 59; `Table1_Updated_Values.csv`; checks in `table1_checks.csv` |
| **Table 2.** Joint comorbidity profiles: standardised risks and RRs | `tables/tables_2_3_reconciled/Table2_Joint_Profiles_FINAL.docx` | `.png` | Script 60; `Table2_Joint_Profiles_Validated.csv`; reconciliation in `Table_Results_Reconciliation.md` |

## Selected supplementary materials

| Item | Files |
|---|---|
| **Supplementary Figure S1.** Study population selection | `figures/figure_cohort_flow` (PDF, PNG, SVG); counts: `tables/figure_cohort_flow_attrition.csv` |
| Age × profile empirical support: hospitalisation | `figure_revision/figure3_final/Supp_hospital_age_profile_support` (PDF, PNG) |
| Age × profile empirical support: death | `figure_revision/figure3_final/Supp_mortality_age_profile_support` (PDF, PNG) |
| Age-specific RRs: hospitalisation | `figure_revision/figure3_final/Supp_hospital_age_specific_RR` (PDF, PNG) |
| Age-specific RRs: death | `figure_revision/figure3_final/Supp_mortality_age_specific_RR` (PDF, PNG) |
| **Supplementary Table.** Individual underlying conditions (marginal contrasts) | `tables/tables_2_3_reconciled/Supplementary_Table_Individual_Conditions_FINAL.docx` (+ `.png`) |

Supplementary numbering beyond S1 is to be assigned.

## Unresolved supplementary candidates (left in place, decision needed)

| Candidate | Location | Type |
|---|---|---|
| Hospitalisation age-interaction robustness (7 specifications) | `figure_revision/figure3_final/Supp_hospital_age_interaction_robustness` | Essential-supplementary candidate (sensitivity analysis) |
| Outcome-missingness: state-year completeness, RR robustness, completeness and event-count tables | `validation/gate32_summary/figure_S1_*`, `figure_S2_*`, `table_S1–S3*.xlsx` | Essential-supplementary candidate (outcome missingness) |
| CKD measurement: SINAN vs GBD prevalence by age | `validation/gate33a/ckd_prevalence_age_comparison.png` | Measurement-validation candidate |
| Comorbidity recording completeness by age, outcome, sex, state-year and region | `figures/fig_condition_recording_by_*.jpg` | Measurement-validation candidate |
| Crude outcomes by underlying condition (two versions) | `tables/clinical_outcomes_by_underlying_condition.docx`, `..._2017_2025_updated.docx`, `tables/table_core3_confirmed_hosp_death_by_condition.docx` | Descriptive supplementary candidates; choose one version |
| Individual-condition risk differences | `tables/table_core3_risk_differences.docx` | Possibly redundant with the individual-condition supplementary table |
| Count-vs-profile cross-validation (prediction) | Data: `figure_revision/model_comparison_metrics.csv`, `model_comparison_fold_metrics.csv`, `figure4/*.csv` (figure archived) | Methodological supplement, as a table if wanted |
| Literature evidence map and prioritisation matrix | `figures/figure_evidence_*` (+ `.pptx`), `tables/evidence_*`, `tables/figure_evidence_map_*` | Separate evidence-review output, outside the clinical-severity figure set |
| Old Table 2 and Table 3 Word files | `tables/table_core3_standardised_risks.docx`, `tables/table_joint_profile_standardised_risks.docx` | Superseded, but **read by script 60** (reconciliation), so not moved |

All validation reports, cohort reconciliation checks and figure-to-table consistency files remain in `validation/`, `figure_revision/` and `tables/`.
