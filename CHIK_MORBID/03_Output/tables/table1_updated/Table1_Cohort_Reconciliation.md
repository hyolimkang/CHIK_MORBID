# Table 1: cohort reconciliation

**Source:** `01_Data/chik_sinan_individual_2015_2025.rds` (cleaned SINAN; outcome coding after Gate 3.1).
**Script:** `02_Script/59_table1_updated.R`.

All values are recomputed directly from the record-level data, not from cached workbooks.

## Cohort definitions
- **Eligible core-3 population:** confirmed chikungunya; notification/onset in 2017 or later; age 0–100 years; sex recorded as male or female; DM, HTN and CKD (`renal_disease`) each recorded as yes or no.
- **Hospitalisation cohort:** eligible cases with `hospitalised` recorded as yes or no.
- **Mortality cohort:** eligible cases with known chikungunya-death status: EVOLUCAO 2 = death from chikungunya; 1 or 3 = not (cured, or died of another cause); codes 4/9/blank = unknown and excluded (Gate 3.1).
- The two cohorts are defined **independently** from the same eligible population. They are not sequential exclusion steps, and unknown status is never counted as "no event".

## Counts

| | Hospitalisation | Mortality |
|---|---:|---:|
| Eligible core-3 population | 1,074,796 | 1,074,796 |
| Outcome status unknown | 298,724 (27.8%) | 90,849 (8.5%) |
| **Final analysis cohort** | **776,072** | **983,947** |
| Outcome events | 25,363 hospitalisations (3.3%) | 1,050 deaths (0.1%) |

Known + unknown = eligible for both outcomes. All counts match:
- the validated values in the brief;
- the cohort flowchart output (`03_Output/tables/figure_cohort_flow_attrition.csv`);
- the profile counts in Figure 2 (`01_Data/core_profile_rr_*.RData`; all 8 profiles in each cohort).

## Discrepancies found

1. **The premise that the current Table 1 shows the full eligible population as the mortality cohort does not hold for the current file.** `03_Output/tables/table1_analytic_cohorts.docx` was regenerated on 8 Oct (Gate 3.1) and already reports mortality N = 983,947 with outcome-specific sex and profile counts (e.g. female 600,128; None 856,342). All numbers in the updated table are identical to that file. What changes:
   - a new cohort-selection and outcome-completeness section;
   - clearer labels;
   - "—" for outcomes that do not apply;
   - an en dash in the IQR;
   - two-decimal precision for rare profiles. DM + CKD was previously shown as "0.0%", which misrepresents a non-zero prevalence.

   If an older Table 1 exists elsewhere (e.g. in a full manuscript draft outside this repository) showing N = 1,074,796 for mortality, it is outdated and should be replaced.
2. **No manuscript text was available to cross-check.** The repository's manuscript file (`Increased risk of hospitalization and death ... Brazil.docx`) is an outline with no tables, Methods or Results text. Two consequences:
   - It could not be updated in place. `Table1_Study_Population_FINAL.docx` is provided as a standalone table to paste into the full manuscript.
   - Its title gives the study period as **2017–2024**, whereas every analysis now covers **2017–2025**. The title needs updating.
3. **Age is identical in both cohorts:** median (IQR) 39 (24–54) in each. This is a genuine result of the heavily overlapping cohorts, not a copy, since each was computed from its own records.
