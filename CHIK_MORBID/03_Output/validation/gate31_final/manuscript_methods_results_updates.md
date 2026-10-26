# Suggested manuscript Methods/Results updates (Gate 3.1 correction)

The manuscript DOCX was **not** edited. This document provides corrected numbers and suggested replacement text for the author to paste in manually.

## Methods — outcome definition (new paragraph/sentence to add)

> Chikungunya-attributed death was defined from the SINAN `EVOLUCAO` (case evolution) field as evolution code 2 ("death from the notified disease"). Records coded as cured (code 1) or death from another cause (code 3) were classified as non-events. Records with an unresolved outcome — death pending cause investigation (code 4), outcome unknown (code 9), or a blank/unmapped code — were excluded from the mortality analysis as having unknown chikungunya-death status, rather than treated as confirmed survivors. This excluded 90,849 of 1,074,796 eligible records (8.5%). A sensitivity analysis treating unresolved outcomes as non-events (reproducing the original approach) and two additional sensitivity scenarios for the subset of records with a confirmed death of unresolved cause (n=142) are reported in [Gate 3.1 validation report / supplementary material].

## Results — cohort sizes (replace existing N's)

| | Old text | Corrected text |
|---|---|---|
| Mortality cohort | "N = 1,074,796" | **"N = 983,947"** |
| Chikungunya deaths | "1,050 deaths" | **"1,050 deaths" (unchanged)** |
| Hospitalisation cohort | "N = 776,072" | unchanged |

## Results — condition-specific mortality RR (mutually adjusted for DM/HTN/CKD, age, sex, year, state)

Replace the mortality row of the condition-specific RR table/text with:

| Condition | Old RR (95% CI) | Corrected RR (95% CI) |
|---|---|---|
| Diabetes | 2.25 (1.90–2.64) | **2.26 (1.92–2.63)** |
| Hypertension | 2.20 (1.91–2.58) | **2.19 (1.88–2.57)** |
| Chronic kidney disease | 4.10 (3.12–5.24) | **4.20 (3.28–5.30)** |

Suggested sentence: *"After mutual adjustment, chronic kidney disease remained the strongest independent predictor of chikungunya-attributed death (RR 4.20, 95% CI 3.28–5.30), followed by diabetes (RR 2.26, 95% CI 1.92–2.63) and hypertension (RR 2.19, 95% CI 1.88–2.57); these estimates were materially unchanged from the uncorrected mortality-outcome definition (<2.4% relative change in each RR)."*

## Results — eight-profile mortality RR (vs no core condition)

| Profile | Old RR (95% CI) | Corrected RR (95% CI) |
|---|---|---|
| DM only | 3.88 (2.92–5.09) | **3.84 (2.85–5.09)** |
| HTN only | 2.55 (2.14–3.00) | **2.52 (2.12–2.97)** |
| CKD only | 7.90 (4.32–13.85) | **7.99 (4.54–14.42)** |
| DM+HTN | 5.03 (4.18–6.04) | **5.04 (4.20–6.08)** |
| DM+CKD | 10.19 (3.93–25.96) | **9.89 (3.80–25.60)** |
| HTN+CKD | 12.14 (7.99–19.23) | **12.58 (8.02–19.46)** |
| DM+HTN+CKD | 15.71 (10.67–22.85) | **15.99 (10.74–23.26)** |

Suggested sentence: *"The highest mortality risk was observed among people with all three core conditions (RR 15.99, 95% CI 10.74–23.26), followed by hypertension+CKD (RR 12.58, 95% CI 8.02–19.46) and diabetes+CKD (RR 9.89, 95% CI 3.80–25.60); this ranking was unchanged after correcting the chikungunya-death outcome definition to exclude unresolved cases."*

## Results — cohort flow / Table 1

The cohort-selection flowchart's mortality branch should read "Mortality cohort, chikungunya death status known, N = 983,947" with a new exclusion box "Excluded: chikungunya death status unknown, n = 90,849" (previously n = 0). Table 1's mortality column N changes from 1,074,796 to 983,947; row percentages for age/sex/condition/profile distributions shift by well under 1 percentage point (see `03_Output/tables/table1_analytic_cohorts.docx`, already regenerated).

## Suggested Limitations paragraph (new or amended)

> Chikungunya-attributed death could not be determined for 8.5% of the analytic cohort (records with outcome "under investigation", "unknown", or unrecorded); these were excluded from the primary mortality analysis rather than assumed to be survivors. A sensitivity analysis imputing a chikungunya-attribution probability for the much smaller subset of confirmed deaths with investigation-pending cause (n=142, 0.01% of the cohort), using the attribution rate observed among already-resolved deaths, produced materially identical estimates. This does not establish that the larger unknown/unrecorded-outcome group is missing at random; residual uncertainty about mortality ascertainment in that group cannot be fully excluded.

## Files to attach/cite as supporting evidence
- `03_Output/validation/gate31/VALIDATION_3_1.md` (original audit)
- `03_Output/validation/gate31_final/implementation_report.md` (this correction's implementation)
- `03_Output/validation/gate31_final/old_vs_corrected_mortality_rr.csv`
- `03_Output/figures/figure_cohort_flow.png` (regenerated)
