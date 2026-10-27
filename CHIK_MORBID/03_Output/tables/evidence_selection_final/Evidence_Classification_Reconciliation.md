# Evidence classification reconciliation

This report compares the historical classifications with a re-check against the completed extraction workbook. The historical classifications come from `02_Script/16_figure_evidence_matrix.R` and the `Condition_decision` sheet of `chik_comorbid_evidence.xlsx`. The workbook snapshot has MD5 `eb1e3e0ebeca7366af4954065fe02819`.

The supplementary table shows the **historical** categories unchanged. Re-check results are reported here only.

Sources: `Evidence_Study_Level_Audit.csv` (74 condition–study–outcome rows) and `Evidence_Prioritisation_Summary.csv`.

## Overview

| Condition | Hospitalisation: historical / re-check | Mortality: historical (severe/fatal) / re-check (death only) | Families, H / M (external) | Prioritisation: historical / re-check |
|---|---|---|---|---|
| Diabetes mellitus | Supported / Supported | Supported / Supported (note) | 3 (1) / 5 (2) | Core / Core |
| Hypertension | Supported / Supported (note) | Limited / Limited, borderline Supported (note) | 3 (1) / 4 (1) | Core / Core |
| Chronic kidney disease | Supported / Supported | Supported / Supported (note) | 3 (1) / 3 (0) | Core / Core |
| Hepatopathy | Insufficient / Insufficient | Limited / Limited | 0 / 1 (0) | Secondary / Secondary |
| Autoimmune disease | Limited / Limited | **Insufficient / Limited (discrepancy)** | 1 (0) / 1 (0) | Secondary / Secondary |
| Haematological disease | Insufficient / Insufficient | Insufficient / Insufficient | 0 / 0 | Not prioritised / Not prioritised |
| Peptic ulcer disease | Insufficient / Insufficient (note) | Insufficient / Insufficient | 0 / 0 | Not prioritised / Not prioritised |

Applied to the historical categories, the workbook prioritisation rule reproduces all seven historical prioritisations; the script asserts this. Under the re-check categories, all seven prioritisations are also unchanged.

## Per-condition audit

Each condition is assessed against seven questions:
1. Direct hospitalisation evidence?
2. Direct mortality evidence (death only)?
3. Consistent direction?
4. Precise?
5. Adjusted?
6. Independent of the target SINAN data?
7. Does the historical category hold?

### Diabetes mellitus — Core
1. **Hospitalisation: yes.**
   - Adjusted positive associations in BRA-A (Furtado aOR 1.488, 1.385–1.599; Cerqueira-Silva, direction only) and BRA-B (Pinto aOR 1.70, 1.44–1.99).
   - External: Perti age-adjusted RR 1.39 (1.06–1.84).
2. **Mortality: yes.**
   - BRA-A: aOR 2.396 (1.974–2.908). Cerqueira-Silva's association is among hospitalised cases.
   - BRA-B: aOR 3.29; the workbook records no CI.
   - BRA-C: crude OR 1.8 (0.9–3.1).
   - Pooled OR 2.86 (1.75–4.69).
3. **Consistency: positive across the Brazilian families.**
   - Economopoulou (Réunion) reported no association with death.
   - Añazco (ICU-selected) crude OR 0.70 (0.11–3.72).
   - Badawi's severe-composite OR 1.20 is not mortality evidence.
4. **Precision:** adequate in BRA-A and Perti. BRA-B has no CI recorded; BRA-C and Añazco are imprecise.
5. **Adjustment:** multivariable in BRA-A and BRA-B; age only in Perti.
6. **Independence:** hospitalisation has one external family. For mortality, both external families are null or imprecise, so the positive mortality signal comes only from SINAN-overlapping data.
7. **Historical category holds (Supported/Supported).** Under the workbook rule, Supported can rest on recurrence across overlapping families. The lack of external corroboration of mortality is stated in the table's limitations column.

### Hypertension — Core
1. **Hospitalisation: yes.**
   - Adjusted positive associations in BRA-A (aOR 1.519, 1.438–1.604) and BRA-B (aOR 1.90, 1.69–2.14).
   - The only external family is null: Perti age-adjusted RR 1.00 (0.63–1.57).
2. **Mortality: yes.**
   - BRA-A: aOR 2.053 (1.723–2.446).
   - BRA-B: aOR 3.74; no CI recorded.
   - BRA-C: crude OR 2.2 (1.2–4.1).
   - Pooled OR 3.10 (2.02–4.77).
   - Añazco: crude OR 1.17 (0.28–4.84).
   - Hypertension was not among the death factors reported by Cerqueira-Silva (BRA-A) or Economopoulou (age ≥85, alcohol).
3. **Consistency:** hospitalisation is consistent within Brazil but not externally. Mortality is mostly positive but inconsistent across studies.
4. **Precision:** adequate in BRA-A. BRA-B has no CI; Añazco is imprecise.
5. **Adjustment:** multivariable in BRA-A and BRA-B for both outcomes. BRA-C is crude.
6. **Independence: no external corroboration for either outcome.** The Réunion aRR 1.7 (1.2–2.4) is for **severe atypical disease**, which is not mortality. The historical "Limited" for severe/fatal partly reflected this composite evidence, and the earlier summary noted that the Réunion data were the independent setting.
7. **Historical categories hold.**
   - Hospitalisation (Supported): defensible by recurrence across BRA-A and BRA-B, but external evidence does not corroborate it.
   - Mortality (Limited): borderline. Counting only death evidence, three Brazilian families point in a positive direction, which could support "Supported". Inconsistency (two studies not listing hypertension) and the absence of external corroboration support keeping "Limited".
   - Either way, hypertension remains **Core**.

### Chronic kidney disease — Core
1. **Hospitalisation: yes.**
   - BRA-A: aOR 1.982 (1.682–2.335); Cerqueira-Silva reports "kidney disease".
   - BRA-B: Pinto aOR 4.56 (3.36–6.17). Silva Junior 2018 gives crude results only; its OR 5.60 comes from a model with CKD as the outcome and is **not** used.
   - External: Perti RR 1.52 (1.19–1.94).
2. **Mortality: yes.**
   - BRA-A: aOR 4.437 (3.171–6.209).
   - BRA-B: aOR 3.14; Silva Junior 2018 crude 3.0% vs 0.2%.
   - BRA-C: aOR 12.77 (2.75–59.4).
   - Pooled OR 5.81 (1.30–25.99).
3. **Consistency:** positive in every family.
4. **Precision:** wide CIs in BRA-C and in the pooled estimate.
5. **Adjustment:** multivariable throughout.
6. **Independence:** hospitalisation has one external family. **Mortality evidence is entirely from SINAN-overlapping Brazilian families.** Añazco reports CKD descriptively (3/34) only.
7. **Historical category holds (Supported/Supported),** with the independence limitation stated.

**Overlap note.** CKD shares much of its evidence base with DM and HTN: the same BRA-A and BRA-B models, and the same meta-analysis. The three core conditions are therefore not independently corroborated. Their joint selection reflects the same few large Brazilian datasets. This is acceptable for prioritisation but should be acknowledged.

### Hepatopathy — Secondary
1. **Hospitalisation:** assessed in BRA-A and BRA-B, but no condition-specific association was extracted. Hepatopathy is not among Cerqueira-Silva's hospitalisation factors. **Insufficient.**
2. **Mortality:** BRA-A only. Furtado aOR 1.781 (1.062–2.987); Cerqueira-Silva reports an association with in-hospital death.
3–6. A single overlapping family, a lower CI bound near 1, no external evidence, and a broad SINAN category.
7. **Historical categories hold (Insufficient/Limited → Secondary).**

### Autoimmune disease — Secondary (discrepancy)
1. **Hospitalisation:** BRA-A only. Furtado aOR 1.536 (1.297–1.819); Cerqueira-Silva direction only. **Limited.**
2. **Mortality:** Furtado 2026 (BRA-A) reports an adjusted death **aOR 2.766 (1.679–4.557)**. Cerqueira-Silva does not list autoimmune disease among its death factors.
7. **Discrepancy.** The historical "Insufficient" for severe/fatal matches the earlier summary's statement: "No clear evidence for CHIK-attributed death in the main evidence set reviewed". However, the completed workbook (modified 2026-09-30) records Furtado's adjusted death OR. Under the workbook definitions, a positive signal confined to a single overlapping family is **Limited**, not Insufficient.
   - **Effect on prioritisation: none.** Limited/Limited is still Secondary.
   - The table shows the historical "Insufficient" with a dagger footnote.
   - **Recommendation:** decide whether to update the category to Limited in a future version. This has not been done here.

### Haematological disease — Not prioritised
1–2. Assessed in BRA-A (Furtado; Cerqueira-Silva) and BRA-B (Pinto). No condition-specific estimate was extracted, and the condition is not among any reported associated factors. It is described only as a comorbidity in Silva Junior 2018.
7. **Insufficient/Insufficient holds.** This means insufficient evidence, not evidence of no effect. The category is also heterogeneous.

### Peptic ulcer disease — Not prioritised
1–2. Assessed in BRA-A (Furtado, "acid-peptic disease") and BRA-B (Pinto). No estimate was extracted.
7. **Insufficient/Insufficient holds.**
   - **Note:** the earlier summary (`chik_comorbid_evidence.xlsx`) states "one small study had crude OR 2.18 with a very wide CI". This estimate **cannot be traced** in the completed workbook.
   - It was not used. The source should be identified, or the statement removed from the earlier summary.

## Other reconciliation points

1. **Severe/fatal versus mortality.** The historical second column combined severe and fatal outcomes. On a death-only re-check, every category is unchanged except autoimmune disease. The hypertension borderline is discussed above.
2. **Script 24 family counts.** `03_Output/tables/evidence_prioritisation_cell_summary.csv` reports different counts. For example, it gives hypertension 3 independent families and CKD 2 independent families. Script 24 matches conditions by text pattern and pools outcomes, so its counts include severe-composite, descriptive and contextual mentions. The counts in this table use stricter rules: comparative evidence only, separated by outcome, with syntheses excluded. The script 24 figure was not modified.
3. **Missing CIs.** The workbook records Silva Junior 2019 estimates without CIs. The earlier summary lists CIs for them (for example, CKD 3.14, 1.82–5.42), but these were not transcribed into the completed workbook, so they are not reported here. If wanted, verify them against the original paper first.
4. **Rounding.** Some estimates in the display table are rounded to two decimals from the workbook values: autoimmune 1.536 → 1.54 and 2.766 → 2.77; liver 1.781 → 1.78. The audit CSV keeps the values as extracted.
5. **Not documented:**
   - prospective prespecification;
   - search dates and terms;
   - screening counts;
   - dual extraction.

   These are not claimed.
6. **Reproducibility.** Scripts 23 and 24 read the workbook from an absolute path outside the repository. Script 61 does the same, but copies a dated snapshot with an MD5.
