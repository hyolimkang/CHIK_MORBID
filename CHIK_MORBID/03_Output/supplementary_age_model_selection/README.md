# Supplementary tables: age-model selection

These tables support the Methods statement that hospitalisation uses **H2** (common age smooth plus penalised profile-specific deviations) and mortality uses **H0** (common age smooth, no profile-by-age interaction).

| File | Content |
|---|---|
| `Supplementary_Table_Age_Model_Selection_FINAL.docx` / `.pdf` | **Table Sx** (page 1): model fit and adult age-interaction tests, Panel A hospitalisation, Panel B death. **Table Sx+1** (page 2): observed deaths by profile and age group |
| `Supplementary_Table_Age_Model_Selection_FINAL.png` | Preview of Table Sx |
| `Supplementary_Table_Mortality_Event_Support_FINAL.png` | Preview of Table Sx+1 |
| `Supplementary_Table_Age_Model_Selection.csv` | Table Sx values (displayed strings and unrounded numbers) |
| `Supplementary_Table_Mortality_Event_Support.csv` | Table Sx+1 values |
| `Full_Model_Comparison_Audit.csv` | All 26 fitted specifications: formula, N, events, AIC, ΔAIC, EDF, adult and full-range Wald tests, coefficient tests, approximate LRTs, convergence, warnings, smoothing, source file, status |
| `MODEL_SELECTION_EVIDENCE_REPORT.md` | Evidence for each claim in the Methods paragraph, plus a suggested linking note |
| `TABLE_SOURCE_RECONCILIATION.md` | Source columns, verification results, discrepancies |
| `verification_checks.csv`, `anchor_reconciliation.csv`, `run_61.log` | Automated checks (24 / 24 passed) |

**Which table supports which statement:**
- "Substantially improved model fit (ΔAIC = −241)" and "limited evidence of age-dependent differences among adults": **Table Sx**.
- "Several profiles had few observed deaths": **Table Sx+1**.

**Reproduce** (from `CHIK_MORBID/`): `Rscript 02_Script/61_supplementary_age_model_selection.R`. Runtime is under 1 minute. No model is refitted. The PDF was rendered from the `.docx` with Microsoft Word.

**Script name.** The script name was specified by the task. It shares the number 61 with `61_evidence_selection_supplementary_table.R`; the two are unrelated.
