# Table 1: statistical validation

**Script:** `02_Script/59_table1_updated.R`. It halts if any check fails; `table1_checks.csv` records each check.

## Automated checks (25/25 passed)

| Check | Hospitalisation | Mortality |
|---|---|---|
| Eligible population = 1,074,796 | ✓ | ✓ |
| Final cohort = validated N (776,072 / 983,947) | ✓ | ✓ |
| Known + unknown = eligible (298,724 / 90,849 unknown) | ✓ | ✓ |
| Events = 25,363 hospitalisations / 1,050 deaths | ✓ | ✓ |
| Female + male = N | ✓ | ✓ |
| Sum of 8 mutually exclusive profiles = N | ✓ | ✓ |
| DM = sum of DM-containing profiles (38,661 / 45,529) | ✓ | ✓ |
| HTN = sum of HTN-containing profiles (93,530 / 110,350) | ✓ | ✓ |
| CKD = sum of CKD-containing profiles (3,739 / 4,412) | ✓ | ✓ |
| 8 profile counts = Figure 2 estimates' counts | ✓ | ✓ (also = brief's list) |
| Cohort N = flowchart output | ✓ | ✓ |

## Denominators
- **Cohort-selection section:** the eligible population (1,074,796). Unknown status: 298,724 / 1,074,796 = 27.8%; 90,849 / 1,074,796 = 8.5%.
- **All other sections:** the outcome-specific final cohort (776,072 or 983,947).
- **Outcome rows:** the event in its own cohort only. The other cohort's cell shows "—" (not applicable), never 0.

## Formatting rules
- Thousands separators; percentages to one decimal place.
- Where either cohort's percentage in a row is below 0.1%, **both** cells in that row use two decimals. This affects DM + CKD (0.03% / 0.03%) and HTN + CKD (0.11% / 0.09%). No underlying value was altered.
- Profile percentages sum to 100% within rounding: hospitalisation 99.94%, mortality 100.12% as displayed.
- Age is reported as median (IQR) of exact age in years, with an en dash in the range.

## Cross-document consistency

| Document | Status |
|---|---|
| Figure 2 (`Figure2_profile_RR_refined`) event/case counts | Consistent (same cohorts and profile counts) |
| Figure 3 cohorts (hospitalisation 776,072 / 25,363; death 983,947 / 1,050) | Consistent |
| Figure 4 within-count analysis (same cohorts) | Consistent |
| Cohort flowchart output (Supplementary Figure S1 source) | Consistent |
| Manuscript Methods/Results | Not checkable: the repository manuscript file is an outline. Its title says 2017–2024; it should say 2017–2025. |

No unrelated result was modified.

## Visual QA
- The table was rendered to `Table1_Study_Population_FINAL.png` with flextable's renderer, because LibreOffice is not installed for a Word-to-PDF render, and inspected.
- Design: three columns, light-grey section rows, thin horizontal rules, no vertical lines, Arial 8.5/9 pt, column widths 2.75 / 1.75 / 1.75 in. These are identical to script 18's Table 1 and fit a portrait page.
- No wrapping or overlap was seen. The Word file's appearance in Word itself should be confirmed once on opening.
