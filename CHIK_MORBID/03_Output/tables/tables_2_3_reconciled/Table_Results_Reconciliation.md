# Tables 2 and 3: results reconciliation

**Script:** `02_Script/60_tables_results_reconciliation.R`.

**Outputs (this folder):**
- `Table2_Joint_Profiles_FINAL.docx` and `.png`
- `Supplementary_Table_Individual_Conditions_FINAL.docx` and `.png`
- `Table2_Joint_Profiles_Validated.csv`
- comparison files `cmp_*.csv`

Previous tables are untouched. The manuscript itself is not modified; the repository copy is an outline with no tables.

## 1. Analysis sources

| Output | Script | Model (per outcome) | Standardisation target | Uncertainty |
|---|---|---|---|---|
| Old Table 2: individual conditions (`table_core3_standardised_risks.docx`) | `17_` ← `12_` §2–6 (`core3_rr_*.RData`) | `bam` fREML logistic: `s(age, k=10, ts) + sex + year + s(state, re) + dm + htn + ckd` | Whole outcome cohort, joint age × sex × year × state × (other two conditions) distribution; focal condition set to 1 vs 0 | 2,000 MVN coefficient draws (`vcov(unconditional = TRUE)`), seed 1; risks and RR from the same draws |
| Old Table 3: joint profiles (`table_joint_profile_standardised_risks.docx`) | `27_` ← `12_` §8 / `28_` (`core_profile_rr_*.RData`) | `gam` REML logistic: `core_profile + sex + year + s(state, re) + s(age, k=10, ts)` | Whole outcome cohort, joint age × sex × year × state distribution; profile set to each level | 2,000 MVN draws, seed 1, paired |
| Figure 2 (`Figure2_profile_RR_refined`) | `48_` | Same caches as old Table 3 | Same | Same |
| Figure 4 within-count RD (`Figure4_within_count_RD_FINAL`) | `56_`/`58_` | Same specification, refitted with `bam` fREML | Same target | 4,000 MVN draws, paired within contrast |

**Cohorts.** Both outcomes use the corrected outcome-known cohorts:
- hospitalisation: 776,072 cases, 25,363 events;
- mortality: 983,947 cases, 1,050 deaths.

The cached estimates were regenerated after Gate 3.1 (8 Oct). Profile counts in the caches sum exactly to these cohorts, and the script stops otherwise.

## 2. Verification

| Check | Result |
|---|---|
| Individual-condition estimates: model refitted on the current corrected cohorts and the standardisation re-run with script 12's seed and draws | Reproduces the cached values **exactly** (maximum relative difference 0) for all 18 risks and 6 RRs with CIs |
| Joint-profile RR = risk(profile) / risk(None), from unrounded risks | Exact (< 10⁻¹²) for all 16 rows |
| New Main Table 2 vs Figure 2 plotted values | Identical (maximum absolute difference 0) |
| Joint-profile risks vs independent `bam` refit (script 56) | Points ≤ 0.01% relative; CI bounds ≤ 1% (hospitalisation) and ≤ 5.5% (death, DM+CKD), which is Monte Carlo variation between draw sets |
| Pairwise differences of Table 2's unrounded risks vs Figure 4 RD points (same estimand and target) | ≤ 0.00005 percentage points for all 12 contrasts |
| Outcome-specific cohorts | Correct for both tables |

## 3. Discrepancy report

### A. Old Table 2 (individual conditions) → new Supplementary Table
**No numerical discrepancies.** All 6 rows (risk absent, risk present, RR, and every CI bound) match exactly (`cmp_old2.csv`). The old table already used the corrected mortality cohort. Formatting changes only:
- en dashes in CIs;
- "Death from chikungunya" relabelled "Chikungunya-attributed death";
- a note distinguishing marginal from joint contrasts, with cohort Ns added.

### B. Old Table 3 (joint profiles) → new Main Table 2
**No numerical discrepancies.** All 16 risks and RRs match (`cmp_old3.csv`). One error was found:

| Item | Old | New | Cause |
|---|---|---|---|
| Table note: mortality cohort N | "N=1,074,796" | 983,947 | **Other: stale hard-coded text.** `27_table_joint_profile_standardised_risks.R` writes the N as a literal string that was not updated after the Gate 3.1 correction. The estimates themselves are from the corrected cohort (their profile counts sum to 983,947). |

Structural changes:
- 5 → 4 columns; the None risk now sits in its own row, replacing the merged "risk under None" column;
- the outcome label is now "Chikungunya-attributed death";
- en dashes in CIs.

### C. New Main Table 2 vs final Figure 2
None: identical source and identical values.

### D. New Main Table 2 vs Figure 4 within-count RD
- **Points:** differ by ≤ 5 × 10⁻⁵ percentage points. **Cause: model specification (fitting engine only).** The specification is the same, but Table 2 uses `gam` (REML) and Figure 4 uses `bam` (fREML, discretised). This is invisible at any displayed precision.
- **CIs:** an RD CI cannot be derived from Table 2's separate risk CIs. Figure 4's RD CIs come from paired draws of both risks. This is **uncertainty estimation by design**, not a discrepancy. Readers should take RD CIs from Figure 4, not from overlap of Table 2 CIs.

### E. Likely source of the mortality mismatch you observed
If a manuscript version of Table 3 differs from Figure 2, it was built from the **pre-Gate-3.1 caches** (`01_Data/legacy_pre_gate31/`). Those used the legacy mortality cohort (N = 1,074,796), in which outcome codes 4/9/blank were counted as survivors. **Cause: outcome recoding, and hence cohort definition.** The numerator (1,050 deaths) is unchanged, but the denominators are larger, so absolute risks are lower and RRs shift slightly. Hospitalisation is unaffected.

| Mortality profile | Legacy risk, % (95% CI) | Corrected risk | Legacy RR (95% CI) | Corrected RR |
|---|---|---|---|---|
| None | 0.06 (0.06–0.07) | 0.07 (0.06–0.08) | 1.00 | 1.00 |
| DM only | 0.24 (0.19–0.32) | 0.26 (0.20–0.35) | 3.88 (2.92–5.09) | 3.84 (2.85–5.09) |
| HTN only | 0.16 (0.14–0.18) | 0.17 (0.15–0.20) | 2.55 (2.14–3.00) | 2.52 (2.12–2.97) |
| CKD only | 0.49 (0.28–0.87) | 0.54 (0.32–0.98) | 7.90 (4.32–13.85) | 7.99 (4.54–14.42) |
| DM + HTN | 0.31 (0.27–0.37) | 0.34 (0.30–0.41) | 5.03 (4.18–6.04) | 5.04 (4.20–6.08) |
| DM + CKD | 0.63 (0.25–1.66) | 0.67 (0.27–1.76) | 10.19 (3.93–25.96) | 9.89 (3.80–25.60) |
| HTN + CKD | 0.75 (0.51–1.20) | 0.85 (0.56–1.32) | 12.14 (7.99–19.23) | 12.58 (8.02–19.46) |
| DM + HTN + CKD | 0.98 (0.68–1.44) | 1.09 (0.75–1.58) | 15.71 (10.67–22.85) | 15.99 (10.74–23.26) |

The individual-condition mortality estimates show the same pattern; for example, the CKD RR was 4.10 (3.12–5.24) under the legacy coding and is 4.20 (3.28–5.30) after correction (`cmp_legacy_core3.csv`). Any manuscript text quoting the legacy values must be updated.

## 4. Conclusion
- **The new Main Table 2 and the Supplementary Table are fully consistent with the final Figures 2 and 4** and with the corrected outcome-specific cohorts.
- **There are no outstanding numerical discrepancies.**
- **One documentation error was fixed:** the mortality N in the old joint-profile table note.

Remaining items:
1. **Older manuscript drafts:** any table or text built before 8 Oct carries legacy mortality values (section 3E) and should be replaced with these tables.
2. **Supplementary table number:** to be assigned (`S[X]`).
3. **Display precision for mortality:** two decimals were kept, matching the original template. Mortality risks such as 0.07% may be better shown with three decimals (e.g. 0.068%) if editors request more precision; this is a display choice only.
4. **Table placement:** the new Table 2 cross-references Table 1 for case and event counts. It uses portrait A4 with 0.6-inch margins (column widths total 6.95 inches), so it fits a manuscript page; the old version was landscape.
