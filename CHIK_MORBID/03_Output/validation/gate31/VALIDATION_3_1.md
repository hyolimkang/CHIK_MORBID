# Gate 3.1 — Validation of chikungunya-attributed death classification

**Scope:** audit and correct `died_from_chik` in `02_clean_chik_sinan_brazil.R`, and assess impact on the mutually-adjusted DM/HTN/CKD and 8-profile mortality RR models in `12_core3_condition_specific_rr.R`. Hospitalisation cohort/outcome untouched. Work is local; nothing pushed; no caches overwritten.

**Script:** `02_Script/38_mortality_outcome_validation.R`
**Full console log:** reproducible by re-running that script; key numbers below are copied directly from its output, not re-derived by hand.

---

## 1. What was wrong

`EVOLUCAO` (field 74 of the official SINAN data dictionary, `01_Data/sinan_chik_docs/dic_dados_chikungunya.pdf`) was verified to have exactly five valid codes:

| Code | Meaning (PT) | Meaning (EN) |
|---|---|---|
| 1 | cura | cured |
| 2 | óbito pelo agravo | death **from chikungunya** |
| 3 | óbito por outras causas | death from another cause |
| 4 | óbito em investigação | death, cause **under investigation** |
| 9 | ignorado | unknown |

`map_evolucao` in `02_clean_chik_sinan_brazil.R` already encodes this correctly — the label mapping was never the problem. The bug is one line later:

```r
died_from_chik = (EVOLUCAO == "2")
```

In R, `x == "2"` evaluates to `FALSE` — not `NA` — for every non-missing string that isn't `"2"`, including `"4"`, `"9"`, and blank. Empirically (see §2), blank `EVOLUCAO` in this pipeline is the empty string `""` after `data.table::fread`, not R's `NA`, so `"" == "2"` is also `FALSE`. **The practical consequence: `died_from_chik` currently contains zero `NA` values anywhere in the cleaned data.** Every downstream `!is.na(died_from_chik)` filter (in `12_`, `13_`, `19_`, `22_`, `28_`) is therefore a no-op — it excludes nobody — and the entire mortality "control" group silently includes everyone whose chikungunya-death status was never actually resolved.

A secondary finding from the raw-data audit: EVOLUCAO also contains **130 records with code `"0"`**, which is not in the official dictionary at all. This code was already (correctly, if incidentally) mapped to `NA` by `recode()`'s unmapped-value fallback, so it shares the same bug as codes 4/9 — currently `FALSE`, should be `NA`.

A DT_OBITO cross-check confirms raw data integrity is otherwise sound: every EVOLUCAO∈{2,3,4} record has a non-missing death date and every EVOLUCAO∈{1,9} record does not, exactly matching the dictionary's mandatory-field rule.

**Conclusion: the suspected error is real, not a false alarm — and is larger in scope than the original concern (the blank/unmapped-code share of the bug had not been identified in the brief; it is roughly the same size as the code-9 share).**

## 2. How many records were affected

Analytic population used throughout (confirmed chikungunya, 2017–2025, valid age/sex, known DM/HTN/CKD — identical filter to `12_`'s `add_core3()`): **N = 1,074,796**.

| `evolution` category | n | % of cohort | current `died_from_chik` | correct classification | currently misclassified? |
|---|---:|---:|---|---|---|
| cured | 982,538 | 91.42% | FALSE | FALSE (non-event) | no |
| death_chik | 1,050 | 0.10% | TRUE | TRUE (event) | no |
| death_other | 359 | 0.03% | FALSE | FALSE (non-event; competing risk) | no |
| death_under_investigation | 142 | 0.013% | FALSE | **NA (exclude)** | **yes** |
| unknown | 44,594 | 4.15% | FALSE | **NA (exclude)** | **yes** |
| NA / blank / unmapped code "0" | 46,113 | 4.29% | FALSE | **NA (exclude)** | **yes** |

**Total misclassified: 90,849 / 1,074,796 = 8.45% of the mortality analytic cohort** — currently coded as confirmed non-deaths when their chikungunya-death status is actually unresolved. Full breakdown: `mortality_status_audit.csv`.

Association with stratifiers (`mortality_status_by_year_profile.csv`, chi-square tests in the script log): unresolved status is strongly associated with **calendar year** and **state** (both p≈0, driven by known reporting/closure-lag heterogeneity across years and states), weakly with **age group** (p≈1.6×10⁻¹⁹, small effect size), **not** associated with **sex** (p=0.77), and statistically but not practically associated with **core_profile** (p=7×10⁻⁵, but the unresolved rate ranges only 7.9%–11.2% across the 8 profiles — DM+CKD 7.9%, None 8.4%, DM 8.3%, DM+HTN 8.6%, HTN 8.7%, DM+HTN+CKD 9.3%, CKD 10.2%, HTN+CKD 11.2%). There is no large differential concentration of unresolved status in any one comorbidity profile.

## 3. Does the manuscript's current mortality N and RRs remain valid?

**Cohort size changes; point estimates and conclusions do not.**

- Mortality cohort N: **1,074,796 → 983,947** (−90,849, −8.45%)
- Deaths: **1,050 → 1,050 (unchanged)** — the correction only removes ambiguous non-events; no confirmed chikungunya death is reclassified (verified by an explicit `stopifnot` in the script)

RR comparison (legacy "B" = current manuscript values vs corrected "A" = primary analysis; full table `mortality_rr_before_after.csv`, figure `mortality_rr_comparison.png`):

| | Legacy RR (95% CI) | Corrected RR (95% CI) | Absolute Δ | % Δ |
|---|---|---|---|---|
| Diabetes | 2.25 (1.90–2.64) | 2.26 (1.92–2.63) | +0.009 | +0.39% |
| Hypertension | 2.20 (1.91–2.58) | 2.19 (1.88–2.57) | −0.011 | −0.52% |
| Chronic kidney disease | 4.10 (3.12–5.24) | 4.20 (3.28–5.30) | +0.098 | +2.38% |
| DM (profile, vs None) | 3.88 (2.92–5.09) | 3.84 (2.85–5.09) | −0.035 | −0.91% |
| HTN (profile) | 2.55 (2.14–3.00) | 2.52 (2.12–2.97) | −0.027 | −1.05% |
| CKD (profile) | 7.90 (4.32–13.85) | 7.99 (4.54–14.42) | +0.085 | +1.08% |
| DM+HTN (profile) | 5.03 (4.18–6.04) | 5.04 (4.20–6.08) | +0.003 | +0.06% |
| DM+CKD (profile) | 10.19 (3.93–25.96) | 9.89 (3.80–25.60) | −0.298 | −2.93% |
| HTN+CKD (profile) | 12.14 (7.99–19.23) | 12.58 (8.02–19.46) | +0.439 | +3.62% |
| DM+HTN+CKD (profile) | 15.71 (10.67–22.85) | 15.99 (10.74–23.26) | +0.287 | +1.83% |

Every corrected point estimate falls well inside the legacy 95% CI and vice versa. **8-profile ranking by RR is identical before and after** (DM+HTN+CKD > HTN+CKD > DM+CKD > CKD > DM+HTN > DM > HTN, both scenarios) — the CKD-driven ranking the manuscript's conclusions rest on is unchanged.

**The manuscript's current mortality N (1,074,796) and reported RRs are therefore quantitatively robust to this bug, but the N is not the technically correct denominator and should not be published as-is.**

## 4. Which code and downstream outputs require updating

**Confirmed needs an update** (the bug is in shared cleaning code, so every consumer of `died_from_chik` inherits it):

- `02_Script/02_clean_chik_sinan_brazil.R` — line defining `died_from_chik`. Recommended fix: replace the boolean with the label-based classifier validated in this gate (`classify_died_from_chik()` in `38_`, built on the already-correct `evolution` column), or equivalently make the boolean NA-aware: `died_from_chik = dplyr::case_when(EVOLUCAO == "2" ~ TRUE, EVOLUCAO %in% c("1","3") ~ FALSE, TRUE ~ NA)`.
- `02_Script/12_core3_condition_specific_rr.R` — death_cohort N and all downstream mortality RR tables/figures change (modestly, per §3) once `died_from_chik` is NA-aware; the cached `01_Data/core3_rr_death.RData` / `core_profile_rr_death.RData` and their draw files will need regenerating.
- `02_Script/13_core3_descriptive.R`, `19_refresh_core3_risk_cis.R`, `28_refresh_joint_profile_risk_cis.R` — all filter on `!is.na(died_from_chik)`; all currently no-ops on the death side, all need rerunning after the fix.
- `02_Script/22_figure_cohort_flow.R` — **directly visualises this exact bug.** The published flowchart's mortality branch currently reads "Mortality cohort N = 1,074,796 / Excluded: Chikungunya death status unknown, n = 0." After the fix this becomes N ≈ 983,947 with n ≈ 90,849 excluded. Flagged to the user during this session; **not regenerated here**, per the instruction to stop after Gate 3.1.
- `02_Script/14_condition_recording_patterns.R`, `26_table_clinical_outcomes_by_condition.R` — use `died_from_chik` for descriptive death counts; should be checked/rerun for consistency, though not formally re-audited in this gate.

**Not affected:** the hospitalisation cohort/model (`hosp_only`/`hospitalised` — untouched, never read by this script) and every non-mortality analysis in the repository.

## 5. Limitations and implications for manuscript interpretation

- **Scenario C's "plausible" imputation is scoped narrowly on purpose.** It only reassigns probability mass for the 142 `death_under_investigation` records (all of which have a confirmed death date — they are known deaths, only the chikungunya-attribution is pending), using the observed attribution ratio among already-resolved deaths (1050/(1050+359) = 74.52%) as a soft prior weight. It deliberately does **not** attempt to impute an outcome for the much larger "unknown"/blank group (90,707 records), because those people are not even confirmed to have died — there is no principled reference population to borrow a chikungunya-death probability from. Scenario A (exclude all three unresolved categories) is the statistically clean primary analysis for that reason.
- **Scenario D is explicitly a mathematical stress test, not an epidemiological estimate** — it assumes all 142 investigation-pending deaths resolve to chikungunya, which has no evidential support. It is reported only to show that even this implausible upper bound moves every RR by at most a few percent (see figure), underscoring that the 142-record group is numerically too small to threaten any conclusion.
- Sparsity caveat carried over from the existing pipeline: `DM+CKD` and `HTN+CKD` already have the widest CIs among the 8 profiles in both legacy and corrected analyses (pre-existing low event counts: 27 and 116 deaths respectively) — this gate does not change that underlying sparsity, it only confirms the point estimates are stable.
- This gate re-derives `evolution`/`died_from_chik` from the already-cleaned RDS rather than re-running `02_clean_chik_sinan_brazil.R` end-to-end; Part A.1's independent read of the raw CSVs (bypassing the cleaned RDS entirely) was the cross-check used to confirm no unmapped raw code is being silently absorbed anywhere in that chain.
- Bootstrap precision differs by scenario by design: Scenario A (primary) uses B=2000 draws (matching the original pipeline's precision) for both the core-3 and 8-profile models; Scenarios C and D use B=500 (adequate for the sensitivity question "does this move the estimate meaningfully", not intended as a publication-precision CI).
- No individual-level SINAN access blocker was encountered — both the raw CSVs and the cleaned RDS were available locally, so no synthetic-data fallback was needed; the unit tests in Phase B are nonetheless included for ongoing regression protection, independent of data availability.

## 6. Assessment: **CONDITIONAL PASS**

**Evidence for PASS:** the originally suspected error is real and confirmed against the official SINAN dictionary (not an assumption); it was fully characterised (8.45% of the mortality cohort, 90,849 records); a corrected, unit-tested classification was implemented and validated; refitting the exact original model structure on the corrected cohort reproduces every RR within the original 95% CIs, with zero change to the 8-profile risk ranking that the manuscript's CKD/multimorbidity conclusions depend on; three additional sensitivity scenarios (legacy, plausible-weighted, stress-test) all agree with the primary corrected estimate within a few percent.

**Why not an unconditional PASS:** the bug is real and currently live in the shared cleaning code and every cached mortality output in the repository (`01_Data/core3_rr_death.RData`, `core_profile_rr_death.RData`, and the figures/tables built from them, including the published cohort-flow diagram's mortality branch, which currently shows a factually incorrect "0 excluded"). Numerical robustness of the *point estimates* does not mean the *code and cached artefacts* are currently correct — they are not, and should not be left as-is before any further manuscript revision or submission.

**Required before this can be closed out as a full PASS:**
1. Apply the corrected `died_from_chik` definition in `02_clean_chik_sinan_brazil.R` (not done in this gate — out of scope per the stop instruction).
2. Rerun `12_`, `13_`, `19_`, `22_`, `28_` (and spot-check `14_`/`26_`) so every cached mortality table/figure reflects N=983,947 rather than N=1,074,796.
3. Re-verify this gate's conclusion (RR robustness, ranking stability) against the actual rerun outputs, not just this validation script's independent refit — the two should agree, but that agreement should be confirmed, not assumed.

---
*Generated by `02_Script/38_mortality_outcome_validation.R`. Baseline (pre-correction) results preserved at `03_Output/validation/gate31/baseline_mortality_cohort_PRESERVED.rds`; original manuscript caches `01_Data/core3_rr_death.RData` and `01_Data/core_profile_rr_death.RData` were read but never modified.*
