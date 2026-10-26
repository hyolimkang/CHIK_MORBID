# Gate 3.1 — Implementation report

Implements and finalises the Gate 3.1 mortality-outcome correction (audited and validated in `03_Output/validation/gate31/VALIDATION_3_1.md`) throughout the active analytical pipeline. Work is local; nothing pushed; manuscript DOCX not touched.

## 1. Exact files modified

**Code (2 files):**
- `02_Script/02_clean_chik_sinan_brazil.R` — `died_from_chik` recoded from `(EVOLUCAO == "2")` to an NA-aware `dplyr::case_when()`.
- `02_Script/38_mortality_outcome_validation.R` — unchanged this turn (already complete from the prior Gate 3.1 audit).

**Data (regenerated, 12 primary caches + 3 ancillary panel files):** see `changed_files_and_outputs.csv` for the full, exact list. In summary: the individual-level cleaned RDS, both `core3_rr_*.RData` files, both `core_profile_rr_*.RData` files (+ their draws and by-age variants), `fig3_age_specific_absolute_risk_data.RData`, `fig3_age_interaction_comparison_data.RData`, and `core_profile_rr_age_curve.RData`.

**Manuscript tables/figures (regenerated, 10 scripts rerun):** `13_`, `14_`, `17_`, `18_`, `19_`, `20_`, `22_`, `26_`, `27_`, `28_` — see §7 and `changed_files_and_outputs.csv`.

**Deliberately NOT modified:** `34_profile_burden_allocation_brazil.R` and `35_`–`37_` (population-burden scripts) — out of scope per instruction; their cached outputs are now stale (§7, `cache_dependency_audit.csv`).

## 2. Outcome recoding implemented

```r
died_from_chik = dplyr::case_when(
  EVOLUCAO == "2"           ~ TRUE,   # death from chikungunya
  EVOLUCAO %in% c("1", "3")  ~ FALSE, # cured, or death from another cause
  TRUE                       ~ NA     # 4, 9, blank, "0", any unmapped code
)
```
`map_evolucao` (the label mapping) was left unchanged — it was already correct. `evolution` (the label column) is retained unmodified as the permanent, auditable record of the raw code. `01_Data/chik_sinan_individual_2015_2025.rds` was rebuilt from the raw SINAN CSVs (available locally), not migrated from a cache — the clean path described in Gate 3.1's Phase B, not the fallback.

## 3. Cohort N and death count after correction

| | Before | After | Change |
|---|---:|---:|---:|
| Mortality cohort N | 1,074,796 | **983,947** | −90,849 (−8.45%) |
| Chikungunya deaths | 1,050 | **1,050** | 0 |
| Hospitalisation cohort N | 776,072 | 776,072 | 0 (unaffected) |
| Hospitalisations | 25,363 | 25,363 | 0 (unaffected) |

Full detail: `final_mortality_cohort_summary.csv`.

## 4. Updated condition-specific and profile-specific mortality RRs with CIs

See `old_vs_corrected_mortality_rr.csv` for the complete machine-readable table and `manuscript_methods_results_updates.md` for publication-ready text. Headline: every RR moved by ≤3.7% in either direction; every corrected point estimate falls inside the original 95% CI; the 8-profile risk ranking is **identical** before and after (DM+HTN+CKD highest, HTN remains lowest of the 7 non-None profiles).

## 5. Confirmation of what happened to cached results

All 11 mortality-relevant RData caches that existed before this correction were **copied, not moved**, to `01_Data/legacy_pre_gate31/` (alongside the pre-correction individual RDS) before any regeneration — the legacy baseline is fully preserved and loadable. The three caches guarded by a `file.exists()` short-circuit in `12_` (`core_profile_rr_{hosp,death}.RData`/`_draws.RData` as one guard, `fig3_age_interaction_comparison_data.RData`, `core_profile_rr_age_curve.RData`) were explicitly **deleted** from their live location before rerunning `12_`, so the guard could not silently reuse a pre-correction result. All other mortality caches in `12_` have no cache guard at all (always recomputed on every run), so simply rerunning `12_` regenerated them correctly without any special handling. Verified post-hoc: all 6 of the guard-relevant files now carry a modification timestamp after the corrected RDS was built (Phase 8, test E1).

## 6. Confirmation that hospitalisation analysis was not inadvertently changed

Verified directly against the actual regenerated cohort data (not assumed): hospitalisation cohort N = 776,072 and hospitalisations = 25,363, both exactly matching the pre-correction baseline (Phase 8, tests C1–C2). No hospitalisation-related code (`hospitalised`, `hosp_only`, `hosp_cohort`, `fit_hosp_*`) was touched by the Phase 2 edit, which only modifies the `died_from_chik` assignment line.

## 7. Outputs regenerated, and outputs still stale

**Regenerated this session (10 scripts, all exit code 0):** `13_core3_descriptive.R`, `14_condition_recording_patterns.R`, `17_table_core3_standardised_risks.R`, `18_table1_analytic_cohorts.R`, `19_refresh_core3_risk_cis.R`, `20_table_core3_risk_differences.R`, `22_figure_cohort_flow.R`, `26_table_clinical_outcomes_by_condition.R`, `27_table_joint_profile_standardised_risks.R`, `28_refresh_joint_profile_risk_cis.R` (plus `12_core3_condition_specific_rr.R` itself, which these all depend on).

**Still STALE (deliberately not regenerated, out of scope for Gate 3.1):**
- `01_Data/brazil_profile_burden_allocation.RData` + `brazil_profile_burden_group_ui.RData` (produced by `34_`, last built 2026-10-06, reads the now-corrected `core_profile_rr_death*.RData` but has not itself been rerun with them)
- All 8 figures from `34_`/`35_`/`36_`/`37_` that derive from the above (full list in `cache_dependency_audit.csv`)

These must be regenerated (rerun `34_`, then `35_`–`37_` in any order) before any population-burden validation work (Gate 3.2+) begins, since they currently embed legacy, pre-correction mortality RRs.

**Not re-audited this gate (used `died_from_chik` per an earlier grep but outside the explicit Phase 6 list and not re-verified):** `02_Script/descriptive_appendix/descriptive.R`, `02_Script/side_analyses/table_basic.R`, `02_Script/archive/*` — flagged, not modified.

## 8. Commands executed and test results

See `tests_and_assertions.md` for the full command list and the 12 formal pass/fail assertions (Phases A–F). **All 12 passed**, including an exact (0.00% difference) match between the production pipeline's regenerated RRs and Gate 3.1's independently-coded validation script.

## 9. Remaining assumptions, limitations, unresolved issues

- The correction assumes EVOLUCAO codes 4 ("death under investigation") and 9 ("unknown") and blank/unmapped codes genuinely carry no usable chikungunya-attribution information — this is the conservative, defensible default, not a claim that these records are missing completely at random.
- Gate 3.1's sensitivity scenarios (plausible-weighted and stress-test) only addressed the 142 investigation-pending-but-confirmed-dead records, using the attribution rate among already-resolved deaths as the imputation probability. **This does not, and was never intended to, establish that the much larger "unknown"/blank group (90,707 records) is missing at random** — there is no analogous resolved-outcome reference population for that group, and no attempt was made to impute an outcome for it. This is an explicit, acknowledged residual limitation, not resolved by this gate.
- `rr_death_model.RData` (used by the separate, older `03_relative_risk.R` 7-condition pipeline) was left untouched — it is a different cohort construction entirely and out of scope for this gate; if that pipeline is still an active manuscript dependency, it has its own, unaudited version of the same `EVOLUCAO` question and should be checked separately.
- Bootstrap draws regenerated this session use the same seeds/model specifications as before the correction, but are of course numerically different realisations (different cohort → different fitted coefficients → different draws) — this is expected and correct, not a reproducibility failure.

## 10. Gate 3.1 status: **PASS**

**Implementation correctness:** PASS. The audited bug is fixed at its source, verified against the official SINAN dictionary; the fix was applied to the actual cleaning script (not just a parallel validation script); the full dependent pipeline was regenerated with legacy results preserved; cache staleness was explicitly eliminated and verified; the regenerated production RRs match the independently-computed Gate 3.1 validation numbers to displayed precision; hospitalisation was verified unaffected; and the manuscript-facing cohort flow, Table 1, and RR tables are now mutually consistent (all reflect N=983,947 for mortality).

**Residual epidemiological uncertainty:** real but unchanged from Gate 3.1's own assessment — the unresolved-outcome group's missingness mechanism remains unverified beyond the narrow, well-justified 142-record investigation-pending subset. This is a property of the data, not of this implementation, and is now explicitly documented in the suggested Limitations text (`manuscript_methods_results_updates.md`) rather than left unstated.

**Repository readiness for the next validation gate (3.2, hospitalisation missingness):** ready to begin. The mortality-side correction is complete, consistent, and non-blocking for that separate question. Before any population-burden validation (beyond Gate 3.2), `34_`–`37_` must first be rerun (§7) — this is a known, explicitly tracked prerequisite, not a blocker for Gate 3.2 itself.
