# Gate 3.2A — Hospitalisation-outcome missingness audit (descriptive)

Brazil, confirmed chikungunya cases, 2017–2025. Produced by `02_Script/39_hospitalisation_missingness_audit.R`. Scope, per explicit instruction: **descriptive missingness characterisation only**. No IPW-weighted outcome model, no multiple imputation, no MNAR sensitivity analysis was implemented in this pass; the optional recording-probability/diagnostic-IPW-weight step (original Phase 5) was deliberately **deferred** (see §9) so it could not delay or dilute the descriptive audit. The primary hospitalisation RR model (`12_core3_condition_specific_rr.R`) was not touched, re-fit, or re-estimated in any way.

## 1. Verified cohort counts

| | N | % of eligible |
|---|---:|---:|
| Core-three eligible population | 1,074,796 | 100.00% |
| Hospitalisation outcome recorded (yes/no) | **776,072** | 72.21% |
| Hospitalisation outcome NOT usable, total | 298,724 | 27.79% |
| &nbsp;&nbsp;.. explicit "unknown" (HOSPITALIZ=9, *Ignorado*) | 25,378 | 2.36% |
| &nbsp;&nbsp;.. blank / never recorded (HOSPITALIZ missing) | 273,346 | 25.43% |
| Hospitalisations among recorded cohort | 25,363 | 3.27% of recorded |

All four counts match the task specification exactly (N=1,074,796 / 776,072 / 298,724 / 25,363). **PASS.**

Two distinct missingness mechanisms exist and are tracked separately throughout: an **explicit "don't know"** response (code 9, 2.36% of eligible — the field was actively completed as unknown) and a **field left blank** (25.43% of eligible — the field was never completed at all). 91.5% of all non-recording is the blank/unrecorded type, not the explicit-unknown type. This distinction matters for interpretation: a genuinely blank field is more plausibly a surveillance/administrative gap (consistent with the strong geographic/temporal patterns in §2–3 below), whereas an explicit "unknown" response is more plausibly a clinical/reporting judgement at the point of care.

Full detail: `cohort_reconciliation.csv`.

## 2. Silent-exclusion check (state covariate)

Phase 2 of the task asked whether `fit_hosp_core3`'s `s(uf_residence, bs="re")` term causes additional silent row-drops beyond the explicit `hospitalised %in% c("no","yes")` filter.

**Finding: uf_residence (SG_UF) has zero missing values** in both the eligible cohort (1,074,796) and the hospitalisation-known cohort (776,072). age_years, sex, and year are guaranteed non-missing by the eligibility filter itself. **No other covariate used by `fit_hosp_core3` can be missing**, so the model's actual fitted N is expected to equal 776,072 exactly — the complete-case cohort — with no additional silent exclusion. **PASS** (verified directly against the cleaned RDS, not assumed).

Note: the raw, unfiltered data does contain a handful of corrupt/garbage `uf_residence` values (e.g. `"%g"`, `"~."`, mojibake fragments) — these do not survive the eligibility filter (`is_confirmed_chik` + valid age/sex/core-3 fields), so they never reach the hospitalisation cohort.

## 3. Completeness and missingness patterns

### 3a. Patient characteristics (`hospitalisation_missingness_by_age_profile.csv`)

| Stratifier | Range of % recorded | Pattern |
|---|---|---|
| Age (5-band and 10-year) | 71.8%–73.0% | Flat — essentially no age gradient |
| Sex | 72.07% (F) / 72.42% (M) | Flat |
| DM present vs absent | 77.68% vs 71.94% | +5.7pp higher when DM present |
| HTN present vs absent | 77.36% vs 71.55% | +5.8pp higher when HTN present |
| CKD present vs absent | 76.32% vs 72.19% | +4.1pp higher when CKD present |
| 8-profile gradient | 71.45% (None) → 79.62% (HTN+CKD) | Monotonic-ish: more comorbidity → more complete recording |
| Lab-confirmation criterion | 57.89% (clinical only) vs 72.14–72.84% (clinical-epi / laboratory) | Clinical-only confirmed cases are markedly under-recorded |

**Key pattern:** recording completeness is NOT flat across the comorbidity dimension that the primary RR analysis is built on. People with any core-3 condition are recorded ~5–8 percentage points more completely than people with none, and recording improves further with each additional condition (CKD-involving profiles — CKD alone, HTN+CKD, DM+CKD — show the highest observed hospitalisation rates among the recorded subset too, 8.99–15.82%, consistent with the known severity ranking). This is a real, non-trivial gradient, even though it is far smaller in magnitude than the geographic/temporal gradient in §3b.

### 3b. Calendar year, state, region (`hospitalisation_missingness_by_year_state.csv`, figures)

**Calendar year** (`figure_missingness_by_year.png`): strongly non-monotonic — 58.47% (2017, worst) → 83.45% (2019, best) → dips to 64–66% through the 2020–2022 pandemic-adjacent years → recovers to 86.53% (2025, best overall). This is a far larger swing (28 percentage points) than any patient-characteristic gradient in §3a.

**State of residence and region** (`figure_state_year_missingness_heatmap.png`): the largest effect found anywhere in this audit. Recording completeness ranges from **24.39% (Distrito Federal, DF)** to **96.25% (Espírito Santo, ES)** — a >70-percentage-point spread across states. Other persistently poor-recording states: RN (49.0%), PE (50.2%), CE (56.8%), PB (58.5%) — all Nordeste. Persistently good-recording states: ES (96.2%), RS (92.8%), SC (92.2%), PR (91.9%) — concentrated in Sul/Sudeste. At the region level: Nordeste 60.41% known vs Sul 91.98% known.

The year×state heatmap shows this is not a uniform regional offset but includes **state-specific, time-localised breakdowns**: DF's recording completeness **collapses from ~60–85% in 2017–2019 to under 20% from 2020 onward** and stays there through 2025 — a sustained, severe, single-state surveillance gap that dominates DF's marginal statistics. GO, BA, and MT each show one or two isolated bad years rather than a sustained pattern.

**No stratum met the `near_complete_nonrecording_flag` (>95% missing) threshold**, including DF at its worst (75.6% missing) — so no cell is being silently reduced to near-zero information, but several (DF year×year, RN, PE) are close enough to materially limit what a state-stratified analysis could support.

### 3c. Laboratory confirmation and cross-tabs

Clinical-only confirmed cases (criterion="clinical", n=16,943, 1.6% of the eligible cohort) have the worst recording completeness of any single stratifier value found (57.89%) outside of specific state/year cells — worth flagging as its own, independent missingness axis, not explained away by age/sex/geography alone without further investigation.

## 4. Differences between recorded and unrecorded populations (`recorded_vs_unrecorded_characteristics.csv`)

Standardised mean differences (SMD; binary/continuous via the standard pooled-SD formula, multi-category variables via the Yang & Dalton 2012 generalised multinomial SMD). Conventional interpretation: <0.1 negligible, 0.1–0.2 small, 0.2–0.4 moderate, >0.4 large.

| Variable | SMD | Interpretation |
|---|---:|---|
| Age (years) | 0.008 | Negligible |
| Sex (% male) | 0.008 | Negligible |
| Diabetes present | 0.062 | Negligible–small |
| Hypertension present | 0.094 | Small (borderline) |
| CKD present | 0.014 | Negligible |
| Laboratory-confirmed | 0.027 | Negligible |
| **Calendar year (9 categories)** | **0.466** | **Large** |
| **Region (5 categories)** | **0.485** | **Large** |
| 8-profile (core_profile) | 0.101 | Small |
| Chikungunya death rate, among death-status-known (secondary diagnostic) | 0.038 | Negligible-to-small on SMD scale, but a 5× relative difference (0.135% recorded vs 0.027% unrecorded) |

**Conclusion:** the dominant axis of selection into "hospitalisation status recorded" is **geography and calendar time**, not patient demographics or individual comorbidity status. The 8-profile SMD (0.101) is non-trivial but an order of magnitude smaller than the year/region SMDs. The secondary mortality-status diagnostic shows recorded cases have a notably higher *observed* chikungunya-death rate than unrecorded cases among people whose death status is separately known — consistent with hospitalisation and death reporting being completed together for more clinically severe/closely-followed cases, though this is descriptive only and not a causal claim about missingness mechanism.

## 5. Recording-probability model specification

**Not fit in this pass.** See §9.

## 6. Calibration and positivity diagnostics

**Not computed in this pass** (depends on §5). See §9.

## 7. Diagnostic IPW weight distribution

**Not computed in this pass** (depends on §5). Placeholder files (`recording_probability_diagnostics.csv`, `diagnostic_ipw_weight_distribution.csv`) were written with an explicit "DEFERRED" status row rather than left absent, so downstream tooling does not silently treat a missing file as an error. See §9.

## 8. Implications for selection bias

- Missingness is **far from uniform**: it is concentrated in specific states (worst: DF, RN, PE, CE, PB) and specific years (worst: 2017; moderately worse: 2020–2022), with DF showing a severe, sustained, single-state collapse from 2020 onward that is large enough to materially affect any state-stratified or recent-year-weighted analysis.
- Missingness shows a **real but modest** gradient across the DM/HTN/CKD profile dimension that the primary RR analysis is built on (profile SMD 0.101; 71.45% recorded for "None" vs up to 79.62% for some comorbid profiles) — people with core-3 conditions are somewhat *more* likely to have hospitalisation status recorded, not less. If anything, this would be expected to work against inflating a comorbidity→hospitalisation RR via differential recording (more complete ascertainment in the exposed groups), rather than to manufacture it, though this is a descriptive inference, not a formal bias-direction proof.
- Age and sex show **no meaningful association** with recording completeness (SMD <0.01 for both), which is reassuring for the age/sex-adjustment structure already in the primary model.
- Laboratory-confirmation status is an **independent, non-trivial** predictor of missingness (clinical-only cases far worse recorded) that is not currently part of the primary model's adjustment set.
- The secondary diagnostic linking hospitalisation-recording status to chikungunya-death status (a different, already-corrected Gate 3.1 outcome) shows a relative 5× difference in observed death rates between the two groups — worth keeping in mind as a plausibility check in Gate 3.2B, though the small absolute Ns (980 vs 70 deaths) limit how far this single contrast can be pushed.

## 9. Major assumptions and what remains unverified

- **The Phase 5 recording-probability model and diagnostic IPW weights were explicitly deferred this pass**, per updated task scope ("descriptive missingness assessment only... the preliminary recording-probability model is optional and should not delay the descriptive audit"). This means Gate 3.2A cannot yet speak to formal positivity/overlap diagnostics, calibration, or IPW weight-tail behaviour — only to the raw, unweighted missingness patterns above. If Gate 3.2B proceeds, the state- and year-level imbalance found here (§3b, §4) strongly suggests that `state` and `calendar_year` — not just age/sex/profile — would need to be central covariates in any such model, given they carry far more of the observed imbalance than any clinical variable.
- As with Gate 3.1, **an accurate recording-probability model would not, by itself, establish that hospitalisation status is missing at random (MAR).** No attempt is made here to test or assume MAR; the geographic/temporal concentration of missingness is at least as consistent with a surveillance-capacity/administrative mechanism (plausibly MAR conditional on state+year) as with an outcome-dependent one, but the data cannot distinguish these from the observed pattern alone.
- The `is_lab_confirmed` boolean column in the cleaned data (`CLASSI_FIN=="13" & CRITERIO=="1"`) uses the same bare `==` pattern that caused the Gate 3.1 `died_from_chik` bug (i.e. it could silently return `NA` rather than `FALSE` for the vanishingly few confirmed cases with missing `CRITERIO`). This audit sidesteps that risk by using the separately-coded, NA-safe `criterion` label column instead (only 53/1,074,796 `NA`). This is **not itself a missingness-mechanism finding** — it is flagged here only as a residual code-quality observation outside Gate 3.2A's scope, parallel to but distinct from the Gate 3.1 fix.
- Sample-size caveats: several state×year cells are sparse (flagged via `sparse_flag` in the by_year_state CSV, n<30) and a few state×year combinations have zero eligible cases (rendered blank in the heatmap) — these do not change the marginal conclusions above but should not be over-interpreted individually.

## 10. Recommendation

The missingness pattern found here is **not plausibly ignorable with respect to geography and calendar time** (SMD 0.47–0.49, a >70-point range in state-level recording completeness, and a severe single-state time-localised collapse in DF), and shows a **real, smaller signal on the comorbidity-profile dimension itself** (SMD 0.10) that is directly relevant to the primary RR analysis. Age, sex, and laboratory-confirmation status show weaker or independent patterns.

Given this: **further sensitivity analysis is scientifically warranted before the primary hospitalisation RR estimates can be considered robust to this missingness**, but the specific form should be chosen deliberately rather than defaulted to standard IPW:

- A **state/year-stratified or state/year-adjusted re-analysis** (e.g. including state and calendar year as covariates or strata in a sensitivity model, or restricting to high-completeness state/year strata as a bounding exercise) is likely to be the most informative next step, since that is where almost all of the imbalance lives — this could proceed with or without formal IPW weights.
- **Standard IPW** is one reasonable option for Gate 3.2B but is not obviously necessary on its own merits yet: the modest profile-level SMD (0.10) suggests IPW's main value here would be correcting for state/year imbalance rather than comorbidity-driven selection, which a stratified/adjusted approach could address more transparently and with less sensitivity to extreme weights in low-recording states like DF.
- An **MNAR sensitivity analysis** is not yet clearly warranted on the evidence in this report alone — nothing here demonstrates outcome-dependent missingness (as opposed to geography/time-dependent missingness), though the hospitalisation–death-status correlation in §8 means it should not be ruled out either.
- Whatever is chosen, **DF (and secondarily RN/PE/CE/PB)** should be treated as a case for explicit sensitivity handling (e.g. a leave-one-state-out check, or separate reporting) given how extreme and sustained its recording gap is relative to every other state.

This recommendation is offered for approval before any further implementation. No weighted model, imputation, or MNAR analysis has been implemented in this gate.
