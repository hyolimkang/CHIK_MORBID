# Gate 3.3B - PNS 2019 population multimorbidity prevalence feasibility assessment

Brazil, PNS 2019 (selected-adult sample). Produced by `02_Script/43_pns2019_joint_profile_feasibility.R`. Feasibility/data-validation analysis only - no SINAN RR model refit, no change to GBD scripts 29_/30_/32_/34_, no burden rerun, no PNS substitution into the burden pipeline, no copula/clustering model fit. Nothing committed/pushed; raw PNS microdata gitignored.

## 1. Survey population and design

Selected-adult analytic population: **N=90,846** (`M001=='1'`, verified against the calibrated selected-adult weight `V00291` with exact 1:1 correspondence - see `pns2019_source_and_survey_design.md` for the full structural-verification narrative). Complex design: **8,027 PSUs**, **574 strata**, calibrated selected-adult weights, implemented via `survey::svydesign(ids=~UPA_PNS, strata=~V0024, weights=~V00291, nest=TRUE)`.

Ages 18-19 preserved separately (2,021 respondents); official-estimate quality check uses 18+; main joint-profile analysis uses 20+ (N=80,228 profile-eligible), aligned with the existing SINAN core-3 age strata.

## 2. Verified variable definitions

Full crosswalk: `pns2019_variable_definition_crosswalk.csv`. Headline: PNS asks *'Has a doctor ever diagnosed you with [condition]?'* for DM (Q03001), HTN (Q00201), and CKD (Q124) - all physician-diagnosis self-report, verified word-for-word against the official IBGE dictionary (not assumed). HTN and DM each have a pregnancy-only follow-up question (Q00202/Q03002); CKD does not. Pregnancy-only diagnoses (1277 HTN, 273 DM, of 90,846 selected adults) were reclassified to 'no' to match SINAN's pre-existing-chronic-disease concept - an explicit, documented methodological choice, not a silent one. Unknown/unanswered responses were kept as a distinct 'unknown' category throughout (never coerced to 'no') - see `pns2019_cohort_and_missingness.csv`.

## 3. Individual-condition prevalence (survey-weighted)

| Condition | 18+ | 20+ |
|---|---|---|
| Diabetes | 8.238% (7.929-8.558%) | 8.499% (8.188-8.82%) |
| Hypertension | 24.41% (23.899-24.928%) | 25.276% (24.76-25.798%) |
| Chronic kidney disease | 1.469% (1.341-1.609%) | 1.501% (1.37-1.646%) |

These 18+ estimates reproduce the approximate officially published PNS 2019 figures closely (DM ~7.7%, HTN ~23.9% officially; see `pns2019_source_and_survey_design.md` Section 'Quality check') - supporting confidence in the extraction/weighting pipeline. Full age-specific breakdown (18-19, 20-39, 40-59, 60-79, 80+), with N, SE, 95% CI and CV throughout: `pns2019_marginal_prevalence_by_age.csv`.

## 4. Eight-profile survey-weighted prevalence

Full table: `pns2019_joint_profile_prevalence_by_age.csv` (Figure: `figure_pns2019_joint_profile_prevalence.png`). Validated: profile prevalences sum to within 0.5 percentage points of 100% in every age group (explicit check in the script); marginal DM/HTN/CKD reconstructed by summing across profiles agrees with the direct marginal estimate to within 0.9 percentage points everywhere (small, expected differences from the stricter all-three-known denominator, not a bug) - see `pns2019_joint_profile_prevalence_by_age.csv` validation notes in the script log.

At the pooled 20+ level, CKD-containing profiles are rare but estimable: CKD-only 0.642%, DM+CKD 0.074%, HTN+CKD 0.575%, DM+HTN+CKD 0.279%.

## 5. Rare-profile reliability

Full diagnostics (unweighted N, distinct PSUs, CV, explicit flags): `pns2019_profile_reliability_diagnostics.csv` / `pns2019_profile_unweighted_counts.csv`.

**Pooled (18+/20+) estimates for all four CKD-containing profiles are reasonably reliable** (CV approximately 7-18% at the pooled level; see Section 4). **Age-specific estimates are a different story**: 3 of 12 age x rare-profile cells (DM+CKD, HTN+CKD, DM+HTN+CKD, across the four 20-year age bands) were flagged with 2 or more reliability concerns (small unweighted N, CV>30%, fewer than 10 contributing PSUs, or a CI touching zero): **DM+CKD in 20-39 (n=4, CV=65%); DM+HTN+CKD in 20-39 (n=2, CV=86%); DM+CKD in 80+ (n=2, CV=82.64%)**. These are concentrated in DM+CKD specifically, at the narrow-age-band level, driven by genuinely tiny unweighted cell counts (as few as 2 respondents) - this is a precision/sample-size limitation of a single cross-sectional survey for a rare tri-condition combination in a narrow age stratum, not a methodological error. These flags are diagnostic, not automatic exclusion criteria, per instruction.

## 6. Comparison with GBD/SINAN

Full table: `pns2019_prevalence_source_comparison.csv` (Figure: `figure_pns2019_CKD_prevalence_comparison.png`). For CKD specifically - the condition flagged in Gate 3.3A as having the largest measurement-definition mismatch - **PNS sits much closer to SINAN than to GBD** across every age band (e.g. 80+: SINAN 1.566%, PNS 3.114%, GBD 60.747% - full values in the CSV). Both SINAN and PNS reflect a physician/clinician-diagnosis concept; GBD reflects a modelled, all-severity-stage estimate. PNS is consistently somewhat higher than SINAN for all three conditions (plausibly reflecting under-ascertainment of comorbidities during acute arbovirus-notification care, or genuine differences between the chikungunya-case population and the general population) - this is a real, unresolved gap, not claimed to be closed. **No correction factor was calculated from any of these ratios, per instruction.**

## 7. Feasibility assessment (Phase 9 questions)

1. **Can PNS reliably estimate national DM/HTN/CKD prevalence?** Yes - all three individual-condition estimates at 18+/20+ have narrow CIs and closely reproduce published benchmarks.
2. **Can PNS reliably estimate the 8 joint profiles at the national adult level?** Yes, at the pooled 18+/20+ level, including the CKD-containing profiles (Section 4-5).
3. **Separately for 20-39/40-59/60-79/80+?** Partially - the dominant profiles (None, HTN, DM, DM+HTN) are reliable in every age band; the rarest CKD-containing combinations are not uniformly reliable at this stratification (Section 5).
4. **Which profiles/age groups are too sparse?** DM+CKD in 20-39 and 80+ specifically (n=2-4 unweighted); to a lesser extent DM+HTN+CKD in 20-39 and CKD-only in 80+. HTN+CKD was reliable in every age band tested.
5. **Would broader age groups suffice, or is partial pooling likely needed?** The pooled 18+/20+ estimates ARE already reasonably precise for every profile including the rare ones - broader age bands (or the pooled estimate itself) would likely suffice for a national burden estimate; age-specific granularity for DM+CKD/DM+HTN+CKD would likely need partial pooling or a coarser age grid (e.g. 3 bands instead of 4) if age-specific detail is required. This is a methodological decision for a later gate, not implemented here.
6. **Does PNS reduce reliance on SINAN-derived population clustering assumptions?** Yes, substantially - PNS directly observes the joint distribution of all three conditions in survey respondents, so the 8 profile prevalences no longer need to be reconstructed from marginals via an independence/clustering assumption (as the current GBD-anchored pipeline in `32_clustering_ratio_r_brazil.R` must do, since GBD only provides marginal, not joint, prevalence).
7. **Which measurement-definition differences remain between PNS and SINAN?** Physician-diagnosis self-report (PNS) vs clinician-recorded comorbidity checkbox at chikungunya notification (SINAN) are related but not identical ascertainment mechanisms; PNS is modestly but consistently higher for all three conditions (Section 6). Neither source captures CKD stage/severity.
8. **How could 2019 PNS prevalence be used with 2017-2025 SINAN RRs?** Only with an explicit, stated temporal-transportability assumption (population prevalence assumed stable from the 2019 survey year across the 2017-2025 chikungunya notification period) - plausible for slowly-changing chronic-disease prevalence over a ~6-8 year window, but not verified here and should be named explicitly if used.
9. **Is a PNS-based population burden model feasible for adults 20+?** At the level of marginal and pooled joint-profile prevalence: yes. At the level of fully age-stratified (4-band) joint-profile prevalence for the rarest profiles: not without additional handling (Section 5).
10. **Minimum additional work before integrating PNS prevalence with SINAN RRs?** (a) A explicit decision on age-stratification granularity for the burden model, informed by Section 5's reliability flags; (b) explicit documentation of the 2019-to-2017-2025 temporal transportability assumption; (c) a decision on whether PNS replaces or supplements the GBD-anchored prevalence in `29_`/`32_`/`34_` - NOT implemented in this gate.

## 8. Necessary vs optional next steps

**Necessary (before any PNS-based burden estimate is produced):**
1. Decide and document the age-stratification approach for CKD-containing profiles (pooled 20+, or a coarser 3-band grid) given the Section 5 reliability flags.
2. Document the 2019-to-2017-2025 temporal transportability assumption explicitly if PNS prevalence is paired with the SINAN RR period.

**Optional / for a later gate:**
3. A sensitivity comparison of burden estimates under PNS-anchored vs GBD-anchored prevalence (not run here - would require modifying `29_`/`32_`/`34_`, explicitly out of scope this gate).
4. Partial pooling / small-area-style smoothing for the narrowest age x rare-profile cells, if full 4-band granularity is ultimately required - a separate methodological decision, not defaulted to here per instruction.

## 9. Decision: **B. PARTIALLY FEASIBLE**

PNS 2019 is judged **PARTIALLY FEASIBLE** as a population multimorbidity prevalence source for the chikungunya burden extension. Marginal DM/HTN/CKD prevalence and pooled (18+/20+) 8-profile prevalence - including all four CKD-containing profiles - are reliably estimable with acceptable precision and closely reproduce official published benchmarks where checked. The qualifier is specific and narrow: 3 of 12 age-specific rare-profile cells (DM+CKD in 20-39 (n=4, CV=65%); DM+HTN+CKD in 20-39 (n=2, CV=86%); DM+CKD in 80+ (n=2, CV=82.64%)) do not meet the reliability thresholds used here, driven by genuinely small sample sizes for a rare tri-condition combination within narrow 20-year age bands - a precision limitation inherent to any single cross-sectional national survey, not a flaw in this analysis. PNS is a conceptually closer match to SINAN's clinically-recognised-disease concept than the GBD all-stage estimate currently used (Gate 3.3A), and uniquely allows direct, model-free estimation of the joint profile distribution - both meaningful advantages over the status quo pipeline, to be weighed against the age-specific precision limitation above in any future decision to integrate PNS into the burden model.

---
*Verified against: PNS_2019.txt, dicionario_PNS_microdados_2019.xls, input_PNS_2019.txt (IBGE, downloaded this session), 01_Data/chik_sinan_individual_2015_2025.rds, 01_Data/gbd_prevalence.csv, 03_Output/validation/gate33a/VALIDATION_3_3A.md. Fixed seed: 1.*
