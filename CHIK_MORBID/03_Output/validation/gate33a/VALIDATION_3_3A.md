# Gate 3.3A - CKD measurement consistency and data-source harmonisation audit

Brazil, confirmed chikungunya cases, 2017-2025. Produced by `02_Script/42_ckd_measurement_consistency_audit.R`. Pure audit: no model refit, no CKD exposure change, no prevalence substitution, no population-burden rerun. Nothing committed/pushed; no primary cache, manuscript table/figure, or DOCX touched.

## 1. Main findings (summary)

- SINAN field 41 (`RENAL`) is explicitly labelled in the official dictionary as "Doenca renal cronica" (chronic kidney disease) - a pre-existing-disease checkbox, unrelated to the separate "acute renal failure" syndrome category that exists elsewhere in the SINAN schema (field 2, id_agravo). **No stage, eGFR, albuminuria, or dialysis information exists anywhere in SINAN for this field.**
- CKD recording completeness among confirmed chikungunya cases is **99.9822%** (eligible population N=1,074,994) - essentially complete, and NOT a material source of selection bias for the clinical RR model (contrast with hospitalisation status, ~28% missing, Gate 3.2A).
- The GBD CKD prevalence extraction in `29_background_prevalence_core3_brazil.R` is **technically correct for the current source file** (20/20 intended age bands matched, zero duplicates, no implausible values) but relies on an **undocumented, currently-latent assumption** that the file contains only one year - true today (2023 only) but not asserted in code.
- The GBD CKD prevalence is a **modelled, all-severity-stage** estimate (reaching ~69% at age 90+ in Brazil), while the SINAN-recorded CKD among chikungunya cases stays under 2% even at the oldest ages - a large, visually obvious gap (Figure: ckd_prevalence_age_comparison.png) consistent with, though not proof of, the two sources measuring different underlying CKD populations.
- The population-burden pipeline (`29_`->`32_`->`34_`) combines the GBD (broad) CKD prevalence with the SINAN (narrow, clinically-recognised) CKD RR with **no transportability adjustment or documented assumption** anywhere in the code.

## 2. Technical data-validation results

| Check | Result |
|---|---|
| SINAN RENAL -> renal_disease -> ckd coding chain (live verification, 9 synthetic codes) | PASS |
| CKD recording completeness within the broader "confirmed, valid age/sex" population (N=1,074,994) | 99.9822% recorded (yes/no); DM=99.9843%, HTN=99.9851% for comparison |
| Core-3 eligible cohort (N=1,074,796) is 100% CKD-recorded by construction | CONFIRMED (PASS) (expected - CKD completeness is part of the eligibility definition itself, not an independent finding) |
| GBD Brazil/CKD/Prevalence/Rate/Both slice: distinct years in file | 2023 (entire file, all causes/locations) |
| GBD age-band match (against the keep_age 20-band grid used by 29_) | 20/20 matched, 0 missing, 0 duplicated |
| GBD implausible values (val<=0 or >100,000 per 100k) | 0 |
| GBD missing val/upper/lower | 0 (fully populated in the matched slice) |

Full detail: `sinan_ckd_coding_audit.csv`, `sinan_ckd_recording_by_age_year.csv`, `gbd_ckd_source_metadata.csv`, `gbd_ckd_age_prevalence_audit.csv`.

## 3. Measurement-definition discrepancies

| dimension | SINAN_RENAL_field41 | GBD_chronic_kidney_disease |
|---|---|---|
| Target population | Confirmed chikungunya cases notified to SINAN, Brazil 2017-2025 (a selected, symptomatic, notified clinical population) | Brazilian general population, all ages, 2023 estimate (modelled) |
| CKD definition | Clinician-reported pre-existing chronic kidney disease (single yes/no checkbox at case notification) | Modelled prevalence of CKD per KDIGO-consistent criteria (reduced eGFR and/or markers of kidney damage), as defined by IHME's standard GBD cause hierarchy - covers the full severity spectrum from early/asymptomatic to end-stage |
| Data collection / estimation method | Clinician checkbox on a mandatory disease-notification form, completed at point of chikungunya care | Statistical modelling (DisMod-MR or successor) synthesising claims data, population lab/survey data (eGFR-based), and other inputs across countries |
| Diagnostic ascertainment | Not specified/standardised - relies on the treating clinician's prior knowledge of the patient's history at the time of chikungunya notification; no lab confirmation required or recorded | Primarily lab-based (eGFR/albuminuria) in its input data where available, blended with other data types via modelling |
| Severity/stage coverage | UNDIFFERENTIATED - no stage, eGFR, or albuminuria information; a 'yes' could represent any severity from mild/managed CKD to dialysis-dependent | ALL STAGES (1-5), including a substantial early/undiagnosed-stage component not clinically apparent to patients or clinicians |
| Age coverage | 0-100 years (chikungunya cases of any age; core-3 eligible cohort spans the full range) | All ages, including children (though CKD prevalence is negligible at young ages) |
| Year | 2017-2025 (chikungunya notification period) | 2023 (the only year present in the local gbd_prevalence.csv extract) |
| Major limitations | Comorbidity under-ascertainment plausible if clinician is unaware of a patient's CKD history; no severity gradient; small absolute N (CKD-recorded-yes is a rare exposure, see Phase 2) | A broad, modelled, ALL-SEVERITY prevalence that is NOT restricted to clinically recognised/diagnosed cases - substantially over-inclusive relative to a 'diagnosed, clinically apparent CKD' concept |
| Suitability for applying the SINAN-derived severity RR | This IS the population the RR was estimated in - directly suitable by construction | NOT DIRECTLY SUITABLE without an explicit transportability assumption - the RR was estimated among clinically-recognised CKD cases, but would be applied (in the population-burden pipeline) to a population-prevalence estimate that includes a large mild/undiagnosed-CKD component the RR was never estimated on |

Full three-way comparison (SINAN / GBD / PNS 2019, 9 dimensions): `ckd_definition_comparison.csv`. **PNS 2019 figures are not independently verified in this session** (no local microdata; general survey-design knowledge only) - marked explicitly as unresolved pending literature confirmation.

## 4. Epidemiological implications

The age-specific descriptive comparison (Phase 5; NOT a validation - different denominators, see Section 3) shows SINAN-recorded CKD prevalence among confirmed chikungunya cases rising only gently with age (roughly 0.28% at ages 0-9 to 1.40% at 90+), while the GBD modelled general-population CKD prevalence rises steeply across the same age range (0.11% to 69.2%). This pattern is consistent with SINAN capturing a narrower, more clinically-apparent CKD population than the GBD all-stage estimate, but the data cannot distinguish how much of the gap reflects (a) genuinely lower CKD prevalence among people well enough to be notified with chikungunya and survive to interview, (b) under-ascertainment by the notifying clinician, or (c) GBD including a large mild/undiagnosed-CKD component that would never appear on any clinical notification form regardless of true disease state. **No prevalence correction factor was calculated from this ratio, per instruction.**

## 5. Downstream dependency map

| step | script | potential_mismatch_consequence |
|---|---|---|
| 1 | 12_core3_condition_specific_rr.R | NONE at this step - the RR is internally consistent (estimated and reported for the SAME SINAN-defined CKD population). The transportability question only arises once this RR is later applied to a DIFFERENT (GBD-defined) population. |
| 2 | 29_background_prevalence_core3_brazil.R | This is the FIRST point where the two CKD definitions enter the SAME pipeline. No harmonisation or flag exists in the script distinguishing 'clinically recognised CKD' (SINAN) from 'any-stage modelled CKD' (GBD) at this join point. |
| 3 | 30_marginal_prevalence_brazil.R | Low - purely descriptive output, not an input to any further RR-weighted calculation. |
| 4 | 32_clustering_ratio_r_brazil.R | The calibrated CKD-containing profile prevalences (CKD, DM+CKD, HTN+CKD, DM+HTN+CKD) inherit the GBD (broad, all-stage) CKD marginal as their anchor - so even the 'observed clustering pattern from SINAN' is being scaled onto a GBD-sized CKD population, not a SINAN-sized one. |
| 5 | 34_profile_burden_allocation_brazil.R | HIGH - the RR estimated in a clinically-recognised-CKD population is applied, via the GBD-anchored prevalence weight, to the CKD-containing profiles' ENTIRE population share - implicitly assuming the RR is constant across the full GBD CKD severity spectrum (stage 1 through dialysis-dependent). This assumption is NOT demonstrated or tested anywhere in the current pipeline. |

Full detail with per-step consequence assessment: `ckd_downstream_dependency_map.csv`. The highest-consequence step is `34_profile_burden_allocation_brazil.R`, where the SINAN-derived RR is multiplied against the GBD-anchored profile prevalence - this is where the transportability assumption becomes load-bearing for any reported burden number.

## 6. Necessary vs optional corrections

**Necessary (before the population-burden extension is presented as a primary, non-exploratory result):**
1. Document the CKD-definition mismatch and transportability assumption explicitly in the burden-modelling methods text.
2. Add an explicit, asserted `year` filter (or an assertion that exactly one year exists) to the GBD extraction in `29_background_prevalence_core3_brazil.R`, so a future GBD data refresh with multiple years fails loudly instead of silently duplicating rows.

**Optional / for a later gate:**
3. A bounded CKD-prevalence sensitivity analysis for the burden estimate (e.g. re-running `34_` under alternative assumptions about which share of GBD-modelled CKD plausibly corresponds to SINAN-recognisable CKD) - valuable, but only AFTER (1) is written down, not as a substitute for documenting the assumption.
4. Pursue PNS 2019 microdata or published tables as a third reference point, if a closer independent benchmark for "diagnosed CKD prevalence" is wanted.
5. Investigate whether CKD recording completeness (already very high, Section 2) shows any meaningful outcome-stratified pattern - included here as descriptive diagnostics (`sinan_ckd_recording_by_outcome.csv`) but not pursued further, as the overall completeness is already too high for this to be a material concern.

## 7. Recommendation for the existing RR manuscript

**No change needed.** CKD is correctly coded, the recode/eligibility logic is NA-safe and consistent between the cleaning script and the GAM, and CKD recording completeness (99.9822%) is not a material missingness concern. The condition-specific and joint-profile RR estimates for CKD are internally valid for the population in which they were estimated (SINAN-recognised, pre-existing CKD among confirmed chikungunya cases).

## 8. Recommendation for the population-burden extension

**Treat current CKD-containing-profile burden estimates as exploratory/prototype, not primary, until the transportability assumption is explicitly documented.** This is consistent with the existing prototype labelling already used throughout the `34_`-`37_` outputs. No code change is required to make this gate pass on coding grounds; a documentation/limitations addition is required before burden numbers are presented as validated.

## 9. Gate 3.3A status

**Data coding correctness: PASS.** **CKD definition harmonisation readiness for population-burden modelling: NOT YET READY (not yet ready without explicit documentation).**

**Overall: CONDITIONAL PASS.** Code executes correctly throughout (SINAN recoding, eligibility construction, and the GBD extraction all passed their technical checks) - this is a coding-correctness PASS. The CONDITIONAL qualifier applies specifically and only to the unresolved CKD-definition mismatch relevant to population-burden modelling (Sections 4-6), which is a scientific-comparability issue, not a code defect - per instruction, these two are reported separately rather than conflated.

---
*Verified against: 01_Data/chik_sinan_individual_2015_2025.rds, 01_Data/sinan_chik_docs/dic_dados_chikungunya.pdf, 01_Data/gbd_prevalence.csv, 02_Script/02_clean_chik_sinan_brazil.R, 12_, 29_, 30_, 32_, 34_.*

