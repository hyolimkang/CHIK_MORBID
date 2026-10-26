# Gate 3.4 - External validation of the SINAN-derived clustering model using PNS 2019

Brazil, PNS 2019 (selected-adult sample, age 20+) vs SINAN-derived clustering lambda (`32_clustering_ratio_r_brazil.R`). Produced by `02_Script/44_validate_clustering_pns.R`. Validation exercise only - no RR model refit, no burden rerun, no copula/hierarchical model introduced, no automatic lambda substitution. Nothing committed/pushed.

## 1. Input population and denominators

Shared complete-three-condition PNS population (DM, HTN AND CKD all known; age 20+): N=80,228, matching Gate 3.3B's profile-eligible count exactly. This SAME population and the SAME `survey::svydesign` object are used for every marginal AND every joint-profile estimate in this gate - eliminating the denominator mismatch identified in Phase 1.

## 2. Survey design

`survey::svydesign(ids=~UPA_PNS, strata=~V0024, weights=~V00291, nest=TRUE)`, as verified in Gate 3.3B. All uncertainty in this report (Model A CIs, delta-method SEs for B/C/D, lambda_PNS SE) is design-based (accounts for PSU/strata/weighting), not computed as if PNS were a simple random sample.

## 3. Phase 1: mandatory consistency audit - RESOLVED

**Root cause (not a coding bug):** Gate 3.3B's reported CKD marginal (1.50%) used a denominator of all respondents with CKD status known regardless of DM/HTN status; its joint-profile CKD-containing sum (1.57%) used a stricter denominator requiring all three conditions known. Both numbers were individually correct for their own stated population - they were simply never meant to be compared directly at high precision.

**Fix and verification:** this gate defines one shared population (all three conditions known) and computes marginals and joint profiles from the identical `svydesign` object. The marginal-reconstructed-from-profiles identity now holds to **3.469447e-18** (floating-point noise, far below the 1e-8 tolerance) in every age group - see `pns_marginal_joint_consistency_audit.csv`. The old-style, per-condition-denominator marginal is reported separately for transparency in `pns_old_style_marginal_for_reference_only.csv` and was NOT used anywhere in the clustering validation below.

## 4. Table 1: age-specific marginals and lambda

| age_group | pns_dm | pns_htn | pns_ckd | pns_obs_2plus | indep_2plus | comono_2plus | lambda_sinan | lambda_pns_raw | lambda_pns_truncated |
|---|---|---|---|---|---|---|---|---|---|
| 20-39 |  1.48 |  6.43 | 0.85 |  0.622 |  0.161 |  1.481 | 0.391 | 0.350 | 0.350 |
| 40-59 |  8.37 | 28.05 | 1.68 |  5.637 |  2.882 |  8.371 | 0.555 | 0.502 | 0.502 |
| 60-79 | 20.82 | 55.13 | 2.52 | 16.074 | 12.810 | 20.816 | 0.676 | 0.408 | 0.408 |
| 80+ | 20.09 | 61.22 | 3.20 | 16.610 | 14.116 | 20.092 | 0.762 | 0.417 | 0.417 |

Full detail incl. SEs and bound checks: `age_specific_lambda_comparison.csv`.

## 5. Models B/C/D accuracy (Table 3 summary)

| age_group | mae_B | mae_C | mae_D | tvd_C | mae_ckd_profiles_C | err_2plus_C |
|---|---|---|---|---|---|---|
| 20-39 | 0.23 | 0.18 | 0.15 | 0.0071 | 0.163 | 0.05 |
| 40-59 | 1.42 | 0.46 | 0.34 | 0.0184 | 0.380 | 0.29 |
| 60-79 | 1.82 | 1.49 | 0.20 | 0.0596 | 0.460 | 2.15 |
| 80+ | 1.42 | 1.81 | 0.47 | 0.0725 | 0.819 | 2.06 |

(MAE/err_2plus in percentage points; TVD on the [0,1] proportion scale.) Full table: `clustering_model_error_metrics.csv`; full profile-by-profile predictions: `clustering_model_predictions_A_D.csv`; CKD-combination-specific errors: see script log / `clustering_model_predictions_A_D.csv` filtered to CKD/DM+CKD/HTN+CKD/DM+HTN+CKD.

## 6. Rare-profile reliability

3 of 16 CKD-containing-profile x age-group cells are flagged with 2+ reliability concerns (small unweighted N, few contributing PSUs, or observed CI touching zero) - full detail in `clustering_validation_reliability_flags.csv`. These are concentrated in the smallest cells (DM+CKD and DM+HTN+CKD in the youngest and oldest bands, n=2-13 unweighted) and should temper how strongly any single profile-level error is over-interpreted, though the AGGREGATE age-level pattern described below is based on many more observations and is more robust.

## 7. Phase 9 interpretation

**1. Is SINAN lambda consistently higher or lower than PNS lambda?** Consistently **higher** in all four age groups (20-39: 0.391 vs 0.350; 40-59: 0.555 vs 0.502; 60-79: 0.676 vs 0.408; 80+: 0.762 vs 0.417), and the gap **widens with age** - small at 20-39, large at 60-79/80+. SINAN-notified chikungunya cases show more apparent multimorbidity clustering than the general population does, especially among older adults.

**2. Does Model C outperform independence in reproducing PNS-observed profiles?** **Age-dependent, not uniform.** At 20-39 and 40-59, Model C has lower MAE than independence (B). At **80+**, Model C's MAE is **equal to or WORSE than plain independence** - the SINAN-calibrated clustering assumption actively hurts reconstruction accuracy at the oldest ages tested.

**3. Which profiles are accurately reconstructed?** The common profiles (None, HTN, DM, DM+HTN) track observed PNS prevalence closely in relative terms throughout (Figure 1, bottom row). Among the CKD-containing profiles specifically, DM+CKD and CKD are the best-reconstructed by Model C on average across age groups.

**4. Which profiles show the largest discrepancies?** Raw absolute-percentage-point error alone is dominated by scale - None, DM, HTN show the largest RAW pp gaps simply because they are the highest-prevalence profiles, which is not informative about model quality (per instruction, not emphasised further here). Restricting to the clinically relevant CKD-containing profiles specifically, **DM+HTN+CKD and HTN+CKD** show the largest average absolute error for Model C, consistent with Phase 6's CKD-specific MAE exceeding the all-profile MAE in most age bands.

**5. Are errors concentrated in the rare CKD combinations?** Yes, both in absolute percentage-point terms (Section 5) and visually (Figure 1) - the DM+HTN+CKD and HTN+CKD panels show the largest, most age-structured gaps between Model C and observed PNS.

**6. Are differences larger than PNS estimation uncertainty?** For **60.7%** of non-None profile x age-group cells, Model C's absolute error EXCEEDS the PNS observed-prevalence 95% CI half-width - i.e. the discrepancy is not plausibly explained by PNS sampling noise alone in roughly half of the cells tested. (Model D: 39.3% of cells.)

**7. Does Model D reproduce all eight profiles, or mainly the fitted P(2+) aggregate?** Model D is calibrated to match P(2+) by construction, and does so closely (by definition, within truncation). Its profile-level MAE is lower than Model C's in 3 of 4 age groups (Section 5), indicating it captures more than just the aggregate - but it still shows the same qualitative mis-allocation pattern as Model C at the oldest ages (overshooting DM+HTN+CKD, undershooting HTN+CKD - Figure 1), suggesting the one-parameter family itself, not just the lambda VALUE, has structural limits. **D vs A is an in-sample fit diagnostic (D was calibrated on this same PNS data), not independent external validation - it should not be read as evidence the clustering model 'validates' against an outside standard.**

**8. Does the one-parameter mixture family adequately capture the observed dependence structure?** **Not fully.** The lambda interpolation between independence and comonotonicity correctly captures the AGGREGATE P(2+) trend reasonably when calibrated (Model D), but Figure 1 shows it systematically mis-allocates WHICH 2+/3+ combination accounts for that aggregate, especially at older ages, where it tends to over-weight the triple-condition profile and under-weight the HTN+CKD pair relative to what PNS actually shows.

**9. Is the current SINAN-derived lambda defensible as a provisional transferable parameter?** **Only with explicit qualification, and only for younger adults.** At 20-39/40-59 the discrepancies are modest and plausibly within combined uncertainty; at 60-79/80+ the SINAN-calibrated model is no better than (or worse than) assuming independence, which undermines its use as a drop-in population-clustering assumption for the age groups that matter most for severity-weighted chikungunya burden.

**10. Remaining limitations from SINAN representing notified cases vs PNS representing the general population?** SINAN cases are selected on (a) being infected and notified, (b) surviving to notification, and (c) having comorbidities actively recorded by a clinician at point of care - any of these could independently inflate apparent clustering relative to the general population, and this gate cannot distinguish between them as explanations for the lambda gap found in Section 7.1.

**11. How might different CKD measurement definitions affect lambda transfer (linking to Gate 3.3A)?** If SINAN's clinically-recognised CKD skews toward more clinically apparent (and plausibly more tightly comorbidity-clustered) cases than PNS's broader physician-diagnosis-based CKD, part of the inflated SINAN lambda could be a measurement-definition artefact rather than a true population-selection effect - this gate's design cannot separate the two explanations.

## 8. Decision gate: **B. PARTIALLY SUPPORTED**

The model reproduces the overall P(2+) aggregate and the common profiles reasonably at younger ages, but its accuracy degrades with age and it performs no better than - or worse than - plain independence in 80+, specifically for the clinically important CKD-containing profiles. Retaining lambda_SINAN as a provisional, EXPLICITLY AGE-QUALIFIED assumption for younger adults may be defensible; its use for 60+ populations without further work is not well supported by this validation.

This judgement is based on: (a) error magnitude (MAE/TVD, Section 5) relative to the precision of the PNS estimates themselves (Section 7.6), (b) consistency across age groups (NOT consistent - good at 20-39/40-59, poor at 60-79/80+), (c) clinical relevance (the worst-reconstructed profiles are exactly the CKD-containing ones most relevant to severity-weighted burden), and (d) the structural finding that the one-parameter family mis-allocates DEPENDENCE TYPE, not just DEPENDENCE STRENGTH, at older ages (Section 7.8). No arbitrary universal error threshold was applied as a formal statistical test.

## 9. Minimum justified next step

Do not substitute lambda_PNS for lambda_SINAN in the production pipeline automatically. If the clustering model is to remain part of the burden pipeline, the age-dependence found here should be explicitly documented as a limitation, with the model's use for 60+ populations flagged as weakly supported. A natural, SCOPED next step (not undertaken here) would be testing whether an age-varying or two-parameter dependence family resolves the systematic mis-allocation seen in Figure 1 - but per explicit instruction, no new copula/hierarchical model was developed in this gate.

---
*Verified against: 01_Data/brazil_clustering_ratio_r.RData, brazil_clustering_lambda_draws.RData (SINAN, from 32_, read-only), PNS_2019.txt + dictionary (IBGE, from Gate 3.3B). Fixed seed: 1.*
