# Model selection evidence report

This report sets out the evidence for the Methods paragraph on outcome-specific age specifications. It covers:

- **Supplementary Table Sx:** model fit and adult age-interaction tests.
- **Supplementary Table Sx+1:** observed deaths by profile and age group.

All values come from the validated outputs of scripts 52 (hospitalisation) and 46 (mortality), and are traceable in `Full_Model_Comparison_Audit.csv` and `TABLE_SOURCE_RECONCILIATION.md`. Nothing was refitted.

## How the evidence was evaluated

- **ΔAIC = AIC(model) − AIC(H0), within each outcome.** AIC compares overall penalised fit.
- **Adult age-interaction test.** This is a joint, penalisation-aware Wald test using the Bayesian covariance `Vp`. It asks whether the log-odds ratios of the seven profiles relative to None are equal at ages 20, 30, …, 90 and at reference age 60.
  - The definition, age grid and covariance are identical in both outcomes.
  - It tests variation in **conditional log-odds ratios**. It is not a test of whether age-specific risk ratios are equal: on the logit scale, RRs vary with baseline risk even without an interaction.
- **AIC and the adult test answer different questions.** A lower AIC can come from better fit at any age, including childhood. It does not by itself show that profile associations vary across adult ages.
- **Approximate LRTs.** The `anova.gam` likelihood-ratio p-values are kept in the audit file but not shown in the table. They are edf-based approximations and are anticonservative for penalised fits (`FIGURE5_VALIDATION.md`, §2).

---

## Claim 1: "We selected outcome-specific age specifications based on model fit and the stability of age-dependent associations."

**Supported by:**
- **Table Sx:** AIC, ΔAIC and adult tests for 7 hospitalisation and 8 mortality specifications.
- **Audit file:** basis-dimension (k = 5/15) and smoothing-parameter (×¼, ×4) variants for both outcomes.
- **Stability of adequately supported age-specific RRs** across specifications:
  - hospitalisation: `hospital_age_model_robustness_summary.csv`;
  - mortality: `robustness_summary.csv`, `FIGURE5_VALIDATION.md` §5.

Three kinds of stability should be kept apart:

1. **Overall evidence for an adult interaction.** Consistent for hospitalisation; specification-dependent for mortality.
2. **Age-specific estimates in adequately supported strata.**
   - Hospitalisation: H2 vs H1 / k = 15 / smoothing ×¼ shift RRs by at most 8–9%.
   - Mortality: alternative interaction models change reported-cell RRs by a median of about 1–4% (`robustness_summary.csv`).
3. **Predictions for rare profiles or unsupported age ranges.** Unstable in both outcomes. This is a property of the data, not evidence for or against an interaction.

## Claim 2: "For hospitalisation, we included profile-by-age interactions because they substantially improved model fit (ΔAIC = −241)."

| Quantity | Value |
|---|---|
| H0 AIC | 204,543.2 |
| H2 AIC | 204,302.1 |
| **ΔAIC (H2 − H0)** | **−241.1** |
| H2 adult test | χ² = 135.6, df = 49, p = 4.7 × 10⁻¹⁰ |

**The improvement does not come only from a more flexible common age curve.** H0 with k = 20 improves AIC by only 32.9. All interaction specifications improve it far more: H1 −211.3, H3 −177.9, linear −76.3, H2 −241.1.

**Adult interaction evidence is consistent across specifications.** The adult test gives p < 0.001 in all 10 H1 and H2 fits (H1, H2, and 8 basis and smoothing variants):
- p ranges from 3.8 × 10⁻⁵² to 6.4 × 10⁻⁷;
- smoothing ×4 is the weakest (p = 6.4 × 10⁻⁷).

The coarser structures (linear slopes, fixed age-group interactions) also reject no interaction (coefficient tests p < 10⁻¹⁴), although they test a different hypothesis.

**Why H2 rather than the other flexible models** (`HOSPITAL_AGE_VALIDATION.md` §3):
- H2 has the best AIC among the k = 10 specifications.
- It shrinks profile deviations towards the common age curve rather than towards a flat line, which is more sensible for sparse profiles.
- Adequately supported age-specific RRs are stable against H2: H1 ≤9%, k = 15 ≤8%, smoothing ×¼ ≤8%, ×4 ≤17%, k = 5 ≤19%.

**H2 is not the minimum-AIC model.**
- H2 (k = 15) has AIC 204,284.3 (ΔAIC −258.9), and H1 (k = 15) has 204,290.0.
- The validation report kept k = 10 because k = 15 adds 7.5 effective degrees of freedom and changes adequate-cell RRs by only ≤8%.
- The report states the reason as "for consistency with the mortality and original models". It does not use the words "parsimony and stability", so manuscript text should not attribute the choice to a formal stability criterion beyond this.

**Caveat.** Statistical evidence of an interaction does not mean every profile's age curve is well estimated:
- DM+CKD is sparse at every age.
- The HTN+CKD and DM+HTN+CKD curves below about age 40 rest on 1–9 events.

## Claim 3: "For chikungunya-attributed death, there was limited evidence of age-dependent differences in profile associations among adults."

| Model (mortality) | AIC | ΔAIC | Adult test p |
|---|---:|---:|---:|
| H0 (selected) | 13,738.6 | 0 | — |
| H1 (profile-specific smooths) | 13,723.8 | −14.8 | 0.47 (χ² 49.0, df 49) |
| H2 (common + deviations) | 13,705.9 | −32.7 | 0.67 (χ² 44.2, df 49) |
| H1, k = 15 | 13,722.5 | −16.1 | 0.51 |
| H1, smoothing ×¼ (audit) | 13,715.7 | −22.9 | 0.20 |
| H1, smoothing ×4 (audit) | 13,738.6 | +0.01 | 0.62 |
| H1, ages ≥15 only (audit; different cohort) | — | — | 0.76 |
| **H1, k = 5** | 13,758.5 | **+19.9** | **0.001** |

- **Some flexible models fit better overall.** H1 and H2 have lower AIC than H0 (by 15 and 33), and this is shown in the table, not hidden.
- **The improvement is not explained by adult age variation.** In every adequately specified variant, the adult test finds little evidence (p = 0.20–0.76). Over the full age range (1–95), the same test is borderline (p = 0.09–0.12; 0.048 with smoothing ×¼). This suggests that part of the AIC gain comes from outside adulthood.
- **Important exception (k = 5).** The H1 model with a k = 5 basis gives a significant adult test (p = 0.001) but fits **worse** than H0 (ΔAIC +19.9). The validation report attributes this to an inadequate age basis transferring age misfit into the profile contrasts. It is reported in the table rather than omitted.
- **Other variation.** Making the profile smooths 4× smoother removes the AIC advantage (ΔAIC +0.01). The coarser interactions (age-group random effects −3.3; linear slopes −1.0, slope test p = 0.07) improve AIC by no more than a more flexible common age smooth (H0, k = 20: −3.2).

**Wording.** This is *limited* evidence for age-varying adult associations. It does **not** show that no interaction exists.

## Claim 4: "Several profiles had few observed deaths."

Supplementary Table Sx+1 (source: `age_profile_support.csv`; verified against the cohort rebuilt in script 62):

| Profile | Deaths / cases | Deaths by age group, 0–14 / 15–39 / 40–59 / 60–74 / ≥75 |
|---|---|---|
| DM + CKD | **4** / 313 | 0 / 0 / 0 / 0 / 4 |
| CKD only | **12** / 1,640 | 1 / 1 / 0 / 0 / 10 |
| HTN + CKD | 22 / 924 | 0 / 1 / 3 / 6 / 12 |
| DM + HTN + CKD | 28 / 1,535 | 0 / 0 / 3 / 8 / 17 |
| DM only | 53 / 15,302 | 0 / 5 / 7 / 9 / 32 |

- Of the 35 non-reference profile × age-group strata, **11 have ≥10 deaths, 12 have 1–9 deaths and 12 have none**.
- Four of the seven profiles have ≥10 deaths in only one age group, and DM + CKD has none.
- The 10-death level describes empirical support; it is not a formal validity threshold.

## Claim 5: "We therefore used a simpler model without age interactions for mortality. Both models allowed baseline risks to vary non-linearly with age."

H0 was selected for three reasons:

1. **Parsimony.** It has about 23 fewer effective degrees of freedom than H1 or H2.
2. **Limited adult interaction evidence.** See Claim 3. The one significant result depends on an inadequate basis.
3. **Event support.** See Claim 4. Separate age curves for profiles with 4–28 deaths cannot be estimated reliably. The historical H1 (`fs_original`) age-specific estimates were unstable in poorly supported regions, for example an upper 95% limit of 80 for the DM + CKD overall RR in the primary-model sensitivity analysis.

**H0 is not the minimum-AIC mortality model.**

H0 retains a non-linear common age smooth (k = 10, EDF 8.4), so baseline mortality risk varies with age. Only the profile-by-age interaction is excluded. The same holds for H2 in hospitalisation, which includes a common age smooth.

---

## Suggested explanatory note linking the table to the Methods

> Alternative age specifications were compared within each outcome (Supplementary Table Sx). For hospitalisation, models allowing profile associations to vary with age fitted substantially better than a model with a common age effect only (ΔAIC −241 for the selected model), and a joint test showed that profile-associated odds ratios varied across ages 20–90 years (p < 0.001), consistently across basis and smoothing choices. For chikungunya-attributed death, some interaction models had lower AIC, but joint tests provided little evidence that profile associations varied across adult ages (p = 0.47 and 0.67 for the two main specifications; significant only under a restrictive basis that fitted worse than the no-interaction model), and four of seven profiles had 10 or more deaths in only one age group (Supplementary Table Sx+1). The no-interaction model, which retains a non-linear effect of age, was therefore used for mortality.
