# Sensitivity summary: harmonised primary models

**Source:** `02_Script/63_primary_models_sensitivity.R`. Each alternative model was refitted and standardised to its own target population, with 1,000 coefficient draws.

Full results:
- `supplementary/primary_vs_alternative_models.csv` (overall estimates)
- `supplementary/age_specific_RR_sensitivity.csv` (age-specific estimates)

No sensitivity estimate enters Figures 1–3 or Table 2.

## Hospitalisation (primary model: H2)

Overall standardised RR vs None, with 95% CI:

| Model | DM | HTN | CKD | DM+HTN | DM+CKD | HTN+CKD | DM+HTN+CKD |
|---|---|---|---|---|---|---|---|
| **H2 (primary)** | 1.78 (1.60–1.99) | 1.49 (1.37–1.63) | 2.86 (2.45–3.40) | 2.05 (1.77–2.44) | 3.22 (2.10–5.13) | 3.66 (2.82–4.86) | 2.86 (2.43–3.44) |
| H0 (historical Figure 1) | 1.74 (1.62–1.87) | 1.52 (1.46–1.59) | 2.90 (2.45–3.42) | 2.07 (1.96–2.18) | 3.64 (2.61–5.10) | 4.52 (3.89–5.29) | 3.62 (3.10–4.20) |
| H2, k = 5 | 1.77 | 1.43 | 2.87 | 1.99 | 3.20 | 3.67 | 2.85 |
| H2, k = 15 | 1.77 | 1.47 | 2.87 | 2.02 | 3.22 | 3.68 | 2.87 |
| H2, smoothing ×0.25 | 1.79 | 1.51 | 2.87 | 2.13 | 3.12 | 3.65 | 2.82 |
| H2, smoothing ×4 | 1.74 | 1.45 | 2.86 | 1.94 | 3.27 | 3.79 | 2.98 |
| H2 refitted on ages ≥15 (target ≥15) | 2.02 | 1.71 | 3.07 | 2.33 | 3.88 | 4.78 | 3.57 |
| H2 primary, standardised to the ≥15 target | 2.01 | 1.68 | 3.16 | 2.33 | 3.73 | 4.32 | 3.49 |
| H2 refitted, state-years with ≥70% recording (C1) | 1.76 | 1.52 | 3.06 | 1.92 | 3.16 | 3.89 | 2.91 |
| H2 primary, standardised to the C1 target | 1.78 | 1.49 | 2.87 | 2.06 | 3.24 | 3.70 | 2.91 |

### Interpretation

1. **Robust to the H2 smoothing specification.** Across k = 5/15 and smoothing parameters fixed at ×¼ or ×4, every overall RR stays within about 5% of the primary estimate. The H2 results are not an artefact of one basis or penalty choice.

2. **H2 differs from H0 mainly for CKD-containing combinations.**
   - HTN+CKD falls from 4.52 to 3.66 (−19%), DM+HTN+CKD from 3.62 to 2.86 (−21%), and DM+CKD from 3.64 to 3.22 (−12%).
   - The single conditions and DM+HTN change by less than 3%.
   - H0 forces one profile log-odds ratio at every age. H2 lets it vary with age, and under H2 the CKD-combination RRs rise with age.
   - For example, HTN+CKD is 3.8–3.9 at 15–59 and 5.4–5.9 at ≥60; DM+HTN+CKD rises from 2.4 to 5.5. Under H0 the corresponding RRs are flat at about 4.2–5.0 and 3.4–3.9.
   - Most cases with these combinations are aged ≥60, which is where the evidence is. But 82% of the standardisation population is under 60.
   - H0 therefore carries the older-age association over to the younger majority. H2 instead uses the lower relative excess it estimates at younger ages, so the whole-cohort standardised RR falls.
   - This is a change in model assumption, not an implementation error.
   - The flip side: the younger-age H2 estimates for these profiles rest on few events (for example, HTN+CKD 12 hospitalisations at 15–39, DM+CKD 1). The fall in the overall RR is therefore partly driven by sparsely observed profile × age regions. The H2 fit reproduces the validated script 52 H2 model exactly: point-estimate difference < 5 × 10⁻¹⁵.

3. **Childhood records have a modest influence on the fit; the standardisation target matters more.**
   - Restricting the target to ages ≥15 raises every RR, because comorbidity-associated excess risk is smaller relative to the high baseline in children.
   - Refitting without children moves the estimates further only for HTN+CKD (4.78 vs 4.32 on the same ≥15 target) and slightly for DM+CKD and DM+HTN+CKD.
   - Profile-specific childhood records of doubtful validity therefore have a small effect on the adult-weighted estimates.

4. **Outcome-recording completeness.** Restricting to state-years with ≥70% hospitalisation recording changes RRs by at most about 7% (CKD 3.06 vs 2.87 on the same target). Missing hospitalisation status is a limitation, but this check gives no sign of strong distortion.

5. **Wider CIs under H2 are expected.**
   - H2 adds about 30 effective degrees of freedom for profile-specific age shapes. Uncertainty in sparsely observed profile × age regions therefore propagates into the overall standardised RRs.
   - For example, the DM+HTN CI widens from 1.96–2.18 to 1.77–2.44. It stays similarly wide on the ≥15 target (2.04–2.72), so it is not driven by childhood records.

**Flag.** DM+CKD (27 hospitalisations among 262 cases) and HTN+CKD (131 / 828) rest on few events at most ages. Their profile-specific age shapes are heavily penalised towards the common age curve, so their overall RRs should be read with their wide CIs.

## Mortality (primary model: H0)

| Model | DM | HTN | CKD | DM+HTN | DM+CKD | HTN+CKD | DM+HTN+CKD | AIC |
|---|---|---|---|---|---|---|---|---|
| **H0 (primary)** | 3.84 (2.92–5.08) | 2.52 (2.13–2.97) | 7.99 (4.56–14.02) | 5.04 (4.20–6.11) | 9.89 (3.64–24.87) | 12.58 (8.24–19.49) | 16.00 (10.80–23.20) | 13,738.6 |
| H0, k = 20 | 3.83 | 2.52 | 7.98 | 5.03 | 9.80 | 12.58 | 16.00 | 13,735.4 |
| fs_original (historical age figure) | 4.08 (3.01–7.03) | 2.72 (2.15–4.21) | 7.02 (4.38–15.49) | 7.16 (4.86–14.43) | 8.23 (3.44–79.88) | 18.27 (11.39–52.62) | 12.93 (8.91–25.30) | 13,723.8 |
| fs_global (common age + deviations) | 3.88 (2.89–6.27) | 2.59 (2.06–3.81) | 6.90 (4.19–15.66) | 6.44 (4.57–11.71) | 7.16 (3.11–37.84) | 16.36 (10.21–47.60) | 12.59 (8.79–22.68) | 13,705.9 |

### Interpretation

1. **Robust: direction and broad gradient.**
   - Under every specification, all seven profiles have RR > 1 with CIs excluding 1.
   - CKD-containing combinations have the largest RRs: HTN+CKD and DM+HTN+CKD are always in the top three.
   - DM > HTN among single conditions.

2. **Sensitive to the flexible age assumption.**
   - The flexible models raise DM+HTN (5.0 → 6.4–7.2) and HTN+CKD (12.6 → 16.4–18.3), and lower DM+HTN+CKD (16.0 → 12.6–12.9). The ordering of HTN+CKD and DM+HTN+CKD therefore reverses.
   - Their CIs are 2–4 times wider, and the DM+CKD upper limit reaches 38–80.
   - The ranking of the top CKD combinations should not be presented as established.

3. **AIC.** Both factor-smooth models have lower AIC than H0, by 15 (`fs_original`) and 33 (`fs_global`). **H0 is not the minimum-AIC mortality model.** It was selected for parsimony and stability:
   - 1,050 deaths in total;
   - 4 deaths for DM+CKD;
   - profile-specific age variation that was not robust across specifications (script 46);
   - unstable flexible estimates in poorly supported age regions, shown by the very wide upper CI limits.

## Age-specific mortality (Figure 2B)

Replacing the factor-smooth estimates with H0 changes some displayed cells materially:

| Cell | Factor-smooth risk (%) | H0 risk (%) | Change |
|---|---|---|---|
| CKD, ≥75 (10 deaths) | 6.35 | 4.09 | −36% |
| HTN, 15–39 (10 deaths) | 0.128 | 0.077 | −40% |
| HTN+CKD, ≥75 (12 deaths) | 4.63 | 6.26 | +35% |
| DM+HTN, 40–59 | 0.252 | 0.196 | −22% |

H0 CIs are narrower in every displayed cell, by 1–81%, because H0 borrows the age shape across profiles. They reflect the H0 assumption that the profile log-odds ratio is constant across age.
