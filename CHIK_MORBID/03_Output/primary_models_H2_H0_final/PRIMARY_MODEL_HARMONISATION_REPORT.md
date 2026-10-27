# Primary model harmonisation report

## 1. Final models

- **Hospitalisation: H2.** Model ID `hospitalisation_H2`: a common smooth of age plus penalised profile-specific age deviations, `s(age, profile, bs = "fs", k = 10, m = 1)`. Cohort: 776,072 cases, 25,363 hospitalisations.
- **Chikungunya-attributed death: H0.** Model ID `mortality_H0`: a common smooth of age with no profile-by-age interaction. Cohort: 983,947 cases, 1,050 deaths.

Both models include the eight-level profile, sex, year (as a factor) and a state random effect, fitted with `bam(binomial, fREML, discrete)`.

**Every main estimate comes from these two fitted objects and their 4,000 coefficient draws.** This includes the Figure 1 RRs, the Figure 2 age-specific risks, the Figure 3 RDs and Table 2.

## 2. Scientific rationale

### Hospitalisation H2

- H2 improves on H0 by ΔAIC = −241 (204,302 vs 204,543).
- There is evidence of profile-specific age variation across adult ages (script 52).
- Overall estimates are stable to the basis dimension (k = 5/15) and to the smoothing penalty (×¼, ×4): all RRs move by ≤5%.
- H2 is **not** the minimum-AIC hospitalisation model: H1_k15 and H2_k15 have slightly lower AIC.
- The k = 10 specification was fixed for parsimony, empirical support and stability.

### Mortality H0

- H0 was chosen for parsimony and stability.
- There are 1,050 deaths in total, with 4 for DM+CKD.
- Profile-specific age variation was not robust across specifications (script 46).
- Flexible models give unstable estimates in poorly supported age regions; for example, the upper CI limit for the DM+CKD RR reaches 38–80.
- **H0 is not the minimum-AIC mortality model.** The factor-smooth models have AIC lower by 15 (`fs_original`) and 33 (`fs_global`).

## 3. Figure changes

| Figure | Panel | Previously | Now | Material change? |
|---|---|---|---|---|
| **1. Overall RR** | A: hospitalisation | H0 (`gam REML`, 2,000 draws) | **H2** | **Yes.** CKD-containing combinations are lower (HTN+CKD −19%, DM+HTN+CKD −21%, DM+CKD −12%). CIs are wider (DM+HTN about 3×, HTN 2×). |
| | B: death | H0 (`gam REML`, 2,000 draws) | **H0** (`bam`, 4,000 draws) | No. Points agree within 0.01%; CI limits are within about 4%. |
| **2. Age-specific risks** | A: hospitalisation | H2 (script 52, 2,000 draws) | **H2** (4,000 draws) | No. Points are identical; CI limits are within ±5.5%. |
| | B: death | Factor-smooth GAM (`fs_original`) | **H0** | **Yes** (below). |
| **3. Within-count RD** | Hospitalisation | H0 (script 56) | **H2** | **Yes** (section 5). |
| | Death | H0 (script 56) | **H0** | No. Points are identical; CIs are within about 4%. |

**Figure 2B (death), cell-level changes:**
- The 16 displayed cells are unchanged under the same display rule (≥10 events and ≥10 non-events).
- Points change from −40% to +35%: CKD aged ≥75, 6.35% → 4.09%; HTN aged 15–39, 0.128% → 0.077%; HTN+CKD aged ≥75, 4.63% → 6.26%.
- Every CI is narrower.

**Design and axes:**
- The figure designs are unchanged.
- All axis windows are kept, except the Figure 3 hospitalisation axis, which widens from −4 to −6. The new HTN+CKD vs DM+CKD lower limit is −4.7.
- No interval is clipped (the scripts assert this).

## 4. Table 2 changes

Hospitalisation, H0 → H2 (risk %, RR):

| Profile | Old risk | New risk | Old RR | New RR |
|---|---|---|---|---|
| None | 2.97 (2.94–3.01) | 2.96 (2.93–3.01) | 1.00 | 1.00 |
| DM | 5.16 (4.75–5.58) | 5.27 (4.75–5.89) | 1.74 (1.59–1.87) | 1.78 (1.60–1.99) |
| HTN | 4.53 (4.36–4.70) | 4.40 (4.06–4.83) | 1.52 (1.46–1.58) | 1.49 (1.37–1.63) |
| CKD | 8.61 (7.29–10.15) | 8.48 (7.27–10.09) | 2.90 (2.46–3.42) | 2.86 (2.45–3.40) |
| DM + HTN | 6.15 (5.85–6.46) | 6.07 (5.24–7.24) | 2.07 (1.96–2.18) | 2.05 (1.77–2.44) |
| DM + CKD | 10.82 (7.69–15.01) | 9.53 (6.21–15.24) | 3.64 (2.59–5.04) | 3.22 (2.10–5.13) |
| HTN + CKD | 13.45 (11.42–15.67) | 10.85 (8.37–14.38) | 4.52 (3.82–5.26) | 3.66 (2.82–4.86) |
| DM + HTN + CKD | 10.77 (9.24–12.40) | 8.47 (7.22–10.23) | 3.62 (3.10–4.18) | 2.86 (2.43–3.44) |

Mortality, H0 → H0. Point estimates are unchanged at two decimals; CIs change in the second decimal only:

| Profile | Risk % (old → new) | RR (old → new) |
|---|---|---|
| None | 0.07 (0.06–0.08) → unchanged | 1.00 |
| DM | 0.26 (0.20–0.35) → unchanged | 3.84 (2.85–5.09) → 3.84 (2.92–5.08) |
| HTN | 0.17 (0.15–0.20) → unchanged | 2.52 (2.12–2.97) → 2.52 (2.13–2.97) |
| CKD | 0.54 (0.32–0.98) → 0.54 (0.32–0.96) | 7.99 (4.54–14.42) → 7.99 (4.56–14.02) |
| DM + HTN | 0.34 (0.30–0.41) → unchanged | 5.04 (4.20–6.08) → 5.04 (4.20–6.11) |
| DM + CKD | 0.67 (0.27–1.76) → 0.67 (0.25–1.71) | 9.89 (3.80–25.60) → 9.89 (3.64–24.87) |
| HTN + CKD | 0.85 (0.56–1.32) → 0.85 (0.58–1.33) | 12.58 (8.02–19.46) → 12.58 (8.24–19.49) |
| DM + HTN + CKD | 1.09 (0.75–1.58) → 1.09 (0.76–1.58) | 15.99 (10.74–23.26) → 16.00 (10.80–23.20) |

**Table note changes:**
- It now describes the two outcome-specific age specifications and 4,000 draws.
- The DM + CKD 4-death caveat is retained.

## 5. Key numerical changes affecting interpretation

1. **Ranking of CKD-containing hospitalisation profiles.**
   - Under H0: HTN+CKD (4.52) > DM+CKD (3.64) ≈ DM+HTN+CKD (3.62) > CKD (2.90).
   - Under H2: HTN+CKD (3.66) > DM+CKD (3.22) > CKD (2.86) ≈ DM+HTN+CKD (2.86).
   - **The three-condition profile no longer exceeds CKD alone for hospitalisation.**
   - Any text describing a gradient by number of conditions for hospitalisation needs revising.
   - The mortality gradient (DM+HTN+CKD 16.0 highest) is unchanged.

2. **Why H2 differs.**
   - Under H2, the RRs for CKD combinations increase with age (for example, HTN+CKD 3.8 at 15–59 vs 5.4–5.9 at ≥60), whereas H0 imposes a constant odds ratio.
   - 82% of the standardisation population is under 60, so the overall RR falls.
   - The lower younger-age estimates rest on few events (`sensitivity_summary.md`).

3. **Relative vs absolute.**
   - The absolute hospitalisation risks of CKD combinations fall: HTN+CKD 13.4% → 10.9%; DM+HTN+CKD 10.8% → 8.5%.
   - Mortality absolute risks are unchanged.

4. **Within-count RDs (hospitalisation):**

   | Contrast | H0 | H2 |
   |---|---|---|
   | DM vs HTN | 0.6 (0.2 to 1.1) | 0.9 (0.2 to 1.6) |
   | CKD vs HTN | 4.1 (2.8 to 5.7) | 4.1 (2.8 to 5.8) |
   | CKD vs DM | 3.4 (2.1 to 5.0) | 3.2 (1.8 to 4.9) |
   | DM+CKD vs DM+HTN | 4.7 (1.5 to 8.8) | **3.5 (−0.1 to 9.2): CI now includes 0** |
   | HTN+CKD vs DM+HTN | 7.3 (5.3 to 9.5) | 4.8 (2.1 to 8.3) |
   | HTN+CKD vs DM+CKD | 2.6 (−2.0 to 6.6) | 1.3 (−4.7 to 6.3) |

   The qualitative message ("CKD-containing profiles carry more risk than equal-count profiles without CKD") still holds for single conditions and for HTN+CKD vs DM+HTN. The DM+CKD vs DM+HTN difference is no longer clearly different from zero.

5. **Age-specific mortality (Figure 2B).** Cell values change by up to ±40% and CIs narrow. Under H0, differences between profiles in the age pattern of risk ratios are not estimated. Any text describing profile-specific age trajectories for death must be removed or reworded.

6. **CI width.**
   - Hospitalisation overall CIs widen under H2 (DM+HTN +207%, HTN +115%, DM +35%).
   - Mortality age-specific CIs narrow under H0.

7. **Historical tests.** The within-count Wald tests and p-values (script 56) were not reused. They tested H0 coefficients, so they do not apply to the H2 standardised RDs.

## 6. Remaining limitations

- **Sparse profiles.** DM+CKD has 4 deaths and 27 hospitalisations; HTN+CKD has 22 deaths. Estimates for these are imprecise, and the mortality ranking of the top CKD combinations is sensitive to the age specification.
- **Missing hospitalisation status.** 27.8% of eligible cases lack hospitalisation status. Restricting to state-years with ≥70% recording changes RRs by ≤7%. Bias from outcomes that are not missing at random cannot be excluded.
- **Childhood comorbidity records.** These are of doubtful validity. Childhood profile cells shown in Figure 2A (display policy unchanged, `exclude_child_profiles = FALSE`) should be interpreted cautiously. Refitting without children changes adult-weighted RRs modestly (largest: HTN+CKD 4.78 vs 4.32).
- **Model-specification sensitivity.**
  - Hospitalisation H0 vs H2: up to 21% difference for CKD combinations.
  - Mortality H0 vs factor-smooth: up to 45% difference for DM+HTN and HTN+CKD.
  - CIs reflect within-model uncertainty only.
- **Unsupported age × profile regions.** Overall H2 standardisation averages over all ages, including regions with very few profile-specific events. Figure 2 shows only cells with ≥10 events and ≥10 non-events, but the overall Figure 1 and Table 2 estimates use the whole target.
- **Numerical and reproducibility issues.** None. The H2 fit gives one expected mgcv warning (repeated 1-d smooths of the same variable). The CI limit for DM+CKD mortality has Monte Carlo error of about 5%.
- **Saved models.** The `.rds` model files contain the individual-level model frame. They are git-ignored and must not be shared.

## 7. Final verification status

| Item | Status |
|---|---|
| All cohort checks passed (N, events, `EVOLUCAO`, profiles, Table 1) | **Pass** (19 of 19) |
| Overall = weighted age-specific identity (point and all 4,000 draws) | **Pass** (≤ 6 × 10⁻¹⁶) |
| Figure 1 and Table 2 agree | **Pass** (identical values) |
| Figure 3 contrasts agree with Table 2 risks | **Pass** (≤ 10⁻¹⁵) |
| All main estimates from the correct primary models (H2 / H0) | **Pass** (model IDs, formulas and draws verified) |
| Independent verification (script 65) | **Pass** (57 of 57) |
| Figures and Word documents rendered | **Pass** (PNG inspected; PDF, SVG and DOCX written) |

**Manuscript actions:**
- Replace Figures 1–3 and Table 2 with the files in `figures/`, `tables/` and `legends/`.
- Update the Methods to describe the outcome-specific age specifications.
- Revise any Results text on (a) the hospitalisation gradient by number of conditions, (b) the DM+CKD vs DM+HTN hospitalisation difference, and (c) profile-specific age patterns of mortality.
- Assign supplementary numbers (S[X] placeholders in the legends).
