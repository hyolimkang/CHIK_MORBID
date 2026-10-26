# Final Figure 3: validation and interpretation

**Figure:** `Figure3_combined_absolute_risks_FINAL.pdf/.png (600 dpi)/.svg`
**Data:**
- `Figure3_combined_absolute_risks_data.csv`: every value in every panel, with a `plotted` / `marker` status.
- `Figure3_combined_support_table.csv`: both outcomes, 80 age × profile cells.

**Scripts:**
- `02_Script/52_hospital_age_validation.R`: hospitalisation audit and estimation.
- `02_Script/53_figure3_combined_final.R`: figure, supplementary figures and output tables.

**Detail:** `HOSPITAL_AGE_VALIDATION.md` (hospitalisation model selection) and `03_Output/validation/figure5_age_validation/FIGURE5_VALIDATION.md` (mortality).

Nothing earlier was overwritten, including the current `03_Output/figure_revision/Figure3_age_specific_absolute_risk_main.*`.

## Figure content

**Layout:** Candidate 1 is selected: rows by outcome, with age trends on the left and adults ≥75 on the right (`Figure3_candidate1_rows_by_outcome.png`). Candidate 2 (columns by outcome, `Figure3_candidate2_columns_by_outcome.png`) uses identical data and rules. Candidate 1 was preferred because each row tells one outcome's story, the panel letters read A→D in order, and the two age panels keep outcome-specific axes, so readers are not invited to compare hospitalisation and death on a shared scale.

| Panel | Content | Model / standardisation | Display rules |
|---|---|---|---|
| A | Hospitalisation risk by age group: None (0–14 to ≥75); HTN, DM+HTN, CKD (15–39 to ≥75) | H2; within-age-group target from the hospitalisation cohort | Only cells with ≥10 hospitalisations and ≥10 non-hospitalisations are drawn. Childhood profile contrasts are omitted because of implausible comorbidity coding in children (shown in the Supplement). Support strip gives hospitalised / cases for every cell. |
| B | Hospitalisation risk at ≥75, all 8 profiles | H2 | Grouped by condition count; dashed reference line = None. DM+CKD (9/45) drawn open with a dashed interval. |
| C | Death risk by adult age group: None, HTN, DM+HTN | Validated mortality age model (fs), unchanged | Identical to current Figure 3A. Cells with <10 deaths not drawn (DM+HTN at 15–39, 5 deaths). |
| D | Death risk at ≥75, all 8 profiles | Same | Identical to current Figure 3B. DM+CKD (4/56) drawn open with a dashed interval. |

**Why these profiles in A:**
- CKD is added to the mortality panel's profiles because it is adequately supported in every hospitalisation age group (15–33 events) and is the profile that departs most from its condition count (Figure 4).
- DM and DM+HTN+CKD are also adequately supported, but are left to the Supplement (`Supp_hospital_age_specific_RR`) to keep the panel readable.

**Why ≥75 in B:** 7 of 8 hospitalisation profiles have ≥10 events at ≥75, and the subgroup matches Panel D. The 60–74 group is equally well supported (also 7/8) and gives a similar ranking with smaller absolute risks. It was not chosen in place of ≥75, so the hospitalisation and mortality profile comparisons share a subgroup; the choice was not made to maximise contrasts.

**Colour and symbols:** each profile has the same colour and symbol in all four panels (Okabe–Ito palette). Risks are on log-scaled percentage axes with outcome-specific ranges. Lines join only consecutive plotted age groups, and no suppressed estimate is plotted, nor shown as zero.

**Consistency checks:**
- **Mortality values:** they reproduce the current Figure 3 exactly (19 values, maximum difference 0; `mortality_reproduces_current_figure3.csv`). No mortality estimate was changed.
- **Different model structures:** hospitalisation uses H2 and mortality the original fs model. This is deliberate and documented. For mortality, the fs and GS versions differ by ≤6–13% in every reported cell (Figure 5 validation), so changing the mortality model would not alter Panels C–D.
- **Outcome-specific standardisation:** the standardisation populations differ by outcome (hospitalisation N = 776,072; mortality N = 983,947), and this should be stated in the legend.

## Supplementary figures (this folder)

| Figure | Files |
|---|---|
| Hospitalisation age × profile support | `Supp_hospital_age_profile_support.*`, also saved as `hospital_age_profile_support_heatmap.*` |
| Hospitalisation age-specific RRs | `Supp_hospital_age_specific_RR.*` |
| Hospitalisation age-interaction robustness | `Supp_hospital_age_interaction_robustness.*` (7 specifications) and `Supp_hospital_RR_curve_validation.*` (original vs selected continuous curves, coloured by local event support) |
| Mortality age × profile support | `Supp_mortality_age_profile_support.*` |
| Mortality age-specific RRs | `Supp_mortality_age_specific_RR.*` |

## Answers to the ten questions

**1. How does absolute hospitalisation risk change across age groups?**
For cases without DM, HTN or CKD, standardised risk is:

| 0–14 | 15–39 | 40–59 | 60–74 | ≥75 |
|---:|---:|---:|---:|---:|
| 7.6% | 2.4% | 2.0% | 2.5% | 4.9% |

Within childhood the crude risk falls from 19.8% in infants to 4.6% at 10–14. Every comorbidity profile follows the same contour at a higher level. For example, HTN goes 4.4% → 2.9% → 3.8% → 9.4% across the adult groups, and CKD goes 7.8% → 4.9% → 8.2% → 22.5%.

**2. Is the pattern U-shaped, monotonic, or otherwise non-linear?**
U-shaped (more precisely J-shaped). Risk is highest in infants and young children, lowest at 40–59, and rises steeply from 60 onwards. Mortality is different: it rises almost monotonically through adulthood (None: 0.03% at 15–39 to 0.51% at ≥75), and childhood mortality is not interpretable by profile.

**3. Which profiles show adequately supported hospitalisation risks across age groups?**
None, DM, HTN, CKD, DM+HTN and DM+HTN+CKD have ≥10 hospitalisations and ≥10 non-hospitalisations in all five age groups. HTN+CKD qualifies at 15 and over. DM+CKD is sparse in every age group (1–9 events). Altogether 34 of 40 cells are adequate and none has zero events.

**4. Are hospitalisation relative associations modified by age?**
Yes, robustly. This differs from mortality.
- Every interaction specification improves AIC substantially (−76 to −259 vs no interaction).
- Penalisation-aware Wald tests reject a constant OR across adult ages (p ≤ 6 × 10⁻⁷ in every variant).
- The partially pooled age-group × profile variance is clearly non-zero (SD 0.29, 95% CI 0.20–0.42).

The relative associations are weakest at 40–74. They are about 1.3–1.5-fold larger at 15–39 and 1.4–1.9-fold larger at ≥75 (ratio of RRs vs 40–59; for example HTN 1.37 [1.23–1.52] and CKD 1.94 [1.28–2.94] at ≥75). They are approximately null in children (HTN RR 0.99, DM 1.19 at 0–14). The childhood attenuation should not be interpreted biologically because of comorbidity misclassification in children.

**5. Which hospitalisation results are robust to alternative model specifications?**
- All adult estimates and the U-shaped baseline. Across H1, H3, basis sizes k = 5–15 and 4-fold changes in smoothing, adequately supported cells shift by ≤9–19% relative to H2.
- The direction and approximate size of the age modification, which appear in every interaction model.

**Not robust:**
- Childhood profile estimates, which shift by up to about 35% between H2 and H3, as well as being of doubtful validity.
- DM+CKD, which is sparse.
- The no-interaction model, which is clearly miscalibrated at the age extremes.

On missing outcome data, adult estimates for None, HTN, DM+HTN and CKD are stable in high-completeness state-years (risks shift ≤14%). CKD-containing profiles at ≥75 are 13–35% lower in the ≥80%-completeness subset. That is model robustness plus partial missingness robustness, not full missingness robustness: no IPW or MNAR analysis has been done.

**6. How do the hospitalisation and mortality age patterns differ?**

| | Hospitalisation | Death |
|---|---|---|
| Baseline age shape | U-shaped, high in children | Rises steeply and almost monotonically in adults (about 18-fold from 15–39 to ≥75 for None) |
| Profile RRs by adult age | Vary with age: lowest in midlife, higher in young and older adults | No robust variation with age |
| Typical RR magnitude | ~1.4–5.9 | ~2–17 |
| Absolute risk at ≥75, most severe profiles | ~22–27% | ~5–8% |

In both outcomes the absolute excess associated with comorbidity is concentrated in older adults.

**7. Which profiles have the highest absolute severity risks in older adults (≥75)?**

| Rank | Hospitalisation | Death |
|---|---|---|
| 1 | DM+HTN+CKD 26.6% (21.8–32.6) | DM+HTN+CKD 8.4% (5.6–12.9) |
| 2 | HTN+CKD 26.4% (21.7–31.7) | CKD 6.4% (3.6–11.1) |
| 3 | CKD 22.5% (16.5–29.7) | HTN+CKD 4.6% (2.9–7.9) |

DM+CKD is sparse for both outcomes (hospitalisation 21.6%; death 6.5%). For comparison, the None reference is 4.9% for hospitalisation and 0.51% for death. CKD-containing profiles carry the highest absolute risk for both outcomes.

**8. What are the limitations of rare CKD profiles?**
- **Sparse events:**
  - DM+CKD has 27 hospitalisations and 4 deaths in total; it is sparse in every age group and its estimates are imprecise.
  - CKD alone, HTN+CKD and DM+HTN+CKD have adequate hospitalisation support in most age groups, but mortality support only at ≥75 (10–17 deaths).
- **Missing-outcome sensitivity:** CKD-containing estimates at ≥75 are the most sensitive to it (−13% to −35%).
- **Overlap:** their standardisation relies more on extrapolation across state-years where the profile is unobserved (overlap 0.29–0.89).
- **Coding:** CKD recording quality (Gate 3.3A) and implausible childhood triple-morbidity records affect exactly these profiles.

**9. Are the final plotted estimates consistent with the corrected outcome-specific cohorts?**
Yes.
- Hospitalisation: N = 776,072 and 25,363 events.
- Mortality: N = 983,947 and 1,050 deaths (post Gate 3.1).
- Scripts 52 and 53 both assert these counts and stop otherwise. The eligible population of 1,074,796 is used only as the denominator for missingness, never for mortality.
- Each outcome is standardised to its own cohort's within-age-group covariate distribution.

**10. What are the strongest defensible conclusions?**
1. Absolute risk of severe outcomes rises sharply in older adults for every multimorbidity profile, and CKD-containing profiles carry the highest absolute risks. At ≥75, around one in four is hospitalised and up to 8% die.
2. Hospitalisation risk is U-shaped by age, with the highest baseline risk in infants and young children irrespective of comorbidity.
3. The relative association between multimorbidity and hospitalisation varies with age: it is weakest in midlife and stronger in younger and older adults. For death, no robust age variation in relative association was found.
4. Childhood profile-specific estimates and all DM+CKD estimates should not be interpreted as findings.

These are associational, model-standardised risks, not causal effects.

## Results-ready text (draft; not inserted into the manuscript)

**Hospitalisation.**
> Among 776,072 confirmed cases with known hospitalisation status (25,363 hospitalisations), standardised hospitalisation risk followed a U-shaped age pattern. Among cases without diabetes, hypertension or chronic kidney disease, risk was 7.6% (95% CI 7.5–7.8) at 0–14 years, fell to 2.0% (2.0–2.1) at 40–59 years and rose to 4.9% (4.6–5.2) at ≥75 years. Each comorbidity profile followed the same age contour at a higher level. At ≥75 years, standardised risk was 9.4% (8.8–10.0) with hypertension alone, 12.5% (11.6–13.6) with diabetes and hypertension, 22.5% (16.5–29.7) with chronic kidney disease alone and 26.6% (21.8–32.6) with all three conditions. Relative associations varied with age: models allowing profile-specific age effects fitted substantially better than a common-age model (ΔAIC −241), and risk ratios were smallest at 40–74 years and larger at 15–39 and ≥75 years. For example, the risk ratio for hypertension was 1.41 (1.31–1.51) at 40–59 years and 1.92 (1.77–2.10) at ≥75 years. Estimates for adults were stable across alternative age specifications and in state-years with ≥70% or ≥80% recording of hospitalisation status. Profile-specific estimates in children were not interpreted because of implausible comorbidity recording in this age group.

**Comparison with mortality.**
> Age patterns differed between outcomes. Whereas hospitalisation risk was U-shaped, chikungunya-attributed mortality rose almost monotonically through adulthood, from 0.03% at 15–39 years to 0.51% at ≥75 years among cases without the three conditions, and was highest at ≥75 years for every profile (up to 8.4% [5.6–12.9] with diabetes, hypertension and chronic kidney disease). Unlike hospitalisation, there was no robust evidence that relative associations with death varied across adult age groups. For both outcomes, the absolute excess risk associated with multimorbidity was concentrated in older adults, and profiles including chronic kidney disease had the highest absolute risks, although estimates for rare chronic kidney disease profiles, particularly diabetes with chronic kidney disease, were imprecise.

## Unresolved issues
1. **Missing hospitalisation outcomes (27.8%).** Only state/year-completeness restriction has been examined. Recording completeness is higher for comorbid than non-comorbid cases, and that gap is widest at ≥75 (≈69% vs 78–84%). If recording is related to severity, this could inflate older-age hospitalisation risks for comorbid profiles. An IPW or MNAR analysis would be needed to address it.
2. **Comorbidity coding in children and for CKD.** Childhood profile contrasts are excluded from the main figure for this reason. A dedicated plausibility filter, e.g. excluding DM/HTN codes in infants, could be explored as a sensitivity analysis.
3. **Different age-model structures across outcomes (H2 vs fs).** This is justified above. If a single specification is preferred for the paper, refitting mortality with the GS model would change reported mortality cells by ≤13%.
4. **Figure title.** The title is drawn inside the figure because the brief requested it. Clinical Infectious Diseases normally places titles in the legend, so a no-title export can be produced by dropping `plot_annotation(title = ...)` in script 53.
