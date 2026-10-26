# Recommended manuscript figure set (Clinical Infectious Diseases submission)

This summarises the figure revision in `03_Output/figure_revision/` and the age validation in `03_Output/validation/figure5_age_validation/`. No original figure files were overwritten, and the manuscript text has not been edited.

The manuscript `.docx` in the repository root is currently a short stub with no embedded figures or figure references. Figure files below are therefore identified by their pipeline file names, not by current manuscript figure numbers.

## Recommended set

| Slot | Recommended file | Status | Replaces / relation to existing files |
|---|---|---|---|
| **Figure 1** | `03_Output/figures/figure_cohort_flow.*` (or `figure1_study_architecture.*`) | Retain; not revised here | Check that it shows mortality N = **983,947** (see unresolved issue 1) |
| **Figure 2** | `figure_revision/Figure2_profile_RR_refined.*` | **New, recommended** | Replaces `figures/fig_core_profile_rr_hosp.*` and `fig_core_profile_rr_death.*`. Same estimates, redesigned. |
| **Figure 3** | `figure_revision/Figure3_age_specific_absolute_risk_main.*` | **New, recommended** | Replaces `figures/fig3_profile_age_specific_relative_risk.*` (the continuous RR curves) and `fig_core_profile_rr_death_by_age.jpg` as the main age figure |
| **Figure 4** | `figure_revision/Figure4_count_vs_profile_main.*` | **New, recommended** (Panel A essential; Panel B may move to the Supplement if space is tight) | New analysis; no predecessor |

### Supplementary age figures
| Supplementary figure | File |
|---|---|
| Age-group-specific mortality RR (sparse cells open, empty cells labelled) | `figure_revision/Supp_Figure_age_specific_mortality_RR.*` |
| Age × profile empirical support (deaths and cases per cell) | `figure_revision/Supp_Figure_age_support_heatmap.*` |
| Original continuous RR curves coloured by local support, with the shared-smooth alternative | `figure_revision/Supp_Figure_age_RR_curve_validation.*` |
| Robustness of age-group RRs across 10 specifications | `validation/figure5_age_validation/Supplement_age_profile_robustness.*` |
| Count vs profile performance table (if Figure 4B moves) | `model_comparison_metrics.csv` → Supplementary Table |

## Existing figures: keep, demote or retire
| Existing file (`03_Output/figures/`) | Recommendation |
|---|---|
| `fig_core_profile_rr_hosp.*`, `fig_core_profile_rr_death.*` | **Replace** with new Figure 2 (identical numbers). |
| `fig3_profile_age_specific_relative_risk.*` (continuous profile RR curves, "original Figure 5") | **Remove from the main text.** If retained, only in the Supplement as `Supp_Figure_age_RR_curve_validation`, which shows local support. Do not present DM+CKD as a curve. |
| `fig_core_profile_rr_death_by_age.jpg`, `fig_core_profile_rr_hosp_by_age.jpg` (5-band forests, bands 0–19 … 80+) | **Retire.** They are superseded by the validated age-group outputs (bands 0–14 … ≥75), which carry support flags. Keeping both band schemes would confuse readers. |
| `fig3_condition_rr_forest.*` (condition-specific mutually adjusted RR) | **Retain as an optional Supplement figure** (or a main figure if the paper also reports per-condition effects). It answers a different question from the profile figures and was not revised here. |
| `fig3_age_specific_absolute_risk.*`, `fig3_age_specific_relative_risk.*`, `fig3_age_interaction_comparison.*` (per-condition age curves) | **Supplement at most.** They overlap with new Figure 3 and were not audited for local support. The per-condition age-interaction curves carry the same extrapolation risk identified for the profile curves. |
| Evidence-map / prioritisation figures, burden prototypes | Outside the scope of this revision. Burden figures depend on stale caches (unresolved issue 6). |

## Why this set
1. **Figure 2** answers the primary question (which profiles are associated with severe outcomes) using validated pooled estimates. Each row carries its event count, so sparse profiles are visible.
2. **Figure 3** carries the robust age message. Absolute risk rises steeply with age for every supported profile, and the profile–death RRs show no robust change across adult ages. Age-specific RR curves mostly display unsupported extrapolation, so they belong in the Supplement.
3. **Figure 4** turns the Figure 2 pattern into a tested claim. Comorbidity count captures most of the gradient, but CKD-containing profiles carry about 2–3 times the risk their count implies. Cross-validation shows this matters for individual risk assessment, not for population-level discrimination. The CKD-excluded sensitivity row makes the CKD-driven nature explicit.
4. **Consistent visual language:** Figures 2–4 share typography, the count-ordered profile grouping, the sequential count colour ramp, and the open-marker convention for fewer than 10 events. Readers learn the encoding once.

## Unresolved issues (to settle before submission)
1. **Mortality cohort consistency.** The current pipeline's mortality cohort is **983,947** records with 1,050 deaths (after the Gate 3.1 outcome correction). The figure **1,074,796** is the eligible core-3 population *before* the outcome filter; it appeared in the Figure 5 brief and in some earlier outputs. Every mortality figure and table must use 983,947. In particular, check the cohort-flow figure, Table 1, abstract and methods. All new figures here assert 983,947 / 1,050 (hospitalisation: 776,072 / 25,363) and stop otherwise.
2. **Hospitalisation age-specific analysis has not been validated.** Support is adequate (34/40 age × profile cells have ≥10 events). However, no age-interaction audit equivalent to the mortality one has been run, and recording completeness by age has not been checked (≈28% of eligible records lack hospitalisation status; Gate 3.2A). Keep Figure 3 mortality-only until both are done. Running `46_` on the hospitalisation cohort plus an age × completeness table would take about an hour of work.
3. **Rare CKD profiles.** DM+CKD has 4 deaths, so all its mortality estimates should be described as imprecise. The Figure 4 conclusion rests on CKD and HTN+CKD (12 and 22 deaths; 124 and 131 hospitalisations). CKD coding quality (Gate 3.3A) therefore matters directly for the main messages of Figures 2 and 4.
4. **Implausible paediatric multimorbidity.** 97 children aged 0–14 are coded DM+HTN+CKD in the mortality cohort. None died, so no reported estimate is affected, but it signals form-completion artefacts. Mention it in limitations, or consider a sensitivity analysis excluding implausible paediatric triple-morbidity records.
5. **Model engine.** Figure 2 uses script 12's `gam(REML)` estimates. Figure 4 uses `bam(fREML)` refits, which agree to within 0.01%, and Figure 3 uses the validated age-interaction `bam` model. State in the methods that these are penalised logistic GAMs fitted with mgcv; the engine difference is immaterial.
6. **Stale downstream caches.** The population-burden scripts (`34_`–`37_`) were not rerun after the Gate 3.1 correction (per the Gate 3.1 implementation report). Any burden figure would use outdated mortality RRs.
7. **Figure production checks.** All figures are exported as PDF (cairo, fonts embedded), SVG and 600 dpi PNG at 170–183 mm width. Final sizing to CID's column widths, and the exact caption text, still need confirming against the journal's current author instructions. Draft captions are in `FIGURE2_REDESIGN_NOTES.md` and `FIGURE3_RECOMMENDATION.md`.

## Reproduce
From the repository root, with R 4.3.3:
```
Rscript 02_Script/46_figure5_age_profile_validation.R   # age validation (~8 min)
Rscript 02_Script/47_figure5_age_profile_figures.R
Rscript 02_Script/48_figure2_profile_rr_refined.R       # Figure 2 (cached estimates)
Rscript 02_Script/49_figure3_age_absolute_risk.R        # Figure 3 + supplementary age figures
Rscript 02_Script/50_figure4_count_vs_profile.R         # Figure 4 analysis (~20 min)
Rscript 02_Script/51_figure4_count_vs_profile_plot.R    # Figure 4 drawing
```
Shared style: `02_Script/figure_revision_theme.R`. All random seeds are fixed.
