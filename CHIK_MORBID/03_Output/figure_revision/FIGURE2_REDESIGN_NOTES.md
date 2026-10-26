# Figure 2 redesign notes: overall profile-specific risk ratios

**Files:** `Figure2_profile_RR_refined.pdf/.png/.svg` (600 dpi PNG) and `figure2_plotted_values.csv`, both in this folder.
**Script:** `02_Script/48_figure2_profile_rr_refined.R`, with the shared style in `02_Script/figure_revision_theme.R`.
**Replaces:** `03_Output/figures/fig_core_profile_rr_hosp.*` and `fig_core_profile_rr_death.*`. These are left untouched.

## Data: no new estimation
The figure plots the validated, cached estimates from `12_core3_condition_specific_rr.R` section 8 (`01_Data/core_profile_rr_hosp.RData`, `core_profile_rr_death.RData`). They are GAM g-computation estimates standardised over each outcome cohort's age × sex × year × state distribution, with 95% CIs from 2,000 parametric coefficient draws.

The script stops if the caches do not match the current cohorts:

| Cohort | N | Events |
|---|---:|---:|
| Hospitalisation | 776,072 | 25,363 hospitalisations |
| Mortality (post Gate 3.1) | 983,947 | 1,050 deaths |

The numbers are therefore identical to those in `table_joint_profile_standardised_risks.*`.

## Design decisions
| Choice | Reason |
|---|---|
| A (hospitalisation) stacked above B (death), each with its own log x-axis | The ranges differ by an order of magnitude (hospitalisation RR up to ~5, death up to ~25). Placed side by side at journal width, the numeric columns collided. |
| Rows grouped by condition count (0 / 1 / 2 / 3), with quiet left-hand group labels and small gaps between groups | Makes the "more conditions, higher risk" gradient and the within-count spread visible without extra ink. This grouping also prepares the reader for Figure 4. |
| One sequential blue hue stepped by count; reference (None) in neutral grey | Count is ordinal, so a light-to-dark ramp encodes it without a legend. A single hue also survives greyscale printing and colour-vision deficiency. |
| Aligned columns "RR (95% CI)" and "events / cases" | Exact values for readers and reviewers; counts make the support behind each row visible (see below). |
| Open marker + grey text for rows with <10 events | Only death DM+CKD (4 deaths / 313 cases) qualifies. Same convention as the age-validation figures. |
| No title, subtitle or legend inside the figure | Methods (standardisation, bootstrap) belong in the caption. Panel letters plus outcome names are enough. |
| Faint vertical gridlines at the tick values; solid grey reference line at RR = 1 | Readers can place each point on the log scale without heavy chrome. |

## Should counts be annotated? Yes, kept restrained
For mortality the events column matters. Three CKD-containing profiles rest on 4, 12 and 22 deaths (DM+CKD, CKD and HTN+CKD). Without counts, their wide intervals could be read as imprecision in a large group rather than as sparse support. The counts are set in grey, one size smaller than the RR text, so they do not compete with the estimates.

## Suggested caption (draft, not inserted into the manuscript)
> **Figure 2.** Standardised risk ratios for (A) hospitalisation and (B) chikungunya-attributed death, by joint profile of diabetes (DM), hypertension (HTN) and chronic kidney disease (CKD), relative to cases with none of the three. Estimates were derived from penalised logistic GAMs adjusted for age (smooth), sex, notification year and state of residence (random effect), and standardised by g-computation to the observed covariate distribution of each outcome cohort. Bars are 95% CIs from 2,000 parametric coefficient draws. Profiles are grouped by number of conditions. The open marker indicates fewer than 10 observed events. Brazil, SINAN, confirmed cases 2017–2025; hospitalisation N = 776,072 (25,363 events); death N = 983,947 (1,050 deaths).

## Points to note in the text
- **Hospitalisation, three conditions:** DM+HTN+CKD (RR 3.62) is lower than HTN+CKD (4.52), with overlapping CIs. The figure shows this honestly; it argues against a simple dose–response in condition count and is examined formally in Figure 4.
- **Death, DM+CKD:** RR 9.9 (3.8–25.6) on 4 deaths. Describe it as imprecise rather than as a finding.
