# Figure 3 recommendation: age-specific absolute mortality risk

**Main figure:** `Figure3_age_specific_absolute_risk_main.pdf/.png/.svg`
**Supplementary figures:**
- `Supp_Figure_age_specific_mortality_RR.*`
- `Supp_Figure_age_support_heatmap.*`
- `Supp_Figure_age_RR_curve_validation.*`

**Supporting files:**
- Script: `02_Script/49_figure3_age_absolute_risk.R`
- Data: `figure3_plotted_values.csv`, `hospitalisation_age_profile_support_counts.csv`
- Upstream validation: `03_Output/validation/figure5_age_validation/FIGURE5_VALIDATION.md`

## What the main figure shows
All estimates are within-age-group g-computation risks from the validated age-interaction GAM (2,000 coefficient draws). They come straight from the Figure 5 validation outputs; nothing was refitted.

### Panel A: standardised risk across adult age groups
Shows the three profiles with ≥10 deaths in at least three adult age groups: None, HTN and DM+HTN. Only cells with ≥10 deaths are drawn. A compact deaths/cases strip below the panel shows the counts for every cell, greyed where the cell is not plotted. That explains why DM+HTN starts at 40–59: there are only 5 deaths at 15–39.

Children (0–14) are omitted from Panel A. Only one death occurred among comorbid children in that group, so no profile contrast is estimable there.

| Profile | 15–39 | 40–59 | 60–74 | ≥75 |
|---|---|---|---|---|
| None | 0.028% | 0.039% | 0.099% | 0.51% |
| HTN | 0.13% | 0.10% | 0.20% | 1.42% |
| DM+HTN | – | 0.25% | 0.52% | 2.35% |

HTN at 15–39 is exactly 10 deaths, so it is borderline.

### Panel B: adults aged ≥75, all 8 profiles
Profiles are ordered and coloured by condition count, matching Figure 2. The dashed line marks the None reference risk (0.51%). Risks range from 1.4% (HTN) to 8.4% (DM+HTN+CKD). DM+CKD (4 deaths / 56 cases) is drawn open, and deaths/cases are given for every row.

Panel B was chosen as the second view because ≥75 is the only age group in which 7 of the 8 profiles have ≥10 deaths. It is therefore the only place where a profile-by-profile comparison of absolute risk is empirically supported.

## 1. Why absolute risk belongs in the main manuscript
- **It carries the robust age signal.** The validation found no robust evidence that RRs change with adult age (penalisation-aware Wald p = 0.47 over ages 20–90; the result depends on specification). Absolute risk, by contrast, rises steeply and consistently. For DM+HTN the excess risk over None goes from 0.21 to 0.42 to 1.84 percentage points across 40–59, 60–74 and ≥75. The clinically important message is "the same comorbidity matters far more in absolute terms in older adults", and only an absolute-risk display shows it.
- **It is what clinicians act on.** Triage and admission decisions depend on absolute risk. An RR of 5 means very different things at a baseline of 0.03% and at 0.5%.
- **It is honest about support.** Plotting only cells with ≥10 deaths, with counts alongside, keeps every plotted point tied to observed events.

## 2. Why age-specific RR curves stay in the Supplement
- In the original continuous curves, 19–77 of the 95 ages per profile lie where that profile has no deaths within ±5 years, or fall outside its observed death-age range. Several striking features fall in those regions: CKD and DM+CKD RR peaks of 16–20 in childhood, and a CKD trough at 40–60.
- Those features are at least partly an artefact of the specification. With no shared age smooth, a sparse profile's curve is shrunk toward flat log-odds, so its RR mirrors the inverse of the None age curve. A shared-smooth alternative fits better (AIC −18) and removes most of the childhood peaks.
- Formal interaction evidence is weak and specification-dependent.

**Supplementary figures recommended:**
- `Supp_Figure_age_specific_mortality_RR`: age-group RR forest plot with sparse cells open and empty cells labelled "0 deaths".
- `Supp_Figure_age_support_heatmap`: deaths and cases per cell.
- `Supp_Figure_age_RR_curve_validation`: the original curves coloured by local support, with the shared-smooth alternative overlaid. This is where the original continuous curves should live, if they are retained at all.

## 3. Should hospitalisation be added? Not now
Support is not the obstacle. Counts in `hospitalisation_age_profile_support_counts.csv` show 34 of 40 age × profile cells with ≥10 hospitalisations; all 6 sparse cells are DM+CKD at every age plus HTN+CKD at 0–14.

The obstacles are validation and cohort definition:
1. **No validated hospitalisation age-specific model exists.** The validation (gate 5) audited only the mortality age-interaction model: interaction tests, smoothing sensitivity, extrapolation and robustness. A hospitalisation panel would need the same audit first; it would take about 10 minutes of compute by rerunning `46_` on the hospitalisation cohort.
2. **Hospitalisation status is missing for about 28% of eligible records** (Gate 3.2A), and recording varies by state and year. The restricted-sample sensitivity analysis (Gate 3.2B) supported the *overall* RRs, but age-specific completeness patterns were not examined. If recording completeness differs by age, age-specific absolute hospitalisation risks would be biased in a way the overall RRs are not.
3. **The two outcomes use different cohorts** (N = 776,072 vs 983,947). Placing them side by side as parallel panels invites a direct comparison of absolute risks that the cohorts do not strictly support.

**Recommendation:** keep Figure 3 mortality-only. If a hospitalisation panel is wanted, first run the gate-5 audit on the hospitalisation cohort plus an age-by-completeness check of `hospitalised`. Then add it as a Supplementary figure, or as a parallel Panel C only if the audit is clean.

## Suggested caption (draft)
> **Figure 3.** Standardised risk of chikungunya-attributed death by age group and comorbidity profile. (A) Adults aged 15 years and over, for profiles with at least 10 deaths in at least three adult age groups. Only age-group cells with ≥10 deaths are plotted; deaths/cases are shown below. (B) Adults aged ≥75 years, all profiles, grouped by number of conditions; the dashed line is the risk for cases with none of DM, HTN or CKD, and the open marker indicates fewer than 10 deaths. Risks are standardised by g-computation, within each age group, to that group's observed age, sex, notification year and state distribution, using a penalised logistic GAM with age-varying profile effects. Bars are 95% CIs from 2,000 coefficient draws. Mortality cohort N = 983,947 (1,050 deaths).
