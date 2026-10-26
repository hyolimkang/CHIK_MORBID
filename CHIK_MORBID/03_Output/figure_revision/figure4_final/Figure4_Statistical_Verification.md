# Figure 4: statistical verification

**Figure:** `Figure4_within_count_RD_FINAL.pdf/.png (600 dpi)/.svg`
**Plotting script:** `02_Script/58_figure4_final.R` (also writes the legend Word file and the CSVs)
**Estimates from:** `02_Script/56_within_count_contrasts.R` → `03_Output/figure_revision/within_count/within_count_direct_contrasts.csv`

No model was refitted and no estimate was changed for this figure.

## Checks (all passed; the script stops if any fails)

| # | Check | Result |
|---|---|---|
| 1 | **Models match the manuscript's primary overall profile analysis.** Per outcome: `y ~ core_profile + sex + year + s(uf_residence, bs = "re") + s(age_years, k = 10, bs = "ts")`, binomial logit, mgcv `bam` (fREML). This is the Figure 2 specification; script 12 used `gam(REML)`. Profile risks agree with the cached Figure 2 estimates to ≤0.01% (`within_count/consistency_with_figure2.csv`). | Pass |
| 2 | **Same standardisation target within each outcome.** Every profile's risk is standardised by g-computation to one target: the observed joint distribution of exact age × sex × notification year × state of residence in the whole outcome cohort (hospitalisation N = 776,072; death N = 983,947). | Pass |
| 3 | **RD definition and paired CIs.** RD = risk(first-listed) − risk(second-listed), in percentage points. The script re-derives every plotted RD from the CSV's profile-specific standardised risks (maximum discrepancy < 10⁻¹⁰ pp). The 95% CIs are percentiles of RD computed *within* each of 4,000 multivariate-normal coefficient draws (`vcov(unconditional = TRUE)`), so the covariance between the two predictions is retained. | Pass |
| 4 | **Explicit sparse-data rule.** As coded in script 56: a contrast is sparse if either profile has <10 outcome events. This is a data-support rule, not a significance rule. It flags exactly two contrasts, both mortality contrasts involving DM+CKD (4 deaths / 313 cases). No hospitalisation contrast is flagged (minimum 27 events, DM+CKD). The script asserts that the flag equals `min(events) < 10`. | Pass |
| 5 | **Displayed numbers match the source.** All 12 RD (95% CI) strings were rebuilt from the CSV and checked against the values transcribed from the previous Candidate B figure (`Figure4_value_check_vs_previous_figure.csv`: 12/12 match). Points and bars are drawn from the same CSV columns as the text. | Pass |
| 6 | **All six pre-specified contrasts per outcome.** The script asserts 12 rows, 6 per outcome, in the fixed order. | Pass |
| 7 | **Interval type.** The displayed intervals are **pointwise 95%**. Bonferroni-simultaneous (99.17%) intervals are reported separately (`rd_lower_simult_pp`, `rd_upper_simult_pp` in `Figure4_within_count_RD_data.csv`) and did not replace the displayed ones. | Stated in legend |
| 8 | **No clipping.** All intervals lie inside the axis ranges (hospitalisation −4 to 10 pp; death −1 to 1.5 pp; asserted in code). No interval is truncated. | Pass |

No discrepancies were found between the figure and the underlying CSV.

## Multiplicity-adjusted sensitivity (from the same draws; not displayed)

| Outcome | Contrast | Pointwise 95% CI (displayed) | Simultaneous 99.17% CI | Holm-adjusted p (Wald, log-odds) |
|---|---|---|---|---|
| Hospitalisation | DM vs HTN | 0.2 to 1.1 | 0.1 to 1.2 | 0.005 |
| | CKD vs HTN | 2.8 to 5.7 | 2.3 to 6.2 | <0.001 |
| | CKD vs DM | 2.1 to 5.0 | 1.7 to 5.7 | <0.001 |
| | DM+CKD vs DM+HTN | 1.5 to 8.8 | 0.6 to 10.5 | 0.005 |
| | HTN+CKD vs DM+HTN | 5.3 to 9.5 | 4.6 to 10.4 | <0.001 |
| | HTN+CKD vs DM+CKD | −2.0 to 6.6 | −3.9 to 7.9 | 0.25 |
| Death | DM vs HTN | 0.02 to 0.17 | 0.01 to 0.21 | 0.022 |
| | CKD vs HTN | 0.14 to 0.80 | 0.08 to 1.01 | <0.001 |
| | CKD vs DM | 0.02 to 0.71 | **−0.02 to 0.92** | 0.066 |
| | DM+CKD vs DM+HTN | −0.09 to 1.36 | −0.17 to 2.19 | 0.36 |
| | HTN+CKD vs DM+HTN | 0.22 to 0.97 | 0.14 to 1.17 | <0.001 |
| | HTN+CKD vs DM+CKD | −0.90 to 0.81 | −1.66 to 1.07 | 0.65 |

Only one conclusion changes after adjustment: mortality **CKD vs DM** becomes borderline (its simultaneous interval includes 0). A nonparametric case bootstrap that refits the model (200 replicates; `within_count/`) agrees closely with the parametric intervals. The exception is the DM+CKD mortality contrasts, where 5/200 replicates contained no DM+CKD deaths.

## Interpretation limits (for the text, not the figure)
- These are standardised associations, not causal effects. They do not demonstrate biological synergy or interaction, and they make no claim about predictive superiority; prediction performance is a separate supplementary analysis.
- Imprecise contrasts (intervals including 0) should be described as uncertain, not as showing no difference.
