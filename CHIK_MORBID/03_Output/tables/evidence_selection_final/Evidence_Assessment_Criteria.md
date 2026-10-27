# Supplementary Methods: Qualitative criteria for evidence-informed selection of underlying conditions

## Purpose

SINAN records seven underlying conditions: diabetes mellitus, hypertension, chronic kidney disease, hepatopathy, autoimmune disease, haematological disease and peptic ulcer disease. We used published evidence to decide which conditions to prioritise for the primary condition-specific and joint-profile analyses. The assessment was a focused, qualitative synthesis of a structured evidence extraction. It is not a systematic review, and it is not a GRADE, ROBINS-I or numerical scoring assessment. This study's own estimates were not used in the assessment.

## Evidence base

Evidence came from a completed extraction workbook with 24 bibliographic records (snapshot and MD5 in `source_snapshot/`). Of these:
- 5 records were excluded: chronic or post-acute outcomes (3), a broad arbovirus–diabetes review (1) and an exact duplicate (1).
- 5 records were retained as contextual only: a study where CHIKV infection was the exposure rather than a pre-existing condition, a fatal-case-only analysis, a renal-pathology series of fatal cases, an ICU case series, and an ecological study. These records were not counted as condition-level prognostic evidence.

Search dates, search terms and screening counts are not recorded in the workbook and are therefore not reported.

For each condition, study and outcome we recorded:
- the evidence type: adjusted estimate; adjusted model with direction reported but no numeric estimate extracted; crude estimate; crude comparison without an estimate; pooled synthesis; synthesis without a pooled estimate; descriptive only; assessed but no result extracted; or contextual;
- the direction of association: positive, null, imprecise, or not reported;
- the estimate as extracted.

Each numeric estimate was checked word for word against the workbook (`Evidence_Study_Level_Audit.csv`, column `verbatim_check`).

## Target outcomes

The two target outcomes were assessed separately:
- **Hospitalisation** among chikungunya cases.
- **Mortality**, meaning death during the acute illness.

Severe or composite outcomes (for example, WHO-defined severe atypical disease) were not counted as mortality evidence. Neither were mortality comparisons within ICU-selected cohorts used as evidence of risk among all cases, nor comparisons among fatal cases only. These records are kept in the audit with their outcome labelled.

## Assessment domains

| Domain | Question | Information used |
|---|---|---|
| Outcome directness | Does the study compare the outcome between people with and without the condition, among all chikungunya cases? | Outcome definition; selected populations (ICU-only, hospitalised-only, fatal-only) |
| Consistency | Is the direction of association recurrent across evidence families? | Direction by family, including null and contrary findings |
| Precision | Are the estimates informative? | Confidence-interval width; whether intervals exclude 1; missing CIs |
| Confounding adjustment | Was the condition-specific estimate adjusted? | Multivariable, age-only or crude |
| Independence | Does the evidence come from populations other than the target SINAN data? | Evidence families (below) |

### Study-level precision flag

The audit's `judgement_precision` column applies a descriptive rule to each extracted estimate:

| Flag | Rule |
|---|---|
| Imprecise (wide CI) | Upper/lower CI ratio > 3 |
| Compatible with null | CI includes 1, and the ratio is ≤ 3 |
| Reasonably precise | Otherwise |
| Not assessable | No CI was recorded, or no estimate was extracted |

This flag is an aid only. The categories were judged across all domains, not from whether a CI excludes 1.

Adjustment covariates are not recorded in the workbook, so they are reported as "Not extracted". The exception is Perti 2016, which is age-adjusted.

## Evidence families

Publications from the same or substantially overlapping populations were grouped into one family and counted once:

| Family | Studies | Relation to the target data |
|---|---|---|
| BRA-A | National SINAN: Furtado 2026 (2017–2023); Cerqueira-Silva 2025 (2015–2024) | Substantial overlap |
| BRA-B | Ceará 2016–2017 surveillance, the same 182,731 notifications: Pinto 2019; Silva Junior 2019; Silva Junior 2018 | Partial overlap (2017) |
| BRA-C | Fortaleza 2017 case-control: Oliveira 2022 | Partial overlap |
| IND-PR | US veterans / Puerto Rico: Perti 2016 | External |
| IND-RE | Réunion outbreak: Economopoulou 2009 | External |
| IND-PY | Paraguay ICU: Añazco 2026 | External (ICU-selected) |
| SYN | Micheleto 2025; Badawi 2018; Costa 2023; Cerbino-Neto 2020; de Almeida Barreto 2018 | Syntheses that include overlapping primary studies |

A family contributes to the count for an outcome when at least one of its publications reports condition-specific comparative evidence (adjusted or crude) for that outcome. Syntheses corroborate findings but are not counted, because they pool primary studies that are already counted.

## Evidence categories (per condition and outcome)

- **Supported.** Direct condition-specific evidence for the outcome, with a recurrent direction of association across analytically informative evidence and/or corroboration from an independent evidence source. A pooled estimate is not required. Overlapping publications are not counted as independent replication.
- **Limited.** A plausible signal exists, but it is restricted by one or more of the following: a single or overlapping study population, crude-only analysis, sparse events or wide confidence intervals, outcome-specific evidence, or otherwise limited replication.
- **Insufficient.** No sufficiently informative direct comparative evidence exists, or the available evidence is descriptive, non-comparative, non-estimable or too sparse. This category does not mean evidence of no association.
- **Not assessable.** Used only at domain level, when no comparative evidence exists to judge that domain.

## Prioritisation rule

| Prioritisation | Rule | Use in analysis |
|---|---|---|
| Core | Supported for at least one target outcome and at least Limited for the other | Primary condition-specific and joint-profile models |
| Secondary | Signal confined to one outcome and/or Limited overall | Not in the primary core-3 model |
| Not prioritised | Insufficient for both outcomes | Not in primary RR estimation; kept in the evidence map |

These category definitions and the prioritisation rule are reproduced from the Definitions sheet of the extraction workbook.

## Provenance and limitations

- The displayed categories are taken unchanged from the project's earlier evidence matrix (`02_Script/16_figure_evidence_matrix.R`). That matrix labelled the second outcome "severe/fatal".
- A re-check against the death-only evidence and the workbook rules is reported separately in `Evidence_Classification_Reconciliation.md`. Discrepancies are flagged there and were not substituted into the table.
- We do not claim the classification was prospectively prespecified. No dated protocol documenting the selection before the analysis was located.
- The extraction workbook is stored outside the repository, so a snapshot was copied to `source_snapshot/` for reproducibility.
- A single reviewer extraction is documented. Duplicate independent extraction is not recorded.

Reproducible script: `02_Script/61_evidence_selection_supplementary_table.R`.
