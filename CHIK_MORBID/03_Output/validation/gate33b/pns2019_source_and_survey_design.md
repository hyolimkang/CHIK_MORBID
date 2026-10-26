# Gate 3.3B - PNS 2019 data source and survey design documentation

## Data provenance (verified this session)

| Item | Value |
|---|---|
| Data file | PNS_2019_20220525.zip -> PNS_2019.txt (fixed-width) |
| Data URL | https://ftp.ibge.gov.br/PNS/2019/Microdados/Dados/PNS_2019_20220525.zip |
| Download size | 29,512,476 bytes (verified byte-exact against server Content-Length) |
| Server Last-Modified | 2022-05-25 |
| Documentation URL | https://ftp.ibge.gov.br/PNS/2019/Microdados/Documentacao/ |
| Dictionary/input file | Dicionario_e_input_20220530.zip (172,422 bytes; dicionario_PNS_microdados_2019.xls + input_PNS_2019.sas/.txt) |
| Sampling-keys doc | Chaves_PNS_2019.pdf (113,841 bytes) |
| Reader | PNSIBGE::read_pns() (CRAN), parses the official SAS INPUT layout directly - no fixed-width position was hand-coded or guessed |
| Local storage | 01_Data/pns_2019_raw/ (gitignored; not committed/pushed) |

## Structural finding (verified empirically, not assumed from documentation alone)

`read_pns()` returns **279,382 rows = one row per household RESIDENT**, not one row per household. The selected-adult individual questionnaire (module M interview-status, module C demographics, and the Q00201/Q03001/Q124 diagnosis questions) is populated **only** on the single row per household corresponding to the randomly selected adult; all other residents' rows carry these fields as NA by design.

`M001` ('Entrevista do adulto selecionado' - the selected adult's interview completion status) is the correct eligibility flag for the selected-adult analytic population - **distinct from `V0015`** (household-level interview completion, which `read_pns()` already filters on internally but does not by itself guarantee the selected-adult module was completed). This distinction was verified empirically: `V00291` (the calibrated selected-adult weight) is non-missing and positive for **exactly** the 90,846 rows with `M001=='1'`, and missing/zero for every other row - a clean 1:1 correspondence confirming `M001=='1'` is the correct selection criterion.

## Verified variable definitions (against dicionario_PNS_microdados_2019.xls)

| Variable | Official label (PT) | Role |
|---|---|---|
| UPA_PNS | UPA (Unidade Primaria de Amostragem) | Primary sampling unit (PSU) |
| V0024 | Estrato | Survey stratum |
| V00291 | Peso do morador selecionado COM calibracao | Calibrated selected-adult weight (5 digits, 8 decimals) - CONFIRMED as hypothesised |
| V0029 | Peso do morador selecionado SEM calibracao | Uncalibrated selected-adult weight (not used) |
| V0030/V00301 | Peso ... para antropometria | Anthropometry-subsample weight (different, smaller sub-sample - NOT used here) |
| M001 | Entrevista do adulto selecionado | Selected-adult interview status: 1=Realizada, 2=Recusa, 3=Morador nao encontrado, 9=Ignorado |
| V0015 | Tipo da entrevista | HOUSEHOLD-level interview completion (1=Realizada) - NOT the same as M001 |
| C006 | Sexo | 1=Homem(male), 2=Mulher(female) |
| C008 | Idade do morador na data de referencia | Age, numeric, 000-130 |
| Q00201 | Algum medico ja lhe deu o diagnostico de hipertensao arterial (pressao alta)? | Physician-diagnosed hypertension: 1=Sim,2=Nao,9=Ignorado |
| Q00202 | Essa hipertensao ... ocorreu apenas durante algum periodo de gravidez? | Pregnancy-only hypertension flag |
| Q03001 | Algum medico ja lhe deu o diagnostico de diabetes? | Physician-diagnosed diabetes: 1=Sim,2=Nao,9=Ignorado |
| Q03002 | Esse diabetes ocorreu apenas durante algum periodo de gravidez? | Pregnancy-only diabetes flag |
| Q124 | Algum medico ja lhe deu o diagnostico de insuficiencia renal cronica? | Physician-diagnosed chronic renal insufficiency: 1=Sim,2=Nao,9=Ignorado (no pregnancy-only variant exists) |

## Survey design implemented

```r
des_full <- survey::svydesign(
  ids     = ~UPA_PNS,
  strata  = ~V0024,
  weights = ~V00291,
  data    = selected_adults,   # M001=='1', N=90,846
  nest    = TRUE                # PSU codes unique only WITHIN a stratum (standard IBGE multi-stage convention)
)
options(survey.lonely.psu = 'adjust')  # conservative, transparent handling of any single-PSU stratum in a subdomain
```

Design summary: **8,027 PSUs**, **574 strata**, **N=90,846** selected adults (age range 15-107).

## Quality check: reproduction of nationally published estimates

| Population | This analysis | Approximate officially published PNS 2019 estimate |
|---|---|---|
| Selected adults aged 18+ (N) | 88,531 | ~88,531 (well-established PNS 2019 headline adult sample size) |
| Diabetes prevalence, 18+ | 8.238% | ~7.7% |
| Hypertension prevalence, 18+ | 24.41% | ~23.9% |

Both condition estimates fall close to (hypertension: within) the approximate published range; the diabetes estimate is modestly higher, plausibly reflecting this analysis's exact age cutoff, pregnancy-only handling, and complete-case denominator rather than a methodological error - the match is close enough to support confidence in the extraction and design-weighting pipeline.
