# CHIK_MORBID

Comorbidity-stratified analysis of chikungunya in Brazil:
**clinical burden + vaccination strategy + benefit-risk**.

Companion project to `CHIK_CLIM` (climate analysis). This project is
**stand-alone**: data and scripts live here so the analysis can be reproduced
independently.

---

## Research questions

**Background analysis (Paper-A scope, descriptive + clinical):**
- How does the clinical burden of chikungunya differ between people with and
  without underlying conditions (diabetes, hypertension, hepatopathy, renal,
  hematologic, peptic, autoimmune, pregnancy) in Brazil, 2015-2024?
- After adjustment for age, sex, region, and time, which comorbidities are
  associated with hospitalisation, death, and chronic chikungunya?
- Are comorbid patients over-represented relative to the general Brazilian
  population (PNS 2019) -> evidence of differential incidence vs reporting?

**Primary analysis (Paper-B scope, modelling + policy):**
- How does an *outbreak-response* vaccination programme differ in benefit and
  risk when targeted by **comorbidity + age** vs **age alone**?
- For each comorbidity group, what is the benefit-risk ratio across plausible
  vaccine-efficacy / AEFI scenarios?
- Which group should be prioritised when supply is limited?

---

## Data sources

| Layer | Source | Granularity | Years |
|---|---|---|---|
| Individual chik cases (with comorbidities) | SINAN DATASUS | individual | 2015-2024 |
| Population denominator | IBGE SIDRA | municipality x year | 2015-2024 |
| Population comorbidity prevalence | PNS 2019 (IBGE) | state x age x sex | 2019 |
| Self-reported comorbidity trends | VIGITEL | capital city x year | varies |
| AEFI / vaccine safety | WHO VigiBase, FDA/EMA, PNI | aggregated | post-licensure |

---

## Folder layout

```
CHIK_MORBID/
├── CHIK_MORBID.Rproj
├── 01_Data/
│   ├── sinan_chik_csv/         # raw SINAN yearly zips (gitignored)
│   ├── sinan_chik_docs/        # SINAN data dictionary PDF
│   ├── chik_sinan_individual_*.rds   # cleaned individual table (regen)
│   ├── ibge_pop_muni_year_*.csv      # population denominator
│   ├── pns_*/                  # PNS microdata (to be added)
│   └── aefi_*/                 # AEFI data (to be added)
├── 02_Script/
│   ├── fetch_chik_sinan_brazil.R     # download raw SINAN (cached)
│   ├── fetch_ibge_population.R       # download IBGE pop estimates
│   ├── clean_chik_sinan_brazil.R     # clean + decode + add comorbidities
│   ├── fetch_pns_2019.R              # download PNS 2019 (TO DO)
│   ├── table1_descriptive.R          # Phase 1 descriptive (TO DO)
│   ├── outcomes_logistic.R           # Phase 2 outcome models (TO DO)
│   ├── smr_vs_pns.R                  # Phase 3 SMR vs general pop (TO DO)
│   ├── vaccine_impact_comorbidity.R  # Paper-B impact model (TO DO)
│   └── benefit_risk_comorbidity.R    # Paper-B BR ratio (TO DO)
└── 03_Output/
    ├── figures/
    └── tables/
```

---

## Roadmap (planned phases)

1. **Phase 1 — Descriptive**: Table 1, time/space distribution by comorbidity.
2. **Phase 2 — Outcomes**: adjusted OR for hospitalisation, death, chronic
   chik (logistic, individual-level).
3. **Phase 3 — Population reference**: SMR vs PNS 2019; reporting-bias check.
4. **Phase 4 — Vaccine impact**: extend existing age-only outbreak-response
   immunisation model to age x comorbidity strata.
5. **Phase 5 — Benefit-risk**: comorbidity-stratified BR ratio, sensitivity
   analyses on VE and AEFI rates.

---

## Reproducibility

To regenerate from scratch:
```r
source("02_Script/fetch_chik_sinan_brazil.R")   # downloads raw zips
source("02_Script/fetch_ibge_population.R")     # downloads pop
source("02_Script/clean_chik_sinan_brazil.R")   # builds individual + panels
# ... downstream scripts
```
