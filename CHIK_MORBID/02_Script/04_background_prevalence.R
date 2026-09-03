# ---------------------------------------------------------------------------
# 04_background_prevalence.R
#
# Loads external background comorbidity data and builds a single country x
# age-band prevalence table, bg_prev_country (+ population-weighted version
# bg_prev_country_pop). This is the IHME/GBD entry point for the pipeline:
# everything from here on (05_corr_matrix.R, 06_morbid_prob_calc.R) treats
# this table as its source of "how common is each condition in country X,
# age band Y" - independent of the Brazil SINAN cohort.
#
# Sources:
#   01_Data/gbd_prevalence.csv    IHME GBD: country x age x sex x cause
#                                  prevalence, for 6 of the 7 conditions
#                                  (diabetes, renal_disease, peptic_ulcer,
#                                  hepatopathy, hematologic, autoimmune)
#   01_Data/ncd_hypertension.csv  NCD-RisC-style country x age hypertension
#                                  prevalence (hypertension isn't in the GBD
#                                  cause list used above, so it's a separate
#                                  source, merged in)
#   01_Data/gbd_pop.csv           IHME GBD: country x age population counts
#
# Output (left in the session for 05_corr_matrix.R / 06_morbid_prob_calc.R):
#   bg_prev_country      country x age-band x condition prevalence (7 conditions)
#   bg_prev_country_pop  bg_prev_country + population, n_with_condition = prevalence * population
#   pop, age_lookup, keep_age, cause_map  (intermediate objects, kept in case
#                                          they're useful for debugging)
# ---------------------------------------------------------------------------

options(scipen = 999)

# ---- 1. GBD prevalence: 6 of the 7 conditions ------------------------------

gbd_raw <- read.csv("01_Data/gbd_prevalence.csv")

gbd <- gbd_raw |>
  filter(
    measure_name == "Prevalence",
    metric_name  == "Rate"
  ) |>
  dplyr::select(
    location_name,
    sex_name,
    age_name,
    cause_name,
    year,
    val, lower, upper
  ) |>
  mutate(
    prevalence = val / 1e5,
    prev_low   = lower / 1e5,
    prev_high  = upper / 1e5
  )

cause_map <- tribble(
  ~cause_name,                                   ~condition,
  "Diabetes mellitus",                           "diabetes",
  "Chronic kidney disease",                      "renal_disease",
  "Peptic ulcer disease",                        "peptic_ulcer",
  "Cirrhosis and other chronic liver diseases",  "hepatopathy",
  "Hemoglobinopathies and hemolytic anemias",    "hematologic",
  "Rheumatoid arthritis",                        "autoimmune"
)

gbd <- gbd |>
  inner_join(cause_map, by = "cause_name")

gbd <- gbd |>
  mutate(age_clean = str_remove(age_name, " years| year"))

keep_age <- c("<5","5-9","10-14","15-19","20-24","25-29","30-34","35-39",
              "40-44","45-49","50-54","55-59","60-64","65-69","70-74",
              "75-79","80-84","85-89","90-94","95+")

gbd <- gbd |>
  filter(age_clean %in% keep_age)

age_lookup <- gbd |>
  distinct(age_clean) |>
  mutate(
    age_start = case_when(
      str_detect(age_clean, "^<")  ~ 0,
      str_detect(age_clean, "\\+") ~ as.numeric(str_remove(age_clean, "\\+")),
      str_detect(age_clean, "-")   ~ as.numeric(str_extract(age_clean, "^[0-9]+")),
      TRUE                         ~ as.numeric(age_clean)
    ),
    age_end = case_when(
      str_detect(age_clean, "^<")  ~ as.numeric(str_remove(age_clean, "^<")) - 1,
      str_detect(age_clean, "\\+") ~ Inf,
      str_detect(age_clean, "-")   ~ as.numeric(str_extract(age_clean, "[0-9]+$")),
      TRUE                         ~ as.numeric(age_clean)
    )
  )

gbd <- gbd |>
  left_join(age_lookup, by = "age_clean")

gbd_long <- gbd |>
  filter(sex_name == "Both") |>
  transmute(
    iso3         = countrycode(location_name, "country.name", "iso3c"),
    country_name = location_name,
    condition,
    age_start,
    age_end,
    prevalence
  ) |>
  filter(!is.na(iso3))

# ---- 2. Hypertension: separate source (not in the GBD cause list above) ---

htn_raw <- read.csv("01_Data/ncd_hypertension.csv", check.names = FALSE)

htn <- htn_raw |>
  dplyr::select(
    country_name = Country,
    iso3         = ISO,
    sex          = Sex,
    year         = Year,
    age          = Age,
    prevalence   = `Prevalence of hypertension`,
    prev_low     = `Prevalence of hypertension lower 95% uncertainty interval`,
    prev_high    = `Prevalence of hypertension upper 95% uncertainty interval`
  )

latest_year <- max(htn$year)

htn <- htn |>
  filter(year == latest_year) |>
  group_by(country_name, iso3, age) |>
  summarise(
    prevalence = mean(prevalence, na.rm = TRUE),
    prev_low   = mean(prev_low,   na.rm = TRUE),
    prev_high  = mean(prev_high,  na.rm = TRUE),
    .groups = "drop"
  )

htn <- htn |>
  mutate(
    condition = "hypertension",
    age_start = as.numeric(str_extract(age, "^[0-9]+")),
    age_end = if_else(
      str_detect(age, "\\+"),
      Inf,
      as.numeric(str_extract(age, "[0-9]+$"))
    )
  )

htn_long <- htn |>
  dplyr::select(
    iso3,
    country_name,
    condition,
    age_start,
    age_end,
    prevalence
  )

# The hypertension source has no data below age 25; treat prevalence in
# 0-29-year-olds as 0 rather than leaving it missing.
htn_young <- htn_long |>
  dplyr::distinct(iso3, country_name) |>
  dplyr::mutate(
    condition = "hypertension",
    age_start = 0,
    age_end = 29,
    prevalence = 0
  ) |>
  dplyr::select(
    iso3,
    country_name,
    condition,
    age_start,
    age_end,
    prevalence
  )

htn_for_merge <- dplyr::bind_rows(
  htn_long,
  htn_young
) |>
  dplyr::group_by(
    iso3,
    country_name,
    condition,
    age_start,
    age_end
  ) |>
  dplyr::summarise(
    prevalence = mean(prevalence, na.rm = TRUE),
    .groups = "drop"
  )

# ---- 3. Combine GBD (6 conditions) + hypertension (separate source) -------

bg_prev_country <- dplyr::bind_rows(
  gbd_long,
  htn_for_merge
)

# ---- 4. Population, and prevalence x population -----------------------

pop_raw <- read.csv("01_Data/gbd_pop.csv")

pop <- pop_raw |>
  filter(measure_name == "Population", metric_name == "Number") |>
  mutate(age_clean = str_remove(age_name, " years| year")) |>
  filter(age_clean %in% keep_age) |>
  left_join(age_lookup, by = "age_clean") |>
  transmute(
    iso3         = countrycode(location_name, "country.name", "iso3c"),
    country_name = location_name,
    age_start,
    age_end,
    population   = val
  ) |>
  filter(!is.na(iso3))

bg_prev_country_pop <- bg_prev_country |>
  left_join(pop, by = c("iso3", "country_name", "age_start", "age_end")) |>
  mutate(n_with_condition = prevalence * population)

# Country x age population lookup, collapsed to one row per (iso3, age_start,
# age_end). Needed by 08_copula_simulation.R (to population-weight the
# simulated comorbidity-count distribution) and by
# 10_background_burden_graphs.R (region population plots).
pop_country_age <- bg_prev_country_pop |>
  dplyr::group_by(
    iso3,
    age_start,
    age_end
  ) |>
  dplyr::summarise(
    country_name = dplyr::first(stats::na.omit(country_name)),
    population = mean(population, na.rm = TRUE),
    .groups = "drop"
  )

message(sprintf(
  "[04] bg_prev_country: %s rows, %d countries, %d conditions (7 = GBD's 6 + hypertension)",
  format(nrow(bg_prev_country), big.mark = ","),
  dplyr::n_distinct(bg_prev_country$iso3),
  dplyr::n_distinct(bg_prev_country$condition)
))
