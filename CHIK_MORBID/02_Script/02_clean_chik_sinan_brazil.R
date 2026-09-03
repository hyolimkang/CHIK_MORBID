# ---------------------------------------------------------------------------
# 02_clean_chik_sinan_brazil.R   (CHIK_MORBID version)
#
# Takes raw SINAN chikungunya yearly CSVs (downloaded by
# 01_fetch_chik_sinan_brazil.R into 01_Data/sinan_chik_csv/) and produces:
#
#   1. individual-level cleaned cases
#      -> 01_Data/chik_sinan_individual_2015_2024.rds
#
#   2. municipality x month case-count panel (zero-filled)
#      -> 01_Data/chik_brazil_muni_month_2015_2024.rds (+ .csv)
#
#   3. municipality x epi-week case-count panel
#      -> 01_Data/chik_brazil_muni_week_2015_2024.rds  (+ .csv)
#
# DIFFERENCE FROM CHIK_CLIM VERSION:
#   * Adds 7 comorbidity columns (DIABETES, HIPERTENSA, HEMATOLOG, HEPATOPAT,
#     RENAL, ACIDO_PEPT, AUTO_IMUNE) and pregnancy trimester (CS_GESTANT),
#     decoded to readable labels.
#   * Individual table is the primary product (for paper-A descriptive +
#     paper-B vaccine impact). Panels are kept for parity with CHIK_CLIM.
#
# Reference : 01_Data/sinan_chik_docs/dic_dados_chikungunya.pdf
# ---------------------------------------------------------------------------

# ---- 0. Packages ----------------------------------------------------------

for (p in c("here", "data.table", "dplyr", "tidyr",
            "lubridate", "readr", "stringr", "tibble")) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p, repos = "https://cloud.r-project.org")
  }
}
suppressPackageStartupMessages({
  library(here); library(data.table); library(dplyr); library(tidyr)
  library(lubridate); library(readr); library(stringr); library(tibble)
})

# ---- 1. Config ------------------------------------------------------------

YEAR_START <- 2015
YEAR_END   <- 2024

DATA_DIR  <- here::here("01_Data")
RAW_DIR   <- file.path(DATA_DIR, "sinan_chik_csv")

OUT_INDIV       <- file.path(DATA_DIR,
                             sprintf("chik_sinan_individual_%d_%d.rds",
                                     YEAR_START, YEAR_END))
OUT_PANEL_M     <- file.path(DATA_DIR,
                             sprintf("chik_brazil_muni_month_%d_%d.rds",
                                     YEAR_START, YEAR_END))
OUT_PANEL_W     <- file.path(DATA_DIR,
                             sprintf("chik_brazil_muni_week_%d_%d.rds",
                                     YEAR_START, YEAR_END))
OUT_PANEL_M_CSV <- sub("\\.rds$", ".csv", OUT_PANEL_M)
OUT_PANEL_W_CSV <- sub("\\.rds$", ".csv", OUT_PANEL_W)

# Comorbidity panel (for paper-A descriptive: counts of cases by
# (muni6 x month x comorbidity status)) - simple summary cache
OUT_COMORB_PANEL <- file.path(DATA_DIR,
                              sprintf("chik_comorb_state_year_%d_%d.csv",
                                      YEAR_START, YEAR_END))

KEEP_COLS <- c(
  # ---- IDs / dates ----
  "DT_NOTIFIC", "DT_SIN_PRI", "DT_INVEST", "DT_ENCERRA", "DT_OBITO",
  "SEM_NOT",    "SEM_PRI",    "NU_ANO",
  # ---- geography ----
  "SG_UF_NOT",  "ID_MUNICIP",
  "SG_UF",      "ID_MN_RESI", "ID_PAIS",
  # ---- demographics ----
  "NU_IDADE_N", "CS_SEXO",    "CS_GESTANT", "CS_RACA",  "CS_ESCOL_N",
  # ---- case definition ----
  "ID_AGRAVO",  "CLASSI_FIN", "CRITERIO",   "CLINC_CHIK","EVOLUCAO",
  # ---- key clinical signs ----
  "FEBRE",      "ARTRALGIA",  "EXANTEMA",   "MIALGIA",  "CEFALEIA",
  # ---- key lab ----
  "RES_CHIKS1", "RES_CHIKS2", "RESUL_PRNT", "RESUL_PCR_",
  # ---- hospitalisation ----
  "HOSPITALIZ",
  # ---- COMORBIDITIES (NEW in CHIK_MORBID) ----
  "DIABETES",   "HEMATOLOG",  "HEPATOPAT",  "RENAL",
  "HIPERTENSA", "ACIDO_PEPT", "AUTO_IMUNE"
)

# ---- 2. Locate raw files (prefer .csv.zip; fall back to .csv) -------------

list_raw_files <- function() {
  files <- list.files(RAW_DIR, pattern = "^CHIKBR\\d{2}\\.csv(\\.zip)?$",
                      full.names = TRUE)
  info <- tibble::tibble(
    path   = files,
    yy     = as.integer(stringr::str_match(basename(files),
                                           "CHIKBR(\\d{2})")[, 2]),
    is_zip = grepl("\\.zip$", files)
  ) %>%
    mutate(year = ifelse(yy >= 50, 1900 + yy, 2000 + yy)) %>%
    filter(year >= YEAR_START, year <= YEAR_END) %>%
    arrange(year, desc(is_zip)) %>%   # zip preferred over plain csv
    distinct(year, .keep_all = TRUE)
  info
}

# ---- 3. Read one yearly file ----------------------------------------------

read_one <- function(path, year) {
  message(sprintf("[read]  %s", basename(path)))
  if (grepl("\\.zip$", path)) {
    inside <- sub("\\.zip$", "", basename(path))
    cmd <- sprintf("unzip -p %s %s", shQuote(path), shQuote(inside))
    dt  <- data.table::fread(cmd = cmd, sep = ",",
                             encoding = "Latin-1",
                             select = KEEP_COLS,
                             colClasses = "character",
                             showProgress = FALSE,
                             fill = TRUE)
  } else {
    dt  <- data.table::fread(path, sep = ",",
                             encoding = "Latin-1",
                             select = KEEP_COLS,
                             colClasses = "character",
                             showProgress = FALSE,
                             fill = TRUE)
  }
  dt[, SOURCE_YEAR := year]
  message(sprintf("  -> %d rows, %d cols", nrow(dt), ncol(dt)))
  dt
}

raw_index <- list_raw_files()
if (nrow(raw_index) == 0) {
  stop("No CHIKBR raw files found in ", RAW_DIR,
       ".\nRun 02_Script/01_fetch_chik_sinan_brazil.R first.")
}
message(sprintf("[plan]  %d yearly files found (%d-%d)",
                nrow(raw_index),
                min(raw_index$year), max(raw_index$year)))

raw_list <- mapply(read_one,
                   path = raw_index$path,
                   year = raw_index$year,
                   SIMPLIFY = FALSE)
raw <- data.table::rbindlist(raw_list, use.names = TRUE, fill = TRUE)
message(sprintf("[bind]  %s rows total",
                format(nrow(raw), big.mark = ",")))

# ---- 4. Decoding helpers --------------------------------------------------
# Source: dic_dados_chikungunya.pdf (Sinan)

# NU_IDADE_N: 4-digit code; 1st digit is the unit (1=hour, 2=day, 3=month, 4=year)
decode_age_years <- function(x) {
  v <- suppressWarnings(as.integer(x))
  unit  <- v %/% 1000
  value <- v %%  1000
  dplyr::case_when(
    unit == 1L ~ value / (24 * 365.25),
    unit == 2L ~ value / 365.25,
    unit == 3L ~ value / 12,
    unit == 4L ~ as.numeric(value),
    TRUE       ~ NA_real_
  )
}

map_sex     <- c(M = "male", F = "female", I = "unknown")

map_classif <- c(
  `5`  = "discarded",
  `8`  = "inconclusive",
  `10` = "dengue",
  `11` = "dengue_warning",
  `12` = "dengue_severe",
  `13` = "chikungunya_confirmed"
)
map_criterio <- c(`1` = "laboratory", `2` = "clinical_epi",
                  `3` = "clinical",   `9` = "unknown")
map_clinic   <- c(`1` = "acute", `2` = "chronic")
map_evolucao <- c(`1` = "cured",
                  `2` = "death_chik",
                  `3` = "death_other",
                  `4` = "death_under_investigation",
                  `9` = "unknown")
map_race     <- c(`1` = "white", `2` = "black", `3` = "asian",
                  `4` = "mixed", `5` = "indigenous", `9` = "unknown")

# 1=Sim / 2=Nao / 9=Ignorado (clinical signs + comorbidities)
map_sim_nao  <- c(`1` = "yes", `2` = "no", `9` = "unknown")

# Lab results: 1=Reagent 2=Non-reagent 3=Inconclusive 4=Not done
map_lab      <- c(`1` = "positive", `2` = "negative",
                  `3` = "inconclusive", `4` = "not_done")

# Pregnancy / CS_GESTANT:
#   1 = 1st trimester, 2 = 2nd, 3 = 3rd,
#   4 = gestational age unknown,
#   5 = not pregnant,
#   6 = not applicable (male / age out of range),
#   9 = ignored
map_gestant  <- c(`1` = "trim1",
                  `2` = "trim2",
                  `3` = "trim3",
                  `4` = "preg_age_unknown",
                  `5` = "not_pregnant",
                  `6` = "not_applicable",
                  `9` = "unknown")

recode <- function(x, map) {
  out <- unname(map[as.character(x)])
  out[x == "" | is.na(x)] <- NA_character_
  out
}

parse_date <- function(x) suppressWarnings(as.Date(x))

# ---- 5. Build cleaned individual-level table ------------------------------

clean <- raw %>%
  as_tibble() %>%
  transmute(
    # ---- source ----
    source_year       = SOURCE_YEAR,

    # ---- dates ----
    date_notification = parse_date(DT_NOTIFIC),
    date_onset        = parse_date(DT_SIN_PRI),
    date_investigation= parse_date(DT_INVEST),
    date_closure      = parse_date(DT_ENCERRA),
    date_death        = parse_date(DT_OBITO),
    epi_week_notif    = SEM_NOT,
    epi_week_onset    = SEM_PRI,
    notif_year        = suppressWarnings(as.integer(NU_ANO)),

    # ---- geography ----
    uf_notif          = SG_UF_NOT,
    muni_notif6       = ifelse(nchar(ID_MUNICIP) == 6, ID_MUNICIP, NA_character_),
    uf_residence      = SG_UF,
    muni_residence6   = ifelse(nchar(ID_MN_RESI) == 6, ID_MN_RESI, NA_character_),
    country_residence = ID_PAIS,

    # ---- demographics ----
    age_years         = decode_age_years(NU_IDADE_N),
    sex               = recode(CS_SEXO,    map_sex),
    race              = recode(CS_RACA,    map_race),
    pregnancy_state   = recode(CS_GESTANT, map_gestant),
    education         = CS_ESCOL_N,

    # ---- case definition ----
    cid10             = ID_AGRAVO,
    classification    = recode(CLASSI_FIN, map_classif),
    criterion         = recode(CRITERIO,   map_criterio),
    clinical_form     = recode(CLINC_CHIK, map_clinic),
    evolution         = recode(EVOLUCAO,   map_evolucao),
    is_confirmed_chik = (CLASSI_FIN == "13"),
    is_lab_confirmed  = (CLASSI_FIN == "13" & CRITERIO == "1"),
    died_from_chik    = (EVOLUCAO  == "2"),

    # ---- key clinical signs ----
    fever             = recode(FEBRE,     map_sim_nao),
    arthralgia        = recode(ARTRALGIA, map_sim_nao),
    rash              = recode(EXANTEMA,  map_sim_nao),
    myalgia           = recode(MIALGIA,   map_sim_nao),
    headache          = recode(CEFALEIA,  map_sim_nao),

    # ---- key lab ----
    igm_serum1        = recode(RES_CHIKS1, map_lab),
    igm_serum2        = recode(RES_CHIKS2, map_lab),
    prnt              = recode(RESUL_PRNT, map_lab),
    rt_pcr            = recode(RESUL_PCR_, map_lab),

    # ---- hospitalisation ----
    hospitalised      = recode(HOSPITALIZ, map_sim_nao),

    # ---- COMORBIDITIES (NEW) ----
    diabetes          = recode(DIABETES,   map_sim_nao),
    hypertension      = recode(HIPERTENSA, map_sim_nao),
    hepatopathy       = recode(HEPATOPAT,  map_sim_nao),
    renal_disease     = recode(RENAL,      map_sim_nao),
    hematologic       = recode(HEMATOLOG,  map_sim_nao),
    peptic_ulcer      = recode(ACIDO_PEPT, map_sim_nao),
    autoimmune        = recode(AUTO_IMUNE, map_sim_nao)
  ) %>%
  # Effective event date: prefer onset, fall back to notification
  mutate(
    event_date       = dplyr::coalesce(date_onset, date_notification),
    event_year_month = lubridate::floor_date(event_date, "month"),
    event_week       = lubridate::floor_date(event_date, "week", week_start = 7)
  ) %>%
  filter(
    !is.na(event_date),
    year(event_date) >= YEAR_START,
    year(event_date) <= YEAR_END
  ) %>%
  # Derived comorbidity summaries (handy for descriptive analysis)
  mutate(
    has_any_comorbidity = dplyr::case_when(
      diabetes == "yes" | hypertension == "yes" | hepatopathy == "yes" |
      renal_disease == "yes" | hematologic == "yes" |
      peptic_ulcer == "yes" | autoimmune == "yes"        ~ "yes",
      diabetes == "no" & hypertension == "no" & hepatopathy == "no" &
      renal_disease == "no" & hematologic == "no" &
      peptic_ulcer == "no" & autoimmune == "no"          ~ "no",
      TRUE                                                ~ "unknown"
    ),
    cardiometabolic = dplyr::case_when(
      diabetes == "yes" | hypertension == "yes"           ~ "yes",
      diabetes == "no"  & hypertension == "no"            ~ "no",
      TRUE                                                ~ "unknown"
    ),
    is_pregnant = pregnancy_state %in% c("trim1", "trim2", "trim3",
                                          "preg_age_unknown"),
    n_comorbidities = (diabetes      == "yes") +
                      (hypertension  == "yes") +
                      (hepatopathy   == "yes") +
                      (renal_disease == "yes") +
                      (hematologic   == "yes") +
                      (peptic_ulcer  == "yes") +
                      (autoimmune    == "yes")
  )

message(sprintf("[clean] %s cleaned individual rows",
                format(nrow(clean), big.mark = ",")))

# Quick completeness audit for comorbidities (sanity-check)
comorb_cols <- c("diabetes", "hypertension", "hepatopathy", "renal_disease",
                 "hematologic", "peptic_ulcer", "autoimmune", "pregnancy_state")
comp <- sapply(clean[comorb_cols], function(x) {
  c(yes_pct      = round(mean(x == "yes",      na.rm = TRUE) * 100, 2),
    no_pct       = round(mean(x == "no",       na.rm = TRUE) * 100, 2),
    unknown_pct  = round(mean(x == "unknown",  na.rm = TRUE) * 100, 2),
    na_pct       = round(mean(is.na(x))                       * 100, 2))
})
message("[audit] comorbidity completeness (% of cleaned rows):")
print(t(comp))

saveRDS(clean, OUT_INDIV)
message(sprintf("[save]  %s", OUT_INDIV))

# ---- 6. Municipality x month/week panels ----------------------------------

with_muni <- clean %>%
  mutate(muni6 = dplyr::coalesce(muni_residence6, muni_notif6)) %>%
  filter(!is.na(muni6))

aggregate_panel <- function(.df, time_col) {
  .df %>%
    group_by(muni6, !!sym(time_col)) %>%
    summarise(
      cases_notified      = dplyr::n(),
      cases_confirmed     = sum(is_confirmed_chik,        na.rm = TRUE),
      cases_lab_confirmed = sum(is_lab_confirmed,         na.rm = TRUE),
      cases_hospitalised  = sum(hospitalised == "yes",    na.rm = TRUE),
      deaths_from_chik    = sum(died_from_chik,           na.rm = TRUE),
      cases_w_comorb      = sum(has_any_comorbidity == "yes", na.rm = TRUE),
      cases_diabetic      = sum(diabetes == "yes",        na.rm = TRUE),
      cases_hypertensive  = sum(hypertension == "yes",    na.rm = TRUE),
      cases_pregnant      = sum(is_pregnant,              na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(muni6, !!sym(time_col))
}

fill_zero <- function(agg, time_col, all_times) {
  out_name <- time_col
  tidyr::expand_grid(
    muni6 = sort(unique(agg$muni6)),
    !!out_name := all_times
  ) %>%
    left_join(agg, by = c("muni6", out_name)) %>%
    mutate(across(starts_with("cases_") | starts_with("deaths_"),
                  ~ tidyr::replace_na(.x, 0L)))
}

# Monthly
all_months <- seq(as.Date(sprintf("%d-01-01", YEAR_START)),
                  as.Date(sprintf("%d-12-01", YEAR_END)),
                  by = "1 month")

panel_month <- with_muni %>%
  rename(year_month = event_year_month) %>%
  aggregate_panel("year_month") %>%
  fill_zero("year_month", all_months)

# Weekly (Sunday-starting weeks, matches SINAN epi-week convention)
all_weeks <- seq(lubridate::floor_date(
                   as.Date(sprintf("%d-01-01", YEAR_START)),
                   "week", week_start = 7),
                 lubridate::floor_date(
                   as.Date(sprintf("%d-12-31", YEAR_END)),
                   "week", week_start = 7),
                 by = "1 week")

panel_week <- with_muni %>%
  rename(week_start = event_week) %>%
  aggregate_panel("week_start") %>%
  fill_zero("week_start", all_weeks) %>%
  mutate(
    epi_year = lubridate::epiyear(week_start),
    epi_week = lubridate::epiweek(week_start)
  ) %>%
  select(muni6, week_start, epi_year, epi_week, everything())

saveRDS(panel_month, OUT_PANEL_M); readr::write_csv(panel_month, OUT_PANEL_M_CSV)
saveRDS(panel_week,  OUT_PANEL_W); readr::write_csv(panel_week,  OUT_PANEL_W_CSV)

message(sprintf(
  "[save]  muni x month -> %s  (%s rows, %d munis, %d months)",
  OUT_PANEL_M, format(nrow(panel_month), big.mark = ","),
  dplyr::n_distinct(panel_month$muni6), length(all_months)))
message(sprintf(
  "[save]  muni x week  -> %s  (%s rows, %d munis, %d weeks)",
  OUT_PANEL_W, format(nrow(panel_week), big.mark = ","),
  dplyr::n_distinct(panel_week$muni6), length(all_weeks)))

# ---- 7. Quick descriptive summary at state x year level -------------------

state_year_summary <- clean %>%
  filter(is_confirmed_chik) %>%
  mutate(
    year  = lubridate::year(event_date),
    state = substr(dplyr::coalesce(muni_residence6, muni_notif6), 1, 2)
  ) %>%
  filter(!is.na(state), state != "") %>%
  group_by(state, year) %>%
  summarise(
    n_confirmed         = dplyr::n(),
    n_hospitalised      = sum(hospitalised == "yes",        na.rm = TRUE),
    n_deaths            = sum(died_from_chik,               na.rm = TRUE),
    n_chronic           = sum(clinical_form == "chronic",   na.rm = TRUE),
    n_diabetic          = sum(diabetes == "yes",            na.rm = TRUE),
    n_hypertensive      = sum(hypertension == "yes",        na.rm = TRUE),
    n_any_comorb        = sum(has_any_comorbidity == "yes", na.rm = TRUE),
    n_pregnant          = sum(is_pregnant,                  na.rm = TRUE),
    median_age          = median(age_years, na.rm = TRUE),
    pct_female          = round(mean(sex == "female", na.rm = TRUE) * 100, 2),
    .groups = "drop"
  )

readr::write_csv(state_year_summary, OUT_COMORB_PANEL)
message(sprintf("[save]  state x year summary -> %s", OUT_COMORB_PANEL))

message("\n[done] Outputs:")
message("  individual cleaned : ", OUT_INDIV)
message("  muni x month       : ", OUT_PANEL_M, "  (+ .csv)")
message("  muni x week        : ", OUT_PANEL_W, "  (+ .csv)")
message("  state x year       : ", OUT_COMORB_PANEL)
