library(dplyr)
library(tidyr)
library(lubridate)
library(openxlsx)

analysis_df <- analysis_df |>
  mutate(
    year = year(event_date),
    age_group = case_when(
      age_years < 20 ~ "0–19",
      age_years < 40 ~ "20–39",
      age_years < 60 ~ "40–59",
      age_years < 80 ~ "60–79",
      age_years <= 100 ~ "80+",
      TRUE ~ NA_character_
    ),
    age_group = factor(
      age_group,
      levels = c("0–19", "20–39", "40–59", "60–79", "80+")
    ),
    hosp_label = if_else(hosp_only, "Yes", "No"),
    death_label = if_else(death_only, "Yes", "No")
  )

summarise_continuous <- function(data, var, label, by_var = "comorb_count_group") {
  
  var <- rlang::ensym(var)
  by_var <- rlang::ensym(by_var)
  
  overall <- data |>
    summarise(
      median = median(!!var, na.rm = TRUE),
      p25 = quantile(!!var, 0.25, na.rm = TRUE),
      p75 = quantile(!!var, 0.75, na.rm = TRUE)
    ) |>
    mutate(
      Characteristic = label,
      Level = "",
      Overall = sprintf("%.1f (%.1f–%.1f)", median, p25, p75)
    ) |>
    dplyr::select(Characteristic, Level, Overall)
  
  by_group <- data |>
    group_by(!!by_var) |>
    summarise(
      median = median(!!var, na.rm = TRUE),
      p25 = quantile(!!var, 0.25, na.rm = TRUE),
      p75 = quantile(!!var, 0.75, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      value = sprintf("%.1f (%.1f–%.1f)", median, p25, p75)
    ) |>
    dplyr::select(!!by_var, value) |>
    pivot_wider(
      names_from = !!by_var,
      values_from = value
    )
  
  bind_cols(overall, by_group)
}

summarise_categorical <- function(data, var, label, by_var = "comorb_count_group") {
  
  var <- rlang::ensym(var)
  by_var <- rlang::ensym(by_var)
  
  overall_n <- nrow(data)
  
  overall <- data |>
    count(!!var, name = "n") |>
    mutate(
      percent = n / overall_n * 100,
      Overall = sprintf("%s (%.1f%%)", format(n, big.mark = ","), percent),
      Characteristic = label,
      Level = as.character(!!var)
    ) |>
    dplyr::select(Characteristic, Level, Overall)
  
  by_group_denoms <- data |>
    count(!!by_var, name = "denom")
  
  by_group <- data |>
    count(!!by_var, !!var, name = "n") |>
    left_join(by_group_denoms, by = rlang::as_name(by_var)) |>
    mutate(
      percent = n / denom * 100,
      value = sprintf("%s (%.1f%%)", format(n, big.mark = ","), percent),
      Level = as.character(!!var)
    ) |>
    dplyr::select(!!by_var, Level, value) |>
    pivot_wider(
      names_from = !!by_var,
      values_from = value
    )
  
  overall |>
    left_join(by_group, by = "Level")
}

summarise_binary_yes <- function(data, var, label, by_var = "comorb_count_group") {
  
  var <- rlang::ensym(var)
  by_var <- rlang::ensym(by_var)
  
  overall_n <- nrow(data)
  
  overall_yes <- data |>
    summarise(
      n = sum(!!var == TRUE, na.rm = TRUE),
      percent = n / overall_n * 100
    ) |>
    mutate(
      Characteristic = label,
      Level = "",
      Overall = sprintf("%s (%.1f%%)", format(n, big.mark = ","), percent)
    ) |>
    dplyr::select(Characteristic, Level, Overall)
  
  by_group <- data |>
    group_by(!!by_var) |>
    summarise(
      denom = n(),
      n = sum(!!var == TRUE, na.rm = TRUE),
      percent = n / denom * 100,
      .groups = "drop"
    ) |>
    mutate(
      value = sprintf("%s (%.1f%%)", format(n, big.mark = ","), percent)
    ) |>
    dplyr::select(!!by_var, value) |>
    pivot_wider(
      names_from = !!by_var,
      values_from = value
    )
  
  bind_cols(overall_yes, by_group)
}


n_row <- analysis_df |>
  group_by(comorb_count_group) |>
  summarise(n = n(), .groups = "drop") |>
  mutate(value = format(n, big.mark = ",")) |>
  dplyr::select(comorb_count_group, value) |>
  pivot_wider(
    names_from = comorb_count_group,
    values_from = value
  )

overall_n <- format(nrow(analysis_df), big.mark = ",")

table1_n <- tibble(
  Characteristic = "Number of cases",
  Level = "",
  Overall = overall_n
) |>
  bind_cols(n_row)

table1_age <- summarise_continuous(
  data = analysis_df,
  var = age_years,
  label = "Age, years, median (IQR)"
)

table1_age_group <- summarise_categorical(
  data = analysis_df,
  var = age_group,
  label = "Age group, years"
)

table1_sex <- summarise_categorical(
  data = analysis_df,
  var = sex,
  label = "Sex"
)

table1_year <- summarise_categorical(
  data = analysis_df,
  var = year,
  label = "Year"
)

table1_hosp <- summarise_binary_yes(
  data = analysis_df,
  var = hosp_only,
  label = "Hospitalised"
)

table1_death <- summarise_binary_yes(
  data = analysis_df,
  var = death_only,
  label = "Died from chikungunya"
)

table1 <- bind_rows(
  table1_n,
  table1_age,
  table1_age_group,
  table1_sex,
  table1_year,
  table1_hosp,
  table1_death
)

table1 <- table1 |>
  rename(
    `0 comorbidities` = `0`,
    `1 comorbidity` = `1`,
    `2 comorbidities` = `2`,
    `3+ comorbidities` = `3+`
  )


crude_outcome_table <- analysis_df |>
  group_by(age_group, comorb_count_group) |>
  summarise(
    cases = n(),
    hospitalised = sum(hosp_only, na.rm = TRUE),
    deaths = sum(death_only, na.rm = TRUE),
    hospitalisation_risk_percent = hospitalised / cases * 100,
    cfr_percent = deaths / cases * 100,
    .groups = "drop"
  ) |>
  mutate(
    cases = format(cases, big.mark = ","),
    hospitalised = format(hospitalised, big.mark = ","),
    deaths = format(deaths, big.mark = ","),
    hospitalisation_risk_percent = sprintf("%.2f", hospitalisation_risk_percent),
    cfr_percent = sprintf("%.3f", cfr_percent)
  ) |>
  rename(
    `Age group` = age_group,
    `Number of comorbidities` = comorb_count_group,
    `Cases` = cases,
    `Hospitalised` = hospitalised,
    `Deaths` = deaths,
    `Hospitalisation risk, %` = hospitalisation_risk_percent,
    `CFR, %` = cfr_percent
  )


base_confirmed <- ind |>
  filter(
    is_confirmed_chik,
    year(event_date) >= 2017
  )

vars_missing <- c(
  "age_years",
  "sex",
  "hospitalised",
  "died_from_chik",
  comorb_cols
)

missingness_table <- lapply(vars_missing, function(v) {
  
  x <- base_confirmed[[v]]
  n_total <- length(x)
  n_missing <- sum(is.na(x))
  n_non_missing <- sum(!is.na(x))
  
  tibble(
    Variable = v,
    `Non-missing, n (%)` = sprintf(
      "%s (%.1f%%)",
      format(n_non_missing, big.mark = ","),
      n_non_missing / n_total * 100
    ),
    `Missing, n (%)` = sprintf(
      "%s (%.1f%%)",
      format(n_missing, big.mark = ","),
      n_missing / n_total * 100
    )
  )
}) |>
  bind_rows()


## Condition composition table ###############################################

condition_composition_table_clear <- analysis_df |>
  filter(comorb_count_group != "0") |>
  dplyr::select(comorb_count_group, all_of(comorb_cols)) |>
  pivot_longer(
    cols = all_of(comorb_cols),
    names_to = "condition",
    values_to = "status"
  ) |>
  mutate(
    condition = recode(condition, !!!comorbidity_labels),
    has_condition = status == "yes"
  ) |>
  group_by(comorb_count_group, condition) |>
  summarise(
    denominator = n(),
    n = sum(has_condition, na.rm = TRUE),
    percent = n / denominator * 100,
    .groups = "drop"
  ) |>
  mutate(
    value = sprintf(
      "%s / %s (%.1f%%)",
      format(n, big.mark = ","),
      format(denominator, big.mark = ","),
      percent
    )
  ) |>
  dplyr:: select(
    condition,
    comorb_count_group,
    value
  ) |>
  pivot_wider(
    names_from = comorb_count_group,
    values_from = value
  ) |>
  rename(
    `Recorded condition` = condition,
    `Among cases with 1 recorded comorbidity` = `1`,
    `Among cases with 2 recorded comorbidities` = `2`,
    `Among cases with ≥3 recorded comorbidities` = `3+`
  )
####
output_dir <- "03_Output/tables"

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

wb <- createWorkbook()

addWorksheet(wb, "Table1_baseline")
writeData(wb, "Table1_baseline", table1)

addWorksheet(wb, "S_crude_outcomes")
writeData(wb, "S_crude_outcomes", crude_outcome_table)

addWorksheet(wb, "S_missingness")
writeData(wb, "S_missingness", missingness_table)

addWorksheet(wb, "condition_composition")
writeData(wb, "condition_composition", condition_composition_table_clear)

saveWorkbook(
  wb,
  file = file.path(output_dir, "chik_sinan_descriptive_tables.xlsx"),
  overwrite = TRUE
)