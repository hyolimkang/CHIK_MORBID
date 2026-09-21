# ---------------------------------------------------------------------------
# 14_condition_recording_patterns.R
#
# Describes reporting completeness of the seven SINAN comorbidity fields.
#
# The estimand is P(condition recorded): for a given condition, the field is
# "recorded" when its value is explicitly "yes" or "no". Values "unknown"
# and NA are both treated as not recorded. This is intentionally distinct from
# P(condition present), which uses only the "yes" values.
#
# Produces unadjusted, denominator-visible summaries by hospitalisation,
# chikungunya death, age, sex, year, macro-region, state and state x year.
# The hospitalisation comparison is limited to patients whose hospitalisation
# status is known; the death comparison is limited to a known death status.
# Other summaries use all confirmed 2017+ cases with a known event date, and
# retain an explicit "Unknown" stratum where a covariate is unavailable.
#
# Needs: 01_Data/chik_sinan_individual_2015_2024.rds and packages from
#        00_setup.R. Runs standalone and does not alter the RR cohorts.
#
# Outputs:
#   01_Data/condition_recording_summaries.RData
#   03_Output/tables/condition_recording_summaries.xlsx
#   03_Output/figures/fig_condition_recording_by_outcome.jpg
#   03_Output/figures/fig_condition_recording_by_age.jpg
#   03_Output/figures/fig_condition_recording_by_sex.jpg
#   03_Output/figures/fig_condition_recording_by_year_region.jpg
#   03_Output/figures/fig_condition_recording_by_state_year.jpg
# ---------------------------------------------------------------------------

fig_dir <- "03_Output/figures"
table_dir <- "03_Output/tables"

if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)
if (!dir.exists(table_dir)) dir.create(table_dir, recursive = TRUE)

ind <- readRDS("01_Data/chik_sinan_individual_2015_2024.rds")

condition_cols <- c(
  "diabetes",
  "hypertension",
  "hepatopathy",
  "renal_disease",
  "hematologic",
  "peptic_ulcer",
  "autoimmune"
)

condition_labels <- c(
  diabetes = "Diabetes",
  hypertension = "Hypertension",
  hepatopathy = "Hepatopathy",
  renal_disease = "Chronic kidney disease",
  hematologic = "Haematologic disease",
  peptic_ulcer = "Peptic ulcer disease",
  autoimmune = "Autoimmune disease"
)

## State is derived from the first two digits of the residence municipality
## code, falling back to notification municipality where residence is absent.
## This is more reliable than the raw SG_UF residence field in the source data.
state_lookup <- tibble::tribble(
  ~state_code, ~state, ~region,
  "11", "RO", "North", "12", "AC", "North", "13", "AM", "North",
  "14", "RR", "North", "15", "PA", "North", "16", "AP", "North",
  "17", "TO", "North", "21", "MA", "Northeast", "22", "PI", "Northeast",
  "23", "CE", "Northeast", "24", "RN", "Northeast", "25", "PB", "Northeast",
  "26", "PE", "Northeast", "27", "AL", "Northeast", "28", "SE", "Northeast",
  "29", "BA", "Northeast", "31", "MG", "Southeast", "32", "ES", "Southeast",
  "33", "RJ", "Southeast", "35", "SP", "Southeast", "41", "PR", "South",
  "42", "SC", "South", "43", "RS", "South", "50", "MS", "Central-West",
  "51", "MT", "Central-West", "52", "GO", "Central-West", "53", "DF", "Central-West"
)

state_levels <- state_lookup$state
region_levels <- c("North", "Northeast", "Southeast", "South", "Central-West")
age_levels <- c("0-19", "20-39", "40-59", "60-79", "80+", "Unknown")

## The analysis starts from all confirmed, event-dated records in the period
## used for the RR analyses. No comorbidity field is used as an eligibility
## criterion, because its missingness is the quantity being described here.
recording_base <- ind |>
  dplyr::filter(
    is_confirmed_chik,
    !is.na(event_date),
    lubridate::year(event_date) >= 2017
  ) |>
  dplyr::mutate(
    year = factor(lubridate::year(event_date)),
    municipality_code = dplyr::coalesce(
      dplyr::na_if(muni_residence6, ""),
      dplyr::na_if(muni_notif6, "")
    ),
    state_code = substr(municipality_code, 1, 2)
  ) |>
  dplyr::left_join(state_lookup, by = "state_code") |>
  dplyr::mutate(
    state = factor(
      dplyr::coalesce(state, "Unknown"),
      levels = c(state_levels, "Unknown")
    ),
    region = factor(
      dplyr::coalesce(region, "Unknown"),
      levels = c(region_levels, "Unknown")
    ),
    age_group = cut(
      age_years,
      breaks = c(0, 20, 40, 60, 80, Inf),
      labels = age_levels[1:5],
      right = FALSE,
      include.lowest = TRUE
    ),
    age_group = forcats::fct_explicit_na(age_group, na_level = "Unknown"),
    age_group = factor(age_group, levels = age_levels),
    sex_group = dplyr::case_when(
      sex == "female" ~ "Female",
      sex == "male" ~ "Male",
      TRUE ~ "Unknown"
    ),
    sex_group = factor(sex_group, levels = c("Female", "Male", "Unknown")),
    hospitalisation_group = dplyr::case_when(
      hospitalised == "no" ~ "Not hospitalised",
      hospitalised == "yes" ~ "Hospitalised",
      TRUE ~ "Unknown"
    ),
    hospitalisation_group = factor(
      hospitalisation_group,
      levels = c("Not hospitalised", "Hospitalised", "Unknown")
    ),
    death_group = dplyr::case_when(
      died_from_chik %in% FALSE ~ "Did not die from chikungunya",
      died_from_chik %in% TRUE ~ "Died from chikungunya",
      TRUE ~ "Unknown"
    ),
    death_group = factor(
      death_group,
      levels = c(
        "Did not die from chikungunya",
        "Died from chikungunya",
        "Unknown"
      )
    )
  )

recording_long <- recording_base |>
  dplyr::select(
    year, state, region, age_group, sex_group,
    hospitalisation_group, death_group,
    dplyr::all_of(condition_cols)
  ) |>
  tidyr::pivot_longer(
    cols = dplyr::all_of(condition_cols),
    names_to = "condition",
    values_to = "condition_value"
  ) |>
  dplyr::mutate(
    condition = factor(
      condition,
      levels = condition_cols,
      labels = unname(condition_labels[condition_cols])
    ),
    recorded = condition_value %in% c("yes", "no")
  )

## Wilson intervals are used solely to display the sampling precision of each
## observed recording probability; they do not account for systematic data
## quality differences between hospitals, states, or time periods.
add_wilson_interval <- function(summary_df) {

  z <- stats::qnorm(0.975)

  summary_df |>
    dplyr::mutate(
      recording_probability = n_recorded / n_patients,
      n_not_recorded = n_patients - n_recorded,
      wilson_denominator = 1 + z^2 / n_patients,
      recording_lower = (
        recording_probability + z^2 / (2 * n_patients) -
          z * sqrt(
            recording_probability * (1 - recording_probability) / n_patients +
              z^2 / (4 * n_patients^2)
          )
      ) / wilson_denominator,
      recording_upper = (
        recording_probability + z^2 / (2 * n_patients) +
          z * sqrt(
            recording_probability * (1 - recording_probability) / n_patients +
              z^2 / (4 * n_patients^2)
          )
      ) / wilson_denominator
    ) |>
    dplyr::select(-wilson_denominator)
}

summarise_recording <- function(data, ...) {
  data |>
    dplyr::group_by(condition, ..., .drop = TRUE) |>
    dplyr::summarise(
      n_patients = dplyr::n(),
      n_recorded = sum(recorded),
      .groups = "drop"
    ) |>
    add_wilson_interval()
}

recording_overall <- summarise_recording(recording_long)

recording_by_hospitalisation <- recording_long |>
  dplyr::filter(hospitalisation_group != "Unknown") |>
  summarise_recording(hospitalisation_group)

recording_by_death <- recording_long |>
  dplyr::filter(death_group != "Unknown") |>
  summarise_recording(death_group)

recording_by_age <- summarise_recording(recording_long, age_group)
recording_by_sex <- summarise_recording(recording_long, sex_group)
recording_by_year <- summarise_recording(recording_long, year)
recording_by_region <- summarise_recording(recording_long, region)
recording_by_state <- summarise_recording(recording_long, state)

recording_by_year_region <- recording_long |>
  dplyr::filter(region != "Unknown") |>
  summarise_recording(year, region)

recording_by_state_year <- recording_long |>
  dplyr::filter(state != "Unknown") |>
  summarise_recording(state, year)

print(recording_by_hospitalisation, n = Inf)
print(recording_by_death, n = Inf)

## --------------------------------------------------------------------------
## Visualisations
## --------------------------------------------------------------------------

recording_outcome_plot_data <- dplyr::bind_rows(
  recording_by_hospitalisation |>
    dplyr::transmute(
      condition,
      comparison = "Hospitalisation",
      group = hospitalisation_group,
      recording_probability,
      recording_lower,
      recording_upper
    ),
  recording_by_death |>
    dplyr::transmute(
      condition,
      comparison = "Chikungunya death",
      group = death_group,
      recording_probability,
      recording_lower,
      recording_upper
    )
)

fig_condition_recording_by_outcome <- ggplot2::ggplot(
  recording_outcome_plot_data,
  ggplot2::aes(
    x = condition,
    y = recording_probability,
    colour = group,
    group = group
  )
) +
  ggplot2::geom_errorbar(
    ggplot2::aes(ymin = recording_lower, ymax = recording_upper),
    width = 0.15,
    linewidth = 0.5
  ) +
  ggplot2::geom_line(linewidth = 0.6) +
  ggplot2::geom_point(size = 2) +
  ggplot2::facet_wrap(~ comparison, ncol = 1) +
  ggplot2::scale_y_continuous(
    limits = c(0, 1),
    labels = scales::label_percent(accuracy = 1)
  ) +
  ggplot2::labs(
    title = "Probability that each underlying-condition field is recorded",
    subtitle = "Hospitalisation comparison excludes records with unknown hospitalisation status; 95% Wilson intervals",
    x = NULL,
    y = "P(condition recorded)",
    colour = NULL
  ) +
  ggplot2::theme_bw(base_size = 10) +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(angle = 30, hjust = 1),
    legend.position = "bottom"
  )

plot_recording_by_stratum <- function(summary_df, stratum, stratum_label) {

  ggplot2::ggplot(
    summary_df,
    ggplot2::aes_string(
      x = stratum,
      y = "recording_probability",
      group = "condition"
    )
  ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        ymin = recording_lower,
        ymax = recording_upper
      ),
      width = 0.12,
      linewidth = 0.4,
      colour = "grey45"
    ) +
    ggplot2::geom_line(colour = "#4C78A8", linewidth = 0.6) +
    ggplot2::geom_point(colour = "#4C78A8", size = 1.7) +
    ggplot2::facet_wrap(~ condition, ncol = 4) +
    ggplot2::scale_y_continuous(
      limits = c(0, 1),
      labels = scales::label_percent(accuracy = 1)
    ) +
    ggplot2::labs(
      title = paste0("Condition-field recording by ", stratum_label),
      subtitle = "Points are observed probabilities; bars are 95% Wilson intervals",
      x = stratum_label,
      y = "P(condition recorded)"
    ) +
    ggplot2::theme_bw(base_size = 10) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 30, hjust = 1),
      strip.background = ggplot2::element_rect(fill = "grey92", colour = NA)
    )
}

fig_condition_recording_by_age <- plot_recording_by_stratum(
  recording_by_age,
  "age_group",
  "age group (years)"
)

fig_condition_recording_by_sex <- plot_recording_by_stratum(
  recording_by_sex,
  "sex_group",
  "sex"
)

fig_condition_recording_by_year_region <- ggplot2::ggplot(
  recording_by_year_region,
  ggplot2::aes(
    x = year,
    y = recording_probability,
    colour = region,
    group = region
  )
) +
  ggplot2::geom_line(linewidth = 0.6) +
  ggplot2::geom_point(size = 1.5) +
  ggplot2::facet_wrap(~ condition, ncol = 4) +
  ggplot2::scale_y_continuous(
    limits = c(0, 1),
    labels = scales::label_percent(accuracy = 1)
  ) +
  ggplot2::labs(
    title = "Condition-field recording by notification year and Brazilian region",
    subtitle = "Observed probabilities among confirmed chikungunya cases",
    x = "Year",
    y = "P(condition recorded)",
    colour = "Region"
  ) +
  ggplot2::theme_bw(base_size = 10) +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    strip.background = ggplot2::element_rect(fill = "grey92", colour = NA),
    legend.position = "bottom"
  )

fig_condition_recording_by_state_year <- ggplot2::ggplot(
  recording_by_state_year,
  ggplot2::aes(x = year, y = state, fill = recording_probability)
) +
  ggplot2::geom_tile(colour = "white", linewidth = 0.1) +
  ggplot2::facet_wrap(~ condition, ncol = 4) +
  ggplot2::scale_fill_viridis_c(
    limits = c(0, 1),
    labels = scales::label_percent(accuracy = 1),
    name = "P(recorded)"
  ) +
  ggplot2::labs(
    title = "Condition-field recording by state and year",
    subtitle = "State is based on residence municipality, falling back to notification municipality",
    x = "Year",
    y = "State"
  ) +
  ggplot2::theme_minimal(base_size = 9) +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(face = "bold"),
    legend.position = "bottom"
  )

ggplot2::ggsave(
  file.path(fig_dir, "fig_condition_recording_by_outcome.jpg"),
  fig_condition_recording_by_outcome,
  width = 10,
  height = 8,
  units = "in",
  dpi = 300
)

ggplot2::ggsave(
  file.path(fig_dir, "fig_condition_recording_by_age.jpg"),
  fig_condition_recording_by_age,
  width = 11,
  height = 7,
  units = "in",
  dpi = 300
)

ggplot2::ggsave(
  file.path(fig_dir, "fig_condition_recording_by_sex.jpg"),
  fig_condition_recording_by_sex,
  width = 11,
  height = 7,
  units = "in",
  dpi = 300
)

ggplot2::ggsave(
  file.path(fig_dir, "fig_condition_recording_by_year_region.jpg"),
  fig_condition_recording_by_year_region,
  width = 11,
  height = 7,
  units = "in",
  dpi = 300
)

ggplot2::ggsave(
  file.path(fig_dir, "fig_condition_recording_by_state_year.jpg"),
  fig_condition_recording_by_state_year,
  width = 13,
  height = 10,
  units = "in",
  dpi = 300
)

save(
  recording_overall,
  recording_by_hospitalisation,
  recording_by_death,
  recording_by_age,
  recording_by_sex,
  recording_by_year,
  recording_by_region,
  recording_by_state,
  recording_by_year_region,
  recording_by_state_year,
  file = "01_Data/condition_recording_summaries.RData"
)

recording_tables <- list(
  overall = recording_overall,
  hospitalisation = recording_by_hospitalisation,
  death = recording_by_death,
  age = recording_by_age,
  sex = recording_by_sex,
  year = recording_by_year,
  region = recording_by_region,
  state = recording_by_state,
  year_region = recording_by_year_region,
  state_year = recording_by_state_year
)

wb <- openxlsx::createWorkbook()

purrr::iwalk(
  recording_tables,
  function(x, sheet_name) {
    openxlsx::addWorksheet(wb, sheet_name)
    openxlsx::writeDataTable(wb, sheet_name, x, withFilter = TRUE)
    openxlsx::freezePane(wb, sheet_name, firstRow = TRUE)
    openxlsx::setColWidths(wb, sheet_name, cols = "auto", widths = "auto")
  }
)

openxlsx::saveWorkbook(
  wb,
  file = file.path(table_dir, "condition_recording_summaries.xlsx"),
  overwrite = TRUE
)

message("[14] Saved condition-recording summaries and figures.")
