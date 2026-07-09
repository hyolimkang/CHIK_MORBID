## 0. Output directories ######################################################

fig_dir <- "03_Output/figures"

if (!dir.exists(fig_dir)) {
  dir.create(fig_dir, recursive = TRUE)
}
## 1. load packages and data ####################################################

## 1. load packages and data ####################################################

library(splines)
library(MASS)
library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(countrycode)
options(scipen = 999)

# 1. open raw data and clean 
ind <- readRDS("01_Data/chik_sinan_individual_2015_2024.rds")
gbd_raw <- read.csv("01_Data/gbd_prevalence.csv")
htn_raw <- read.csv("01_Data/ncd_hypertension.csv",
                    check.names = FALSE)

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

# Add 0-29 hypertension prevalence as 0
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

# Combine adult hypertension + young hypertension
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

# Combine GBD 6 conditions + hypertension
bg_prev_country <- dplyr::bind_rows(
  gbd_long,
  htn_for_merge
)

## Add pop
pop_raw <- read.csv("01_Data/gbd_pop.csv")

pop <- pop_raw |>
  filter(measure_name == "Population", metric_name == "Number") |>
  mutate(age_clean = str_remove(age_name, " years| year")) |>
  filter(age_clean %in% keep_age) |>
  left_join(age_lookup, by = "age_clean") |>       #
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

######################################################################
bg_prev_brazil <- bg_prev_country |>
  filter(iso3 == "BRA") |>
  dplyr::select(condition, age_start, age_end, prevalence)

bg_prev_brazil |> count(condition)   

analysis_df <- analysis_df |>
  mutate(.row_id = dplyr::row_number())

analysis_df <- analysis_df |>
  left_join(
    bg_prev_brazil,
    by = join_by(age_years >= age_start, age_years <= age_end)
  ) |>
  dplyr::select(-age_start, -age_end)

analysis_df <- analysis_df |>
  tidyr::pivot_wider(
    names_from   = condition,
    values_from  = prevalence,
    names_prefix = "bg_prev_"
  )

######################################################################
bridge_comorb_cols <- c(
  "diabetes",
  "hypertension",
  "hepatopathy",
  "renal_disease",
  "hematologic",
  "peptic_ulcer",
  "autoimmune"
)

gbd_prev_cols <- paste0("bg_prev_", bridge_comorb_cols)

comorb_bin_bridge <- analysis_df |>
  mutate(
    across(
      all_of(bridge_comorb_cols),
      ~ as.integer(.x == "yes")
    )
  ) |>
  dplyr::select(
    age_years,
    all_of(bridge_comorb_cols)
  ) |>
  mutate(
    age_band_corr = cut(
      age_years,
      breaks = c(0, 20, 40, 60, 80, 101),
      labels = c("0-19", "20-39", "40-59", "60-79", "80+"),
      include.lowest = TRUE,
      right = FALSE
    )
  )

corr_by_age_bridge <- comorb_bin_bridge |>
  group_by(age_band_corr) |>
  group_split() |>
  setNames(levels(comorb_bin_bridge$age_band_corr)) |>
  purrr::map(function(df) {
    cor(
      df |> dplyr::select(all_of(bridge_comorb_cols)),
      use = "pairwise.complete.obs"
    )
  })

corr_by_age_bridge[["60-79"]]

analysis_df <- analysis_df |>
  dplyr::select(
    -matches("^bg_prev_.*\\.x$"),
    -matches("^bg_prev_.*\\.y$")
  )

### population graph
theme_lancet_clean <- function(base_size = 9) {
  ggplot2::theme_classic(base_size = base_size) +
    ggplot2::theme(
      axis.line = ggplot2::element_line(linewidth = 0.35, colour = "black"),
      axis.ticks = ggplot2::element_line(linewidth = 0.35, colour = "black"),
      axis.text = ggplot2::element_text(colour = "black", size = base_size - 1),
      axis.title = ggplot2::element_text(colour = "black", size = base_size),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(
        face = "bold",
        colour = "black",
        hjust = 0,
        size = base_size
      ),
      legend.position = "top",
      legend.title = ggplot2::element_text(face = "bold", size = base_size - 1),
      legend.text = ggplot2::element_text(size = base_size - 1),
      plot.title = ggplot2::element_text(
        face = "bold",
        colour = "black",
        size = base_size + 1,
        hjust = 0
      ),
      plot.subtitle = ggplot2::element_text(
        colour = "black",
        size = base_size - 1,
        hjust = 0
      ),
      panel.spacing.y = grid::unit(0.8, "lines"),
      panel.spacing.x = grid::unit(0.8, "lines")
    )
}

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


bg_count_dist_pop <- bg_count_dist_wide |>
  dplyr::left_join(
    pop_country_age,
    by = c("iso3", "age_start", "age_end"),
    suffix = c("", "_pop")
  ) |>
  dplyr::mutate(
    country_name = dplyr::coalesce(country_name, country_name_pop)
  ) |>
  dplyr::select(
    -dplyr::any_of("country_name_pop")
  )

bg_count_dist_pop <- bg_count_dist_pop |>
  dplyr::mutate(
    pop_comorb_0     = population * prev_comorb_0,
    pop_comorb_1     = population * prev_comorb_1,
    pop_comorb_2     = population * prev_comorb_2,
    pop_comorb_3plus = population * prev_comorb_3plus
  )

count_pop_long <- bg_count_dist_pop |>
  dplyr::mutate(
    age_group = dplyr::case_when(
      is.infinite(age_end) ~ paste0(age_start, "+"),
      TRUE ~ paste0(age_start, "-", age_end)
    ),
    age_mid = dplyr::case_when(
      is.infinite(age_end) ~ 97.5,
      TRUE ~ (age_start + age_end) / 2
    )
  ) |>
  tidyr::pivot_longer(
    cols = c(
      pop_comorb_0,
      pop_comorb_1,
      pop_comorb_2,
      pop_comorb_3plus
    ),
    names_to = "comorb_count_group",
    values_to = "population_count"
  ) |>
  dplyr::mutate(
    comorb_count_group = dplyr::case_when(
      comorb_count_group == "pop_comorb_0" ~ "0",
      comorb_count_group == "pop_comorb_1" ~ "1",
      comorb_count_group == "pop_comorb_2" ~ "2",
      comorb_count_group == "pop_comorb_3plus" ~ "3+"
    ),
    comorb_count_group = factor(
      comorb_count_group,
      levels = c("0", "1", "2", "3+")
    )
  )

count_pop_long <- count_pop_long |>
  dplyr::mutate(
    region = countrycode::countrycode(
      iso3,
      origin = "iso3c",
      destination = "continent"
    )
  )

region_count_plot <- count_pop_long |>
  dplyr::filter(!is.na(region)) |>
  dplyr::group_by(
    region,
    age_start,
    age_end,
    age_mid,
    comorb_count_group
  ) |>
  dplyr::summarise(
    population_count = sum(population_count, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::group_by(
    region,
    age_start,
    age_end,
    age_mid
  ) |>
  dplyr::mutate(
    total_population = sum(population_count, na.rm = TRUE),
    population_percent = population_count / total_population
  ) |>
  dplyr::ungroup()

count_palette <- c(
  "0"  = "#BDBDBD",
  "1"  = "#FDD081",
  "2"  = "#F28E2B",
  "3+" = "#B2182B"
)

p_abs <- region_count_plot |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = age_mid,
      y = population_count / 1e6,
      fill = comorb_count_group
    )
  ) +
  ggplot2::geom_area(
    colour = "white",
    linewidth = 0.15,
    alpha = 0.98
  ) +
  ggplot2::facet_wrap(
    ~ region,
    ncol = 1,
    scales = "free_y"
  ) +
  ggplot2::scale_fill_manual(
    values = count_palette,
    name = "Comorbidity count"
  ) +
  ggplot2::scale_x_continuous(
    breaks = seq(0, 100, by = 20),
    limits = c(0, 100),
    expand = c(0, 0)
  ) +
  ggplot2::scale_y_continuous(
    expand = ggplot2::expansion(mult = c(0, 0.04))
  ) +
  ggplot2::labs(
    x = NULL,
    y = "Population, millions",
    title = "Population by comorbidity count"
  ) +
  theme_lancet_clean(base_size = 8)

ggsave(
  filename = file.path(fig_dir, "fig_region_comorb_share.jpg"),
  plot = p_abs,
  width = 7,
  height = 5.5,
  units = "in",
  dpi = 300
)


region_pct_ribbon <- region_count_plot |>
  dplyr::mutate(
    comorb_count_group = factor(
      comorb_count_group,
      levels = c("3+", "2", "1", "0")
    )
  ) |>
  dplyr::arrange(
    region,
    age_mid,
    comorb_count_group
  ) |>
  dplyr::group_by(
    region,
    age_start,
    age_end,
    age_mid
  ) |>
  dplyr::mutate(
    ymax = cumsum(population_percent),
    ymin = ymax - population_percent,
    ymin = pmax(ymin, 0),
    ymax = pmin(ymax, 1)
  ) |>
  dplyr::ungroup()

p_pct <- region_pct_ribbon |>
  dplyr::arrange(
    region,
    comorb_count_group,
    age_mid
  ) |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = age_mid,
      ymin = ymin,
      ymax = ymax,
      fill = comorb_count_group,
      group = interaction(region, comorb_count_group)
    )
  ) +
  ggplot2::geom_ribbon(
    alpha = 1,
    colour = NA
  ) +
  ggplot2::facet_wrap(
    ~ region,
    ncol = 1
  ) +
  ggplot2::scale_fill_manual(
    values = count_palette,
    breaks = c("0", "1", "2", "3+"),
    name = "Comorbidity count"
  ) +
  ggplot2::scale_x_continuous(
    breaks = seq(0, 100, by = 20),
    limits = c(0, 100),
    expand = c(0, 0)
  ) +
  ggplot2::scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = c(0, 0)
  ) +
  ggplot2::coord_cartesian(
    ylim = c(0, 1),
    clip = "on"
  ) +
  ggplot2::labs(
    x = "Age",
    y = "Population, %",
    title = "Distribution by comorbidity count"
  ) +
  theme_lancet_clean(base_size = 8) +
  ggplot2::theme(
    legend.position = "none"
  )

p_pct

ggsave(
  filename = file.path(fig_dir, "fig_region_comorb_stack.jpg"),
  plot = p_pct,
  width = 7,
  height = 5.5,
  units = "in",
  dpi = 300
)
