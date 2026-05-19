ind <- readRDS("01_Data/chik_sinan_individual_2015_2024.rds")

# 1. 전체 구조
str(ind, max.level = 1)
View(head(ind, 50))

# 2. 한 환자가 어떻게 생겼는지
ind[1, ] |> as.list()

# 3. 분석 대상 (2017-2024 confirmed) 만들기
library(dplyr); library(lubridate)
conf <- ind |> filter(is_confirmed_chik, year(event_date) >= 2017)

# 4. comorbidity별 인구학 분포
conf |> count(has_any_comorbidity, sex)
conf |> group_by(has_any_comorbidity) |>
  summarise(n = n(),
            median_age = median(age_years, na.rm = TRUE),
            pct_female = mean(sex == "female", na.rm = TRUE),
            hosp_pct = mean(hospitalised == "yes", na.rm = TRUE),
            death_pct = mean(died_from_chik))

# 5. cross-tabulation 예시
table(conf$hypertension, conf$hospitalised, useNA = "ifany")

# 6. 임산부 분석
conf |> filter(is_pregnant) |>
  count(pregnancy_state, hospitalised)

# 7. age × hypertension joint distribution
conf |> mutate(age_grp = cut(age_years, c(0,18,40,60,80,Inf))) |>
  count(age_grp, hypertension)

# age specific cormobidity
ind_age <- ind |>
  filter(
    is_confirmed_chik,
    year(event_date) >= 2017,
    !is.na(age_years),
    age_years >=0, age_years <=100
  ) |>
  mutate(
    age_bin = cut(
      age_years,
      breaks = seq(0, 100, by = 10),
      include.lowest = TRUE,
      right = FALSE
    )
  )

# long format of all types of comorbidity 
comorb_cols <- c("diabetes", "hypertension", "hepatopathy",
                 "renal_disease", "hematologic", "peptic_ulcer",
                 "autoimmune")

long <- ind_age |>
  select(age_bin, all_of(comorb_cols)) |>
  pivot_longer(
    cols = all_of(comorb_cols),
    names_to = "comorbidity",
    values_to = "status"
  )

prop <- long |>
  filter(status %in% c("yes", "no")) |>
  group_by(age_bin, comorbidity) |>
  summarise(
    n_known = n(),
    n_yes = sum(status == "yes"),
    pct = n_yes / n_known * 100,
    .groups = "drop"
  )

p <-
  ggplot(prop, aes(x = age_bin, y = pct,
         color = comorbidity, group = comorbidity))+
  geom_line(linewidth = 0.8)+
  geom_point(size = 1.5)+
  labs(
    title = "% comorbidity by age group",
    x = "Age (Years)",
    y = "% of confirmed chik cases with comorbidity",
    color = "Comorbidity"
  ) +
  theme_bw() 
  
###### 
cohort <- ind |>
  filter(
    is_confirmed_chik,                      # 확진
    year(event_date) >= 2017,               # comorbidity reporting 시작
    !is.na(age_years), age_years <= 100,    # 유효 연령
    sex %in% c("male", "female")            # 명확한 성별 (선택)
  ) |>
  mutate(
    # 분석 편의 변수들도 한 번에 만들어 cohort에 넣어두기
    year        = year(event_date),
    age_group5  = cut(age_years, seq(0, 100, 5),  right = FALSE, include.lowest = TRUE),
    age_group10 = cut(age_years, seq(0, 100, 10), right = FALSE, include.lowest = TRUE),
    age_clin    = cut(age_years,
                      c(0, 1, 5, 18, 40, 60, 80, Inf),
                      right = FALSE, include.lowest = TRUE,
                      labels = c("<1","1-4","5-17","18-39","40-59","60-79","80+")),
    region      = case_when(
      substr(coalesce(muni_residence6, muni_notif6), 1, 1) == "1" ~ "Norte",
      substr(coalesce(muni_residence6, muni_notif6), 1, 1) == "2" ~ "Nordeste",
      substr(coalesce(muni_residence6, muni_notif6), 1, 1) == "3" ~ "Sudeste",
      substr(coalesce(muni_residence6, muni_notif6), 1, 1) == "4" ~ "Sul",
      substr(coalesce(muni_residence6, muni_notif6), 1, 1) == "5" ~ "Centro-Oeste",
      TRUE ~ NA_character_
    )
  )

summary_df <- cohort |>
  filter(
    hospitalised %in% c("no", "yes"),
    has_any_comorbidity %in% c("no", "yes"),
  ) |>
  group_by(age_clin, has_any_comorbidity) |>
  summarise(
    n = n(),
    n_hosp = sum(hospitalised == "yes"),
    n_death = sum(died_from_chik == "TRUE"),
    n_chronic  = sum(clinical_form == "chronic"),
    pct_hosp = n_hosp / n * 100,
    pct_death = n_death / n * 100,
    pct_chronic = n_chronic / n * 100,  
    .groups = "drop"
  )

plot_df <- summary_df |>
  pivot_longer(
    cols = c(pct_hosp, pct_death),
    names_to = "outcome",
    values_to = "pct"
  ) |>
  mutate(
    outcome = dplyr::recode(
      outcome,
      pct_hosp = "Hospitalisation",
      pct_death = "Death",
      pct_chronic = "Chronic chik"
    )
  )

ggplot(plot_df, aes(x = age_clin, y = pct, fill = has_any_comorbidity)) + 
  geom_col(position = position_dodge(width = 0.7), width = 0.6) + 
  facet_wrap(~ outcome, scales = "free_y") + 
  scale_fill_manual(values = c(no = "grey70", yes = "#c0392b"),
                    name = "Any comorbidity")+
  labs(
    title = "% hospitalisation and death by age group",
    x = "Age group",
    y = "%"
  ) + 
  theme_minimal()

ggplot(summary_df,
       aes(x = age_clin,
           y = n,
           fill = has_any_comorbidity)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(
    aes(label = scales::comma(n)),
    position = position_dodge(width = 0.7),
    vjust = -0.4,
    size  = 3
  )+
  scale_y_continuous(
    trans = "log10",
    labels = scales::comma,
    expand = expansion(mult = c(0, 0.15))
  )+
  scale_fill_manual(values = c(no = "grey70", yes = "#c0392b"),
                    name = "Any comorbidity")+
  labs(
    title    = "Number of confirmed chikungunya cases by age group",
    subtitle = "Brazil, 2017-2024 (n shown on each bar)",
    x = "Age group",
    y = "N cases (log scale)"
  ) +
  theme_minimal(base_size = 12)


wide <- summary_df |>
  select(age_clin, has_any_comorbidity, pct_hosp, pct_death) |>
  pivot_wider(
    names_from = has_any_comorbidity,
    values_from = c(pct_hosp, pct_death)
  )

rd_df <- wide |>
  mutate(
    rd_hosp = pct_hosp_yes - pct_hosp_no,
    rd_death = pct_death_yes - pct_death_no
  )|>
  select(age_clin, rd_hosp, rd_death) |>
  pivot_longer(
    cols = starts_with("rd_"),
    names_to = "outcome",
    values_to = "rd"
  )|>
  mutate(
    outcome = dplyr::recode(
      outcome,
      rd_hosp = "Hospitalisation",
      rd_death = "Death"
    )
  )

ggplot(rd_df, aes(x = age_clin, y = rd, fill = rd > 0)) +
  geom_col(width = 0.7) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +   # ← 핵심: 0선
  facet_wrap(~ outcome, scales = "free_y") +
  scale_fill_manual(values = c(`TRUE` = "#c0392b", `FALSE` = "#3498db"),
                    guide = "none") +     # 색만 표시, 범례 안 넣음
  labs(
    title    = "Absolute risk difference: comorbid vs no comorbid",
    subtitle = "Confirmed chikungunya, Brazil 2017-2024 | y > 0: higher risk with comorbidity",
    x        = "Age group",
    y        = "Risk difference (percentage points)"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

burden_base <- cohort|>
  mutate(comorb_group = case_when(
    has_any_comorbidity == "yes" ~ "Any comorbidity",
    has_any_comorbidity == "no"  ~ "No comorbidity",
    TRUE ~ "Missing"
  ),
  comorb_group = factor(comorb_group, 
                        levels = c("No comorbidity",
                                   "Any comorbidity",
                                   "Missing"))
)

burden_long <- burden_base |>
  summarise(
    .by = comorb_group,
    n_cases = n(),
    n_hosp = sum(hospitalised == "yes", na.rm =  TRUE),
    n_death = sum(died_from_chik, na.rm = TRUE),
  ) |>
  pivot_longer(
    cols = starts_with("n_"),
    names_to = "outcome",
    values_to = "n"
  ) |>
  mutate(
    outcome = dplyr::recode(
      outcome,
      n_cases = "Cases",
      n_hosp   = "Hospitalisations",
      n_death  = "Deaths"
    ),
    outcome = factor(outcome, levels = c("Cases", "Hospitalisations", "Deaths"))
  )

burden_long <- burden_long |>
  group_by(outcome) |>
  mutate(pct = n / sum(n) * 100)|>
  ungroup()

ggplot(burden_long,
       aes(x = pct, y = outcome, fill = comorb_group)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(round(pct), "%")),
            position = position_stack(vjust = 0.5),
            color = "white", size = 3.5, fontface = "bold") +
  scale_fill_manual(
    values = c(`No comorbidity`  = "grey70",
               `Any comorbidity` = "#c0392b",
               `Missing`         = "grey40"),
    name = NULL
  ) +
  scale_x_continuous(labels = scales::percent_format(scale = 1),
                     expand = c(0, 0)) +
  labs(
    title    = "Burden concentration of chikungunya by comorbidity status",
    subtitle = "Confirmed cases, Brazil 2017-2024",
    x        = "Share of total within each outcome",
    y        = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.y = element_blank(),
        legend.position    = "top")


burden_age <- burden_base |>
  summarise(
    .by = c(age_clin, comorb_group),               # ← age_clin 추가
    n_cases  = n(),
    n_hosp   = sum(hospitalised == "yes", na.rm = TRUE),
    n_death  = sum(died_from_chik,         na.rm = TRUE)
  ) |>
  pivot_longer(
    cols      = starts_with("n_"),
    names_to  = "outcome",
    values_to = "n"
  ) |>
  mutate(
    outcome = dplyr::recode(outcome,
                            n_cases = "Cases", n_hosp = "Hospitalisations", n_death = "Deaths"),
    outcome = factor(outcome, levels = c("Cases", "Hospitalisations", "Deaths"))
  ) |>
  group_by(age_clin, outcome) |>                          # 어느 두 변수가 분모 그룹?
  mutate(pct = n / sum(n) * 100) |>
  ungroup()

ggplot(burden_age,
       aes(x = pct, y = age_clin, fill = comorb_group)) +
  geom_col(width = 0.75) +
  facet_wrap(~ outcome, nrow = 1) +
  scale_fill_manual(
    values = c(`No comorbidity`  = "grey70",
               `Any comorbidity` = "#c0392b",
               `Missing`         = "grey40"),
    name = NULL
  ) +
  scale_x_continuous(labels = scales::percent_format(scale = 1),
                     expand = c(0, 0),
                     breaks = c(0, 25, 50, 75, 100)) +
  labs(
    title    = "Share of cases, hospitalisations, deaths from each comorbidity group",
    subtitle = "Confirmed chikungunya, Brazil 2017-2024",
    x = "Share within each age group and outcome",
    y = "Age group"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.y = element_blank(),
        legend.position    = "top")


# 1. 합병증 수별 cases와 deaths 집계
lorenz_base <- cohort |>
  filter(!is.na(n_comorbidities)) |>           # known만
  count(n_comorbidities, name = "n_cases") |>
  left_join(
    cohort |>
      filter(died_from_chik) |>
      count(n_comorbidities, name = "n_deaths"),
    by = "n_comorbidities"
  ) |>
  mutate(n_deaths = tidyr::replace_na(n_deaths, 0L)) |>
  arrange(n_comorbidities)                      # 0, 1, 2, 3+

# 2. 누적 분포 계산
lorenz <- lorenz_base |>
  mutate(
    cum_pop    = cumsum(n_cases)  / sum(n_cases)  * 100,
    cum_deaths = cumsum(n_deaths) / sum(n_deaths) * 100
  ) |>
  # 시작점 (0,0) 추가
  bind_rows(tibble(n_comorbidities = -1, n_cases = 0, n_deaths = 0,
                   cum_pop = 0, cum_deaths = 0)) |>
  arrange(cum_pop)

# 3. plot
ggplot(lorenz, aes(cum_pop, cum_deaths)) +
  # 대각선 (equality reference)
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", color = "grey50") +
  # Lorenz curve
  geom_line(linewidth = 1, color = "#c0392b") +
  geom_point(size = 3, color = "#c0392b") +
  geom_text(aes(label = paste0("n=", n_comorbidities, " comorb")),
            vjust = -1, size = 3) +
  scale_x_continuous(labels = scales::percent_format(scale = 1),
                     breaks = seq(0, 100, 25), limits = c(0, 100)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1),
                     breaks = seq(0, 100, 25), limits = c(0, 100)) +
  labs(
    title    = "Lorenz curve: concentration of chikungunya deaths",
    subtitle = "Population ordered by number of comorbidities (0, 1, 2, 3+)",
    x = "Cumulative share of cases",
    y = "Cumulative share of deaths"
  ) +
  coord_equal() +
  theme_minimal(base_size = 12)


ratio_df <- burden_long |>      # age 합쳐진 원본 (전체용)
  group_by(comorb_group) |>
  mutate(
    baseline = pct[outcome == "Cases"],     # 어떤 outcome이 기준?
    ratio    = pct / baseline
  ) |>
  ungroup()

ggplot(ratio_df,
       aes(x = outcome, y = ratio,
           color = comorb_group, group = comorb_group)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey50") +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_text(aes(label = sprintf("%.1fx", ratio)),
            vjust = -0.8, size = 3.5,
            show.legend = FALSE) +
  scale_color_manual(values = c(`No comorbidity`  = "grey50",
                                `Any comorbidity` = "#c0392b",
                                `Missing`         = "grey70"),
                     name = NULL) +
  labs(
    title    = "Over-representation of comorbid cases in severe outcomes",
    subtitle = "Ratio = (% in outcome) / (% in cases). 1.0 = no signal; >1 = over-represented",
    x        = NULL,
    y        = "Over-representation ratio"
  ) +
  theme_minimal(base_size = 12)