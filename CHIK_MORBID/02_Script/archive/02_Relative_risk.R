### Comorbidity by each condition


# 1. load packages and data
# 2. Define analysis cohort
# 3. Define severe outcome
# 4. Prepare comorbidity vars.
# 5. Fit main risk model 
# 6. Predicted risks by age and condition
# 7. Plots and tables 

################################################################################
## 1. load packages and data ####################################################

library(splines)
ind <- readRDS("01_Data/chik_sinan_individual_2015_2024.rds")

## 2. 

## 3. 

## 4. 

## 5. 
comorb_cols <- c(
  "diabetes",
  "hypertension",
  "hepatopathy",
  "renal_disease",
  "hematologic",
  "peptic_ulcer",
  "autoimmune"
)


analysis_df <- ind |>
  filter(
    is_confirmed_chik,
    year(event_date) >=2017,
    !is.na(age_years),
    age_years >=0,
    age_years <=100,
    sex %in% c("male", "female"),
    hospitalised %in% c("no", "yes")
  )

analysis_df <- analysis_df |>
  mutate(
    severe = hospitalised == "yes" | died_from_chik == TRUE
  )

analysis_df <- analysis_df |>
  filter(
    if_all(all_of(comorb_cols), ~ .x %in% c("no", "yes"))
  ) |>
  mutate(
    across(
      all_of(comorb_cols),
      ~factor(.x, levels = c("no", "yes"))
    )
  )


analysis_df <- analysis_df |>
  mutate(
    n_comorbid_model = rowSums(
      across(all_of(comorb_cols), ~ .x == "yes")
    ),
    multimorbid = n_comorbid_model >= 2
  )

analysis_df <- analysis_df |>
  mutate(
    sex = factor(sex, levels = c("female", "male"))
  )


## fit condition only (without any age interaction)
fit_condition_only <- glm(
  severe ~ ns(age_years, df = 4) + 
    diabetes + hypertension + hepatopathy + renal_disease +
    hematologic + peptic_ulcer + autoimmune +
    sex,
  data = analysis_df,
  family = binomial()
)

make_pred_one_condition <- function(condition_name, fit_model) {
  
  pred_df <- tidyr::expand_grid(
    age_years = seq(0, 100, by = 1),
    condition_status = factor(c("no", "yes"), levels = c("no", "yes")),
    sex = factor("female", levels = levels(analysis_df$sex))
  )
  
  for (v in comorb_cols) {
    pred_df[[v]] <- factor("no", levels = c("no", "yes"))
  }
  
  pred_df[[condition_name]] <- pred_df$condition_status
  
  pred_df <- pred_df |>
    mutate(
      condition = condition_name
    )
  
  pred_link <- predict(
    fit_model,
    newdata = pred_df,
    type = "link",
    se.fit = TRUE
  )
  
  pred_df <- pred_df |>
    mutate(
      eta = pred_link$fit,
      se_eta = pred_link$se.fit,
      p_severe = plogis(eta),
      lower = plogis(eta - 1.96 * se_eta),
      upper = plogis(eta + 1.96 * se_eta)
    )
  
  pred_df
}

pred_all <- purrr::map_dfr(
  comorb_cols,
  make_pred_one_condition,
  fit_model = fit_condition_only
)

ggplot(
  pred_all,
  aes(
    x = age_years,
    y = p_severe,
    color = condition_status,
    fill = condition_status,
    group = condition_status
  )
) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper),
    alpha = 0.25,
    color = NA
  ) +
  geom_line(linewidth = 1) +
  facet_wrap(~ condition) +
  labs(
    x = "Age",
    y = "Predicted probability of severe outcome",
    color = "Condition status",
    fill  = "Condition status"
  ) +
  theme_minimal()

pred_all_severe <- purrr::map_dfr(
  comorb_cols,
  make_pred_one_condition
)

pred_all_severe <- pred_all_severe |>
  mutate(
    p_severe = predict(fit, newdata = pred_all_severe, type = "response")
  )

rr_all_severe <- pred_all_severe |>
  select(condition, age_years, condition_status, p_severe) |>
  tidyr::pivot_wider(
    names_from = condition_status,
    values_from = p_severe
  ) |>
  mutate(
    rr = yes/no,
    risk_difference = yes - no
  )


ggplot(rr_all_severe, aes(x = age_years, y = condition, fill = rr)) +
  geom_tile()+
  geom_contour(
    aes(z = rr),
    color = "white",
    linewidth = 0.3,
    bins = 6
  ) + 
  scale_fill_viridis_c(
    option = "magma",
    name   = "RR"
  )+
  labs(
    x = "Age",
    y = "Condition",
    title = "Age-specific relative risk surface"
  )+
  theme_minimal()

#################################################################################

cfr_by_condition <- analysis_df |>
  select(died_from_chik, all_of(comorb_cols)) |>
  tidyr::pivot_longer(
    cols = all_of(comorb_cols),
    names_to = "condition",
    values_to = "condition_status"
  ) |>
  group_by(condition, condition_status) |>
  summarise(
    n_cases = n(),
    n_deaths = sum(died_from_chik, na.rm = TRUE),
    cfr = n_deaths / n_cases,
    .groups = "drop"
  )


fit_cfr_condition <- glm(
  died_from_chik ~ ns(age_years, df = 3) +
    diabetes + hypertension + hepatopathy + renal_disease +
    hematologic + peptic_ulcer + autoimmune +
    sex,
  data = analysis_df,
  family = binomial()
)

fit_cfr_burden <- glm(
  died_from_chik ~ ns(age_years, df = 3) +
    comorb_count_group +
    sex,
  data = analysis_df,
  family = binomial()
)

make_pred_one_condition_cfr <- function(condition_name, fit_model) {
  
  pred_df <- tidyr::expand_grid(
    age_years = seq(0, 100, by = 1),
    condition_status = factor(c("no", "yes"), levels = c("no", "yes")),
    sex = factor("female", levels = levels(analysis_df$sex))
  )
  
  # Set all comorbidities to no
  for (v in comorb_cols) {
    pred_df[[v]] <- factor("no", levels = c("no", "yes"))
  }
  
  # Turn one condition on/off
  pred_df[[condition_name]] <- pred_df$condition_status
  
  # Because this is one-condition-at-a-time prediction,
  # multimorbid should be FALSE
  pred_df <- pred_df |>
    mutate(
      condition = condition_name,
      multimorbid = FALSE
    )
  
  pred_link <- predict(
    fit_model,
    newdata = pred_df,
    type = "link",
    se.fit = TRUE
  )
  
  pred_df <- pred_df |>
    mutate(
      eta = pred_link$fit,
      se_eta = pred_link$se.fit,
      p_cfr = plogis(eta),
      lower = plogis(eta - 1.96 * se_eta),
      upper = plogis(eta + 1.96 * se_eta)
    )
  
  pred_df
}

make_pred_cfr_burden <- function(fit_model) {
  
  pred_df <- tidyr::expand_grid(
    age_years = seq(0, 100, by = 1),
    comorb_count_group = factor(
      c("0", "1", "2", "3+"),
      levels = levels(analysis_df$comorb_count_group)
    ),
    sex = factor("female", levels = levels(analysis_df$sex))
  )
  
  pred_link <- predict(
    fit_model,
    newdata = pred_df,
    type = "link",
    se.fit = TRUE
  )
  
  pred_df <- pred_df |>
    mutate(
      eta = pred_link$fit,
      se_eta = pred_link$se.fit,
      p_cfr = plogis(eta),
      lower = plogis(eta - 1.96 * se_eta),
      upper = plogis(eta + 1.96 * se_eta)
    )
  
  pred_df
}

pred_all_cfr <- purrr::map_dfr(
  comorb_cols,
  make_pred_one_condition_cfr,
  fit_model = fit_cfr_condition
)

pred_cfr_burden <- make_pred_cfr_burden(fit_cfr_burden)

## condition specific cfr
ggplot(
  pred_all_cfr,
  aes(
    x = age_years,
    y = p_cfr,
    color = condition_status,
    fill = condition_status,
    group = condition_status
  )
) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper),
    alpha = 0.20,
    color = NA
  ) +
  geom_line(linewidth = 1) +
  facet_wrap(~ condition, scales = "free_y") +
  labs(
    x = "Age",
    y = "Predicted CFR",
    color = "Condition status",
    fill = "Condition status"
  ) +
  theme_minimal()


### burden specific cfr
ggplot(
  pred_cfr_burden,
  aes(
    x = age_years,
    y = p_cfr,
    color = comorb_count_group,
    fill = comorb_count_group,
    group = comorb_count_group
  )
) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper),
    alpha = 0.20,
    color = NA
  ) +
  geom_line(linewidth = 1) +
  labs(
    x = "Age",
    y = "Predicted CFR",
    color = "Number of comorbidities",
    fill = "Number of comorbidities"
  ) +
  theme_minimal()


rr_all_cfr <- pred_all_cfr |>
  select(condition, age_years, condition_status, p_cfr) |>
  tidyr::pivot_wider(
    names_from = condition_status,
    values_from = p_cfr
  ) |>
  mutate(
    rr = yes / no,
    risk_difference = yes - no
  )

ggplot(rr_all_cfr, aes(x = age_years, y = rr)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey50") +
  geom_line(linewidth = 1, color = "#c0392b") +
  facet_wrap(~ condition) +
  labs(
    x = "Age",
    y = "CFR ratio: yes vs no",
    title = "Model-based relative CFR by condition"
  ) +
  theme_minimal()

#################################################################################
analysis_df <- analysis_df |>
  mutate(
    comorb_count_group = case_when(
      n_comorbid_model == 0 ~ "0",
      n_comorbid_model == 1 ~ "1",
      n_comorbid_model == 2 ~ "2",
      n_comorbid_model >= 3 ~ "3+"
    ),
    comorb_count_group = factor(
      comorb_count_group,
      levels = c("0", "1", "2", "3+")
    )
  )

severe_by_count <- analysis_df |>
  group_by(comorb_count_group) |>
  summarise(
    n_cases = n(),
    n_severe = sum(severe, na.rm = TRUE),
    severe_risk = n_severe / n_cases,
    .groups = "drop"
  )
cfr_by_count <- analysis_df |>
  group_by(comorb_count_group) |>
  summarise(
    n_cases = n(),
    n_deaths = sum(died_from_chik, na.rm = TRUE),
    cfr = n_deaths / n_cases,
    .groups = "drop"
  )


#main model
fit_count_main <- glm(
  severe ~ ns(age_years, df = 4) +
    comorb_count_group +
    sex,
  data = analysis_df,
  family = binomial()
)


#age interaction model
fit_count_age <- glm(
  severe ~ ns(age_years, df = 4) * comorb_count_group +
    sex,
  data = analysis_df,
  family = binomial()
)

pred_count_severe <- tidyr::expand_grid(
  age_years = seq(0, 100, by = 1),
  comorb_count_group = factor(c("0", "1", "2", "3+"),
                              levels = c("0", "1", "2", "3+")),
  sex = factor("female", levels = levels(analysis_df$sex))
)

pred_count_severe <- pred_count_severe |>
  mutate(
    p_severe = predict(
      #fit_count_age,
      fit_count_main,
      newdata = pred_count_severe,
      type = "response"
    )
  )

ggplot(pred_count_severe,
       aes(x = age_years, y = p_severe, color = comorb_count_group)) +
  geom_line(linewidth = 1) +
  labs(
    x = "Age",
    y = "Predicted probability of severe outcome",
    color = "Number of comorbidities"
  ) +
  theme_minimal()


