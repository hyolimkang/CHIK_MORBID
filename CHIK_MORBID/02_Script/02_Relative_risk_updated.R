## 0. Output directories ######################################################

fig_dir <- "03_Output/figures"

if (!dir.exists(fig_dir)) {
  dir.create(fig_dir, recursive = TRUE)
}
## 1. load packages and data ####################################################

library(splines)
library(MASS)
ind <- readRDS("01_Data/chik_sinan_individual_2015_2024.rds")

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
    hosp_only  = hospitalised == "yes",
    death_only = died_from_chik == TRUE
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

analysis_df <- analysis_df |>
  mutate(
    sex = factor(sex, levels = c("female", "male"))
  )

## 2. Functions for count model
make_pred_burden <- function(fit_model, pred_name = "p") {
  
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
      p = plogis(eta),
      lower = plogis(eta - 1.96 * se_eta),
      upper = plogis(eta + 1.96 * se_eta)
    )
  
  names(pred_df)[names(pred_df) == "p"] <- pred_name
  
  pred_df
}

## 3. Model fit 
fit_hosp_burden <- glm(
  hosp_only ~ ns(age_years, df = 4) +
    comorb_count_group +
    sex,
  data = analysis_df,
  family = binomial()
)


fit_death_burden <- glm(
  death_only ~ ns(age_years, df = 4) +
    comorb_count_group +
    sex,
  data = analysis_df,
  family = binomial()
)

# 4. Predict
pred_hosp_burden <- make_pred_burden(
  fit_model = fit_hosp_burden,
  pred_name = "p_hosp"
)

pred_death_burden <- make_pred_burden(
  fit_model = fit_death_burden,
  pred_name = "p_death"
)

# 5. Hosp plot
fig_hosp_prob <- ggplot(
  pred_hosp_burden,
  aes(
    x = age_years,
    y = p_hosp,
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
    y = "Predicted probability of hospitalisation",
    color = "Number of comorbidities",
    fill = "Number of comorbidities"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(fig_dir, "fig_predicted_hosp_prob.jpg"),
  plot = fig_hosp_prob,
  width = 7,
  height = 5,
  units = "in",
  dpi = 300
)

# 5-1. Death plot
fig_death_prob <- ggplot(
  pred_death_burden,
  aes(
    x = age_years,
    y = p_death,
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

ggsave(
  filename = file.path(fig_dir, "fig_predicted_cfr.jpg"),
  plot = fig_death_prob,
  width = 7,
  height = 5,
  units = "in",
  dpi = 300
)

# 6. RR 
make_rr_burden <- function(
  fit_model,
  B = 1000,
  ref_group = "0",
  seed = 1
) {
  
  set.seed(seed)
  
  pred_df <- tidyr::expand_grid(
    age_years = seq(0, 100, by = 1),
    comorb_count_group = factor(
      c("0", "1", "2", "3+"),
      levels = levels(analysis_df$comorb_count_group)
    ),
    sex = factor("female", levels = levels(analysis_df$sex))
  )
  
  X <- model.matrix(
    delete.response(terms(fit_model)),
    data = pred_df,
    contrasts.arg = fit_model$contrasts
  )
  
  beta_hat <- coef(fit_model)
  V <- vcov(fit_model)
  
  beta_draws <- MASS::mvrnorm(
    n = B,
    mu = beta_hat,
    Sigma = V
  )
  
  p_draws <- plogis(X %*% t(beta_draws))
  
  rr_draws <- p_draws
  
  for (a in unique(pred_df$age_years)) {
    idx_age <- which(pred_df$age_years == a)
    idx_ref <- idx_age[pred_df$comorb_count_group[idx_age] == ref_group]
    
    rr_draws[idx_age, ] <- sweep(
      p_draws[idx_age, , drop = FALSE],
      2,
      p_draws[idx_ref, ],
      FUN = "/"
    )
  }
  
  rr_summary <- pred_df |>
    mutate(
      rr = apply(rr_draws, 1, median, na.rm = TRUE),
      lower = apply(rr_draws, 1, quantile, probs = 0.025, na.rm = TRUE),
      upper = apply(rr_draws, 1, quantile, probs = 0.975, na.rm = TRUE)
    )
  
  rr_summary
  
}


# 7. RR results run
rr_hosp_ci <- make_rr_burden(
  fit_model = fit_hosp_burden,
  B = 1000,
  ref_group = "0",
  seed = 1
)

rr_death_ci <- make_rr_burden(
  fit_model = fit_death_burden,
  B = 1000,
  ref_group = "0",
  seed = 1
)

# 8. RR plots
fig_hosp_rr <- ggplot(
  rr_hosp_ci,
  aes(
    x = age_years,
    y = rr,
    color = comorb_count_group,
    fill = comorb_count_group,
    group = comorb_count_group
  )
) +
  geom_hline(yintercept = 1, linetype = 2) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper),
    alpha = 0.15,
    color = NA
  ) +
  geom_line(linewidth = 1) +
  labs(
    x = "Age",
    y = "Relative risk of hosp",
    color = "Number of comorbidities",
    fill = "Number of comorbidities"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(fig_dir, "fig_rr_hosp.jpg"),
  plot = fig_hosp_rr,
  width = 7,
  height = 5,
  units = "in",
  dpi = 300
)

fig_death_rr <- ggplot(
  rr_death_ci,
  aes(
    x = age_years,
    y = rr,
    color = comorb_count_group,
    fill = comorb_count_group,
    group = comorb_count_group
  )
) +
  geom_hline(yintercept = 1, linetype = 2) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper),
    alpha = 0.15,
    color = NA
  ) +
  geom_line(linewidth = 1) +
  labs(
    x = "Age",
    y = "Relative risk of CFR",
    color = "Number of comorbidities",
    fill = "Number of comorbidities"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(fig_dir, "fig_rr_cfr.jpg"),
  plot = fig_death_rr,
  width = 7,
  height = 5,
  units = "in",
  dpi = 300
)
## 8-1. Forest plots for RRs
rr_hosp_plot <- rr_hosp_ci |>
  mutate(
    outcome = "Hospitalisation",
    rr_type = "RR_hosp"
  )

rr_death_plot <- rr_death_ci |>
  mutate(
    outcome = "Death / CFR",
    rr_type = "RR_death"
  )

rr_all <- bind_rows(
  rr_hosp_plot,
  rr_death_plot
)

forest_ages <- c(10, 30, 50, 70, 90)

rr_forest <- rr_all |>
  filter(
    age_years %in% forest_ages,
    comorb_count_group != "0"
  ) |>
  mutate(
    age_label = paste0("Age ", age_years),
    comorb_count_group = factor(
      comorb_count_group,
      levels = c("1", "2", "3+")
    ),
    age_label = factor(
      age_label,
      levels = paste0("Age ", forest_ages)
    )
  )

fig_rr_forest <- ggplot(
  rr_forest,
  aes(
    x = rr,
    y = comorb_count_group,
    xmin = lower,
    xmax = upper,
    color = comorb_count_group
  )
) +
  geom_vline(xintercept = 1, linetype = 2) +
  geom_errorbarh(height = 0.2, linewidth = 0.8) +
  geom_point(size = 2.5) +
  facet_grid(outcome ~ age_label, scales = "free_x") +
  scale_x_log10() +
  labs(
    x = "Relative risk compared with 0 comorbidities",
    y = "Number of comorbidities",
    color = "Number of comorbidities"
  ) +
  theme_bw()

ggsave(
  filename = file.path(fig_dir, "fig_forest_rr_selected_ages.jpg"),
  plot = fig_rr_forest,
  width = 11,
  height = 5.5,
  units = "in",
  dpi = 300
)

## 8-2. Age band RR
make_rr_ageband_burden <- function(
    fit_model,
    B = 1000,
    ref_group = "0",
    seed = 1
) {
  
  set.seed(seed)
  
  pred_df <- tidyr::expand_grid(
    age_years = seq(0, 100, by = 1),
    comorb_count_group = factor(
      c("0", "1", "2", "3+"),
      levels = levels(analysis_df$comorb_count_group)
    ),
    sex = factor("female", levels = levels(analysis_df$sex))
  ) |>
    mutate(
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
      )
    )
  
  X <- model.matrix(
    delete.response(terms(fit_model)),
    data = pred_df,
    contrasts.arg = fit_model$contrasts
  )
  
  beta_hat <- coef(fit_model)
  V <- vcov(fit_model)
  
  beta_draws <- MASS::mvrnorm(
    n = B,
    mu = beta_hat,
    Sigma = V
  )
  
  p_draws <- plogis(X %*% t(beta_draws))
  
  age_groups <- levels(pred_df$age_group)
  count_groups <- levels(pred_df$comorb_count_group)
  
  out <- list()
  
  counter <- 1
  
  for (ag in age_groups) {
    
    for (cg in count_groups) {
      
      idx <- which(
        pred_df$age_group == ag &
          pred_df$comorb_count_group == cg
      )
      
      p_band_draws <- colMeans(
        p_draws[idx, , drop = FALSE],
        na.rm = TRUE
      )
      
      out[[counter]] <- tibble::tibble(
        age_group = ag,
        comorb_count_group = cg,
        draw = seq_len(B),
        p = p_band_draws
      )
      
      counter <- counter + 1
    }
  }
  
  p_band <- dplyr::bind_rows(out) |>
    mutate(
      age_group = factor(
        age_group,
        levels = age_groups
      ),
      comorb_count_group = factor(
        comorb_count_group,
        levels = count_groups
      )
    )
  
  ref_band <- p_band |>
    filter(comorb_count_group == ref_group) |>
    dplyr::select(
      age_group,
      draw,
      p_ref = p
    )
  
  rr_band <- p_band |>
    left_join(
      ref_band,
      by = c("age_group", "draw")
    ) |>
    mutate(
      rr = p / p_ref
    ) |>
    group_by(age_group, comorb_count_group) |>
    summarise(
      rr = median(rr, na.rm = TRUE),
      lower = quantile(rr, probs = 0.025, na.rm = TRUE),
      upper = quantile(rr, probs = 0.975, na.rm = TRUE),
      p = median(p, na.rm = TRUE),
      p_lower = quantile(p, probs = 0.025, na.rm = TRUE),
      p_upper = quantile(p, probs = 0.975, na.rm = TRUE),
      .groups = "drop"
    )
  
  rr_band
}

rr_hosp_ageband <- make_rr_ageband_burden(
  fit_model = fit_hosp_burden,
  B = 1000,
  ref_group = "0",
  seed = 1
)

rr_death_ageband <- make_rr_ageband_burden(
  fit_model = fit_death_burden,
  B = 1000,
  ref_group = "0",
  seed = 1
)

rr_hosp_ageband_plot <- rr_hosp_ageband |>
  mutate(
    outcome = "Hospitalisation"
  )

rr_death_ageband_plot <- rr_death_ageband |>
  mutate(
    outcome = "Death"
  )

rr_ageband_all <- bind_rows(
  rr_hosp_ageband_plot,
  rr_death_ageband_plot
) |>
  filter(
    comorb_count_group != "0"
  ) |>
  mutate(
    comorb_count_group = factor(
      comorb_count_group,
      levels = c("1", "2", "3+"),
      labels = c("1", "2", "≥3")
    ),
    age_group = factor(
      age_group,
      levels = c("0–19", "20–39", "40–59", "60–79", "80+")
    )
  )


fig_rr_ageband_forest <- ggplot(
  rr_ageband_all,
  aes(
    x = rr,
    y = comorb_count_group,
    xmin = lower,
    xmax = upper,
    color = comorb_count_group
  )
) +
  geom_vline(
    xintercept = 1,
    linetype = 2
  ) +
  geom_errorbarh(
    height = 0.2,
    linewidth = 0.8
  ) +
  geom_point(
    size = 2.5
  ) +
  facet_grid(
    outcome ~ age_group,
    scales = "free_x"
  ) +
  scale_x_log10() +
  labs(
    x = "Relative risk compared with 0 recorded comorbidities",
    y = "Number of recorded comorbidities",
    color = "Number of recorded comorbidities"
  ) +
  theme_bw(base_size = 12)

fig_rr_ageband_forest