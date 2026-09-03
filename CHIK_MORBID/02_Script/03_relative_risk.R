## 0. Output directories ######################################################

fig_dir <- "03_Output/figures"

if (!dir.exists(fig_dir)) {
  dir.create(fig_dir, recursive = TRUE)
}

age_band_levels <- c(
  "[0,10)",
  "[10,20)",
  "[20,30)",
  "[30,40)",
  "[40,50)",
  "[50,60)",
  "[60,70)",
  "[70,80)",
  "[80,+)"
)

age_band_labels <- c(
  "0–9",
  "10–19",
  "20–29",
  "30–39",
  "40–49",
  "50–59",
  "60–69",
  "70–79",
  "80+"
)

theme_lancet_clean <- function(base_size = 12, base_family = "") {
  ggplot2::theme_classic(
    base_size = base_size,
    base_family = base_family
  ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        size = base_size + 2,
        face = "bold",
        hjust = 0,
        margin = ggplot2::margin(b = 6)
      ),
      
      plot.subtitle = ggplot2::element_text(
        size = base_size,
        colour = "grey30",
        hjust = 0,
        lineheight = 1.1,
        margin = ggplot2::margin(b = 12)
      ),
      
      axis.title = ggplot2::element_text(
        size = base_size,
        colour = "black"
      ),
      
      axis.title.x = ggplot2::element_text(
        margin = ggplot2::margin(t = 8)
      ),
      
      axis.title.y = ggplot2::element_text(
        margin = ggplot2::margin(r = 8)
      ),
      
      axis.text = ggplot2::element_text(
        size = base_size - 1,
        colour = "black"
      ),
      
      axis.line = ggplot2::element_line(
        colour = "black",
        linewidth = 0.5
      ),
      
      axis.ticks = ggplot2::element_line(
        colour = "black",
        linewidth = 0.4
      ),
      
      axis.ticks.length = grid::unit(
        2.5,
        "mm"
      ),
      
      panel.grid = ggplot2::element_blank(),
      
      strip.background = ggplot2::element_blank(),
      
      strip.text = ggplot2::element_text(
        size = base_size,
        face = "bold",
        hjust = 0
      ),
      
      legend.position = "bottom",
      
      legend.title = ggplot2::element_text(
        size = base_size - 1,
        face = "bold"
      ),
      
      legend.text = ggplot2::element_text(
        size = base_size - 1
      ),
      
      legend.key = ggplot2::element_blank(),
      
      plot.margin = ggplot2::margin(
        t = 10,
        r = 12,
        b = 10,
        l = 10
      )
    )
}
## 1. Load data ###############################################################
## Packages are loaded by 02_Script/00_setup.R - run that first.

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
      n_comorbid_model >= 2 ~ "2+",
      TRUE ~ NA_character_
    ),
    comorb_count_group = factor(
      comorb_count_group,
      levels = c("0", "1", "2+")
    )
  )

analysis_df <- analysis_df |>
  mutate(
    sex = factor(sex, levels = c("female", "male"))
  )

# observed hospitalisation
observed_hosp_5yr <- analysis_df |>
  filter(
    !is.na(hosp_only),
    !is.na(age_years),
    age_years >= 0,
    age_years < 90,
    !is.na(comorb_count_group),
    !is.na(sex)
  ) |>
  mutate(
    age_start_5yr =
      floor(age_years / 5) * 5,
    
    age_mid_5yr =
      age_start_5yr + 2.5
  ) |>
  group_by(
    age_start_5yr,
    age_mid_5yr,
    comorb_count_group,
    sex
  ) |>
  summarise(
    n = n(),
    hospitalisations = sum(
      hosp_only,
      na.rm = TRUE
    ),
    
    observed_risk =
      hospitalisations / n,
    
    .groups = "drop"
  )

observed_death_5yr <- analysis_df |>
  dplyr::filter(
    !is.na(age_years),
    age_years >= 0,
    age_years <= 100,
    !is.na(comorb_count_group),
    !is.na(sex),
    !is.na(death_only)
  ) |>
  dplyr::mutate(
    ## The final category represents age 95+
    age_start_5yr = pmin(
      floor(age_years / 5) * 5,
      95
    ),
    
    age_mid_5yr = dplyr::if_else(
      age_start_5yr == 95,
      97.5,
      age_start_5yr + 2.5
    )
  ) |>
  dplyr::group_by(
    age_start_5yr,
    age_mid_5yr,
    comorb_count_group,
    sex
  ) |>
  dplyr::summarise(
    cases = dplyr::n(),
    deaths = sum(death_only == 1),
    observed_risk = deaths / cases,
    
    ## Jeffreys binomial interval:
    ## works even when the observed number of deaths is zero
    observed_lower = stats::qbeta(
      0.025,
      deaths + 0.5,
      cases - deaths + 0.5
    ),
    
    observed_upper = stats::qbeta(
      0.975,
      deaths + 0.5,
      cases - deaths + 0.5
    ),
    
    .groups = "drop"
  )

## 1-1. completeness check 
## does comorbid information more frequently recorded among hospitalised patients?
## and if so, does that difference vary by region and time? 
missingness_df <- ind |>
  filter(
    is_confirmed_chik,
    year(event_date) >= 2017,
    !is.na(age_years),
    age_years >= 0,
    age_years <= 100,
    sex %in% c("male", "female"),
    hospitalised %in% c("no", "yes")
  ) |>
  mutate(
    year = year(event_date),
    
    n_comorb_known = rowSums(
      across(
        all_of(comorb_cols),
        ~ .x %in% c("no", "yes")
      )
    ),
    
    n_comorb_missing = length(comorb_cols) - n_comorb_known,
    
    complete_comorb = n_comorb_known == length(comorb_cols),
    
    hospitalised = factor(
      hospitalised,
      levels = c("no", "yes"),
      labels = c("Not hospitalised", "Hospitalised")
    )
  )

overall_completeness <- missingness_df |>
  group_by(hospitalised) |>
  summarise(
    n_patients = n(),
    complete_n = sum(complete_comorb),
    complete_percent = 100 * mean(complete_comorb),
    mean_variables_known = mean(n_comorb_known),
    median_variables_known = median(n_comorb_known),
    .groups = "drop"
  )

overall_completeness

completeness_by_year <- missingness_df |>
  group_by(year, hospitalised) |>
  summarise(
    n = n(),
    complete_percent = 100 * mean(complete_comorb),
    mean_variables_known = mean(n_comorb_known),
    .groups = "drop"
  )


## 2. Functions for count model
make_pred_burden <- function(fit_model, pred_name = "p") {
  
  pred_df <- tidyr::expand_grid(
    age_years = seq(0, 100, by = 1),
    comorb_count_group = factor(
      c("0", "1", "2+"),
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

make_pred_comorb <- function(fit_model, comorb_var, pred_name = "p") {
  
  stopifnot(comorb_var %in% names(analysis_df))
  
  pred_df <- tidyr::expand_grid(
    age_years = seq(0, 100, by = 1),
    sex = factor("female", levels = levels(analysis_df$sex)),
    comorb_status = factor(
      c("no", "yes"),
      levels = levels(analysis_df[[comorb_var]])
    )
  )
  
  names(pred_df)[names(pred_df) == "comorb_status"] <- comorb_var
  
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


make_pred_burden_both_sexes <- function(
    fit_model,
    pred_name = "p_hosp",
    B = 2000,
    seed = 1
) {
  
  set.seed(seed)
  
  sex_weights <- analysis_df |>
    count(sex, name = "n") |>
    mutate(weight = n / sum(n)) |>
    select(sex, weight)
  
  pred_df <- tidyr::expand_grid(
    age_years = 0:100,
    
    comorb_count_group = factor(
      c("0", "1", "2+"),
      levels = levels(analysis_df$comorb_count_group)
    ),
    
    sex = factor(
      levels(analysis_df$sex),
      levels = levels(analysis_df$sex)
    )
  ) |>
    left_join(
      sex_weights,
      by = "sex"
    )
  
  X <- model.matrix(
    delete.response(terms(fit_model)),
    data = pred_df,
    contrasts.arg = fit_model$contrasts
  )
  
  beta_draws <- MASS::mvrnorm(
    n = B,
    mu = coef(fit_model),
    Sigma = vcov(fit_model)
  )
  
  p_draws <- plogis(
    X %*% t(beta_draws)
  )
  
  groups <- split(
    seq_len(nrow(pred_df)),
    interaction(
      pred_df$age_years,
      pred_df$comorb_count_group,
      drop = TRUE
    )
  )
  
  pred_summary <- lapply(
    groups,
    function(idx) {
      
      # Average the male/female predicted probabilities, weighted by the
      # observed sex ratio in the data
      p_standardised <- colSums(
        sweep(
          p_draws[idx, , drop = FALSE],
          MARGIN = 1,
          STATS = pred_df$weight[idx],
          FUN = "*"
        )
      )
      
      tibble(
        age_years =
          pred_df$age_years[idx[1]],
        
        comorb_count_group =
          pred_df$comorb_count_group[idx[1]],
        
        p = median(p_standardised),
        
        lower = quantile(
          p_standardised,
          0.025
        ),
        
        upper = quantile(
          p_standardised,
          0.975
        )
      )
    }
  ) |>
    bind_rows()
  
  names(pred_summary)[
    names(pred_summary) == "p"
  ] <- pred_name
  
  pred_summary
}

## 3. Model fit
## fit_death_burden (GLM, no interaction) is kept because it is used later
## for the absolute predicted-CFR plot (section 5-1). 
fit_death_burden <- glm(
  death_only ~ ns(age_years, df = 4) +
    comorb_count_group +
    sex,
  data = analysis_df,
  family = binomial()
)

fit_hosp_logit_interaction <- glm(
  hosp_only ~
    ns(age_years, df = 4) * comorb_count_group +
    sex,
  data = analysis_df,
  family = binomial()
)

## GAM: note that the coefficient here is not directly RR
fit_hosp_gam <- gam(
  hosp_only ~
    comorb_count_group +
    sex +
    s(
      age_years,
      by = comorb_count_group,
      k = 6,
      bs = "ts", # penalty (to penalise extra complexity)
      id = 1
    ),
  data = analysis_df,
  family = binomial(link = "logit"),
  method = "REML"
)

## Model selection for the death GAM: compare a smooth-age-only model
## against one that lets the age smooth vary by comorbidity group, by AIC.
fit_death_gam_by_comorb <- mgcv::gam(
  death_only ~
    comorb_count_group +
    sex +
    s(
      age_years,
      by = comorb_count_group,
      k = 8,
      bs = "ts",
      id = 1
    ),
  family = binomial(link = "logit"),
  method = "REML",
  data = analysis_df,
  na.action = na.exclude
)

summary(fit_death_gam_by_comorb)

mgcv::gam.check(fit_death_gam_by_comorb)

fit_death_gam_simple_k15 <- mgcv::gam(
  death_only ~
    comorb_count_group +
    sex +
    s(
      age_years,
      k = 15,
      bs = "ts"
    ),
  family = binomial(link = "logit"),
  method = "REML",
  data = analysis_df,
  na.action = na.exclude
)

AIC(
  fit_death_gam_simple_k15,
  fit_death_gam_by_comorb
)

# model selection: the simpler smooth-age-only model (lower AIC) is chosen
fit_death_gam <- fit_death_gam_simple_k15

## GAM predicted curves for hosp
pred_hosp_grid <- tidyr::expand_grid(
  age_years = seq(0, 89, by = 0.5),
  
  comorb_count_group = factor(
    levels(analysis_df$comorb_count_group),
    levels = levels(analysis_df$comorb_count_group)
  ),
  
  sex = factor(
    levels(analysis_df$sex),
    levels = levels(analysis_df$sex)
  )
)

pred_hosp_link <- predict(
  fit_hosp_gam,
  newdata = pred_hosp_grid,
  type = "link",
  se.fit = TRUE,
  unconditional = TRUE
)

pred_hosp_grid <- pred_hosp_grid |>
  mutate(
    predicted_risk = plogis(
      pred_hosp_link$fit
    ),
    
    lower = plogis(
      pred_hosp_link$fit -
        1.96 * pred_hosp_link$se.fit
    ),
    
    upper = plogis(
      pred_hosp_link$fit +
        1.96 * pred_hosp_link$se.fit
    )
  )

## plots: predicted + observed
p_hosp_absolute_risk <- ggplot(
  pred_hosp_grid,
  aes(
    x = age_years,
    y = predicted_risk * 100,
    colour = comorb_count_group,
    fill = comorb_count_group
  )
) +
  geom_ribbon(
    aes(
      ymin = lower * 100,
      ymax = upper * 100
    ),
    alpha = 0.12,
    colour = NA
  ) +
  geom_line(
    linewidth = 1
  ) +
  geom_point(
    data = observed_hosp_5yr,
    aes(
      x = age_mid_5yr,
      y = observed_risk * 100,
      colour = comorb_count_group
    ),
    inherit.aes = FALSE,
    size = 1.8,
    alpha = 0.65
  ) +
  facet_wrap(
    ~ sex,
    ncol = 1
  ) +
  scale_x_continuous(
    breaks = seq(0, 90, by = 10)
  ) +
  labs(
    x = "Age, years",
    y = "Hospitalisation probability (%)",
    colour = "Number of comorbidities",
    fill = "Number of comorbidities",
    title = "Age-specific hospitalisation probability",
    subtitle = paste(
      "Lines: penalised GAM predictions;",
      "points: observed rates in 5-year age groups"
    )
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "bottom"
  )

p_hosp_absolute_risk

ggsave(
  filename = file.path(fig_dir, "fig_absolute_risk_hosp.jpg"),
  plot = p_hosp_absolute_risk,
  width = 8,
  height = 6,
  units = "in",
  dpi = 300
)

## GAM prediction vs. observed death prob graph
## ============================================================
## GAM-predicted death probabilities
## ============================================================

pred_death_grid <- tidyr::expand_grid(
  age_years = seq(
    0,
    100,
    by = 0.5
  ),
  
  comorb_count_group = factor(
    levels(analysis_df$comorb_count_group),
    levels = levels(analysis_df$comorb_count_group)
  ),
  
  sex = factor(
    levels(analysis_df$sex),
    levels = levels(analysis_df$sex)
  )
)

pred_death_link <- predict(
  fit_death_gam,
  newdata = pred_death_grid,
  type = "link",
  se.fit = TRUE,
  unconditional = TRUE
)

pred_death_grid <- pred_death_grid |>
  dplyr::mutate(
    predicted_risk = plogis(
      pred_death_link$fit
    ),
    
    lower = plogis(
      pred_death_link$fit -
        1.96 * pred_death_link$se.fit
    ),
    
    upper = plogis(
      pred_death_link$fit +
        1.96 * pred_death_link$se.fit
    )
  )

## ============================================================
## Observed and GAM-predicted death probability
## ============================================================
comorb_colours <- c(
  "0" = "#4C78A8",
  "1" = "#F2A541",
  "2+" = "#C95D63"
)

p_death_absolute_risk <- ggplot2::ggplot(
  pred_death_grid,
  ggplot2::aes(
    x = age_years,
    y = predicted_risk * 100,
    colour = comorb_count_group,
    fill = comorb_count_group
  )
) +
  ggplot2::geom_ribbon(
    ggplot2::aes(
      ymin = lower * 100,
      ymax = upper * 100
    ),
    alpha = 0.12,
    colour = NA
  ) +
  ggplot2::geom_line(
    linewidth = 1
  ) +
  ggplot2::geom_errorbar(
    data = observed_death_5yr,
    ggplot2::aes(
      x = age_mid_5yr,
      ymin = observed_lower * 100,
      ymax = observed_upper * 100,
      colour = comorb_count_group
    ),
    inherit.aes = FALSE,
    width = 0,
    linewidth = 0.45,
    alpha = 0.55
  ) +
  ggplot2::geom_point(
    data = observed_death_5yr,
    ggplot2::aes(
      x = age_mid_5yr,
      y = observed_risk * 100,
      colour = comorb_count_group
    ),
    inherit.aes = FALSE,
    size = 1.8,
    alpha = 0.75
  ) +
  ggplot2::facet_wrap(
    ~ sex,
    ncol = 1
  ) +
  ggplot2::scale_x_continuous(
    breaks = seq(
      0,
      100,
      by = 10
    )
  ) +
  ggplot2::scale_y_continuous(
    labels = scales::label_number(
      accuracy = 0.01
    ),
    expand = ggplot2::expansion(
      mult = c(0, 0.08)
    )
  ) +
  ggplot2::scale_colour_manual(
    values = comorb_colours,
    drop = FALSE
  ) +
  ggplot2::scale_fill_manual(
    values = comorb_colours,
    drop = FALSE
  ) +
  ggplot2::labs(
    x = "Age, years",
    y = "Death probability (%)",
    colour = "Number of comorbidities",
    fill = "Number of comorbidities",
    title = "Age-specific probability of death",
    subtitle = paste(
      "Lines and ribbons: penalised GAM predictions and 95% CIs;",
      "points and error bars: observed rates and 95% binomial intervals"
    )
  ) +
  theme_lancet_clean() +
  ggplot2::theme(
    legend.position = "bottom"
  )

p_death_absolute_risk

ggsave(
  filename = file.path(fig_dir, "fig_absolute_risk_death.jpg"),
  plot = p_death_absolute_risk,
  width = 8,
  height = 7,
  units = "in",
  dpi = 300
)


# RR of hosp
comorb_levels <- levels(
  analysis_df$comorb_count_group
)

sex_levels <- levels(
  analysis_df$sex
)

max_age_for_standardisation <- 89

hosp_standard_grid <- tidyr::expand_grid(
  age_years = 0:max_age_for_standardisation,
  
  sex = factor(
    sex_levels,
    levels = sex_levels
  ),
  
  comorb_count_group = factor(
    comorb_levels,
    levels = comorb_levels
  )
) |>
  dplyr::mutate(
    rr_age_group = cut(
      age_years,
      breaks = c(
        0, 10, 20, 30, 40,
        50, 60, 70, 80, Inf
      ),
      right = FALSE,
      labels = age_band_levels
    ),
    
    rr_age_group = factor(
      rr_age_group,
      levels = age_band_levels,
      ordered = TRUE
    )
  )

## Point-estimate-only standardisation used to live here (hosp_age_sex_standardised,
## hosp_ageband_standardised, hosp_reference_risk, rr_hosp_gam_table). It was removed:
## the parametric-bootstrap version immediately below recomputes the same
## standardised risks per draw and derives point estimate + 95% CI from the draws,
## making the point-estimate-only version fully redundant.

### 95% uncertainty intervals via parametric bootstrap
set.seed(123)

B <- 2000

X_hosp <- predict(
  fit_hosp_gam,
  newdata = hosp_standard_grid,
  type = "lpmatrix"
)

beta_hat <- coef(
  fit_hosp_gam
)

V_beta <- vcov(
  fit_hosp_gam,
  unconditional = TRUE
)

beta_draws <- MASS::mvrnorm(
  n = B,
  mu = beta_hat,
  Sigma = V_beta
)


eta_draws <- X_hosp %*% t(beta_draws)

risk_draws <- plogis(
  eta_draws
)

cell_meta <- hosp_standard_grid |>
  distinct(
    rr_age_group,
    comorb_count_group
  ) |>
  arrange(
    rr_age_group,
    comorb_count_group
  )

hosp_risk_ageband_draws <- lapply(
  seq_len(nrow(cell_meta)),
  function(i) {
    
    current_age_group <-
      cell_meta$rr_age_group[i]
    
    current_comorb_group <-
      cell_meta$comorb_count_group[i]
    
    idx <- which(
      hosp_standard_grid$rr_age_group ==
        current_age_group &
        hosp_standard_grid$comorb_count_group ==
        current_comorb_group
    )
    
    if (length(idx) == 0) {
      stop(
        "No prediction-grid rows found for age/comorbidity cell."
      )
    }
    
    standardised_risk <- colMeans(
      risk_draws[idx, , drop = FALSE]
    )
    
    tibble(
      draw = seq_len(B),
      
      rr_age_group =
        current_age_group,
      
      comorb_count_group =
        current_comorb_group,
      
      standardised_hosp_risk =
        standardised_risk
    )
  }
) |>
  bind_rows()

hosp_reference_draws <- hosp_risk_ageband_draws |>
  filter(
    as.character(comorb_count_group) == "0"
  ) |>
  transmute(
    draw,
    rr_age_group,
    reference_risk =
      standardised_hosp_risk
  )

rr_hosp_gam_draws <- hosp_risk_ageband_draws |>
  left_join(
    hosp_reference_draws,
    by = c(
      "draw",
      "rr_age_group"
    ),
    relationship = "many-to-one"
  ) |>
  mutate(
    rr_hosp =
      standardised_hosp_risk /
      reference_risk
  )


rr_hosp_gam_summary <- rr_hosp_gam_draws |>
  group_by(
    rr_age_group,
    comorb_count_group
  ) |>
  summarise(
    risk_median = median(
      standardised_hosp_risk
    ),
    
    risk_lower = quantile(
      standardised_hosp_risk,
      probs = 0.025,
      names = FALSE
    ),
    
    risk_upper = quantile(
      standardised_hosp_risk,
      probs = 0.975,
      names = FALSE
    ),
    
    rr_median = median(
      rr_hosp
    ),
    
    rr_lower = quantile(
      rr_hosp,
      probs = 0.025,
      names = FALSE
    ),
    
    rr_upper = quantile(
      rr_hosp,
      probs = 0.975,
      names = FALSE
    ),
    
    rr_sd = sd(
      rr_hosp
    ),
    
    .groups = "drop"
  ) |>
  mutate(
    comorbidity = case_when(
      as.character(comorb_count_group) == "0" ~ "0",
      as.character(comorb_count_group) == "1" ~ "1",
      as.character(comorb_count_group) %in%
        c("2", "2+") ~ "2+",
      TRUE ~ NA_character_
    ),
    
    comorbidity = factor(
      comorbidity,
      levels = c("0", "1", "2+")
    )
  ) |>
  arrange(
    rr_age_group,
    comorbidity
  )

rr_hosp_plot_data <- rr_hosp_gam_summary |>
  filter(
    comorbidity != "0"
  ) |>
  mutate(
    age_group_plot = factor(
      rr_age_group,
      levels = age_band_levels,
      labels = age_band_labels,
      ordered = TRUE
    ),
    
    comorbidity = factor(
      comorbidity,
      levels = c("1", "2+"),
      labels = c(
        "1 comorbidity",
        "2+ comorbidities"
      )
    )
  )

comorb_colours_rr <- c(
  "1 comorbidity" = "#D55E00",
  "2+ comorbidities" = "#0072B2"
)

comorb_shapes_rr <- c(
  "1 comorbidity" = 16,
  "2+ comorbidities" = 17
)

p_rr_hosp_gam <- ggplot(
  rr_hosp_plot_data,
  aes(
    x = age_group_plot,
    y = rr_median,
    colour = comorbidity,
    shape = comorbidity,
    group = comorbidity
  )
) +
  geom_hline(
    yintercept = 1,
    linetype = "dashed",
    linewidth = 0.45,
    colour = "grey40"
  ) +
  geom_line(
    linewidth = 0.75
  ) +
  geom_errorbar(
    aes(
      ymin = rr_lower,
      ymax = rr_upper
    ),
    width = 0.10,
    linewidth = 0.75
  ) +
  geom_point(
    size = 2.6,
    stroke = 0.3
  ) +
  scale_colour_manual(
    values = comorb_colours_rr
  ) +
  scale_shape_manual(
    values = comorb_shapes_rr
  ) +
  scale_y_continuous(
    breaks = seq(
      1,
      ceiling(
        max(
          rr_hosp_plot_data$rr_upper,
          na.rm = TRUE
        )
      ),
      by = 0.5
    ),
    expand = expansion(
      mult = c(0.02, 0.08)
    )
  ) +
  labs(
    title =
      "Age-specific relative risk of chikungunya hospitalisation",
    
    subtitle = paste0(
      "Penalised GAM estimates with model-based 95% confidence ",
      "intervals; standardised to 50% female and 50% male"
    ),
    
    x = "Age group (years)",
    
    y = paste0(
      "Relative risk of hospitalisation\n",
      "(vs 0 recorded comorbidities)"
    ),
    
    colour = NULL,
    shape = NULL
  ) +
  theme_classic(
    base_size = 11,
    base_family = "Arial"
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 12,
      margin = margin(b = 4)
    ),
    
    plot.subtitle = element_text(
      size = 10.5,
      margin = margin(b = 10)
    ),
    
    axis.title = element_text(
      size = 11,
      colour = "black"
    ),
    
    axis.text = element_text(
      size = 10,
      colour = "black"
    ),
    
    axis.line = element_line(
      linewidth = 0.45,
      colour = "black"
    ),
    
    axis.ticks = element_line(
      linewidth = 0.4,
      colour = "black"
    ),
    
    panel.grid.major.y = element_line(
      colour = "grey88",
      linewidth = 0.3
    ),
    
    panel.grid.major.x =
      element_blank(),
    
    panel.grid.minor =
      element_blank(),
    
    legend.position = "top",
    
    legend.justification = "left",
    
    legend.box.just = "left",
    
    legend.key.width =
      grid::unit(1.3, "lines"),
    
    plot.margin =
      margin(8, 12, 8, 8)
  )

p_rr_hosp_gam

ggsave(
  filename = file.path(fig_dir, "fig_rr_hosp_gam.jpg"),
  plot = p_rr_hosp_gam,
  width = 8,
  height = 5,
  units = "in",
  dpi = 300
)

## ============================================================
## Age-specific death RR from the penalised GAM
## ============================================================
## Same approach as the hospitalisation RR above: build a standardisation
## grid, then go straight to the parametric bootstrap (below) rather than
## also computing a redundant point-estimate-only version first.

comorb_levels <- levels(
  droplevels(analysis_df$comorb_count_group)
)

sex_levels <- levels(
  droplevels(analysis_df$sex)
)

## Age band 80-89 (not 80-100): kept consistent with the burden framework
## used for hospitalisation above. Change max_age_for_standardisation to
## 100 if the intended estimand is genuinely "80 and over".
max_age_for_standardisation <- 89

death_standard_grid <- tidyr::expand_grid(
  age_years = 0:max_age_for_standardisation,

  sex = factor(
    sex_levels,
    levels = sex_levels
  ),

  comorb_count_group = factor(
    comorb_levels,
    levels = comorb_levels
  )
) |>
  dplyr::mutate(
    rr_age_group = cut(
      age_years,
      breaks = c(
        0, 10, 20, 30, 40,
        50, 60, 70, 80, Inf
      ),
      right = FALSE,
      labels = age_band_levels
    ),

    rr_age_group = factor(
      rr_age_group,
      levels = age_band_levels,
      ordered = TRUE
    )
  )

## ============================================================
## Create coefficient draws and predicted death-risk draws
## ============================================================

set.seed(123)

B <- 2000

## 1. Linear-predictor matrix for the standardisation grid
X_death <- predict(
  fit_death_gam,
  newdata = death_standard_grid,
  type = "lpmatrix"
)

## 2. Estimated GAM coefficients
beta_hat_death <- coef(
  fit_death_gam
)

## 3. Unconditional covariance matrix
V_beta_death <- vcov(
  fit_death_gam,
  unconditional = TRUE
)

## 4. Draw GAM coefficients
beta_draws_death <- MASS::mvrnorm(
  n = B,
  mu = beta_hat_death,
  Sigma = V_beta_death
)

## 5. Linear predictor for every grid row and every draw
eta_draws_death <- X_death %*% t(
  beta_draws_death
)

## 6. Convert linear predictors to death probabilities
risk_draws_death <- plogis(
  eta_draws_death
)

death_cell_meta <- death_standard_grid |>
  dplyr::distinct(
    rr_age_group,
    comorb_count_group
  ) |>
  dplyr::arrange(
    rr_age_group,
    comorb_count_group
  )

death_risk_ageband_draws <- lapply(
  seq_len(nrow(death_cell_meta)),
  
  function(i) {
    
    current_age_group <-
      death_cell_meta$rr_age_group[i]
    
    current_comorb_group <-
      death_cell_meta$comorb_count_group[i]
    
    idx <- which(
      death_standard_grid$rr_age_group ==
        current_age_group &
        death_standard_grid$comorb_count_group ==
        current_comorb_group
    )
    
    if (length(idx) == 0) {
      stop(
        "No prediction-grid rows found for age/comorbidity cell."
      )
    }
    
    standardised_risk <- colMeans(
      risk_draws_death[
        idx,
        ,
        drop = FALSE
      ]
    )
    
    tibble::tibble(
      draw = seq_len(B),
      rr_age_group = current_age_group,
      comorb_count_group = current_comorb_group,
      standardised_death_risk = standardised_risk
    )
  }
) |>
  dplyr::bind_rows()


death_reference_draws <- death_risk_ageband_draws |>
  dplyr::filter(as.character(comorb_count_group) == "0") |>
  dplyr::transmute(
    draw,
    rr_age_group,
    reference_risk = standardised_death_risk
  )

rr_death_gam_draws <- death_risk_ageband_draws |>
  dplyr::left_join(
    death_reference_draws,
    by = c("draw", "rr_age_group"),
    relationship = "many-to-one"
  ) |>
  dplyr::mutate(
    rr_death = standardised_death_risk / reference_risk
  )
rr_death_gam_summary <- rr_death_gam_draws |>
  dplyr::group_by(rr_age_group, comorb_count_group) |>
  dplyr::summarise(
    risk_median = median(standardised_death_risk),
    risk_lower = quantile(
      standardised_death_risk, 0.025, names = FALSE
    ),
    risk_upper = quantile(
      standardised_death_risk, 0.975, names = FALSE
    ),
    rr_median = median(rr_death),
    rr_lower = quantile(rr_death, 0.025, names = FALSE),
    rr_upper = quantile(rr_death, 0.975, names = FALSE),
    rr_sd = sd(rr_death),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    comorbidity = factor(
      as.character(comorb_count_group),
      levels = c("0", "1", "2+")
    )
  ) |>
  dplyr::arrange(rr_age_group, comorbidity)

print(rr_death_gam_summary, n = Inf)

rr_death_plot_data <- rr_death_gam_summary |>
  dplyr::filter(comorbidity != "0") |>
  dplyr::mutate(
    age_group_plot = factor(
      rr_age_group,
      levels = age_band_levels,
      labels = age_band_labels,
      ordered = TRUE
    ),
    comorbidity = factor(
      comorbidity,
      levels = c("1", "2+"),
      labels = c("1 comorbidity", "2+ comorbidities")
    )
  )

p_rr_death_gam <- ggplot2::ggplot(
  rr_death_plot_data,
  ggplot2::aes(
    x = age_group_plot,
    y = rr_median,
    colour = comorbidity,
    shape = comorbidity,
    group = comorbidity
  )
) +
  ggplot2::geom_hline(
    yintercept = 1,
    linetype = "dashed",
    linewidth = 0.45,
    colour = "grey40"
  ) +
  ggplot2::geom_line(linewidth = 0.75) +
  ggplot2::geom_errorbar(
    ggplot2::aes(ymin = rr_lower, ymax = rr_upper),
    width = 0.10,
    linewidth = 0.75
  ) +
  ggplot2::geom_point(size = 2.6) +
  ggplot2::scale_colour_manual(values = comorb_colours_rr) +
  ggplot2::scale_shape_manual(values = comorb_shapes_rr) +
  ggplot2::scale_y_continuous(
    breaks = seq(
      1,
      ceiling(max(rr_death_plot_data$rr_upper, na.rm = TRUE)),
      by = 0.5
    ),
    expand = ggplot2::expansion(mult = c(0.02, 0.08))
  ) +
  ggplot2::labs(
    title = "Age-specific relative risk of chikungunya death",
    subtitle = paste0(
      "Penalised GAM estimates with model-based 95% confidence ",
      "intervals; standardised to 50% female and 50% male"
    ),
    x = "Age group (years)",
    y = paste0(
      "Relative risk of death\n",
      "(vs 0 recorded comorbidities)"
    ),
    colour = NULL,
    shape = NULL
  ) +
  ggplot2::theme_classic(base_size = 11) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      face = "bold", size = 12,
      margin = ggplot2::margin(b = 4)
    ),
    plot.subtitle = ggplot2::element_text(
      size = 10.5,
      margin = ggplot2::margin(b = 10)
    ),
    axis.text = ggplot2::element_text(colour = "black"),
    panel.grid.major.y = ggplot2::element_line(
      colour = "grey88", linewidth = 0.3
    ),
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    legend.position = "top",
    legend.justification = "left",
    plot.margin = ggplot2::margin(8, 12, 8, 8)
  )

p_rr_death_gam

ggsave(
  filename = file.path(fig_dir, "fig_rr_death_gam.jpg"),
  plot = p_rr_death_gam,
  width = 8,
  height = 5,
  units = "in",
  dpi = 300
)

### table
age_order <- c(
  "[0,10)",
  "[10,20)",
  "[20,30)",
  "[30,40)",
  "[40,50)",
  "[50,60)",
  "[60,70)",
  "[70,80)",
  "[80,+)"
)

comorb_order <- c(
  "0",
  "1",
  "2+"
)

rr_hosp_model <- rr_hosp_gam_summary |>
  transmute(
    rr_age_group =
      as.character(rr_age_group),
    
    comorbidity =
      as.character(comorbidity),
    
    rr_hosp =
      as.numeric(rr_median),
    
    rr_hosp_lo =
      as.numeric(rr_lower),
    
    rr_hosp_hi =
      as.numeric(rr_upper)
  ) |>
  mutate(
    rr_age_group = factor(
      rr_age_group,
      levels = age_order,
      ordered = TRUE
    ),
    
    comorbidity = factor(
      comorbidity,
      levels = comorb_order
    )
  ) |>
  arrange(
    comorbidity,
    rr_age_group
  ) |>
  mutate(
    rr_age_group =
      as.character(rr_age_group),
    
    comorbidity =
      as.character(comorbidity)
  )


### table (reuses age_order / comorb_order defined just above for rr_hosp_model)
rr_death_model <- rr_death_gam_summary |>
  transmute(
    rr_age_group =
      as.character(rr_age_group),
    
    comorbidity =
      as.character(comorbidity),
    
    rr_death =
      as.numeric(rr_median),
    
    rr_death_lo =
      as.numeric(rr_lower),
    
    rr_death_hi =
      as.numeric(rr_upper)
  ) |>
  mutate(
    rr_age_group = factor(
      rr_age_group,
      levels = age_order,
      ordered = TRUE
    ),
    
    comorbidity = factor(
      comorbidity,
      levels = comorb_order
    )
  ) |>
  arrange(
    comorbidity,
    rr_age_group
  ) |>
  mutate(
    rr_age_group =
      as.character(rr_age_group),
    
    comorbidity =
      as.character(comorbidity)
  )


save(rr_hosp_model, file = "01_Data/rr_hosp_model.RData")
save(rr_death_model, file = "01_Data/rr_death_model.RData")



# 3-1. Individual disease models for probability prediction
fit_hosp_by_comorb <- setNames(
  lapply(
    comorb_cols,
    function(col) {
      glm(
        reformulate(
          c("ns(age_years, df = 4)", "sex", col),
          response = "hosp_only"
        ),
        data = analysis_df,
        family = binomial()
      )
    }
  ),
  comorb_cols
)

fit_death_by_comorb <- setNames(
  lapply(
    comorb_cols,
    function(col) {
      glm(
        reformulate(
          c("ns(age_years, df = 4)", "sex", col),
          response = "death_only"
        ),
        data = analysis_df,
        family = binomial()
      )
    }
  ),
  comorb_cols
)

pred_hosp_by_comorb <- lapply(
  names(fit_hosp_by_comorb),
  function(col) {
    make_pred_comorb(
      fit_model = fit_hosp_by_comorb[[col]],
      comorb_var = col,
      pred_name = paste0("p_hosp_", col)
    ) |>
      mutate(comorb_var = col)
  }
)

pred_death_by_comorb <- lapply(
  names(fit_death_by_comorb),
  function(col) {
    make_pred_comorb(
      fit_model = fit_death_by_comorb[[col]],
      comorb_var = col,
      pred_name = paste0("p_death_", col)
    ) |>
      mutate(comorb_var = col)
  }
)

pred_hosp_by_comorb <- dplyr::bind_rows(pred_hosp_by_comorb)
pred_death_by_comorb <- dplyr::bind_rows(pred_death_by_comorb)

# 3-2. Disease-specific probability plots (faceted)
pred_hosp_by_comorb <- pred_hosp_by_comorb |>
  mutate(
    comorb_status = dplyr::coalesce(!!!rlang::syms(comorb_cols)),
    p_hosp = dplyr::coalesce(!!!rlang::syms(paste0("p_hosp_", comorb_cols))),
    comorb_var = factor(comorb_var, levels = comorb_cols),
    comorb_status = factor(comorb_status, levels = c("no", "yes"))
  )

pred_death_by_comorb <- pred_death_by_comorb |>
  mutate(
    comorb_status = dplyr::coalesce(!!!rlang::syms(comorb_cols)),
    p_death = dplyr::coalesce(!!!rlang::syms(paste0("p_death_", comorb_cols))),
    comorb_var = factor(comorb_var, levels = comorb_cols),
    comorb_status = factor(comorb_status, levels = c("no", "yes"))
  )

fig_hosp_by_comorb <- ggplot(
  pred_hosp_by_comorb,
  aes(
    x = age_years,
    y = p_hosp,
    color = comorb_status,
    fill = comorb_status,
    group = comorb_status
  )
) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper),
    alpha = 0.15,
    color = NA
  ) +
  geom_line(linewidth = 1) +
  facet_wrap(~ comorb_var,  ncol = 2) +
  labs(
    x = "Age",
    y = "Predicted probability of hospitalisation",
    color = "Disease status",
    fill = "Disease status"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(fig_dir, "fig_predicted_hosp_prob_by_comorb.jpg"),
  plot = fig_hosp_by_comorb,
  width = 10,
  height = 8,
  units = "in",
  dpi = 300
)

fig_hosp_by_comorb

fig_death_by_comorb <- ggplot(
  pred_death_by_comorb,
  aes(
    x = age_years,
    y = p_death,
    color = comorb_status,
    fill = comorb_status,
    group = comorb_status
  )
) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper),
    alpha = 0.15,
    color = NA
  ) +
  geom_line(linewidth = 1) +
  facet_wrap(~ comorb_var,  ncol = 2) +
  labs(
    x = "Age",
    y = "Predicted probability of death",
    color = "Disease status",
    fill = "Disease status"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(fig_dir, "fig_predicted_death_prob_by_comorb.jpg"),
  plot = fig_death_by_comorb,
  width = 10,
  height = 8,
  units = "in",
  dpi = 300
)

# 3-3. Relative risk by individual comorbidity
## Same age-sex-standardised parametric-bootstrap approach as the
## comorb_count_group RR above (draw coefficients -> plogis -> sex-weighted
## average -> ratio), applied here to each single-condition GLM in turn
## (fit_hosp_by_comorb / fit_death_by_comorb, section 3-1). Those models have
## no age x condition interaction, so within each disease the RR can only
## vary with age through the logistic link's non-linearity, not through an
## explicit interaction term - i.e. these curves are expected to be smoother
## / closer to constant than the comorb_count_group RR above, by design.
make_rr_comorb <- function(fit_model, comorb_var, B = 2000, seed = 1) {

  set.seed(seed)

  sex_weights <- analysis_df |>
    count(sex, name = "n") |>
    mutate(weight = n / sum(n)) |>
    select(sex, weight)

  pred_df <- tidyr::expand_grid(
    age_years = 0:100,
    sex = factor(levels(analysis_df$sex), levels = levels(analysis_df$sex)),
    comorb_status = factor(c("no", "yes"), levels = levels(analysis_df[[comorb_var]]))
  ) |>
    left_join(sex_weights, by = "sex")

  names(pred_df)[names(pred_df) == "comorb_status"] <- comorb_var

  X <- model.matrix(
    delete.response(terms(fit_model)),
    data = pred_df,
    contrasts.arg = fit_model$contrasts
  )

  beta_draws <- MASS::mvrnorm(n = B, mu = coef(fit_model), Sigma = vcov(fit_model))
  p_draws <- plogis(X %*% t(beta_draws))

  status_vec <- pred_df[[comorb_var]]

  purrr::map_dfr(
    split(seq_len(nrow(pred_df)), pred_df$age_years),
    function(idx) {

      idx_no  <- idx[status_vec[idx] == "no"]
      idx_yes <- idx[status_vec[idx] == "yes"]

      # sex-standardise: weight each sex's predicted probability by its
      # observed share of analysis_df, then sum
      p_no_draws  <- colSums(p_draws[idx_no,  , drop = FALSE] * pred_df$weight[idx_no])
      p_yes_draws <- colSums(p_draws[idx_yes, , drop = FALSE] * pred_df$weight[idx_yes])

      rr_draws <- p_yes_draws / p_no_draws

      tibble(
        age_years = pred_df$age_years[idx[1]],
        rr = median(rr_draws),
        lower = quantile(rr_draws, 0.025, names = FALSE),
        upper = quantile(rr_draws, 0.975, names = FALSE)
      )
    }
  ) |>
    arrange(age_years)
}

rr_hosp_by_comorb <- purrr::map_dfr(
  comorb_cols,
  function(col) {
    make_rr_comorb(fit_hosp_by_comorb[[col]], comorb_var = col) |>
      mutate(comorb_var = factor(col, levels = comorb_cols))
  }
)

rr_death_by_comorb <- purrr::map_dfr(
  comorb_cols,
  function(col) {
    make_rr_comorb(fit_death_by_comorb[[col]], comorb_var = col) |>
      mutate(comorb_var = factor(col, levels = comorb_cols))
  }
)

fig_rr_hosp_by_comorb <- ggplot(
  rr_hosp_by_comorb,
  aes(x = age_years, y = rr)
) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey40") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "#c0392b") +
  geom_line(colour = "#c0392b", linewidth = 0.9) +
  facet_wrap(~ comorb_var, ncol = 2) +
  labs(
    x = "Age, years",
    y = "Relative risk of hospitalisation\n(condition present vs absent)",
    title = "Age-specific relative risk of hospitalisation by individual comorbidity"
  ) +
  theme_bw(base_size = 11)

ggsave(
  filename = file.path(fig_dir, "fig_rr_hosp_by_comorb.jpg"),
  plot = fig_rr_hosp_by_comorb,
  width = 10,
  height = 8,
  units = "in",
  dpi = 300
)

fig_rr_death_by_comorb <- ggplot(
  rr_death_by_comorb,
  aes(x = age_years, y = rr)
) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey40") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "#c0392b") +
  geom_line(colour = "#c0392b", linewidth = 0.9) +
  facet_wrap(~ comorb_var, ncol = 2) +
  labs(
    x = "Age, years",
    y = "Relative risk of death\n(condition present vs absent)",
    title = "Age-specific relative risk of death by individual comorbidity"
  ) +
  theme_bw(base_size = 11)

ggsave(
  filename = file.path(fig_dir, "fig_rr_death_by_comorb.jpg"),
  plot = fig_rr_death_by_comorb,
  width = 10,
  height = 8,
  units = "in",
  dpi = 300
)

# 4. Predict
## Uses the age x comorbidity interaction model, sex-standardised via
## parametric bootstrap (see make_pred_burden_both_sexes() above).
pred_hosp_burden <- make_pred_burden_both_sexes(
  fit_model = fit_hosp_logit_interaction,
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

# 6. Relative risk (official result)
## The age-sex-standardised, GAM-based parametric-bootstrap RR computed above
## (rr_hosp_gam_summary / rr_death_gam_summary -> rr_hosp_model / rr_death_model,
## saved to 01_Data/rr_hosp_model.RData and 01_Data/rr_death_model.RData) is the
## single official RR result for this analysis.
##
## A second, GLM-based relative-risk pass used to live here (a continuous-age
## version, an age-band version, and a broken GAM "interaction" re-implementation
## that used exp() instead of plogis() on a logit-link model, so its output was
## not valid probabilities). None of it fed any additional saved output, and its
## final step silently overwrote rr_hosp_model.RData / rr_death_model.RData with
## the buggy GLM result. It has been removed; the GAM-based result above is now
## the only place these two files get written.


## crude rr
get_crude_rr <- function(df, age_max, ref_group = "0", comp_group) {
  sub <- df |>
    dplyr::filter(age_years < age_max) |>
    dplyr::filter(!is.na(death_only), !is.na(comorb_count_group))
  
  a  <- sum(sub$comorb_count_group == comp_group & sub$death_only)  # event, exposed
  n1 <- sum(sub$comorb_count_group == comp_group)                   # total exposed
  c  <- sum(sub$comorb_count_group == ref_group  & sub$death_only)  # event, ref
  n0 <- sum(sub$comorb_count_group == ref_group)                    # total ref
  
  p1 <- a / n1
  p0 <- c / n0
  rr <- p1 / p0
  
  # Wald SE on the log scale (same formula as epitools' method = "wald")
  se_log_rr <- sqrt(1/a - 1/n1 + 1/c - 1/n0)
  lower <- exp(log(rr) - 1.96 * se_log_rr)
  upper <- exp(log(rr) + 1.96 * se_log_rr)

  tibble::tibble(
    age_band = paste0("[0,", age_max, ")"),
    comorb_count_group = comp_group,
    rr = rr, lower = lower, upper = upper,
    n_events_exposed = a, n_total_exposed = n1,
    n_events_ref = c, n_total_ref = n0
  )
}

crude_rr_young <- dplyr::bind_rows(
  get_crude_rr(analysis_df, age_max = 30, comp_group = "1"),
  get_crude_rr(analysis_df, age_max = 30, comp_group = "2+")
)

crude_rr_young


