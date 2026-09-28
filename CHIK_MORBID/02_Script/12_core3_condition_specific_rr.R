# ---------------------------------------------------------------------------
# 12_core3_condition_specific_rr.R
#
# A second, independent cohort/analysis branch alongside 03_relative_risk.R.
#
# Why this exists (per project discussion, 2026-09):
#   03_relative_risk.R requires all 7 comorbidity fields to be known
#   (complete-case on diabetes/hypertension/hepatopathy/renal_disease/
#   hematologic/peptic_ulcer/autoimmune) before a record enters analysis_df.
#   That is far stricter than needed once the exposures of interest are
#   narrowed to three "core" conditions - diabetes (DM), hypertension (HTN),
#   and chronic kidney disease (CKD, SINAN field `renal_disease`) - and it
#   silently drops anyone with a missing liver/autoimmune/haematological/
#   peptic-ulcer value even though that value plays no role in the question
#   being asked here.
#
# Cohort rules used below:
#   - complete-case filtering applies ONLY to dm/htn/ckd. A record with
#     unknown hepatopathy/hematologic/peptic_ulcer/autoimmune status is
#     still included.
#   - having one of those 4 non-core conditions is NOT an exclusion, and it
#     does not change how a person is coded on dm/htn/ckd - e.g. a patient
#     with diabetes AND liver disease is retained and coded dm = 1, exactly
#     like a patient with diabetes alone. Those 4 fields are not used
#     anywhere in this script.
#   - the hospitalisation cohort and death cohort are built SEPARATELY, each
#     requiring only its own outcome to be known. A record with unknown
#     hospitalisation status can still contribute to the death model (and
#     vice versa) - unlike 03_relative_risk.R, which requires hospitalised
#     to be known even for the death model.
#
# "Condition-specific RR" here means: one mutually-adjusted multivariable
# model per outcome (dm + htn + ckd + age + sex, main effects only), so each
# condition's estimate already accounts for the other two - e.g. the DM
# estimate answers "how much higher is hospitalisation risk for people with
# diabetes than people without, after accounting for HTN and CKD?". This is
# different from 03_relative_risk.R section 3-3 (`make_rr_comorb()`), which
# fits one univariable model per condition with no adjustment for the
# others.
#
# Because the outcome is modelled with a logistic link, the raw model
# coefficients give an adjusted odds ratio (OR), not a risk ratio. To report
# an actual RR (consistent with the "RR" framing used throughout this
# pipeline, see 03_relative_risk.R / PIPELINE.md section 3), this script
# additionally computes a standardised RR by g-computation: predict every
# person's risk twice - once with the condition forced to present, once
# forced to absent, keeping their actual age/sex/other-core-conditions -
# and ratio the two population-average predicted risks. 95% CIs for the
# standardised RR come from a parametric bootstrap of the model
# coefficients (MASS::mvrnorm), the same approach 03_relative_risk.R uses
# for its GAM-based RR.
#
# Needs: 01_Data/chik_sinan_individual_2015_2025.rds (from
# 02_clean_chik_sinan_brazil.R) and the packages loaded by 00_setup.R.
# Runs standalone - does not need 03-11 to have been sourced first, and
# does not modify or depend on analysis_df from 03.
#
# Output:
#   hosp_cohort, death_cohort              the two cohorts built here
#   fit_hosp_core3, fit_death_core3        the mutually-adjusted GAMs
#                                          (age/state smooths, sex/year/
#                                          dm/htn/ckd parametric - same
#                                          adjustment set as section 8's
#                                          core_profile models)
#   core3_rr_hosp, core3_rr_death          adjusted OR + standardised RR per
#                                          condition, one row each
#   -> saved: 01_Data/core3_rr_hosp.RData, 01_Data/core3_rr_death.RData
#   -> table: 03_Output/tables/core3_rr_tables.xlsx (sheets: hosp, death)
#   -> figures: 03_Output/figures/fig_core3_rr_hosp.jpg,
#               03_Output/figures/fig_core3_rr_death.jpg
#   core_profile_rr_hosp_by_age, core_profile_rr_death_by_age
#                                          profile RR vs None within five age
#                                          groups (0-19, 20-39, 40-59, 60-79,
#                                          80+)
#   -> saved: 01_Data/core_profile_rr_hosp_by_age.RData,
#             01_Data/core_profile_rr_death_by_age.RData
#   -> figures: 03_Output/figures/fig_core_profile_rr_hosp_by_age.jpg,
#               03_Output/figures/fig_core_profile_rr_death_by_age.jpg
#   fig3_ageint_aic, fig3_rr_model_comparison_data
#                                          AIC of, and age-specific RR curves
#                                          from, a version of the Figure 3
#                                          models (section 10) that lets each
#                                          of dm/htn/ckd have its own
#                                          age-varying deviation via
#                                          s(age_years, by = <condition>),
#                                          compared against section 10's
#                                          original age-invariant-OR models
#   -> saved: 01_Data/fig3_age_interaction_comparison_data.RData
#   -> figure: 03_Output/figures/fig3_age_interaction_comparison.pdf/.png
#   core_profile_rr_age_curve             continuous-age (not 5-band)
#                                          RR-vs-None per profile, from a
#                                          factor-smooth model
#                                          (s(age, core_profile, bs = "fs"))
#                                          that shares one smoothing
#                                          parameter across all 8 profiles
#   -> saved: 01_Data/core_profile_rr_age_curve.RData
#   -> figure: 03_Output/figures/fig3_profile_age_specific_relative_risk.pdf/.png
# ---------------------------------------------------------------------------

fig_dir <- "03_Output/figures"
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

table_dir <- "03_Output/tables"
if (!dir.exists(table_dir)) dir.create(table_dir, recursive = TRUE)

## Shared publication theme (Nature-style: quiet neutrals, small serif-free
## type, minimal chrome). Defined here (rather than down in section 10,
## where it originated) so every figure in this script - including the
## section 8/9 profile forest plots - can use the same, single definition.
theme_figure3_nature <- function() {
  ggplot2::theme_classic(base_size = 8.5, base_family = "Arial") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold", size = 12, colour = "#1A1A1A",
        margin = ggplot2::margin(b = 3)
      ),
      plot.subtitle = ggplot2::element_text(
        size = 8.5, colour = "#4D4D4D",
        margin = ggplot2::margin(b = 8)
      ),
      strip.background = ggplot2::element_rect(
        fill = "#F2F2F2", colour = NA
      ),
      strip.text = ggplot2::element_text(
        face = "bold", size = 8.5, colour = "#1A1A1A"
      ),
      axis.title = ggplot2::element_text(size = 8.5, colour = "#1A1A1A"),
      axis.text = ggplot2::element_text(size = 7.5, colour = "#333333"),
      axis.line = ggplot2::element_line(linewidth = 0.35, colour = "#333333"),
      legend.position = "top",
      legend.justification = "left",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 8),
      plot.margin = ggplot2::margin(6, 10, 6, 6)
    )
}

## ============================================================
## 1. Load data and build the two core-3 cohorts
## ============================================================

ind <- readRDS("01_Data/chik_sinan_individual_2015_2025.rds")

core_cols <- c(
  "diabetes",
  "hypertension",
  "renal_disease"
)

core_profile_levels <- c(
  "None",
  "DM", "HTN", "CKD",
  "DM+HTN", "DM+CKD", "HTN+CKD",
  "DM+HTN+CKD"
)

## These broad bands match the age-band bridge used in 05_corr_matrix.R and
## 07_gbd_age_alignment.R. They retain enough events for an age-stratified
## profile analysis; finer 10-year bands leave many profile/death cells empty.
profile_age_levels <- c("0-19", "20-39", "40-59", "60-79", "80+")

## Shared eligibility + core-3 coding, applied independently to build each
## outcome's own cohort below. Liver disease/autoimmune/haematological/
## peptic ulcer status is never referenced.
add_core3 <- function(df) {
  df |>
    dplyr::filter(
      is_confirmed_chik,
      lubridate::year(event_date) >= 2017,
      !is.na(age_years),
      age_years >= 0,
      age_years <= 100,
      sex %in% c("male", "female"),
      dplyr::if_all(
        dplyr::all_of(core_cols),
        ~ .x %in% c("no", "yes")
      )
    ) |>
    dplyr::mutate(
      year         = factor(lubridate::year(event_date)),
      sex          = factor(sex, levels = c("female", "male")),
      uf_residence = factor(uf_residence),
      dm   = as.integer(diabetes == "yes"),
      htn  = as.integer(hypertension == "yes"),
      ckd  = as.integer(renal_disease == "yes")
    ) |>
    dplyr::mutate(
      ## 8 mutually-exclusive joint profiles, rather than collapsing every
      ## condition into one undifferentiated "comorbidity count" - see
      ## section 8 below for why that distinction matters here.
      core_profile = dplyr::case_when(
        dm == 0 & htn == 0 & ckd == 0 ~ "None",
        dm == 1 & htn == 0 & ckd == 0 ~ "DM",
        dm == 0 & htn == 1 & ckd == 0 ~ "HTN",
        dm == 0 & htn == 0 & ckd == 1 ~ "CKD",
        dm == 1 & htn == 1 & ckd == 0 ~ "DM+HTN",
        dm == 1 & htn == 0 & ckd == 1 ~ "DM+CKD",
        dm == 0 & htn == 1 & ckd == 1 ~ "HTN+CKD",
        dm == 1 & htn == 1 & ckd == 1 ~ "DM+HTN+CKD"
      ),
      core_profile = factor(
        core_profile,
        levels = core_profile_levels
      ),
      age_band_profile = cut(
        age_years,
        breaks = c(0, 20, 40, 60, 80, 101),
        labels = profile_age_levels,
        right = FALSE,
        include.lowest = TRUE
      ),
      ## This factor supplies a partially pooled profile x age-band deviation
      ## in the stratified model below. Keeping all 40 levels explicitly
      ## makes prediction to every profile/age-band combination unambiguous.
      profile_age = factor(
        paste(core_profile, age_band_profile, sep = " | "),
        levels = as.vector(outer(
          core_profile_levels,
          profile_age_levels,
          paste,
          sep = " | "
        ))
      )
    )
}

## Hospitalisation cohort: needs hospitalised known; death status is
## irrelevant to this cohort.
hosp_cohort <- ind |>
  add_core3() |>
  dplyr::filter(hospitalised %in% c("no", "yes")) |>
  dplyr::mutate(hosp_only = hospitalised == "yes") |>
  dplyr::select(
    age_years, sex, year, uf_residence, dm, htn, ckd,
    core_profile, age_band_profile, profile_age, hosp_only
  )

## Death cohort: needs died_from_chik known; hospitalisation status is
## irrelevant to this cohort.
death_cohort <- ind |>
  add_core3() |>
  dplyr::filter(!is.na(died_from_chik)) |>
  dplyr::mutate(death_only = died_from_chik) |>
  dplyr::select(
    age_years, sex, year, uf_residence, dm, htn, ckd,
    core_profile, age_band_profile, profile_age, death_only
  )

## No later calculation needs the 51-column raw table. Dropping it here is
## important for the profile GAMs and bootstrap, which otherwise retain three
## large copies of the individual-level data in memory.
rm(ind)
gc()

message(sprintf(
  "[12] hosp cohort: %s records (%s hospitalised)",
  format(nrow(hosp_cohort), big.mark = ","),
  format(sum(hosp_cohort$hosp_only), big.mark = ",")
))

message(sprintf(
  "[12] death cohort: %s records (%s deaths)",
  format(nrow(death_cohort), big.mark = ","),
  format(sum(death_cohort$death_only), big.mark = ",")
))

## ============================================================
## 2. Mutually-adjusted models: dm + htn + ckd + age + sex + year + state
## ============================================================
## Main effects only for dm/htn/ckd - each condition's coefficient is
## adjusted for the other two, matching the question this script is built
## to answer ("how much higher is risk for DM, after accounting for HTN and
## CKD?"). age/sex/year/state are deliberately the SAME adjustment set as
## the core_profile model in section 8 below (age via s(bs="ts"), state via
## a random-effect smooth to avoid the zero-death-state separation problem
## documented there) - the two models are meant to differ only in how
## dm/htn/ckd enter (additive main effects here vs a free 8-level factor in
## section 8), not in what else they adjust for.

fit_hosp_core3 <- mgcv::bam(
  hosp_only ~
    s(age_years, k = 10, bs = "ts") +
    sex +
    year +
    s(uf_residence, bs = "re") +
    dm + htn + ckd,
  data = hosp_cohort,
  family = binomial(link = "logit"),
  method = "fREML",
  discrete = TRUE
)

fit_death_core3 <- mgcv::bam(
  death_only ~
    s(age_years, k = 10, bs = "ts") +
    sex +
    year +
    s(uf_residence, bs = "re") +
    dm + htn + ckd,
  data = death_cohort,
  family = binomial(link = "logit"),
  method = "fREML",
  discrete = TRUE,
  na.action = na.exclude
)

summary(fit_hosp_core3)
summary(fit_death_core3)

## ============================================================
## 3. Adjusted odds ratios (direct from model coefficients)
## ============================================================

get_adjusted_or <- function(fit, exposure_var) {

  est <- coef(fit)[[exposure_var]]
  se  <- sqrt(diag(vcov(fit, unconditional = TRUE)))[[exposure_var]]

  tibble::tibble(
    condition = exposure_var,
    or        = exp(est),
    or_lower  = exp(est - 1.96 * se),
    or_upper  = exp(est + 1.96 * se)
  )
}

## ============================================================
## 4. Standardised RR by g-computation, with a parametric-coefficient
##    bootstrap for 95% CIs (MASS::mvrnorm, same approach used for the
##    GAM-based RR in 03_relative_risk.R)
## ============================================================
## fit_*_core3 has only main effects in dm/htn/ckd, so the predicted
## probability for any individual depends only on (age_years, sex, year,
## uf_residence, dm, htn, ckd) - grouping the cohort into its unique
## combinations of these and weighting by how many records share each one
## gives EXACTLY the same population-average risk as looping over every
## individual, just far cheaper. Because the model now contains smooth
## terms (s(age_years), s(uf_residence)), predict(..., type = "lpmatrix")
## is used instead of model.matrix() - the same reason section 8's
## standardise_profile_rr() below uses lpmatrix rather than model.matrix().

standardised_rr <- function(
  fit, data, exposure_var, B = 2000, seed = 1, draw_block_size = 100
) {

  combo <- data |>
    dplyr::count(age_years, sex, year, uf_residence, dm, htn, ckd, name = "n")

  combo_exposed   <- combo
  combo_unexposed <- combo
  combo_exposed[[exposure_var]]   <- 1L
  combo_unexposed[[exposure_var]] <- 0L

  X1 <- predict(fit, newdata = combo_exposed, type = "lpmatrix")
  X0 <- predict(fit, newdata = combo_unexposed, type = "lpmatrix")

  w <- combo$n

  beta_hat <- coef(fit)
  p1_point <- plogis(as.numeric(X1 %*% beta_hat))
  p0_point <- plogis(as.numeric(X0 %*% beta_hat))

  risk_exposed_point   <- sum(p1_point * w) / sum(w)
  risk_unexposed_point <- sum(p0_point * w) / sum(w)
  rr_point <- risk_exposed_point / risk_unexposed_point

  set.seed(seed)
  beta_draws <- MASS::mvrnorm(
    n = B, mu = beta_hat, Sigma = vcov(fit, unconditional = TRUE)
  )

  ## Process coefficient draws in blocks. Retaining the full
  ## (standardisation cells x B) prediction matrices can require several GB;
  ## only the B weighted-average risks are needed for the CIs.
  risk_exposed_draws   <- numeric(B)
  risk_unexposed_draws <- numeric(B)
  weight_sum <- sum(w)

  for (first_draw in seq.int(1, B, by = draw_block_size)) {
    draw_index <- first_draw:min(first_draw + draw_block_size - 1, B)
    beta_block <- t(beta_draws[draw_index, , drop = FALSE])

    p1_block <- plogis(X1 %*% beta_block)
    risk_exposed_draws[draw_index] <- as.numeric(crossprod(w, p1_block)) / weight_sum
    rm(p1_block)

    p0_block <- plogis(X0 %*% beta_block)
    risk_unexposed_draws[draw_index] <- as.numeric(crossprod(w, p0_block)) / weight_sum
    rm(p0_block)
  }

  rr_draws <- risk_exposed_draws / risk_unexposed_draws
  risk_difference_draws <- risk_exposed_draws - risk_unexposed_draws

  tibble::tibble(
    condition                   = exposure_var,
    rr                          = rr_point,
    rr_lower                    = quantile(rr_draws, 0.025, names = FALSE),
    rr_upper                    = quantile(rr_draws, 0.975, names = FALSE),
    standardised_risk_exposed   = risk_exposed_point,
    standardised_risk_exposed_lower = quantile(
      risk_exposed_draws, 0.025, names = FALSE
    ),
    standardised_risk_exposed_upper = quantile(
      risk_exposed_draws, 0.975, names = FALSE
    ),
    standardised_risk_unexposed = risk_unexposed_point,
    standardised_risk_unexposed_lower = quantile(
      risk_unexposed_draws, 0.025, names = FALSE
    ),
    standardised_risk_unexposed_upper = quantile(
      risk_unexposed_draws, 0.975, names = FALSE
    ),
    standardised_risk_difference = risk_exposed_point - risk_unexposed_point,
    standardised_risk_difference_lower = quantile(
      risk_difference_draws, 0.025, names = FALSE
    ),
    standardised_risk_difference_upper = quantile(
      risk_difference_draws, 0.975, names = FALSE
    )
  )
}

## ============================================================
## 5. Crude 2x2 counts (context alongside the adjusted estimates)
## ============================================================

get_crude_counts <- function(data, exposure_var, outcome_var) {

  exposed   <- data[[exposure_var]] == 1
  outcome   <- data[[outcome_var]]

  tibble::tibble(
    condition            = exposure_var,
    n_exposed            = sum(exposed),
    n_events_exposed     = sum(outcome[exposed]),
    n_unexposed          = sum(!exposed),
    n_events_unexposed   = sum(outcome[!exposed])
  )
}

## ============================================================
## 6. Assemble one summary table per outcome
## ============================================================

build_core3_summary <- function(fit, data, outcome_var) {

  purrr::map_dfr(
    core_cols,
    function(col) {

      exposure_var <- dplyr::case_when(
        col == "diabetes"      ~ "dm",
        col == "hypertension"  ~ "htn",
        col == "renal_disease" ~ "ckd"
      )

      or_tbl  <- get_adjusted_or(fit, exposure_var)
      rr_tbl  <- standardised_rr(fit, data, exposure_var)
      cnt_tbl <- get_crude_counts(data, exposure_var, outcome_var)

      or_tbl |>
        dplyr::left_join(rr_tbl,  by = "condition") |>
        dplyr::left_join(cnt_tbl, by = "condition")
    }
  ) |>
    dplyr::mutate(
      condition = dplyr::recode(
        condition,
        dm  = "Diabetes",
        htn = "Hypertension",
        ckd = "Chronic kidney disease"
      ),
      condition = factor(
        condition,
        levels = c("Diabetes", "Hypertension", "Chronic kidney disease")
      )
    )
}

core3_rr_hosp <- build_core3_summary(
  fit_hosp_core3,
  hosp_cohort,
  "hosp_only"
)

core3_rr_death <- build_core3_summary(
  fit_death_core3,
  death_cohort,
  "death_only"
)

print(core3_rr_hosp)
print(core3_rr_death)

save(core3_rr_hosp, file = "01_Data/core3_rr_hosp.RData")
save(core3_rr_death, file = "01_Data/core3_rr_death.RData")

## Also as a workbook, one sheet per outcome, alongside 13_/14_'s xlsx
## tables - so the mutually-adjusted per-condition results (adjusted OR,
## standardised RR, and the crude exposed/unexposed counts behind them)
## are readable outside R too.
core3_rr_wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(core3_rr_wb, "hosp")
openxlsx::writeDataTable(core3_rr_wb, "hosp", core3_rr_hosp, withFilter = TRUE)
openxlsx::addWorksheet(core3_rr_wb, "death")
openxlsx::writeDataTable(core3_rr_wb, "death", core3_rr_death, withFilter = TRUE)
openxlsx::saveWorkbook(
  core3_rr_wb,
  file.path(table_dir, "core3_rr_tables.xlsx"),
  overwrite = TRUE
)

## ============================================================
## 7. Forest-style plots of the standardised RR
## ============================================================

plot_core3_rr <- function(summary_tbl, outcome_label) {

  ggplot(
    summary_tbl,
    aes(x = rr, y = condition)
  ) +
    geom_vline(
      xintercept = 1,
      linetype = "dashed",
      colour = "grey40"
    ) +
    geom_errorbarh(
      aes(xmin = rr_lower, xmax = rr_upper),
      height = 0.15,
      linewidth = 0.7
    ) +
    geom_point(
      size = 3,
      colour = "#C95D63"
    ) +
    scale_x_continuous(
      breaks = scales::pretty_breaks()
    ) +
    labs(
      title = paste0(
        "Condition-specific relative risk of ", outcome_label
      ),
      subtitle = paste0(
        "Mutually adjusted for the other two core conditions, age, sex, ",
        "year, and state\n",
        "95% CI from parametric bootstrap"
      ),
      x = paste0("Relative risk of ", outcome_label, " (present vs absent)"),
      y = NULL
    ) +
    theme_bw(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank()
    )
}

fig_core3_rr_hosp <- plot_core3_rr(core3_rr_hosp, "hospitalisation")
fig_core3_rr_death <- plot_core3_rr(core3_rr_death, "death")

fig_core3_rr_hosp
fig_core3_rr_death

ggsave(
  filename = file.path(fig_dir, "fig_core3_rr_hosp.jpg"),
  plot = fig_core3_rr_hosp,
  width = 8,
  height = 4,
  units = "in",
  dpi = 300
)

ggsave(
  filename = file.path(fig_dir, "fig_core3_rr_death.jpg"),
  plot = fig_core3_rr_death,
  width = 8,
  height = 4,
  units = "in",
  dpi = 300
)

## ============================================================
## 8. Joint core-3 profile (8 mutually-exclusive combinations)
## ============================================================
## Two separate limitations this section addresses, in two different models:
##   1. Sections 2-7 above (core3) put dm/htn/ckd into the SAME model but
##      still treat each as a separate ADDITIVE term - which silently
##      assumes their effects simply add up on the log-odds scale, and
##      gives no way to see whether, say, DM+CKD together carry more or
##      less risk than DM-alone x CKD-alone would predict if the effects
##      really did just multiply (they don't - see the sub-multiplicative
##      comparison this script's discussion derives from core_profile_rr_*
##      below: observed DM+CKD is well under the naive product).
##   2. 03_relative_risk.R's older comorb_count_group scheme (0/1/2+) is a
##      DIFFERENT limitation: it counts HOW MANY conditions someone has,
##      not WHICH ones, so it forces a diabetes-only patient and a
##      CKD-only patient to have the same estimated risk multiplier just
##      because both are "1 comorbidity". core3 does not have this problem
##      (dm/htn/ckd already get separate coefficients there), but it also
##      does not fix limitation 1 above.
##
## `core_profile` (built in add_core3() above) instead assigns every person
## to exactly one of 8 mutually-exclusive categories (single conditions,
## every pairwise combination, and all three together). One GAM per
## outcome regresses on this 8-level factor directly, plus age/sex/year/
## state, so each profile gets its own freely-estimated risk - no additive
## assumption at all. This is the result meant to feed the background
## comorbidity-prevalence model, where the population is likewise broken
## into joint profiles (via the copula simulation, see PIPELINE.md section
## 4) rather than a single comorbidity count.
##
## State is entered as a random-effect smooth (s(..., bs = "re")), not
## 27 fixed dummies: with only 921 deaths total, 4 of Brazil's 27 states
## have ZERO recorded deaths in this cohort. A fixed factor(uf_residence)
## on the death model tries to drive those states' log-odds to -Inf to
## match, which is quasi-complete separation - confirmed by fitting it
## that way first: mgcv returned "step failure - check results carefully",
## the intercept came out at -25.5 (SE 6066), and most state coefficients
## landed at implausible values (12-19 on the log-odds scale) with equally
## enormous standard errors, which fed through into wildly unstable
## bootstrap draws for the profile RRs below (e.g. DM+CKD's 95% CI spanned
## 1.3-13.3). A random-effect smooth shrinks sparse/zero-event states
## toward the population mean instead of letting them diverge - the same
## shrinkage principle already used for age via bs = "ts" - and eliminates
## the warning entirely. The hospitalisation model has no such problem
## (21,585 events spread across every state) but uses the same random
## effect for state, for consistency between the two outcome models.

## The pooled-profile output is unchanged by the age-stratified extension. If
## an already-computed copy is available, reuse it so rerunning this script to
## update the new age-specific analyses does not repeat its costly bootstrap.
if (
  file.exists("01_Data/core_profile_rr_hosp.RData") &&
    file.exists("01_Data/core_profile_rr_death.RData")
) {

  load("01_Data/core_profile_rr_hosp.RData")
  load("01_Data/core_profile_rr_death.RData")
  message("[12] reusing existing pooled core-profile RR summaries")

} else {

fit_hosp_profile <- mgcv::gam(
  hosp_only ~
    core_profile +
    sex +
    year +
    s(uf_residence, bs = "re") +
    s(age_years, k = 10, bs = "ts"),
  data = hosp_cohort,
  family = binomial(link = "logit"),
  method = "REML"
)

fit_death_profile <- mgcv::gam(
  death_only ~
    core_profile +
    sex +
    year +
    s(uf_residence, bs = "re") +
    s(age_years, k = 10, bs = "ts"),
  data = death_cohort,
  family = binomial(link = "logit"),
  method = "REML",
  na.action = na.exclude
)

summary(fit_hosp_profile)
summary(fit_death_profile)

## ------------------------------------------------------------
## Standardised risk per profile, and RR vs "None", by g-computation
## ------------------------------------------------------------
## Because the model has no interaction between core_profile and
## age/sex/year/state, the predicted probability for any individual
## depends only on that 5-tuple (core_profile, age_years, sex, year,
## uf_residence) - so, exactly as in standardised_rr() above, grouping the
## cohort into its unique (age_years, sex, year, uf_residence)
## combinations (the covariates NOT being manipulated) and weighting by how
## many records share each combination gives the same population-average
## risk as looping over every individual, at a fraction of the cost.
## Unlike standardised_rr() this uses predict(..., type = "lpmatrix")
## rather than model.matrix(), because the model contains a smooth term
## (s(age_years)) that model.matrix() cannot build correctly on its own -
## the same reason 03_relative_risk.R's GAM-based RR bootstrap uses
## lpmatrix.

## Calculate one weighted mean risk per coefficient draw without materialising
## the full (prediction cells x 2,000 draws) matrix at once. The full profile
## standardisation has tens of thousands of cells, so doing this in blocks
## prevents a transient multi-gigabyte allocation while giving exactly the
## same draw-specific risks as the one-shot calculation.
weighted_risk_draws <- function(X, beta_draws, w, draw_block_size = 500) {

  draw_count <- nrow(beta_draws)
  output <- numeric(draw_count)

  for (first_draw in seq.int(1, draw_count, by = draw_block_size)) {

    draw_index <- first_draw:min(
      first_draw + draw_block_size - 1,
      draw_count
    )

    p_draws_block <- plogis(
      X %*% t(beta_draws[draw_index, , drop = FALSE])
    )

    output[draw_index] <- colSums(p_draws_block * w) / sum(w)
  }

  output
}

standardise_profile_rr <- function(fit, data, B = 2000, seed = 1) {

  profile_levels <- levels(data$core_profile)

  combo <- data |>
    dplyr::count(age_years, sex, year, uf_residence, name = "n")

  w <- combo$n

  beta_hat <- coef(fit)

  set.seed(seed)
  beta_draws <- MASS::mvrnorm(
    n = B,
    mu = beta_hat,
    Sigma = vcov(fit, unconditional = TRUE)
  )

  risk_point <- setNames(numeric(length(profile_levels)), profile_levels)
  risk_draws <- matrix(
    NA_real_,
    nrow = B,
    ncol = length(profile_levels),
    dimnames = list(NULL, profile_levels)
  )

  for (p in profile_levels) {

    combo_p <- combo
    combo_p$core_profile <- factor(p, levels = profile_levels)

    X_p <- predict(fit, newdata = combo_p, type = "lpmatrix")

    p_point <- plogis(as.numeric(X_p %*% beta_hat))
    risk_point[p] <- sum(p_point * w) / sum(w)

    risk_draws[, p] <- weighted_risk_draws(X_p, beta_draws, w)
  }

  rr_point <- risk_point / risk_point[["None"]]
  rr_draws <- sweep(risk_draws, 1, risk_draws[, "None"], FUN = "/")

  tibble::tibble(
    core_profile = factor(profile_levels, levels = profile_levels),
    standardised_risk = as.numeric(risk_point),
    rr = as.numeric(rr_point),
    rr_lower = apply(rr_draws, 2, quantile, probs = 0.025, names = FALSE),
    rr_upper = apply(rr_draws, 2, quantile, probs = 0.975, names = FALSE)
  )
}

## Crude N/events per profile - attached mainly so a reader can judge how
## much sampling noise to expect behind each row (e.g. a profile with a
## handful of events can easily look non-monotonic vs. its neighbours by
## chance alone; the CI already reflects this, but the raw counts make it
## legible at a glance).
get_crude_profile_counts <- function(data, outcome_var) {
  data |>
    dplyr::group_by(core_profile) |>
    dplyr::summarise(
      n_profile = dplyr::n(),
      n_events_profile = sum(.data[[outcome_var]]),
      .groups = "drop"
    )
}

core_profile_rr_hosp <- standardise_profile_rr(fit_hosp_profile, hosp_cohort) |>
  dplyr::left_join(
    get_crude_profile_counts(hosp_cohort, "hosp_only"),
    by = "core_profile"
  )

core_profile_rr_death <- standardise_profile_rr(fit_death_profile, death_cohort) |>
  dplyr::left_join(
    get_crude_profile_counts(death_cohort, "death_only"),
    by = "core_profile"
  )

print(core_profile_rr_hosp, n = Inf)
print(core_profile_rr_death, n = Inf)

save(core_profile_rr_hosp, file = "01_Data/core_profile_rr_hosp.RData")
save(core_profile_rr_death, file = "01_Data/core_profile_rr_death.RData")

}

## `weighted_risk_draws()` is normally created in the calculation branch
## above. Define the same memory-safe helper when cached pooled results are
## reused, because the age-stratified bootstrap below also needs it.
if (!exists("weighted_risk_draws", mode = "function")) {
  weighted_risk_draws <- function(X, beta_draws, w, draw_block_size = 500) {

    draw_count <- nrow(beta_draws)
    output <- numeric(draw_count)

    for (first_draw in seq.int(1, draw_count, by = draw_block_size)) {
      draw_index <- first_draw:min(
        first_draw + draw_block_size - 1,
        draw_count
      )
      p_draws_block <- plogis(
        X %*% t(beta_draws[draw_index, , drop = FALSE])
      )
      output[draw_index] <- colSums(p_draws_block * w) / sum(w)
    }

    output
  }
}

## ------------------------------------------------------------
## Plots
## ------------------------------------------------------------

## Same publication style as Figure 3 (fig3_condition_rr_forest):
## theme_figure3_nature(), log-scale RR axis, PDF + 600dpi PNG.
plot_core_profile_rr <- function(summary_tbl, outcome_label) {

  ggplot2::ggplot(
    summary_tbl,
    ggplot2::aes(x = rr, y = forcats::fct_rev(core_profile))
  ) +
    ggplot2::geom_vline(
      xintercept = 1,
      linetype = "dashed",
      linewidth = 0.4,
      colour = "#666666"
    ) +
    ggplot2::geom_errorbarh(
      ggplot2::aes(xmin = rr_lower, xmax = rr_upper),
      height = 0.14,
      linewidth = 0.6,
      colour = "#1A1A1A"
    ) +
    ggplot2::geom_point(size = 2.6, colour = "#0072B2") +
    ggplot2::scale_x_log10(
      breaks = scales::log_breaks(n = 5),
      labels = scales::label_number(accuracy = 0.1)
    ) +
    ggplot2::labs(
      title = paste0(
        "Relative risk of ", outcome_label, " by joint core-3 profile"
      ),
      subtitle = paste0(
        "Age/sex/year/state-standardised; reference = no DM, HTN, or CKD\n",
        "95% CI from parametric bootstrap"
      ),
      x = paste0("Relative risk of ", outcome_label, " vs None (log scale)"),
      y = NULL
    ) +
    theme_figure3_nature() +
    ggplot2::theme(legend.position = "none")
}

fig_core_profile_rr_hosp <- plot_core_profile_rr(
  core_profile_rr_hosp,
  "hospitalisation"
)

fig_core_profile_rr_death <- plot_core_profile_rr(
  core_profile_rr_death,
  "death"
)

fig_core_profile_rr_hosp
fig_core_profile_rr_death

ggplot2::ggsave(
  filename = file.path(fig_dir, "fig_core_profile_rr_hosp.pdf"),
  plot = fig_core_profile_rr_hosp,
  width = 160,
  height = 105,
  units = "mm",
  device = grDevices::cairo_pdf
)

ggplot2::ggsave(
  filename = file.path(fig_dir, "fig_core_profile_rr_hosp.png"),
  plot = fig_core_profile_rr_hosp,
  width = 160,
  height = 105,
  units = "mm",
  dpi = 600,
  bg = "white"
)

ggplot2::ggsave(
  filename = file.path(fig_dir, "fig_core_profile_rr_death.pdf"),
  plot = fig_core_profile_rr_death,
  width = 160,
  height = 105,
  units = "mm",
  device = grDevices::cairo_pdf
)

ggplot2::ggsave(
  filename = file.path(fig_dir, "fig_core_profile_rr_death.png"),
  plot = fig_core_profile_rr_death,
  width = 160,
  height = 105,
  units = "mm",
  dpi = 600,
  bg = "white"
)

## ============================================================
## 9. Joint core-3 profile RR within age groups
## ============================================================
## The pooled result above answers the marginal question over the whole
## cohort. Here, the same 8 profiles are compared *within* five broad age
## groups. Sex, notification year, residence state, and exact age within the
## age group remain standardised by g-computation.
##
## Several death cells are very sparse (including zero observed deaths), so a
## fully unpenalised core_profile * age_band_profile interaction is prone to
## separation and implausibly infinite estimates. `s(profile_age, bs = "re")`
## is a partially pooled interaction: it permits each profile's RR to differ
## by age group while shrinking the least-informative cells toward the shared
## profile and age-band main effects. This is deliberately more stable than
## fitting five unrelated death models.

## `bam(..., discrete = TRUE)` is mgcv's scalable implementation of the same
## penalised GAM. It is needed here because these two cohorts contain roughly
## 0.7 and 1.0 million records, respectively; the standard `gam()` fit keeps
## an unnecessarily large dense working matrix in memory.
fit_hosp_profile_by_age <- mgcv::bam(
  hosp_only ~
    core_profile +
    age_band_profile +
    sex +
    year +
    s(uf_residence, bs = "re") +
    s(age_years, k = 10, bs = "ts") +
    s(profile_age, bs = "re"),
  data = hosp_cohort,
  family = binomial(link = "logit"),
  method = "fREML",
  discrete = TRUE
)

fit_death_profile_by_age <- mgcv::bam(
  death_only ~
    core_profile +
    age_band_profile +
    sex +
    year +
    s(uf_residence, bs = "re") +
    s(age_years, k = 10, bs = "ts") +
    s(profile_age, bs = "re"),
  data = death_cohort,
  family = binomial(link = "logit"),
  method = "fREML",
  discrete = TRUE,
  na.action = na.exclude
)

summary(fit_hosp_profile_by_age)
summary(fit_death_profile_by_age)

## Within every age group, standardise predictions over that group's observed
## exact-age, sex, year, and state distribution. Every profile is then set in
## turn, with the denominator set to the otherwise identical "None" profile.
## 1,000 coefficient draws give stable percentile intervals here while keeping
## the 40 profile-by-age predictions practical on a typical workstation.
standardise_profile_rr_by_age <- function(fit, data, B = 1000, seed = 1) {

  beta_hat <- coef(fit)

  set.seed(seed)
  beta_draws <- MASS::mvrnorm(
    n = B,
    mu = beta_hat,
    Sigma = vcov(fit, unconditional = TRUE)
  )

  purrr::map_dfr(
    profile_age_levels,
    function(current_age_band) {

      combo <- data |>
        dplyr::filter(age_band_profile == current_age_band) |>
        dplyr::count(age_years, sex, year, uf_residence, name = "n") |>
        dplyr::mutate(
          age_band_profile = factor(
            current_age_band,
            levels = profile_age_levels
          )
        )

      w <- combo$n
      risk_point <- setNames(
        numeric(length(core_profile_levels)),
        core_profile_levels
      )
      risk_draws <- matrix(
        NA_real_,
        nrow = B,
        ncol = length(core_profile_levels),
        dimnames = list(NULL, core_profile_levels)
      )

      for (p in core_profile_levels) {

        combo_p <- combo |>
          dplyr::mutate(
            core_profile = factor(p, levels = core_profile_levels),
            profile_age = factor(
              paste(p, current_age_band, sep = " | "),
              levels = levels(data$profile_age)
            )
          )

        X_p <- predict(fit, newdata = combo_p, type = "lpmatrix")

        p_point <- plogis(as.numeric(X_p %*% beta_hat))
        risk_point[p] <- sum(p_point * w) / sum(w)

        risk_draws[, p] <- weighted_risk_draws(X_p, beta_draws, w)
      }

      rr_point <- risk_point / risk_point[["None"]]
      rr_draws <- sweep(risk_draws, 1, risk_draws[, "None"], FUN = "/")

      tibble::tibble(
        age_band_profile = factor(
          current_age_band,
          levels = profile_age_levels
        ),
        core_profile = factor(
          core_profile_levels,
          levels = core_profile_levels
        ),
        standardised_risk = as.numeric(risk_point),
        rr = as.numeric(rr_point),
        rr_lower = apply(
          rr_draws, 2, quantile, probs = 0.025, names = FALSE
        ),
        rr_upper = apply(
          rr_draws, 2, quantile, probs = 0.975, names = FALSE
        )
      )
    }
  )
}

get_crude_profile_counts_by_age <- function(data, outcome_var) {
  data |>
    dplyr::group_by(age_band_profile, core_profile) |>
    dplyr::summarise(
      n_profile = dplyr::n(),
      n_events_profile = sum(.data[[outcome_var]]),
      .groups = "drop"
    )
}

core_profile_rr_hosp_by_age <-
  standardise_profile_rr_by_age(fit_hosp_profile_by_age, hosp_cohort) |>
  dplyr::left_join(
    get_crude_profile_counts_by_age(hosp_cohort, "hosp_only"),
    by = c("age_band_profile", "core_profile")
  )

core_profile_rr_death_by_age <-
  standardise_profile_rr_by_age(fit_death_profile_by_age, death_cohort) |>
  dplyr::left_join(
    get_crude_profile_counts_by_age(death_cohort, "death_only"),
    by = c("age_band_profile", "core_profile")
  )

print(core_profile_rr_hosp_by_age, n = Inf)
print(core_profile_rr_death_by_age, n = Inf)

save(
  core_profile_rr_hosp_by_age,
  file = "01_Data/core_profile_rr_hosp_by_age.RData"
)
save(
  core_profile_rr_death_by_age,
  file = "01_Data/core_profile_rr_death_by_age.RData"
)

plot_core_profile_rr_by_age <- function(summary_tbl, outcome_label) {

  ggplot(
    summary_tbl,
    aes(x = rr, y = forcats::fct_rev(core_profile))
  ) +
    geom_vline(
      xintercept = 1,
      linetype = "dashed",
      colour = "grey40"
    ) +
    geom_errorbarh(
      aes(xmin = rr_lower, xmax = rr_upper),
      height = 0.15,
      linewidth = 0.6
    ) +
    geom_point(size = 2.5, colour = "#4C78A8") +
    facet_wrap(~ age_band_profile, ncol = 3, scales = "free_x") +
    scale_x_log10(
      breaks = scales::log_breaks(n = 4),
      labels = scales::label_number(accuracy = 0.1)
    ) +
    labs(
      title = paste0(
        "Age-stratified relative risk of ", outcome_label,
        " by joint core-3 profile"
      ),
      subtitle = paste0(
        "Within-age-group standardisation for exact age, sex, year and state; ",
        "reference = no DM, HTN or CKD\n",
        "Profile-by-age effects are partially pooled because sparse death cells ",
        "are unstable; 95% CI from parametric bootstrap"
      ),
      x = paste0("Relative risk of ", outcome_label, " vs None (log scale)"),
      y = NULL
    ) +
    theme_bw(base_size = 10) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      strip.background = element_rect(fill = "grey92", colour = NA)
    )
}

fig_core_profile_rr_hosp_by_age <- plot_core_profile_rr_by_age(
  core_profile_rr_hosp_by_age,
  "hospitalisation"
)

fig_core_profile_rr_death_by_age <- plot_core_profile_rr_by_age(
  core_profile_rr_death_by_age,
  "death"
)

fig_core_profile_rr_hosp_by_age
fig_core_profile_rr_death_by_age

ggsave(
  filename = file.path(fig_dir, "fig_core_profile_rr_hosp_by_age.jpg"),
  plot = fig_core_profile_rr_hosp_by_age,
  width = 11,
  height = 7,
  units = "in",
  dpi = 300
)

ggsave(
  filename = file.path(fig_dir, "fig_core_profile_rr_death_by_age.jpg"),
  plot = fig_core_profile_rr_death_by_age,
  width = 11,
  height = 7,
  units = "in",
  dpi = 300
)

## ============================================================
## 10. Figure 3: age-specific absolute risk, condition absent vs present
## ============================================================
## The profile plots above address joint-condition combinations. Figure 3 is
## the more direct clinical presentation: for each of the three core
## conditions, it shows the modelled absolute probability of hospitalisation
## or chikungunya death at every age when that condition is set to absent or
## present. The other two core conditions and sex are standardised to their
## observed joint distribution in the relevant outcome cohort. Thus, the two
## curves differ only in the focal condition, not in the distribution of the
## other core conditions or sex.
##
## Uses the SAME adjustment set as the core3 GAMs in section 2 (age/state
## smooths, sex/year/dm/htn/ckd parametric) - not the section-2 fit objects
## directly, because those interaction-free main-effects fits are exactly
## what a per-age curve needs, but make_core3_absolute_risk() below still
## needs its own refit here (rather than reusing fit_hosp_core3) so this
## section stays self-contained and independently rerunnable. Age is a
## penalised smooth (not a low-df natural spline) to avoid unstable
## boundary extrapolation at ages with few observations (especially age 0
## and ages above 95), while still avoiding condition-specific age smooths
## in the sparse death data.
##
## Earlier versions of this section omitted year/state from this specific
## model, reasoning that adding an 8-year x 27-state standardisation grid
## to a per-single-year-of-age curve would be too memory-heavy for the
## bootstrap. make_core3_absolute_risk() below now processes bootstrap
## draws in blocks (the same reshape-and-matrix-multiply trick used by
## weighted_risk_draws()/make_profile_age_curve() elsewhere in this file)
## specifically so that shortcut is no longer necessary, and this model can
## match sections 2 and 8 exactly.

fit_hosp_fig3 <- mgcv::bam(
  hosp_only ~
    s(age_years, k = 10, bs = "ts") +
    sex +
    year +
    s(uf_residence, bs = "re") +
    dm + htn + ckd,
  data = hosp_cohort,
  family = binomial(link = "logit"),
  method = "fREML",
  discrete = TRUE
)

fit_death_fig3 <- mgcv::bam(
  death_only ~
    s(age_years, k = 10, bs = "ts") +
    sex +
    year +
    s(uf_residence, bs = "re") +
    dm + htn + ckd,
  data = death_cohort,
  family = binomial(link = "logit"),
  method = "fREML",
  discrete = TRUE,
  na.action = na.exclude
)

figure3_ages <- 1:95

## Standardises over sex x year x state x other-two-conditions (the full
## adjustment set shared with sections 2 and 8) at every single year of
## age. Bootstrap draws are processed in blocks of `draw_block_size` and
## immediately collapsed to a (cell x block) weighted-average-risk matrix
## via one reshape + matrix multiply, rather than ever materialising the
## full (grid rows x B draws) matrix - the same memory-safety approach as
## weighted_risk_draws() (section 8) and make_profile_age_curve() (section
## 12), needed here because adding year/state makes the standardisation
## grid far larger than a sex-only one.
make_core3_absolute_risk <- function(
  fit,
  data,
  condition_var,
  B = 2000,
  seed = 1,
  draw_block_size = 200
) {

  other_conditions <- setdiff(c("dm", "htn", "ckd"), condition_var)
  status_levels <- c("Condition absent", "Condition present")

  standardisation_strata <- data |>
    dplyr::count(
      sex, year, uf_residence,
      dplyr::across(dplyr::all_of(other_conditions)),
      name = "n"
    ) |>
    dplyr::mutate(weight = n / sum(n))

  n_strata <- nrow(standardisation_strata)
  w_rep <- standardisation_strata$weight

  ## expand_grid() varies its LAST argument fastest, so - for a fixed
  ## (age_years, condition_status) cell - the strata rows always appear in
  ## the same order and count (n_strata). That fixed layout is what lets
  ## the matrix-reshape trick below stand in for an explicit group-by.
  prediction_grid <- tidyr::expand_grid(
    age_years = figure3_ages,
    condition_status = factor(status_levels, levels = status_levels),
    standardisation_strata
  ) |>
    dplyr::mutate(
      focal_value = as.integer(condition_status == "Condition present")
    )

  prediction_grid[[condition_var]] <- prediction_grid$focal_value
  prediction_grid$focal_value <- NULL

  X <- stats::predict(fit, newdata = prediction_grid, type = "lpmatrix")
  beta_hat <- stats::coef(fit)

  cell_meta <- tidyr::expand_grid(
    age_years = figure3_ages,
    condition_status = factor(status_levels, levels = status_levels)
  )
  n_cells <- nrow(cell_meta)

  point_risk_flat <- stats::plogis(as.numeric(X %*% beta_hat))
  cell_point <- colSums(matrix(point_risk_flat, nrow = n_strata) * w_rep)

  set.seed(seed)
  beta_draws <- MASS::mvrnorm(
    n = B,
    mu = beta_hat,
    Sigma = stats::vcov(fit, unconditional = TRUE)
  )

  cell_draws <- matrix(NA_real_, nrow = n_cells, ncol = B)

  for (first_draw in seq.int(1, B, by = draw_block_size)) {

    draw_index <- first_draw:min(first_draw + draw_block_size - 1, B)

    p_block <- stats::plogis(X %*% t(beta_draws[draw_index, , drop = FALSE]))

    ## Reshape (n_age*n_status*n_strata x block) -> (n_strata x
    ## n_cells*block), take the weighted sum over strata in one matrix
    ## multiply, then reshape back to (n_cells x block). Safe because R
    ## matrices are column-major and expand_grid's row order (above) means
    ## strata is the fastest-varying index within each (age, status) cell.
    mat2 <- matrix(p_block, nrow = n_strata)
    weighted_row <- w_rep %*% mat2
    cell_draws[, draw_index] <- matrix(weighted_row, nrow = n_cells)
  }

  absolute_risk <- cell_meta |>
    dplyr::mutate(
      absolute_risk = cell_point,
      risk_lower = apply(cell_draws, 1, stats::quantile, probs = 0.025, names = FALSE),
      risk_upper = apply(cell_draws, 1, stats::quantile, probs = 0.975, names = FALSE)
    )

  ## Paired bootstrap draws for absent/present (same draw index, same age)
  ## give a proper CI for their ratio, rather than incorrectly dividing two
  ## separately-computed confidence intervals.
  idx_absent  <- which(cell_meta$condition_status == "Condition absent")
  idx_present <- which(cell_meta$condition_status == "Condition present")

  rr_draws_mat <- cell_draws[idx_present, , drop = FALSE] /
    cell_draws[idx_absent, , drop = FALSE]

  relative_risk <- tibble::tibble(
    age_years = cell_meta$age_years[idx_absent],
    rr = cell_point[idx_present] / cell_point[idx_absent],
    rr_lower = apply(rr_draws_mat, 1, stats::quantile, probs = 0.025, names = FALSE),
    rr_upper = apply(rr_draws_mat, 1, stats::quantile, probs = 0.975, names = FALSE)
  )

  list(absolute_risk = absolute_risk, relative_risk = relative_risk)
}

core3_condition_labels <- c(
  dm = "Diabetes",
  htn = "Hypertension",
  ckd = "Chronic kidney disease"
)

fig3_hosp_predictions <- purrr::imap(
  core3_condition_labels,
  function(condition_label, condition_var) {
    make_core3_absolute_risk(
      fit_hosp_fig3,
      hosp_cohort,
      condition_var = condition_var,
      seed = match(condition_var, names(core3_condition_labels))
    )
  }
)

fig3_death_predictions <- purrr::imap(
  core3_condition_labels,
  function(condition_label, condition_var) {
    make_core3_absolute_risk(
      fit_death_fig3,
      death_cohort,
      condition_var = condition_var,
      seed = 10 + match(condition_var, names(core3_condition_labels))
    )
  }
)

fig3_absolute_risk_data <- dplyr::bind_rows(
  purrr::imap_dfr(
    fig3_hosp_predictions,
    function(prediction, condition_var) {
      prediction$absolute_risk |>
        dplyr::mutate(
          condition = core3_condition_labels[[condition_var]],
          outcome = "Hospitalisation"
        )
    }
  ),
  purrr::imap_dfr(
    fig3_death_predictions,
    function(prediction, condition_var) {
      prediction$absolute_risk |>
        dplyr::mutate(
          condition = core3_condition_labels[[condition_var]],
          outcome = "Death from chikungunya"
        )
    }
  )
) |>
  dplyr::mutate(
    condition = factor(condition, levels = unname(core3_condition_labels)),
    outcome = factor(
      outcome,
      levels = c("Hospitalisation", "Death from chikungunya")
    )
  )

fig3_relative_risk_data <- dplyr::bind_rows(
  purrr::imap_dfr(
    fig3_hosp_predictions,
    function(prediction, condition_var) {
      prediction$relative_risk |>
        dplyr::mutate(
          condition = core3_condition_labels[[condition_var]],
          outcome = "Hospitalisation"
        )
    }
  ),
  purrr::imap_dfr(
    fig3_death_predictions,
    function(prediction, condition_var) {
      prediction$relative_risk |>
        dplyr::mutate(
          condition = core3_condition_labels[[condition_var]],
          outcome = "Death from chikungunya"
        )
    }
  )
) |>
  dplyr::mutate(
    condition = factor(condition, levels = unname(core3_condition_labels)),
    outcome = factor(
      outcome,
      levels = c("Hospitalisation", "Death from chikungunya")
    )
  )

## Nature-style palette: a quiet neutral for absence and a colour-blind-safe
## vermilion accent for the condition-present curve. The uncluttered axes,
## restrained ribbons and PDF output are deliberate publication settings.
fig3_colours <- c(
  "Condition absent" = "#7A7A7A",
  "Condition present" = "#D55E00"
)

## theme_figure3_nature() moved to the top of the script (just after fig_dir)
## so section 8/9's profile plots can use it too.

fig3_age_specific_absolute_risk <- ggplot2::ggplot(
  fig3_absolute_risk_data,
  ggplot2::aes(
    x = age_years,
    y = absolute_risk * 100,
    colour = condition_status,
    fill = condition_status
  )
) +
  ggplot2::geom_ribbon(
    ggplot2::aes(
      ymin = risk_lower * 100,
      ymax = risk_upper * 100
    ),
    alpha = 0.14,
    linewidth = 0,
    colour = NA
  ) +
  ggplot2::geom_line(linewidth = 0.8) +
  ggplot2::facet_grid(outcome ~ condition, scales = "free_y") +
  ggplot2::scale_colour_manual(values = fig3_colours) +
  ggplot2::scale_fill_manual(values = fig3_colours) +
  ggplot2::scale_x_continuous(
    breaks = seq(0, 100, by = 20),
    limits = range(figure3_ages),
    expand = ggplot2::expansion(mult = c(0.01, 0.02))
  ) +
  ggplot2::scale_y_continuous(
    labels = function(x) paste0(scales::label_number(accuracy = 0.1)(x), "%"),
    expand = ggplot2::expansion(mult = c(0.03, 0.08))
  ) +
  ggplot2::labs(
    title = "Age-specific absolute risk by underlying condition status",
    subtitle = paste0(
      "Curves are standardised to the observed sex and other core-condition ",
      "distribution; shaded bands show 95% confidence intervals"
    ),
    x = "Age (years)",
    y = "Predicted absolute risk",
    colour = NULL,
    fill = NULL
  ) +
  theme_figure3_nature()

fig3_age_specific_relative_risk <- ggplot2::ggplot(
  fig3_relative_risk_data,
  ggplot2::aes(x = age_years, y = rr)
) +
  ggplot2::geom_hline(
    yintercept = 1,
    linetype = "dashed",
    linewidth = 0.4,
    colour = "#666666"
  ) +
  ggplot2::geom_ribbon(
    ggplot2::aes(ymin = rr_lower, ymax = rr_upper),
    fill = "#0072B2",
    alpha = 0.14,
    linewidth = 0,
    colour = NA
  ) +
  ggplot2::geom_line(colour = "#0072B2", linewidth = 0.8) +
  ggplot2::facet_grid(outcome ~ condition, scales = "free_y") +
  ggplot2::scale_x_continuous(
    breaks = seq(0, 100, by = 20),
    limits = range(figure3_ages),
    expand = ggplot2::expansion(mult = c(0.01, 0.02))
  ) +
  ggplot2::scale_y_log10(
    breaks = scales::log_breaks(n = 4),
    labels = scales::label_number(accuracy = 0.1),
    expand = ggplot2::expansion(mult = c(0.05, 0.1))
  ) +
  ggplot2::labs(
    title = "Age-specific relative risk by underlying condition status",
    subtitle = paste0(
      "Condition present versus absent; the same paired bootstrap draws as ",
      "the absolute-risk curves are used for the 95% confidence intervals"
    ),
    x = "Age (years)",
    y = "Relative risk (log scale)"
  ) +
  theme_figure3_nature() +
  ggplot2::theme(legend.position = "none")

fig3_forest_data <- dplyr::bind_rows(
  core3_rr_hosp |>
    dplyr::transmute(
      condition,
      outcome = "Hospitalisation",
      rr,
      rr_lower,
      rr_upper
    ),
  core3_rr_death |>
    dplyr::transmute(
      condition,
      outcome = "Death from chikungunya",
      rr,
      rr_lower,
      rr_upper
    )
) |>
  dplyr::mutate(
    condition = factor(condition, levels = rev(unname(core3_condition_labels))),
    outcome = factor(
      outcome,
      levels = c("Hospitalisation", "Death from chikungunya")
    )
  )

fig3_condition_rr_forest <- ggplot2::ggplot(
  fig3_forest_data,
  ggplot2::aes(x = rr, y = condition)
) +
  ggplot2::geom_vline(
    xintercept = 1,
    linetype = "dashed",
    linewidth = 0.4,
    colour = "#666666"
  ) +
  ggplot2::geom_errorbarh(
    ggplot2::aes(xmin = rr_lower, xmax = rr_upper),
    height = 0.14,
    linewidth = 0.6,
    colour = "#1A1A1A"
  ) +
  ggplot2::geom_point(size = 2.6, colour = "#0072B2") +
  ggplot2::facet_wrap(~ outcome, ncol = 1, scales = "free_x") +
  ggplot2::scale_x_log10(
    breaks = scales::log_breaks(n = 4),
    labels = scales::label_number(accuracy = 0.1)
  ) +
  ggplot2::labs(
    title = "Overall relative risk by underlying condition",
    subtitle = paste0(
      "Mutually adjusted for the other two core conditions, age, sex, ",
      "year, and state; 95% confidence intervals from parametric bootstrap"
    ),
    x = "Relative risk, present versus absent (log scale)",
    y = NULL
  ) +
  theme_figure3_nature() +
  ggplot2::theme(legend.position = "none")

fig3_age_specific_absolute_risk
fig3_age_specific_relative_risk
fig3_condition_rr_forest

ggplot2::ggsave(
  filename = file.path(fig_dir, "fig3_age_specific_absolute_risk.pdf"),
  plot = fig3_age_specific_absolute_risk,
  width = 183,
  height = 150,
  units = "mm",
  device = grDevices::cairo_pdf
)

ggplot2::ggsave(
  filename = file.path(fig_dir, "fig3_age_specific_absolute_risk.png"),
  plot = fig3_age_specific_absolute_risk,
  width = 183,
  height = 150,
  units = "mm",
  dpi = 600,
  bg = "white"
)

ggplot2::ggsave(
  filename = file.path(fig_dir, "fig3_age_specific_relative_risk.pdf"),
  plot = fig3_age_specific_relative_risk,
  width = 183,
  height = 150,
  units = "mm",
  device = grDevices::cairo_pdf
)

ggplot2::ggsave(
  filename = file.path(fig_dir, "fig3_age_specific_relative_risk.png"),
  plot = fig3_age_specific_relative_risk,
  width = 183,
  height = 150,
  units = "mm",
  dpi = 600,
  bg = "white"
)

ggplot2::ggsave(
  filename = file.path(fig_dir, "fig3_condition_rr_forest.pdf"),
  plot = fig3_condition_rr_forest,
  width = 183,
  height = 105,
  units = "mm",
  device = grDevices::cairo_pdf
)

ggplot2::ggsave(
  filename = file.path(fig_dir, "fig3_condition_rr_forest.png"),
  plot = fig3_condition_rr_forest,
  width = 183,
  height = 105,
  units = "mm",
  dpi = 600,
  bg = "white"
)

save(
  fig3_absolute_risk_data,
  fig3_relative_risk_data,
  fig3_forest_data,
  file = "01_Data/fig3_age_specific_absolute_risk_data.RData"
)

## ============================================================
## 11. Age-interaction sensitivity check for Figure 3
## ============================================================
## fit_hosp_fig3 / fit_death_fig3 above give every one of dm/htn/ckd a
## SINGLE, age-invariant log-odds effect - i.e. one constant odds ratio (OR)
## per condition, the same at every age. But the reported "relative risk" is
## a RISK ratio (a probability ratio), and a constant OR does NOT translate
## into a constant RR once baseline absolute risk p0(age) varies with age:
##   RR(age) = OR / (1 + p0(age) * (OR - 1))
## Because probability is bounded at 1 while odds are not, RR is compressed
## toward 1 wherever p0(age) is large. Chikungunya's baseline (condition-
## absent) hospitalisation risk is itself U-shaped by age (elevated in
## infants and the elderly, lowest around age 30-60), and its baseline death
## risk rises monotonically with age. Reconstructing fig3_relative_risk_data
## from ONE constant OR (read off at age 1) via the formula above reproduces
## the reported RR at every other age to within 0.01 RR units - i.e. the
## reverse-U shape of CKD's hospitalisation RR, and the age-related decline
## in every condition's death RR, are near-fully explained by this purely
## mathematical OR-to-RR compression, not by a real age-varying biological
## effect.
##
## To separate a real effect-modification signal from that artifact, each
## condition gets its own age-varying deviation from the shared baseline
## smooth, via a `by =` interaction smooth. This is NOT a free/unconstrained
## per-age effect - mgcv's shrinkage penalty (bs = "ts") pulls a condition's
## interaction smooth all the way to ~0 EDF (effectively removing it) if the
## data give no support for the effect actually varying by age.

fig3_ageint_cache <- "01_Data/fig3_age_interaction_comparison_data.RData"

if (file.exists(fig3_ageint_cache)) {

  load(fig3_ageint_cache)
  message("[12] reusing existing Figure 3 age-interaction comparison")

} else {

## Same adjustment set as fit_hosp_fig3/fit_death_fig3 (year, state) plus
## the age-interaction smooths - kept identical to the no-interaction
## fits in everything else, so the AIC comparison just below isolates the
## effect of adding these three smooths rather than mixing it with a
## change in adjustment set.
fit_hosp_fig3_ageint <- mgcv::bam(
  hosp_only ~
    s(age_years, k = 10, bs = "ts") +
    s(age_years, by = dm,  k = 10, bs = "ts") +
    s(age_years, by = htn, k = 10, bs = "ts") +
    s(age_years, by = ckd, k = 10, bs = "ts") +
    sex + year + s(uf_residence, bs = "re") + dm + htn + ckd,
  data = hosp_cohort,
  family = binomial(link = "logit"),
  method = "fREML",
  discrete = TRUE
)

fit_death_fig3_ageint <- mgcv::bam(
  death_only ~
    s(age_years, k = 10, bs = "ts") +
    s(age_years, by = dm,  k = 10, bs = "ts") +
    s(age_years, by = htn, k = 10, bs = "ts") +
    s(age_years, by = ckd, k = 10, bs = "ts") +
    sex + year + s(uf_residence, bs = "re") + dm + htn + ckd,
  data = death_cohort,
  family = binomial(link = "logit"),
  method = "fREML",
  discrete = TRUE,
  na.action = na.exclude
)

## AIC: does allowing age-varying effects actually improve the fit?
fig3_ageint_aic <- dplyr::bind_rows(
  AIC(fit_hosp_fig3, fit_hosp_fig3_ageint) |>
    tibble::rownames_to_column("model") |>
    dplyr::mutate(outcome = "Hospitalisation"),
  AIC(fit_death_fig3, fit_death_fig3_ageint) |>
    tibble::rownames_to_column("model") |>
    dplyr::mutate(outcome = "Death from chikungunya")
) |>
  dplyr::mutate(
    model = dplyr::recode(
      model,
      fit_hosp_fig3         = "No age interaction",
      fit_hosp_fig3_ageint  = "With age interaction",
      fit_death_fig3        = "No age interaction",
      fit_death_fig3_ageint = "With age interaction"
    )
  ) |>
  dplyr::select(outcome, model, df, AIC)

print(fig3_ageint_aic)

## EDF of each condition's interaction smooth is what actually decides
## whether its curve above is trustworthy: an EDF shrunk to ~0 means mgcv
## found no support for that condition's effect varying by age at all.
summary(fit_hosp_fig3_ageint)
summary(fit_death_fig3_ageint)

## Re-derive the age-specific RR curves with make_core3_absolute_risk()
## (defined in section 10 above) applied to the interaction models instead,
## then compare against fig3_relative_risk_data (the no-interaction curves
## already computed in section 10).
fig3_hosp_predictions_ageint <- purrr::imap(
  core3_condition_labels,
  function(condition_label, condition_var) {
    make_core3_absolute_risk(
      fit_hosp_fig3_ageint,
      hosp_cohort,
      condition_var = condition_var,
      seed = 100 + match(condition_var, names(core3_condition_labels))
    )
  }
)

fig3_death_predictions_ageint <- purrr::imap(
  core3_condition_labels,
  function(condition_label, condition_var) {
    make_core3_absolute_risk(
      fit_death_fig3_ageint,
      death_cohort,
      condition_var = condition_var,
      seed = 110 + match(condition_var, names(core3_condition_labels))
    )
  }
)

fig3_relative_risk_ageint_data <- dplyr::bind_rows(
  purrr::imap_dfr(
    fig3_hosp_predictions_ageint,
    function(prediction, condition_var) {
      prediction$relative_risk |>
        dplyr::mutate(
          condition = core3_condition_labels[[condition_var]],
          outcome = "Hospitalisation"
        )
    }
  ),
  purrr::imap_dfr(
    fig3_death_predictions_ageint,
    function(prediction, condition_var) {
      prediction$relative_risk |>
        dplyr::mutate(
          condition = core3_condition_labels[[condition_var]],
          outcome = "Death from chikungunya"
        )
    }
  )
) |>
  dplyr::mutate(
    condition = factor(condition, levels = unname(core3_condition_labels)),
    outcome = factor(
      outcome,
      levels = c("Hospitalisation", "Death from chikungunya")
    )
  )

fig3_rr_model_comparison_data <- dplyr::bind_rows(
  fig3_relative_risk_data |>
    dplyr::mutate(model = "No age interaction"),
  fig3_relative_risk_ageint_data |>
    dplyr::mutate(model = "With age interaction")
) |>
  dplyr::mutate(
    model = factor(model, levels = c("No age interaction", "With age interaction"))
  )

save(
  fig3_ageint_aic,
  fig3_relative_risk_ageint_data,
  fig3_rr_model_comparison_data,
  file = fig3_ageint_cache
)

}

## ------------------------------------------------------------
## Comparison figure: main-effects model vs age-interaction model
## ------------------------------------------------------------

fig3_ageint_colours <- c(
  "No age interaction" = "#7A7A7A",
  "With age interaction" = "#0072B2"
)

fig3_age_interaction_comparison <- ggplot2::ggplot(
  fig3_rr_model_comparison_data,
  ggplot2::aes(x = age_years, y = rr, colour = model, fill = model)
) +
  ggplot2::geom_hline(
    yintercept = 1,
    linetype = "dashed",
    linewidth = 0.4,
    colour = "#666666"
  ) +
  ggplot2::geom_ribbon(
    ggplot2::aes(ymin = rr_lower, ymax = rr_upper),
    alpha = 0.15,
    linewidth = 0,
    colour = NA
  ) +
  ggplot2::geom_line(linewidth = 0.8) +
  ggplot2::facet_grid(outcome ~ condition, scales = "free_y") +
  ggplot2::scale_colour_manual(values = fig3_ageint_colours) +
  ggplot2::scale_fill_manual(values = fig3_ageint_colours) +
  ggplot2::scale_x_continuous(
    breaks = seq(0, 100, by = 20),
    limits = range(figure3_ages),
    expand = ggplot2::expansion(mult = c(0.01, 0.02))
  ) +
  ggplot2::scale_y_log10(
    breaks = scales::log_breaks(n = 4),
    labels = scales::label_number(accuracy = 0.1)
  ) +
  ggplot2::labs(
    title = "Age-specific relative risk: main-effects model vs age-interaction model",
    subtitle = paste0(
      "Grey: one constant odds ratio per condition at every age.\n",
      "Blue: adds a per-condition age-varying deviation, shrunk to zero ",
      "when unsupported.\n",
      "AIC delta (no-interaction minus with-interaction), positive favours ",
      "interaction - Hospitalisation: ",
      sprintf(
        "%+.0f",
        fig3_ageint_aic$AIC[fig3_ageint_aic$outcome == "Hospitalisation" &
          fig3_ageint_aic$model == "No age interaction"] -
          fig3_ageint_aic$AIC[fig3_ageint_aic$outcome == "Hospitalisation" &
            fig3_ageint_aic$model == "With age interaction"]
      ),
      "; Death: ",
      sprintf(
        "%+.1f",
        fig3_ageint_aic$AIC[fig3_ageint_aic$outcome == "Death from chikungunya" &
          fig3_ageint_aic$model == "No age interaction"] -
          fig3_ageint_aic$AIC[fig3_ageint_aic$outcome == "Death from chikungunya" &
            fig3_ageint_aic$model == "With age interaction"]
      )
    ),
    x = "Age (years)",
    y = "Relative risk (log scale)",
    colour = NULL,
    fill = NULL
  ) +
  theme_figure3_nature()

fig3_age_interaction_comparison

ggplot2::ggsave(
  filename = file.path(fig_dir, "fig3_age_interaction_comparison.pdf"),
  plot = fig3_age_interaction_comparison,
  width = 183,
  height = 150,
  units = "mm",
  device = grDevices::cairo_pdf
)

ggplot2::ggsave(
  filename = file.path(fig_dir, "fig3_age_interaction_comparison.png"),
  plot = fig3_age_interaction_comparison,
  width = 183,
  height = 150,
  units = "mm",
  dpi = 600,
  bg = "white"
)

## ============================================================
## 12. Joint core-3 profile RR as a continuous age curve (line + ribbon)
## ============================================================
## Section 9 compares the 8 profiles within five broad age BANDS (a forest
## plot per band). This section instead produces one continuous age curve
## per profile - the profile-model analogue of section 10/11's per-condition
## Figure 3 curves.
##
## This needs its OWN model, not section 8's fit_hosp_profile /
## fit_death_profile or section 9's fit_hosp_profile_by_age /
## fit_death_profile_by_age:
##   - section 8's model has one age smooth SHARED by all 8 profiles (no
##     age x profile interaction at all) - exactly the specification that
##     sections 10-11 showed produces a compressed/distorted age shape
##     (constant OR -> non-constant RR) once translated to the risk scale.
##   - section 9 lets the profile-age interaction vary, but only across 5
##     coarse bands via a partially-pooled random effect - not a continuous
##     curve.
## First attempt used `s(age_years, by = core_profile, bs = "ts")`: because
## core_profile is a FACTOR (not 0/1 numeric, unlike dm/htn/ckd in section
## 11), that gives one full, INDEPENDENTLY penalised smooth per profile
## level, each with its own separately-estimated smoothing parameter. This
## turned out to be unsafe for the smallest profiles: DM+CKD has only 215
## hosp-cohort records in total, and just 8 under age 20 (0 hospitalised) -
## with its own free smoothing parameter, mgcv found "not much penalty" fit
## that sparse handful of points, and the resulting curve plunged to a
## clearly implausible RR ~0.05 at age 1 (95% CI running from near 0 to
## >10). `bs = "ts"` shrinks a smooth toward flat, but only using evidence
## from *that profile's own* data - it cannot borrow strength from the other
## profiles.
##
## `s(age_years, core_profile, bs = "fs")` (a "factor-smooth interaction",
## Pedersen et al. 2019) fixes this: every profile still gets its own curve,
## but ALL profiles share ONE smoothing parameter, estimated mostly from the
## data-rich profiles (None, DM, HTN, DM+HTN). A sparse profile like DM+CKD
## is then held to that shared, sensible amount of smoothness instead of
## being allowed to fit noise - the small-group equivalent of a
## random-effect shrinkage prior on the smooth's shape.

fig_core_profile_age_curve_cache <- "01_Data/core_profile_rr_age_curve.RData"

if (file.exists(fig_core_profile_age_curve_cache)) {

  load(fig_core_profile_age_curve_cache)
  message("[12] reusing existing continuous age-curve core-profile RR")

} else {

fit_hosp_profile_ageint <- mgcv::bam(
  hosp_only ~
    core_profile +
    sex +
    year +
    s(uf_residence, bs = "re") +
    s(age_years, core_profile, bs = "fs", k = 10),
  data = hosp_cohort,
  family = binomial(link = "logit"),
  method = "fREML",
  discrete = TRUE
)

fit_death_profile_ageint <- mgcv::bam(
  death_only ~
    core_profile +
    sex +
    year +
    s(uf_residence, bs = "re") +
    s(age_years, core_profile, bs = "fs", k = 10),
  data = death_cohort,
  family = binomial(link = "logit"),
  method = "fREML",
  discrete = TRUE,
  na.action = na.exclude
)

summary(fit_hosp_profile_ageint)
summary(fit_death_profile_ageint)

## Standardises over the cohort's overall sex x year x state distribution
## (NOT over age - age is the curve's x-axis, evaluated one single year at
## a time) at every age in figure3_ages, for each profile in turn. B draws
## are processed in blocks of `draw_block_size` and immediately collapsed to
## a (n_age x block_size) weighted-average-risk matrix via one reshape +
## matrix multiply, rather than ever materialising the full
## (n_age * n_strata rows x B draws) matrix - the same memory-safety
## motivation as weighted_risk_draws() in section 8, adapted here to also
## keep the age dimension (that helper collapses stratum rows to a single
## pooled number; this one must keep one number per age).
make_profile_age_curve <- function(fit, data, B = 1000, seed = 1, draw_block_size = 200) {

  strata <- data |>
    dplyr::count(sex, year, uf_residence, name = "n") |>
    dplyr::mutate(weight = n / sum(n))

  n_strata <- nrow(strata)
  n_age <- length(figure3_ages)
  w_rep <- strata$weight

  beta_hat <- coef(fit)

  set.seed(seed)
  beta_draws <- MASS::mvrnorm(
    n = B,
    mu = beta_hat,
    Sigma = vcov(fit, unconditional = TRUE)
  )

  risk_point <- matrix(
    NA_real_,
    nrow = n_age,
    ncol = length(core_profile_levels),
    dimnames = list(NULL, core_profile_levels)
  )
  risk_draws <- array(
    NA_real_,
    dim = c(n_age, B, length(core_profile_levels)),
    dimnames = list(NULL, NULL, core_profile_levels)
  )

  for (p in core_profile_levels) {

    ## expand_grid() varies its LAST argument fastest, so every age's block
    ## of rows repeats `strata` in the same row order - which is what lets
    ## the matrix-reshape tricks below treat this as a simple
    ## (n_strata x n_age) layout without needing an explicit group-by.
    grid_p <- tidyr::expand_grid(age_years = figure3_ages, strata) |>
      dplyr::mutate(
        core_profile = factor(p, levels = core_profile_levels)
      )

    X_p <- predict(fit, newdata = grid_p, type = "lpmatrix")

    p_point <- plogis(as.numeric(X_p %*% beta_hat))
    risk_point[, p] <- colSums(matrix(p_point, nrow = n_strata) * w_rep)

    for (first_draw in seq.int(1, B, by = draw_block_size)) {

      draw_index <- first_draw:min(first_draw + draw_block_size - 1, B)

      p_block <- plogis(X_p %*% t(beta_draws[draw_index, , drop = FALSE]))

      ## Reshape (n_age*n_strata x block) -> (n_strata x n_age*block), take
      ## the weighted sum over strata in one matrix multiply, then reshape
      ## back to (n_age x block). Both reshapes are safe because R matrices
      ## are stored column-major and expand_grid's row order (above) means
      ## strata is the fastest-varying index within each age.
      mat2 <- matrix(p_block, nrow = n_strata)
      weighted_row <- w_rep %*% mat2
      risk_draws[, draw_index, p] <- matrix(weighted_row, nrow = n_age)
    }
  }

  rr_point <- sweep(risk_point, 1, risk_point[, "None"], FUN = "/")
  rr_draws <- sweep(risk_draws, c(1, 2), risk_draws[, , "None"], FUN = "/")

  purrr::map_dfr(core_profile_levels, function(p) {
    tibble::tibble(
      core_profile = factor(p, levels = core_profile_levels),
      age_years = figure3_ages,
      standardised_risk = risk_point[, p],
      rr = rr_point[, p],
      rr_lower = apply(rr_draws[, , p], 1, quantile, probs = 0.025, names = FALSE),
      rr_upper = apply(rr_draws[, , p], 1, quantile, probs = 0.975, names = FALSE)
    )
  })
}

core_profile_rr_hosp_age_curve <- make_profile_age_curve(
  fit_hosp_profile_ageint,
  hosp_cohort,
  seed = 201
) |>
  dplyr::mutate(outcome = "Hospitalisation")

core_profile_rr_death_age_curve <- make_profile_age_curve(
  fit_death_profile_ageint,
  death_cohort,
  seed = 202
) |>
  dplyr::mutate(outcome = "Death from chikungunya")

core_profile_rr_age_curve <- dplyr::bind_rows(
  core_profile_rr_hosp_age_curve,
  core_profile_rr_death_age_curve
) |>
  dplyr::mutate(
    outcome = factor(
      outcome,
      levels = c("Hospitalisation", "Death from chikungunya")
    )
  )

save(
  core_profile_rr_age_curve,
  file = fig_core_profile_age_curve_cache
)

}

## ------------------------------------------------------------
## Plot: same publication style as Figure 3 (section 10) -
## facet_grid(outcome ~ profile), one line + ribbon per panel,
## theme_figure3_nature(), PDF + 600dpi PNG. "None" is dropped from the
## facets (its curve is the trivial flat RR = 1 line already drawn by
## geom_hline in every other panel) so the 7 informative profiles get the
## space instead.
## ------------------------------------------------------------

fig3_profile_rr_data <- core_profile_rr_age_curve |>
  dplyr::filter(core_profile != "None") |>
  dplyr::mutate(core_profile = droplevels(core_profile))

fig3_profile_age_specific_relative_risk <- ggplot2::ggplot(
  fig3_profile_rr_data,
  ggplot2::aes(x = age_years, y = rr)
) +
  ggplot2::geom_hline(
    yintercept = 1,
    linetype = "dashed",
    linewidth = 0.4,
    colour = "#666666"
  ) +
  ggplot2::geom_ribbon(
    ggplot2::aes(ymin = rr_lower, ymax = rr_upper),
    fill = "#0072B2",
    alpha = 0.14,
    linewidth = 0,
    colour = NA
  ) +
  ggplot2::geom_line(colour = "#0072B2", linewidth = 0.8) +
  ggplot2::facet_grid(outcome ~ core_profile, scales = "free_y") +
  ggplot2::scale_x_continuous(
    breaks = seq(0, 100, by = 25),
    limits = range(figure3_ages),
    expand = ggplot2::expansion(mult = c(0.02, 0.03))
  ) +
  ggplot2::scale_y_log10(
    breaks = scales::log_breaks(n = 4),
    labels = scales::label_number(accuracy = 0.1)
  ) +
  ggplot2::labs(
    title = "Age-specific relative risk by joint core-3 profile",
    subtitle = paste0(
      "Each profile vs None, standardised over sex/year/state.\n",
      "s(age, profile, bs = \"fs\") shares one smoothing parameter across ",
      "profiles, so sparse ones (e.g. DM+CKD) borrow strength from the rest."
    ),
    x = "Age (years)",
    y = "Relative risk vs None (log scale)"
  ) +
  theme_figure3_nature() +
  ggplot2::theme(legend.position = "none")

fig3_profile_age_specific_relative_risk

ggplot2::ggsave(
  filename = file.path(fig_dir, "fig3_profile_age_specific_relative_risk.pdf"),
  plot = fig3_profile_age_specific_relative_risk,
  width = 300,
  height = 110,
  units = "mm",
  device = grDevices::cairo_pdf
)

ggplot2::ggsave(
  filename = file.path(fig_dir, "fig3_profile_age_specific_relative_risk.png"),
  plot = fig3_profile_age_specific_relative_risk,
  width = 300,
  height = 110,
  units = "mm",
  dpi = 600,
  bg = "white"
)
