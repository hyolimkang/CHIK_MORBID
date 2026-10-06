# ---------------------------------------------------------------------------
# 29_background_prevalence_core3_brazil.R
#
# DRAFT / exploratory - Brazil-only prototype for the first step of the
# GBD-background-prevalence -> joint-profile-RR -> population-burden
# extension (the 3-core, 8-profile analogue of 04_background_prevalence.R ->
# 05_corr_matrix.R -> 08_copula_simulation.R, which was built for the OLD
# 7-condition scheme). Final method for the combination categories (plain
# independence / SINAN-derived clustering ratio / full copula) is NOT yet
# decided - this script lays out and compares the options for one country
# (Brazil) before anything is generalised to other GBD countries.
#
# What this does, in order:
#   PART A - empirical check, using SINAN itself: does the independence
#            assumption under/over-estimate real DM/HTN/CKD co-occurrence?
#            (pooled, then by the same 5 age bands 12_ uses for
#            age_band_profile). Answer: yes, badly for the rarer
#            combinations - DM+HTN+CKD is observed ~66x more often than
#            independence predicts (pooled); 2-way combos ~2-10x; single
#            conditions are over-predicted by independence (ratio <1).
#   PART B - Brazil's 3 marginal prevalences on one 5-year age grid:
#              DM, CKD  <- gbd_prevalence.csv (sex_name == "Both", full
#                           0-100+ age range, no extrapolation needed)
#              HTN      <- ncd_hypertension.csv (Men/Women, ages 30-34 to
#                           75-79 only). Men/Women combined using SINAN's
#                           own proportion-male by age band (no sex-specific
#                           population counts available locally). Ages
#                           outside 30-79 are extrapolated using a
#                           logit-quadratic fit through the 10 observed
#                           bands (smooth, bounded in (0,1), reproduces the
#                           deceleration already visible in the data) rather
#                           than a flat carry-back/-forward, which left an
#                           implausible step right at the data boundary.
#            Below age 15 HTN is clamped to 0 (too far from any data to
#            extrapolate; paediatric primary hypertension is genuinely rare).
#   PART C - independence-assumption 8-profile prevalence (closed form, no
#            simulation needed: P(DM only) = p_DM*(1-p_HTN)*(1-p_CKD) etc.)
#   PART D - maximum-clustering (comonotonic / Frechet-Hoeffding upper
#            bound) 8-profile prevalence - the "other extreme": imagine the
#            whole population ranked on one underlying risk scale, with each
#            condition occupying the top p_k share of that ranking. Because
#            all 3 conditions share the SAME ranking, the rarer condition's
#            top slice always nests entirely inside the more common
#            conditions' top slices. Only 4 of the 8 profiles can be
#            non-zero under this extreme: None, (largest-prevalence
#            condition) only, (largest+middle) only, and all three -
#            "smallest only", "middle only", and any pairing that skips the
#            largest condition are structurally impossible.
#   PART E - two-extremes comparison table (independence vs max-clustering,
#            every age band x profile) -> exported to
#            03_Output/tables/brazil_profile_prevalence_two_extremes_PROTOTYPE.xlsx
#
# CAVEATS carried forward, not yet resolved:
#   - The true population distribution lies somewhere between Part C and
#     Part D's bounds, not at either extreme.
#   - Any attempt to pin down where in that range the truth sits currently
#     has to borrow Brazil SINAN's own observed DM/HTN/CKD correlation
#     structure (Part A) - this is the CHIKUNGUNYA CASE population, not a
#     general-population survey, so it carries the same transportability
#     assumption that 05_corr_matrix.R/08_copula_simulation.R already make
#     for the old 7-condition scheme (SINAN correlation as a proxy for
#     general-population correlation). No Brazil general-population
#     multimorbidity survey (e.g. PNS) is available locally yet.
#   - A single POOLED (all-age) SINAN clustering ratio breaks badly for the
#     rare DM+HTN+CKD cell (inflates it to ~75-95% of the 80+ population -
#     clearly impossible) because the pooled ratio is dominated by noisy,
#     near-zero-denominator estimates from the youngest age bands. Using
#     the same 5 age bands as 12_'s age_band_profile instead avoids that
#     specific failure, but introduces step discontinuities at the 20/40/
#     60/80 boundaries and the youngest-band DM+HTN+CKD estimate (n=174,
#     ratio ~7869x) is still small-sample-unstable.
#
# Needs: 01_Data/gbd_prevalence.csv, 01_Data/ncd_hypertension.csv,
#        01_Data/chik_sinan_individual_2015_2025.rds
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stringr); library(purrr)
  library(ggplot2); library(scales); library(openxlsx)
})
options(scipen = 999)

fig_dir <- "03_Output/figures"
table_dir <- "03_Output/tables"

core_profile_levels <- c("None","DM","HTN","CKD","DM+HTN","DM+CKD","HTN+CKD","DM+HTN+CKD")

## ============================================================
## PART A: empirical check - does independence under/over-estimate real
## DM/HTN/CKD co-occurrence in Brazil SINAN?
## ============================================================

ind <- readRDS("01_Data/chik_sinan_individual_2015_2025.rds")
core_cols <- c("diabetes", "hypertension", "renal_disease")

sinan3 <- ind |>
  dplyr::filter(
    is_confirmed_chik,
    lubridate::year(event_date) >= 2017,
    !is.na(age_years), age_years >= 0, age_years <= 100,
    sex %in% c("male", "female"),
    dplyr::if_all(dplyr::all_of(core_cols), ~ .x %in% c("no", "yes"))
  ) |>
  dplyr::mutate(
    dm  = as.integer(diabetes == "yes"),
    htn = as.integer(hypertension == "yes"),
    ckd = as.integer(renal_disease == "yes")
  )

n_tot <- nrow(sinan3)
p_dm  <- mean(sinan3$dm); p_htn <- mean(sinan3$htn); p_ckd <- mean(sinan3$ckd)

message(sprintf("[29][A] SINAN pooled marginals (N=%s): p_DM=%.4f  p_HTN=%.4f  p_CKD=%.4f",
                 format(n_tot, big.mark = ","), p_dm, p_htn, p_ckd))

profile_label <- function(dm, htn, ckd) {
  dplyr::case_when(
    dm==0 & htn==0 & ckd==0 ~ "None", dm==1 & htn==0 & ckd==0 ~ "DM",
    dm==0 & htn==1 & ckd==0 ~ "HTN",  dm==0 & htn==0 & ckd==1 ~ "CKD",
    dm==1 & htn==1 & ckd==0 ~ "DM+HTN", dm==1 & htn==0 & ckd==1 ~ "DM+CKD",
    dm==0 & htn==1 & ckd==1 ~ "HTN+CKD", dm==1 & htn==1 & ckd==1 ~ "DM+HTN+CKD"
  )
}

sinan_vs_independence_pooled <- sinan3 |>
  dplyr::count(dm, htn, ckd) |>
  dplyr::mutate(
    p_obs = n / n_tot,
    p_indep = ifelse(dm==1, p_dm, 1-p_dm) * ifelse(htn==1, p_htn, 1-p_htn) * ifelse(ckd==1, p_ckd, 1-p_ckd),
    profile = profile_label(dm, htn, ckd),
    ratio_obs_over_indep = p_obs / p_indep
  ) |>
  dplyr::arrange(dm, htn, ckd) |>
  dplyr::select(profile, dm, htn, ckd, n, p_obs, p_indep, ratio_obs_over_indep)

## Same check, stratified by the 5 age bands used in 12_'s age_band_profile
## (so a later SINAN-based clustering-ratio calibration, if used, has
## somewhere less unstable than the pooled ratio to draw on).
sinan3 <- sinan3 |>
  dplyr::mutate(age_band_profile = cut(age_years, breaks = c(0, 20, 40, 60, 80, 101),
                                        labels = c("0-19","20-39","40-59","60-79","80+"),
                                        right = FALSE, include.lowest = TRUE))

sinan_vs_independence_by_age <- sinan3 |>
  dplyr::group_by(age_band_profile) |>
  dplyr::group_modify(~{
    d <- .x; n <- nrow(d)
    pd <- mean(d$dm); ph <- mean(d$htn); pc <- mean(d$ckd)
    d |>
      dplyr::count(dm, htn, ckd) |>
      tidyr::complete(dm = 0:1, htn = 0:1, ckd = 0:1, fill = list(n = 0)) |>
      dplyr::mutate(
        p_obs = n / !!n,
        p_indep = ifelse(dm==1, pd, 1-pd) * ifelse(htn==1, ph, 1-ph) * ifelse(ckd==1, pc, 1-pc),
        profile = profile_label(dm, htn, ckd),
        r_k = p_obs / p_indep
      ) |>
      dplyr::select(profile, n, p_obs, p_indep, r_k)
  }) |>
  dplyr::ungroup()

message("[29][A] Pooled and age-banded observed-vs-independence ratios computed (see sinan_vs_independence_pooled / _by_age).")

## ============================================================
## PART B: Brazil's 3 marginal prevalences, 5-year age grid
## ============================================================

keep_age <- c("<5","5-9","10-14","15-19","20-24","25-29","30-34","35-39",
              "40-44","45-49","50-54","55-59","60-64","65-69","70-74",
              "75-79","80-84","85-89","90-94","95+")

age_lookup <- tibble::tibble(age_clean = keep_age) |>
  dplyr::mutate(
    age_start = dplyr::case_when(
      stringr::str_detect(age_clean, "^<")  ~ 0,
      stringr::str_detect(age_clean, "\\+") ~ as.numeric(stringr::str_remove(age_clean, "\\+")),
      TRUE ~ as.numeric(stringr::str_extract(age_clean, "^[0-9]+"))
    ),
    age_end = dplyr::case_when(
      stringr::str_detect(age_clean, "^<")  ~ as.numeric(stringr::str_remove(age_clean, "^<")) - 1,
      stringr::str_detect(age_clean, "\\+") ~ Inf,
      TRUE ~ as.numeric(stringr::str_extract(age_clean, "[0-9]+$"))
    )
  )

## ---- B1. GBD: DM + CKD, Brazil, Both sex, full age range -------------------

gbd_raw <- read.csv("01_Data/gbd_prevalence.csv")

gbd_bra <- gbd_raw |>
  dplyr::filter(
    measure_name == "Prevalence", metric_name == "Rate",
    location_name == "Brazil", sex_name == "Both",
    cause_name %in% c("Diabetes mellitus", "Chronic kidney disease")
  ) |>
  dplyr::mutate(
    age_clean = stringr::str_remove(age_name, " years| year"),
    condition = ifelse(cause_name == "Diabetes mellitus", "DM", "CKD"),
    prevalence = val / 1e5
  ) |>
  dplyr::filter(age_clean %in% keep_age) |>
  dplyr::left_join(age_lookup, by = "age_clean") |>
  dplyr::select(condition, age_clean, age_start, age_end, prevalence)

## ---- B2. HTN: NCD-RisC Brazil M/F, weighted by SINAN's own sex ratio ------

htn_raw <- read.csv("01_Data/ncd_hypertension.csv", check.names = FALSE)

htn_bra <- htn_raw |>
  dplyr::filter(Country == "Brazil", Year == max(Year)) |>
  dplyr::transmute(sex = tolower(Sex), age = Age, prevalence = `Prevalence of hypertension`)

## No sex-specific population counts available locally (gbd_pop.csv and
## ncd_hypertension.csv both lack them) - SINAN's own proportion-male by age
## band is used as a practical substitute. This is the chikungunya-case sex
## ratio, not the Brazilian general-population sex ratio; fine for a
## Brazil-only prototype, but should become a real census sex ratio before
## this is generalised to other countries.
sinan_sex_by_band <- sinan3 |>
  dplyr::mutate(band = cut(age_years, breaks = c(seq(30, 80, by = 5), Inf),
                            labels = c("30-34","35-39","40-44","45-49","50-54","55-59",
                                       "60-64","65-69","70-74","75-79","80+"),
                            right = FALSE, include.lowest = TRUE)) |>
  dplyr::filter(!is.na(band), band != "80+") |>
  dplyr::group_by(band) |>
  dplyr::summarise(prop_male = mean(sex == "male"), n = dplyr::n(), .groups = "drop")

htn_wide <- htn_bra |>
  tidyr::pivot_wider(names_from = sex, values_from = prevalence) |>
  dplyr::left_join(sinan_sex_by_band, by = c("age" = "band")) |>
  dplyr::mutate(
    prop_male = ifelse(is.na(prop_male), 0.5, prop_male),
    prevalence = prop_male * men + (1 - prop_male) * women
  ) |>
  dplyr::select(age, prevalence)

## Extrapolate beyond the observed 30-34..75-79 range. logit(prevalence) vs
## age-band midpoint is close to a smooth, slightly-decelerating curve within
## the observed data (per-band logit increments shrink from ~0.36 to ~0.26
## across the 10 bands) - fit logit(prevalence) ~ poly(age_mid, 2) and use it
## for the SHORT extrapolation just outside the data (15-29 below, 80-95+
## above); a flat carry-back/-forward instead leaves an implausible step
## right at the data boundary (0% -> ~20% at age 30 in the original version
## of this check). Below age 15 (3+ bands outside the fitted range), clamp
## to 0 - too far from any data to trust an extrapolation of an adults-only
## curve, and clinically justified (paediatric primary hypertension is rare).
htn_fit_data <- htn_wide |>
  dplyr::left_join(age_lookup, by = c("age" = "age_clean")) |>
  dplyr::mutate(age_mid = (age_start + age_end) / 2, logit_p = qlogis(prevalence))

htn_logit_fit <- lm(logit_p ~ poly(age_mid, 2), data = htn_fit_data)

htn_bra_full <- age_lookup |>
  dplyr::left_join(htn_wide, by = c("age_clean" = "age")) |>
  dplyr::mutate(
    age_mid_pred = ifelse(is.infinite(age_end), age_start + 2.5, (age_start + age_end) / 2),
    fitted_logit = predict(htn_logit_fit, newdata = data.frame(age_mid = age_mid_pred)),
    prevalence = dplyr::case_when(
      !is.na(prevalence) ~ prevalence,
      age_start < 15     ~ 0,
      TRUE               ~ plogis(fitted_logit)
    ),
    condition = "HTN"
  ) |>
  dplyr::select(condition, age_clean, age_start, age_end, prevalence)

stopifnot(!anyNA(htn_bra_full$prevalence))

## ---- B3. Combine ------------------------------------------------------

bg_prev_brazil3 <- dplyr::bind_rows(gbd_bra, htn_bra_full) |>
  dplyr::select(age_clean, age_start, age_end, condition, prevalence) |>
  tidyr::pivot_wider(names_from = condition, values_from = prevalence) |>
  dplyr::arrange(age_start)

message("[29][B] bg_prev_brazil3 built: 3-condition marginal prevalence, Brazil, 20 age bands.")

## ============================================================
## PART C: independence-assumption 8-profile prevalence (closed form)
## ============================================================

profile_prev_brazil <- bg_prev_brazil3 |>
  dplyr::mutate(
    None          = (1-DM)*(1-HTN)*(1-CKD),
    DM_only       = DM*(1-HTN)*(1-CKD),
    HTN_only      = (1-DM)*HTN*(1-CKD),
    CKD_only      = (1-DM)*(1-HTN)*CKD,
    `DM+HTN`      = DM*HTN*(1-CKD),
    `DM+CKD`      = DM*(1-HTN)*CKD,
    `HTN+CKD`     = (1-DM)*HTN*CKD,
    `DM+HTN+CKD`  = DM*HTN*CKD
  ) |>
  dplyr::rename(DM_marg = DM, HTN_marg = HTN, CKD_marg = CKD) |>
  dplyr::rename(DM = DM_only, HTN = HTN_only, CKD = CKD_only)

stopifnot(all(abs(rowSums(profile_prev_brazil[, core_profile_levels]) - 1) < 1e-9))
message("[29][C] profile_prev_brazil (independence) built; row sums verified = 1.")

## ============================================================
## PART D: maximum-clustering (comonotonic) 8-profile prevalence
## ============================================================
## Everyone ranked on one underlying risk scale; each condition occupies the
## top p_k share of that ranking. Shared ranking => the rarer condition's
## top slice nests entirely inside the more common conditions' top slices.
## Only 4 of 8 profiles can be non-zero: None, (largest) only,
## (largest+middle) only, all three.

condition_order <- c("DM", "HTN", "CKD")
pair_label <- function(a, b) paste(condition_order[condition_order %in% c(a, b)], collapse = "+")

max_clustering_row <- function(p_dm, p_htn, p_ckd) {
  p <- c(DM = p_dm, HTN = p_htn, CKD = p_ckd)
  ord <- names(sort(p))  # ascending: smallest, middle, largest
  smallest <- ord[1]; middle <- ord[2]; largest <- ord[3]
  p_min <- p[[smallest]]; p_mid <- p[[middle]]; p_max <- p[[largest]]

  out <- setNames(rep(0, length(core_profile_levels)), core_profile_levels)
  out["None"] <- 1 - p_max
  out[largest] <- p_max - p_mid
  out[pair_label(largest, middle)] <- p_mid - p_min
  out["DM+HTN+CKD"] <- p_min
  out
}

max_clustering_brazil <- bg_prev_brazil3 |>
  dplyr::rename(DM_marg = DM, HTN_marg = HTN, CKD_marg = CKD) |>
  dplyr::mutate(profs = purrr::pmap(list(DM_marg, HTN_marg, CKD_marg), max_clustering_row)) |>
  tidyr::unnest_wider(profs)

stopifnot(all(abs(rowSums(max_clustering_brazil[, core_profile_levels]) - 1) < 1e-9))
message("[29][D] max_clustering_brazil (comonotonic) built; row sums verified = 1.")

save(bg_prev_brazil3, profile_prev_brazil, max_clustering_brazil,
     sinan_vs_independence_pooled, sinan_vs_independence_by_age,
     file = "01_Data/brazil_profile_prevalence_two_extremes.RData")

## ============================================================
## PART E: comparison table + figure
## ============================================================

indep_long <- profile_prev_brazil |>
  dplyr::select(age_clean, dplyr::all_of(core_profile_levels)) |>
  tidyr::pivot_longer(dplyr::all_of(core_profile_levels), names_to = "profile", values_to = "independence")

max_long <- max_clustering_brazil |>
  dplyr::select(age_clean, dplyr::all_of(core_profile_levels)) |>
  tidyr::pivot_longer(dplyr::all_of(core_profile_levels), names_to = "profile", values_to = "max_clustering")

marginals <- profile_prev_brazil |> dplyr::select(age_clean, age_start, DM_marg, HTN_marg, CKD_marg)

two_extremes_table <- indep_long |>
  dplyr::left_join(max_long, by = c("age_clean", "profile")) |>
  dplyr::left_join(marginals, by = "age_clean") |>
  dplyr::mutate(profile = factor(profile, levels = core_profile_levels)) |>
  dplyr::arrange(age_start, profile) |>
  dplyr::mutate(
    profile = as.character(profile),
    independence_pct = sprintf("%.2f%%", independence * 100),
    max_clustering_pct = sprintf("%.2f%%", max_clustering * 100),
    bound_width_pp = sprintf("%.2f", (max_clustering - independence) * 100)
  ) |>
  dplyr::select(age_band = age_clean, profile, DM_marg, HTN_marg, CKD_marg,
                independence, independence_pct, max_clustering, max_clustering_pct, bound_width_pp)

wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "two_extremes")
openxlsx::writeDataTable(wb, "two_extremes", two_extremes_table, withFilter = TRUE)
openxlsx::setColWidths(wb, "two_extremes", cols = 1:10, widths = "auto")
openxlsx::saveWorkbook(wb, file.path(table_dir, "brazil_profile_prevalence_two_extremes_PROTOTYPE.xlsx"), overwrite = TRUE)

profile_colours <- c(
  "None" = "#D9D9D9", "DM" = "#4C72B0", "HTN" = "#DD8452", "CKD" = "#55A868",
  "DM+HTN" = "#C44E52", "DM+CKD" = "#8172B2", "HTN+CKD" = "#937860",
  "DM+HTN+CKD" = "#CCB974"
)

plot_both <- dplyr::bind_rows(
  indep_long |> dplyr::rename(prevalence = independence) |> dplyr::mutate(method = "1. Independence (minimum clustering)"),
  max_long |> dplyr::rename(prevalence = max_clustering) |> dplyr::mutate(method = "2. Maximum clustering (comonotonic)")
) |>
  dplyr::left_join(age_lookup, by = "age_clean") |>
  dplyr::mutate(
    age_mid = ifelse(is.infinite(age_end), age_start + 2.5, (age_start + age_end) / 2),
    profile = factor(profile, levels = rev(core_profile_levels)),
    method = factor(method, levels = c("1. Independence (minimum clustering)", "2. Maximum clustering (comonotonic)"))
  )

fig_two_extremes <- ggplot2::ggplot(plot_both, ggplot2::aes(x = age_mid, y = prevalence, fill = profile)) +
  ggplot2::geom_area(position = "stack", colour = "white", linewidth = 0.15) +
  ggplot2::facet_wrap(~method, ncol = 2) +
  ggplot2::scale_fill_manual(values = profile_colours, breaks = core_profile_levels) +
  ggplot2::scale_x_continuous(breaks = seq(0, 100, 20), expand = c(0, 0)) +
  ggplot2::scale_y_continuous(labels = scales::percent, expand = c(0, 0)) +
  ggplot2::labs(
    title = "Brazil DM/HTN/CKD joint-profile prevalence: the two theoretical bounds",
    subtitle = "Both panels use only the 3 marginal prevalences (GBD + NCD-RisC) - no SINAN data. The true population value lies between them.",
    x = "Age (years)", y = "Share of population", fill = NULL
  ) +
  ggplot2::theme_classic(base_size = 10.5) +
  ggplot2::theme(legend.position = "bottom")

ggplot2::ggsave(file.path(fig_dir, "fig_profile_prevalence_brazil_two_extremes_PROTOTYPE.png"),
                fig_two_extremes, width = 260, height = 130, units = "mm", dpi = 300, bg = "white")

message("[29][E] Saved table: 03_Output/tables/brazil_profile_prevalence_two_extremes_PROTOTYPE.xlsx")
message("[29][E] Saved figure: 03_Output/figures/fig_profile_prevalence_brazil_two_extremes_PROTOTYPE.png")
message("[29] DONE.")
