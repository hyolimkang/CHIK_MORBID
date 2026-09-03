# ===========================================================================
# descriptive.R
#
# 목적
# ----
# SINAN chikungunya 환자 자료 (2015-2024)로 comorbidity 관련 기술분석:
#
#   (1) 연령그룹별로 comorbidity 보유 여부에 따른
#         a. 사례(case) 분포 (%)
#         b. 입원율   (hospitalised / cases, %)
#         c. 치명률   (death / cases, %)
#       을 막대그래프로 비교.
#
#   (2) 연령그룹별로 전체 case / hospitalisation / death 중에서
#       comorbidity = yes / no / missing-not reported 가 차지하는 비중 (stacked bar).
#       → "어느 연령에서 comorbidity 자료가 얼마나 결측인지"도 같이 보임.
#
#   (3) 개별 comorbidity 종류별 prevalence (line graph).
#       각 comorbidity (diabetes, hypertension, ...)이 case / hosp / death
#       군에서 얼마나 흔한지를 연령 그룹을 따라 그림.
#
#   (4) (comorbidity 보유 여부 × 연령 그룹) 단위로 Lorenz curve.
#       사례를 사망률 기준으로 정렬했을 때 누적 사망의 집중도(불평등).
#       곡선 아래 영역 음영 처리 + Gini 계수 표시.
#
# 입력
# ----
#   01_Data/sinan_chik_csv/CHIKBR{YY}.csv.zip
#     fetch_chik_sinan_brazil.R 로 미리 받아두어야 함.
#
# 출력
# ----
#   01_Data/chik_analytic.rds            전처리된 분석용 tibble (캐시)
#   03_Output/fig_01_outcome_by_age_comorb.png
#   03_Output/fig_02_comorb_share_by_age.png
#   03_Output/fig_03_comorb_prevalence_by_type.png
#   03_Output/fig_04_lorenz_age_comorb.png
#   03_Output/tab_summary_by_age_comorb.csv
#
# 비고
# ----
# - 코드 스타일은 base pipe `|>` 위주 (R >= 4.1).
# - tidyverse 만으로 충분하고, comorbidity 결측을 일관되게 다루기 위해
#   recode 단계에서 1=yes / 2=no / (9 또는 NA)=missing 으로 통일.
# ===========================================================================


# ---- 0. Packages ----------------------------------------------------------

pkgs <- c("here", "dplyr", "tidyr", "readr", "stringr",
          "ggplot2", "forcats", "scales", "purrr")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}
suppressPackageStartupMessages({
  library(here);   library(dplyr);   library(tidyr);   library(readr)
  library(stringr); library(ggplot2); library(forcats); library(scales)
  library(purrr)
})


# ---- 1. Config ------------------------------------------------------------

YEAR_START <- 2015
YEAR_END   <- 2024

ZIP_DIR  <- here::here("01_Data", "sinan_chik_csv")
OUT_DIR  <- here::here("03_Output")
CACHE    <- here::here("01_Data", "descriptive_appendix", "chik_analytic.rds")

dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# SINAN chikungunya 의 표준 comorbidity 변수.
# 모든 연도에 다 있지 않을 수 있어, 없는 컬럼은 자동으로 NA로 채움.
COMORB_VARS <- c(
  DIABETES   = "Diabetes",
  HEMATOLOG  = "Haematological",
  HEPATOPAT  = "Hepatic",
  RENAL      = "Renal",
  HIPERTENSA = "Hypertension",
  ACIDO_PEPT = "Acid-peptic",
  AUTO_IMUNE = "Autoimmune"
)

# 분석에 필요한 SINAN 컬럼만 골라 읽어들이는 화이트리스트.
KEEP_COLS <- c(
  "DT_NOTIFIC", "NU_ANO",
  "NU_IDADE_N", "CS_SEXO",
  "HOSPITALIZ", "EVOLUCAO", "DT_OBITO",
  names(COMORB_VARS)
)


# ---- 2. Helpers -----------------------------------------------------------

# SINAN NU_IDADE_N : 4-digit 코드. 첫 자리 = 단위, 뒤 3자리 = 값.
#   1xxx = xx 시간,  2xxx = xx 일,  3xxx = xx 개월,  4xxx = xx 년
decode_age_years <- function(x) {
  x <- suppressWarnings(as.integer(x))
  unit <- x %/% 1000
  val  <- x %%  1000
  dplyr::case_when(
    is.na(x)   ~ NA_real_,
    unit == 4  ~ as.numeric(val),
    unit == 3  ~ val / 12,
    unit == 2  ~ val / 365.25,
    unit == 1  ~ val / (24 * 365.25),
    TRUE       ~ NA_real_
  )
}

# 1=yes / 2=no / 9 또는 NA = missing.  결과는 factor.
recode_yn <- function(x) {
  x <- suppressWarnings(as.integer(x))
  out <- dplyr::case_when(
    x == 1L ~ "yes",
    x == 2L ~ "no",
    TRUE    ~ "missing"     # 9 또는 NA 또는 그 외 코드 모두 missing
  )
  factor(out, levels = c("yes", "no", "missing"))
}

# 한 해 CSV.zip을 읽어 KEEP_COLS만 추출.
read_one_year <- function(zip_path) {
  message("[read]  ", basename(zip_path))

  # SINAN CHIKBR: comma-separated (see clean_chik_sinan_brazil.R), Latin1.
  raw <- tryCatch(
    readr::read_delim(
      zip_path,
      delim         = ",",
      locale        = readr::locale(encoding = "Latin1"),
      col_types     = readr::cols(.default = readr::col_character()),
      show_col_types = FALSE,
      progress      = FALSE
    ),
    error = function(e) {
      message("  -> read failed: ", conditionMessage(e)); NULL
    }
  )
  if (is.null(raw)) return(NULL)

  # KEEP_COLS 중 실제 존재하는 것만 추리고, 없는 컬럼은 NA로 채워서 같은 폭 유지.
  present <- intersect(KEEP_COLS, names(raw))
  out <- raw |> dplyr::select(dplyr::all_of(present))
  missing_cols <- setdiff(KEEP_COLS, present)
  for (mc in missing_cols) out[[mc]] <- NA_character_

  out <- out |> dplyr::select(dplyr::all_of(KEEP_COLS))
  message(sprintf("  -> %s rows", format(nrow(out), big.mark = ",")))
  out
}


# ---- 3. Load + clean (캐시 사용) -----------------------------------------

if (file.exists(CACHE)) {
  message("[cache] using ", CACHE)
  dat <- readRDS(CACHE)
} else {
  zips <- sort(list.files(ZIP_DIR, pattern = "^CHIKBR\\d{2}\\.csv\\.zip$",
                          full.names = TRUE))
  if (length(zips) == 0) {
    stop("01_Data/sinan_chik_csv/ 에 CHIKBR*.csv.zip 이 없습니다.\n",
         "먼저 02_Script/fetch_chik_sinan_brazil.R 을 실행해 raw 자료를 받아주세요.")
  }

  raw <- purrr::map(zips, read_one_year) |>
    purrr::compact() |>
    dplyr::bind_rows()

  message("[clean] recoding age / comorbidity / outcomes ...")

  dat <- raw |>
    dplyr::mutate(
      age_years    = decode_age_years(NU_IDADE_N),
      year         = suppressWarnings(as.integer(NU_ANO)),
      sex          = dplyr::case_when(CS_SEXO == "M" ~ "M",
                                      CS_SEXO == "F" ~ "F",
                                      TRUE           ~ NA_character_),
      hospitalised = recode_yn(HOSPITALIZ),
      # death = chikungunya로 인한 사망 (EVOLUCAO = 2).
      # EVOLUCAO 3 = 다른 원인 사망. all-cause 가 필요하면 c(2,3) 으로 변경.
      death        = dplyr::case_when(
        suppressWarnings(as.integer(EVOLUCAO)) == 2L ~ "yes",
        suppressWarnings(as.integer(EVOLUCAO)) %in% c(1L, 3L) ~ "no",
        TRUE ~ "missing"
      ) |> factor(levels = c("yes", "no", "missing"))
    ) |>
    # 개별 comorbidity 변수를 recode_yn 으로 일괄 처리.
    dplyr::mutate(dplyr::across(dplyr::all_of(names(COMORB_VARS)), recode_yn,
                                .names = "cm_{.col}")) |>
    dplyr::select(year, age_years, sex,
                  hospitalised, death,
                  dplyr::starts_with("cm_"))

  # 연령 그룹.
  age_breaks <- c(-Inf, 1, 5, 15, 30, 45, 60, 75, Inf)
  age_labels <- c("<1", "1-4", "5-14", "15-29", "30-44",
                  "45-59", "60-74", "75+")
  dat <- dat |>
    dplyr::mutate(
      age_group = cut(age_years, breaks = age_breaks,
                      labels = age_labels, right = FALSE,
                      include.lowest = TRUE)
    )

  # any_comorbidity: 하나라도 yes 면 yes, 모두 no 면 no, 그 외(전부 또는 일부 missing)면 missing.
  cm_cols <- grep("^cm_", names(dat), value = TRUE)
  has_yes <- dat |>
    dplyr::select(dplyr::all_of(cm_cols)) |>
    purrr::pmap_lgl(function(...) any(c(...) == "yes",     na.rm = TRUE))
  all_no  <- dat |>
    dplyr::select(dplyr::all_of(cm_cols)) |>
    purrr::pmap_lgl(function(...) all(c(...) == "no",      na.rm = TRUE) &&
                                  !any(c(...) == "missing", na.rm = TRUE))
  dat$any_comorbidity <- dplyr::case_when(
    has_yes ~ "yes",
    all_no  ~ "no",
    TRUE    ~ "missing"
  ) |> factor(levels = c("yes", "no", "missing"))

  saveRDS(dat, CACHE)
  message("[save]  ", CACHE, " (", format(nrow(dat), big.mark = ","), " rows)")
}


# ---- 4. 공통 색상 / 테마 -------------------------------------------------

theme_set(theme_minimal(base_size = 11) +
            theme(panel.grid.minor = element_blank(),
                  strip.background = element_rect(fill = "grey92", colour = NA),
                  legend.position  = "bottom"))

col_comorb <- c(yes = "#C0392B", no = "#2E86C1", missing = "grey60")
col_outcome <- c(case = "#7F8C8D", hosp = "#F39C12", death = "#C0392B")


# ---- 5. Analysis 1 --------------------------------------------------------
# 연령 그룹 × comorbidity(yes/no) 별 case 분포, 입원율, 치명률 비교.
# (missing 은 분모 왜곡을 피하기 위해 1번 그림에서는 빼고, 2번 그림에서 따로 보여줌)

tab1 <- dat |>
  dplyr::filter(!is.na(age_group), any_comorbidity %in% c("yes", "no")) |>
  dplyr::mutate(any_comorbidity = droplevels(any_comorbidity)) |>
  dplyr::group_by(age_group, any_comorbidity) |>
  dplyr::summarise(
    cases  = dplyr::n(),
    hosp   = sum(hospitalised == "yes", na.rm = TRUE),
    deaths = sum(death        == "yes", na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::group_by(any_comorbidity) |>
  dplyr::mutate(case_pct = cases / sum(cases) * 100) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    hosp_rate  = hosp   / cases * 100,
    death_rate = deaths / cases * 100
  )

# wide → long 으로 펴서 facet 하기 쉽게.
tab1_long <- tab1 |>
  dplyr::select(age_group, any_comorbidity,
                `Case distribution (%)` = case_pct,
                `Hospitalisation (%)`   = hosp_rate,
                `Case fatality (%)`     = death_rate) |>
  tidyr::pivot_longer(c(-age_group, -any_comorbidity),
                      names_to = "metric", values_to = "pct") |>
  dplyr::mutate(metric = factor(metric,
                                levels = c("Case distribution (%)",
                                           "Hospitalisation (%)",
                                           "Case fatality (%)")))

p1 <- ggplot(tab1_long,
             aes(x = age_group, y = pct, fill = any_comorbidity)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  facet_wrap(~ metric, scales = "free_y", ncol = 1) +
  scale_fill_manual(values = col_comorb, name = "Comorbidity") +
  labs(x = "Age group (years)", y = NULL,
       title = "Outcome by age group, stratified by comorbidity status",
       subtitle = "SINAN Brazil chikungunya, 2015-2024 (missing-not reported excluded here)") +
  theme(axis.text.x = element_text(angle = 0))

ggsave(file.path(OUT_DIR, "fig_01_outcome_by_age_comorb.png"),
       p1, width = 8, height = 9, dpi = 150)


# ---- 6. Analysis 2 --------------------------------------------------------
# 연령 그룹별 전체 case / hosp / death 중 comorbidity yes/no/missing 비중.
# missing-not reported 도 같이 보여줌.

tab2 <- dat |>
  dplyr::filter(!is.na(age_group)) |>
  dplyr::group_by(age_group, any_comorbidity) |>
  dplyr::summarise(
    case  = dplyr::n(),
    hosp  = sum(hospitalised == "yes", na.rm = TRUE),
    death = sum(death        == "yes", na.rm = TRUE),
    .groups = "drop"
  ) |>
  tidyr::pivot_longer(c(case, hosp, death),
                      names_to = "outcome", values_to = "n") |>
  dplyr::group_by(age_group, outcome) |>
  dplyr::mutate(share = n / sum(n)) |>
  dplyr::ungroup() |>
  dplyr::mutate(outcome = factor(outcome,
                                 levels = c("case", "hosp", "death"),
                                 labels = c("Cases",
                                            "Hospitalisations",
                                            "Deaths")))

p2 <- ggplot(tab2,
             aes(x = age_group, y = share, fill = any_comorbidity)) +
  geom_col(position = "stack", width = 0.85) +
  facet_wrap(~ outcome, ncol = 1) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = col_comorb, name = "Comorbidity") +
  labs(x = "Age group (years)", y = "Share of total within age group",
       title = "Share of comorbidity status within cases, hospitalisations, deaths",
       subtitle = "Missing/not-reported shown explicitly so reporting completeness is visible")

ggsave(file.path(OUT_DIR, "fig_02_comorb_share_by_age.png"),
       p2, width = 9, height = 9, dpi = 150)


# ---- 7. Analysis 3 --------------------------------------------------------
# 개별 comorbidity 종류별 prevalence (cases / hosp / death 안에서) by age group.

# 도움함수: 한 outcome 안에서 각 cm 변수의 'yes' 비율을 연령그룹별로 계산.
prev_one <- function(df, label) {
  cm_cols <- grep("^cm_", names(df), value = TRUE)
  df |>
    dplyr::select(age_group, dplyr::all_of(cm_cols)) |>
    tidyr::pivot_longer(-age_group, names_to = "comorbidity", values_to = "v") |>
    dplyr::group_by(age_group, comorbidity) |>
    dplyr::summarise(
      n_reported = sum(v %in% c("yes", "no"), na.rm = TRUE),
      n_yes      = sum(v == "yes",            na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      prev    = ifelse(n_reported > 0, n_yes / n_reported, NA_real_),
      outcome = label
    )
}

tab3 <- dplyr::bind_rows(
  prev_one(dat,                                                "Cases"),
  prev_one(dplyr::filter(dat, hospitalised == "yes"),          "Hospitalisations"),
  prev_one(dplyr::filter(dat, death        == "yes"),          "Deaths")
) |>
  dplyr::filter(!is.na(age_group)) |>
  dplyr::mutate(
    outcome     = factor(outcome,
                         levels = c("Cases", "Hospitalisations", "Deaths")),
    comorbidity = stringr::str_remove(comorbidity, "^cm_"),
    comorbidity = dplyr::recode(comorbidity, !!!COMORB_VARS)
  )

p3 <- ggplot(tab3,
             aes(x = age_group, y = prev, colour = outcome, group = outcome)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.6) +
  facet_wrap(~ comorbidity, scales = "free_y") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_colour_manual(values = c(Cases             = col_outcome[["case"]],
                                 Hospitalisations  = col_outcome[["hosp"]],
                                 Deaths            = col_outcome[["death"]]),
                      name = NULL) +
  labs(x = "Age group (years)",
       y = "Prevalence among reported (yes / yes+no)",
       title = "Prevalence of each comorbidity within cases, hospitalisations, deaths",
       subtitle = "Denominator excludes missing/ignored entries (so reporting gaps don't dilute prevalence)") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave(file.path(OUT_DIR, "fig_03_comorb_prevalence_by_type.png"),
       p3, width = 11, height = 7.5, dpi = 150)


# ---- 8. Analysis 4 --------------------------------------------------------
# Lorenz curve: (comorbidity status × 연령 그룹) 을 사망률 기준으로 정렬해
# 누적 case 대비 누적 death의 분포. 곡선 아래 영역 음영 + Gini.
#
# 해석: 곡선이 대각선 (perfect equality) 에서 아래로 멀어질수록,
# 소수의 (age × comorbidity) 그룹이 대부분의 사망을 차지함 (불평등 큼).

tab4 <- dat |>
  dplyr::filter(!is.na(age_group),
                any_comorbidity %in% c("yes", "no", "missing")) |>
  dplyr::group_by(any_comorbidity, age_group) |>
  dplyr::summarise(
    cases  = dplyr::n(),
    deaths = sum(death == "yes", na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::filter(cases > 0) |>
  dplyr::mutate(death_rate = deaths / cases) |>
  dplyr::arrange(death_rate) |>
  dplyr::mutate(
    cum_cases  = cumsum(cases)  / sum(cases),
    cum_deaths = cumsum(deaths) / sum(deaths)
  )

# Lorenz curve의 시작점 (0, 0) 추가.
lorenz_df <- dplyr::bind_rows(
  tibble::tibble(cum_cases = 0, cum_deaths = 0,
                 any_comorbidity = NA, age_group = NA,
                 cases = 0L, deaths = 0L, death_rate = 0),
  tab4
)

# Gini = 1 - 2 * (Lorenz curve 아래 면적). trapezoidal rule로 적분.
xv <- lorenz_df$cum_cases
yv <- lorenz_df$cum_deaths
auc <- sum(diff(xv) * (head(yv, -1) + tail(yv, -1)) / 2)
gini <- 1 - 2 * auc

p4 <- ggplot(lorenz_df, aes(x = cum_cases, y = cum_deaths)) +
  # Lorenz curve 아래 음영
  geom_area(fill = "#C0392B", alpha = 0.25) +
  # 완전 평등 (대각선)
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", colour = "grey40") +
  # Lorenz curve 본체
  geom_line(linewidth = 0.9, colour = "#C0392B") +
  geom_point(data = tab4,
             aes(x = cum_cases, y = cum_deaths),
             size = 1.6, colour = "#C0392B") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1), expand = c(0, 0)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1), expand = c(0, 0)) +
  coord_equal() +
  annotate("text", x = 0.6, y = 0.1,
           label = sprintf("Gini = %.2f", gini),
           hjust = 0, size = 4.2, fontface = "bold") +
  labs(x = "Cumulative share of cases",
       y = "Cumulative share of deaths",
       title = "Lorenz curve: concentration of chikungunya deaths",
       subtitle = "Groups = comorbidity status x age group, sorted by death rate")

ggsave(file.path(OUT_DIR, "fig_04_lorenz_age_comorb.png"),
       p4, width = 6.5, height = 6.5, dpi = 150)


# ---- 9. Summary table -----------------------------------------------------

summary_tab <- tab1 |>
  dplyr::select(age_group, any_comorbidity,
                cases, hosp, deaths,
                hosp_rate, death_rate, case_pct) |>
  dplyr::arrange(any_comorbidity, age_group)

readr::write_csv(summary_tab,
                 file.path(OUT_DIR, "tab_summary_by_age_comorb.csv"))

message("\n[done] all figures + table in:\n  ", OUT_DIR)
message(sprintf("  Gini (cases vs deaths over age x comorbidity) = %.3f", gini))
