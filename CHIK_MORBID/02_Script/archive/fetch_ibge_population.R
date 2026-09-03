# ===========================================================================
# fetch_ibge_population.R
#
# 목적
# ----
# 브라질 IBGE (통계청) 의 "추정 인구 (estimativas de população)"를
# municipality x year 단위로 받아서, chikungunya panel과 join하기 좋은
# 형태로 저장한다.
#
# 자료원
# ------
# SIDRA (Sistema IBGE de Recuperação Automática) API. IP 제한 없음.
#
# Brazil은 2022년에 Census를 새로 실시했기 때문에 한 테이블로 2015-2024를
# 다 못 받는다. 연도별로 다른 SIDRA 테이블을 써야 한다:
#
#   2015-2021, 2024 : Table 6579,  Variable 9324
#                     "Estimativas da População Residente"
#                     매년 7/1 기준 추정. 2022/2023은 Census 때문에 빠짐.
#   2022           : Table 4714,  Variable 93
#                     "Censo Demográfico 2022 - População residente"
#                     2022년 8월 1일 Census 실측값.
#   2023           : IBGE가 muni 단위 공식 추정치를 발표하지 않은 해
#                     (Census 2022 직후라 SKIP 됨). 그래서 이 스크립트는
#                     **2022 Census와 2024 estimate 사이 선형 보간**으로
#                     2023 muni 인구를 채운다. Brazil 공중보건 연구에서
#                     널리 쓰는 표준 workaround.
#
# 이렇게 합치면 muni × year (2015-2024) 인구를 끊김 없이 만들 수 있다.
#
# 출력
# ----
#   01_Data/ibge_pop_muni_year_2015_2024.rds   (+ .csv)
#     columns : muni7        IBGE 7-digit municipality code
#               muni6        6-digit code (SINAN과 join용)
#               uf           2-character state abbreviation (e.g. "BA")
#               year         integer
#               population   integer
#
# 메모
# ----
# - SINAN chikungunya 데이터의 muni_residence6는 6-digit 코드를 쓰는 반면,
#   IBGE의 정식 코드는 7-digit (마지막 자릿수는 check digit).
#   → 변환은 단순히 `muni7 %/% 10`.
# - 매년 별도 호출 (10번)로 나누는 이유: SIDRA가 한 번에 너무 많은 셀을
#   요청하면 timeout이 잘 발생. 연도별로 5,570 rows씩 받으면 안정적.
# ===========================================================================

# ---- 0. Packages ---------------------------------------------------------
# sidrar : SIDRA API용 wrapper. CRAN에 있음.
# here   : .Rproj root 기준으로 파일 경로 잡아주는 편의 패키지.
#          (working directory가 어디든 같은 경로가 나옴)

for (p in c("here", "sidrar", "dplyr", "readr")) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}
suppressPackageStartupMessages({
  library(here); library(sidrar); library(dplyr); library(readr)
})

# ---- 1. Config -----------------------------------------------------------

YEAR_START <- 2015
YEAR_END   <- 2024

DATA_DIR <- here::here("01_Data")
dir.create(DATA_DIR, showWarnings = FALSE, recursive = TRUE)

OUT_RDS <- file.path(DATA_DIR,
                     sprintf("ibge_pop_muni_year_%d_%d.rds",
                             YEAR_START, YEAR_END))
OUT_CSV <- sub("\\.rds$", ".csv", OUT_RDS)

# ---- 2. Helper : 한 해 인구를 받아오기 ------------------------------------
#
# get_sidra() 인자
#   x          : SIDRA table 번호
#   variable   : 변수 코드 (6579 → 9324, 4714/9514 → 93)
#   period     : 연도 (문자열). 여러 해를 한 번에 줄 수도 있지만,
#                안정성을 위해 한 해씩 호출함.
#   geo        : 지리 단위. "City" = 시 (municipality)
#   geo.filter : NULL이면 전국 모든 시 (5,570개).
#
# 반환은 data.frame인데 컬럼 이름이 포르투갈어 ("Município (Código)" 등)라,
# select() + rename()으로 우리가 쓸 형태로 다듬는다.

fetch_one_year <- function(yr, table, variable) {
  message(sprintf("[sidra] year %d  (table %d, var %d) ...",
                  yr, table, variable))

  raw <- tryCatch(
    sidrar::get_sidra(
      x        = table,
      variable = variable,
      period   = as.character(yr),
      geo      = "City"
    ),
    error = function(e) {
      message("  -> failed: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(raw) || nrow(raw) == 0) return(NULL)

  # SIDRA가 돌려주는 컬럼명은 길고 포어. 필요한 것만 골라 rename.
  out <- raw %>%
    transmute(
      muni7      = `Município (Código)`,            # 7-digit IBGE 코드 (문자)
      population = suppressWarnings(as.integer(Valor)),
      year       = yr
    ) %>%
    # 인구가 NA / 0 / 음수인 row 제거 (SIDRA가 ".." 같이 결측 표시)
    filter(!is.na(population), population > 0)

  message(sprintf("  -> %d municipalities", nrow(out)))
  out
}

# ---- 3. 연도별 자료원 매핑 -----------------------------------------------
#
# `year_plan`은 "어느 해에 어떤 SIDRA 테이블/변수를 쓸지" 명시한 표.
# fetch loop이 이 표를 그대로 따라간다.

year_plan <- dplyr::bind_rows(
  tibble::tibble(year = c(2015:2021, 2024), table = 6579, variable = 9324),
  tibble::tibble(year = 2022L,              table = 4714, variable = 93)
  # 2023은 IBGE에서 muni-level 자료가 없어서 fetch 단계에서는 skip하고,
  # 아래(step 5)에서 2022-2024 사이 선형 보간으로 채운다.
) %>%
  filter(year >= YEAR_START, year <= YEAR_END) %>%
  arrange(year)

# ---- 4. Loop years -------------------------------------------------------

raw_list <- vector("list", nrow(year_plan))
for (i in seq_len(nrow(year_plan))) {
  raw_list[[i]] <- fetch_one_year(
    yr       = year_plan$year[i],
    table    = year_plan$table[i],
    variable = year_plan$variable[i]
  )
}
raw_list <- Filter(Negate(is.null), raw_list)
if (length(raw_list) == 0) stop("SIDRA returned no data at all.")

pop_all <- dplyr::bind_rows(raw_list)

# ---- 5a. 2023 보간 채우기 ------------------------------------------------
# IBGE는 2023년 muni-level 추정치를 발표하지 않았다 (Census 2022 직후).
# 2022 (Census) ↔ 2024 (estimate) 사이 선형 보간으로 채운다.
# 보간 공식 : pop_2023 ≈ (pop_2022 + pop_2024) / 2  (1년 간격 균등 분할).
# muni가 2022나 2024 중 한 쪽에라도 없으면 2023 행은 안 만든다.

if (all(c(2022L, 2024L) %in% pop_all$year) &&
    !(2023L %in% pop_all$year)) {
  message("[fill]  IBGE에 2023 muni-level 자료가 없어 2022-2024 선형 보간으로 채움")
  bracket <- pop_all %>%
    filter(year %in% c(2022L, 2024L)) %>%
    select(muni7, year, population) %>%
    tidyr::pivot_wider(names_from = year, values_from = population,
                       names_prefix = "pop_") %>%
    filter(!is.na(pop_2022), !is.na(pop_2024)) %>%
    mutate(year = 2023L,
           population = as.integer(round((pop_2022 + pop_2024) / 2))) %>%
    select(muni7, year, population)
  pop_all <- dplyr::bind_rows(pop_all, bracket)
  message(sprintf("  -> %d municipalities interpolated for 2023",
                  nrow(bracket)))
}

# ---- 5b. Add 6-digit (SINAN) code + UF abbreviation ----------------------
#
# - muni6 : SINAN쪽 join key. 7-digit 마지막 한 자리 떼면 됨.
# - uf_code : 첫 두 자리. IBGE 표준.
# - uf      : 보기 좋은 영문/포어 약자.

uf_table <- tibble::tribble(
  ~uf_code, ~uf,
  "11","RO","12","AC","13","AM","14","RR","15","PA","16","AP","17","TO",
  "21","MA","22","PI","23","CE","24","RN","25","PB","26","PE","27","AL",
  "28","SE","29","BA",
  "31","MG","32","ES","33","RJ","35","SP",
  "41","PR","42","SC","43","RS",
  "50","MS","51","MT","52","GO","53","DF"
)

pop_clean <- pop_all %>%
  mutate(
    muni7   = sprintf("%07s", muni7),                 # 0-padding 보장
    muni6   = substr(muni7, 1, 6),                    # SINAN join key
    uf_code = substr(muni7, 1, 2)
  ) %>%
  left_join(uf_table, by = "uf_code") %>%
  select(muni7, muni6, uf, year, population) %>%
  arrange(uf, muni7, year)

# ---- 6. Save -------------------------------------------------------------

saveRDS(pop_clean, OUT_RDS)
readr::write_csv(pop_clean, OUT_CSV)

message(sprintf(
  "[save] IBGE municipality-year population panel\n  %s\n  %s rows | %d municipalities | %d years",
  OUT_RDS,
  format(nrow(pop_clean), big.mark = ","),
  dplyr::n_distinct(pop_clean$muni6),
  dplyr::n_distinct(pop_clean$year)
))
