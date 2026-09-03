# ---------------------------------------------------------------------------
# 00_setup.R
#
# Run this FIRST, once per R session, before any other script in
# 02_Script/. It installs (if missing) and loads every package used
# anywhere in the main pipeline, and confirms the project root via `here`.
#
# Pipeline order (see 02_Script/PIPELINE.md for full details) - strictly
# linear, no script needs to be run twice:
#   00_setup.R
#   -> 01_fetch_chik_sinan_brazil.R
#   -> 02_clean_chik_sinan_brazil.R
#   -> 03_relative_risk.R
#   -> 04_background_prevalence.R
#   -> 05_corr_matrix.R
#   -> 06_corr_heatmap.R
#   -> 07_gbd_age_alignment.R
#   -> 08_copula_simulation.R
#   -> 09_copula_result_graphs.R
#   -> 10_background_burden_graphs.R
#
# Side analyses, any time after 03_relative_risk.R, not part of the main
# sequence above: side_analyses/table_basic.R, side_analyses/hosp_rate_validation.R
# ---------------------------------------------------------------------------

required_packages <- c(
  # ---- non-tidyverse packages: MUST load before dplyr/tidyr (see note below) ----
  "here", "curl", "data.table",
  "splines",   # base R, ships with R itself
  "MASS",      # recommended package, ships with R
  "Matrix",    # recommended package, ships with R
  "mgcv",      # recommended package, ships with R
  "binom", "openxlsx", "countrycode",
  # ---- tidyverse-adjacent packages: load LAST so their functions win any
  #      name clash (e.g. MASS::select() vs dplyr::select()) ----
  "readr", "dplyr", "tidyr", "tibble", "stringr", "purrr", "forcats",
  "rlang", "lubridate", "ggplot2", "scales"
)

for (p in required_packages) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p, repos = "https://cloud.r-project.org")
  }
}

# Load order matters: MASS::select() and MASS::filter()-like generics would
# otherwise mask dplyr's if MASS were attached after dplyr. Loading MASS (and
# every other non-tidyverse package) first, then dplyr/tidyr/etc. last,
# guarantees the tidyverse versions win every such clash.
suppressPackageStartupMessages({
  library(here)
  library(curl)
  library(data.table)
  library(splines)
  library(MASS)
  library(Matrix)
  library(mgcv)
  library(binom)
  library(openxlsx)
  library(countrycode)

  library(readr)
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(stringr)
  library(purrr)
  library(forcats)
  library(rlang)
  library(lubridate)
  library(ggplot2)
  library(scales)
})

message("[setup] project root (via here::here()): ", here::here())
message("[setup] all packages loaded.")
