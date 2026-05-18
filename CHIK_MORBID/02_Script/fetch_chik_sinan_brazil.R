# ---------------------------------------------------------------------------
# fetch_chik_sinan_brazil.R
#
# Goal : Download Brazilian SINAN chikungunya yearly CSVs from the public
#        Ministério da Saúde S3 mirror (no IP restriction). Output is just
#        the raw .csv.zip files in 01_Data/sinan_chik_csv/.
#        Cleaning / reshaping is done by clean_chik_sinan_brazil.R.
#
# Source : https://dadosabertos.saude.gov.br/dataset/arboviroses-febre-de-chikungunya
#          (CKAN resource list -> AWS S3 sa-east-1)
#
# URL template:
#   https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SINAN/Chikungunya/csv/CHIKBR<YY>.csv.zip
# ---------------------------------------------------------------------------

# ---- 0. Packages ----------------------------------------------------------

for (p in c("here", "curl")) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}
suppressPackageStartupMessages({ library(here); library(curl) })

# ---- 1. Config ------------------------------------------------------------

YEAR_START <- 2015        # first year with national CHIKBR file
YEAR_END   <- 2024        # exclude 2025-2026 (reporting still incomplete)

ZIP_DIR <- here::here("01_Data", "sinan_chik_csv")
DOC_DIR <- here::here("01_Data", "sinan_chik_docs")
dir.create(ZIP_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(DOC_DIR, showWarnings = FALSE, recursive = TRUE)

S3_BASE   <- "https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SINAN/Chikungunya/csv/"
DIC_URL   <- "https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SINAN/Chikungunya/dic_dados_chikungunya.pdf"

# ---- 2. Download helpers -------------------------------------------------

download_if_missing <- function(url, dest, label = basename(dest),
                                max_attempts = 3) {
  if (file.exists(dest) && file.info(dest)$size > 1024) {
    message(sprintf("[skip]  %s already present (%.1f MB)",
                    label, file.info(dest)$size / 1024^2))
    return(invisible(dest))
  }
  # Clean any stale partials from a previous run that crashed mid-download.
  for (junk in c(dest, paste0(dest, ".curltmp"))) {
    if (file.exists(junk)) unlink(junk, force = TRUE)
  }

  for (attempt in seq_len(max_attempts)) {
    message(sprintf("[get]   %s  (attempt %d/%d)",
                    label, attempt, max_attempts))
    t0 <- Sys.time()
    h  <- new_handle(timeout = 600, connecttimeout = 30)
    ok <- try(curl_download(url, dest, handle = h, mode = "wb"),
              silent = TRUE)
    bytes <- if (file.exists(dest)) file.info(dest)$size else 0L
    if (!inherits(ok, "try-error") && bytes > 1024) {
      message(sprintf("  -> ok (%.1f MB, %.1fs)",
                      bytes / 1024^2,
                      as.numeric(difftime(Sys.time(), t0, units = "secs"))))
      return(invisible(dest))
    }
    msg <- if (inherits(ok, "try-error"))
      attr(ok, "condition")$message else
      sprintf("empty / too small (%d bytes)", bytes)
    message(sprintf("  -> failed: %s", msg))
    for (junk in c(dest, paste0(dest, ".curltmp"))) {
      if (file.exists(junk)) unlink(junk, force = TRUE)
    }
    if (attempt < max_attempts) Sys.sleep(5)
  }
  message(sprintf("[fail]  %s  -- giving up after %d attempts",
                  label, max_attempts))
  invisible(NULL)
}

# ---- 3. Run ---------------------------------------------------------------

message("[fetch] data dictionary")
download_if_missing(
  url   = DIC_URL,
  dest  = file.path(DOC_DIR, "dic_dados_chikungunya.pdf"),
  label = "dic_dados_chikungunya.pdf"
)

message("[fetch] yearly CSV zips: ", YEAR_START, "-", YEAR_END)
for (yr in YEAR_START:YEAR_END) {
  fname <- sprintf("CHIKBR%02d.csv.zip", yr %% 100)
  download_if_missing(
    url   = paste0(S3_BASE, fname),
    dest  = file.path(ZIP_DIR, fname),
    label = fname
  )
}

message("\n[done] all files in:\n  ", ZIP_DIR,
        "\nNow run 02_Script/clean_chik_sinan_brazil.R to build the analysis-ready dataset.")
