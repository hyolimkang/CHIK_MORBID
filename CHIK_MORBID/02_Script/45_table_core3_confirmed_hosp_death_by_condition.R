# ---------------------------------------------------------------------------
# 45_table_core3_confirmed_hosp_death_by_condition.R
#
# Simple descriptive table (requested directly, not part of a numbered
# validation gate): for each of the three core conditions (DM, HTN, CKD),
# how many confirmed chikungunya cases, hospitalisations, and
# chikungunya-attributed deaths are there in the CURRENT (Gate 3.1-corrected)
# core-three eligible cohort. Read-only - does not touch any primary script,
# cache, or existing manuscript table/figure.
#
# Needs: 01_Data/chik_sinan_individual_2015_2025.rds
# Output: 03_Output/tables/table_core3_confirmed_hosp_death_by_condition.docx
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(lubridate); library(officer); library(flextable)
})
options(scipen = 999)

font_family <- "Arial"
out_dir <- "03_Output/tables"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

## ---- reconstruct the current core-three eligible cohort (identical logic
## to 12_core3_condition_specific_rr.R's add_core3(); not sourced) ----
ind <- readRDS("01_Data/chik_sinan_individual_2015_2025.rds")
core_cols <- c("diabetes", "hypertension", "renal_disease")

eligible <- ind |>
  dplyr::filter(
    is_confirmed_chik, lubridate::year(event_date) >= 2017,
    !is.na(age_years), age_years >= 0, age_years <= 100, sex %in% c("male", "female"),
    dplyr::if_all(dplyr::all_of(core_cols), ~ .x %in% c("no", "yes"))
  )
stopifnot(nrow(eligible) == 1074796)

hosp_known  <- eligible |> dplyr::filter(hospitalised %in% c("no","yes")) |> dplyr::mutate(hosp_only = hospitalised == "yes")
death_known <- eligible |> dplyr::filter(!is.na(died_from_chik))
stopifnot(nrow(hosp_known) == 776072, sum(hosp_known$hosp_only) == 25363)
stopifnot(nrow(death_known) == 983947, sum(death_known$died_from_chik) == 1050)

## ---- build one row per condition (+ an overall row) ----
build_row <- function(label, condvar) {
  if (is.null(condvar)) {
    confirmed <- eligible
    hk <- hosp_known
    dk <- death_known
  } else {
    confirmed <- eligible |> dplyr::filter(.data[[condvar]] == "yes")
    hk <- hosp_known |> dplyr::filter(.data[[condvar]] == "yes")
    dk <- death_known |> dplyr::filter(.data[[condvar]] == "yes")
  }
  n_confirmed <- nrow(confirmed)
  n_hosp_known <- nrow(hk); n_hosp <- sum(hk$hosp_only)
  n_death_known <- nrow(dk); n_death <- sum(dk$died_from_chik)
  tibble::tibble(
    condition = label,
    confirmed_n = n_confirmed,
    hosp_known_n = n_hosp_known,
    hosp_n = n_hosp,
    hosp_pct = round(100*n_hosp/n_hosp_known, 2),
    death_known_n = n_death_known,
    death_n = n_death,
    death_pct = round(100*n_death/n_death_known, 3)
  )
}

none_row <- {
  confirmed <- eligible |> dplyr::filter(diabetes=="no", hypertension=="no", renal_disease=="no")
  hk <- hosp_known |> dplyr::filter(diabetes=="no", hypertension=="no", renal_disease=="no")
  dk <- death_known |> dplyr::filter(diabetes=="no", hypertension=="no", renal_disease=="no")
  tibble::tibble(condition="None of DM/HTN/CKD", confirmed_n=nrow(confirmed),
                 hosp_known_n=nrow(hk), hosp_n=sum(hk$hosp_only), hosp_pct=round(100*sum(hk$hosp_only)/nrow(hk),2),
                 death_known_n=nrow(dk), death_n=sum(dk$died_from_chik), death_pct=round(100*sum(dk$died_from_chik)/nrow(dk),3))
}
tbl <- dplyr::bind_rows(
  build_row("Diabetes mellitus (DM)", "diabetes"),
  build_row("Hypertension (HTN)", "hypertension"),
  build_row("Chronic kidney disease (CKD)", "renal_disease"),
  none_row,
  build_row("Overall (core-three eligible cohort)", NULL)
)

print(as.data.frame(tbl), row.names = FALSE)

## ---- format for display ----
fmt_n <- function(x) format(x, big.mark=",")
display_tbl <- tbl |>
  dplyr::transmute(
    Condition = condition,
    `Confirmed cases, N` = fmt_n(confirmed_n),
    `Hospitalised, N (%)*` = sprintf("%s (%.2f%%)", fmt_n(hosp_n), hosp_pct),
    `Died from chikungunya, N (%)**` = sprintf("%s (%.2f%%)", fmt_n(death_n), death_pct)
  )

## ---- Word table (house style: officer + flextable, matches 18_/26_/27_) ----
ft <- flextable::flextable(display_tbl)
ft <- flextable::bg(ft, part = "header", bg = "#EEF1F3")
ft <- flextable::bg(ft, i = nrow(display_tbl), part = "body", bg = "#F5F7F8")
ft <- flextable::color(ft, part = "all", color = "#263238")
ft <- flextable::font(ft, part = "all", fontname = font_family)
ft <- flextable::fontsize(ft, part = "header", size = 10)
ft <- flextable::fontsize(ft, part = "body", size = 9.5)
ft <- flextable::bold(ft, part = "header", bold = TRUE)
ft <- flextable::bold(ft, i = nrow(display_tbl), part = "body", bold = TRUE)
ft <- flextable::align(ft, j = "Condition", align = "left", part = "all")
ft <- flextable::align(ft, j = setdiff(names(display_tbl), "Condition"), align = "center", part = "all")
ft <- flextable::valign(ft, part = "all", valign = "center")
ft <- flextable::padding(ft, part = "all", padding.top = 4, padding.bottom = 4, padding.left = 6, padding.right = 6)
ft <- flextable::width(ft, j = "Condition", width = 2.3)
ft <- flextable::width(ft, j = "Confirmed cases, N", width = 1.5)
ft <- flextable::width(ft, j = c("Hospitalised, N (%)*","Died from chikungunya, N (%)**"), width = 2.0)
ft <- flextable::set_table_properties(ft, layout = "fixed", opts_word = list(split = FALSE, keep_with_next = TRUE))

doc <- officer::read_docx()
doc <- officer::body_add_fpar(doc, value = officer::fpar(
  officer::ftext("Table. ", prop = officer::fp_text(font.family = font_family, bold = TRUE, font.size = 11)),
  officer::ftext("Confirmed chikungunya cases, hospitalisations, and chikungunya-attributed deaths by core-three underlying condition, Brazil 2017-2025",
                 prop = officer::fp_text(font.family = font_family, font.size = 11))
))
doc <- officer::body_add_par(doc, value = "", style = "Normal")
doc <- flextable::body_add_flextable(doc, value = ft, align = "center")
doc <- officer::body_add_par(doc, value = "", style = "Normal")
doc <- officer::body_add_fpar(doc, value = officer::fpar(
  officer::ftext("Note. ", prop = officer::fp_text(font.family = font_family, italic = TRUE, font.size = 8.5)),
  officer::ftext(
    sprintf("Core-three eligible cohort (confirmed chikungunya, 2017-2025, valid age/sex, DM/HTN/CKD status all explicit yes/no), N=%s. ", fmt_n(nrow(eligible))),
    prop = officer::fp_text(font.family = font_family, font.size = 8.5)
  )
))
doc <- officer::body_add_fpar(doc, value = officer::fpar(
  officer::ftext(
    sprintf("*Hospitalised %% is among the subset with hospitalisation status explicitly known (N=%s overall, N=%s DM, N=%s HTN, N=%s CKD; %s unknown/missing overall, excluded from this denominator). ",
            fmt_n(tbl$hosp_known_n[tbl$condition=="Overall (core-three eligible cohort)"]),
            fmt_n(tbl$hosp_known_n[tbl$condition=="Diabetes mellitus (DM)"]),
            fmt_n(tbl$hosp_known_n[tbl$condition=="Hypertension (HTN)"]),
            fmt_n(tbl$hosp_known_n[tbl$condition=="Chronic kidney disease (CKD)"]),
            fmt_n(nrow(eligible) - tbl$hosp_known_n[tbl$condition=="Overall (core-three eligible cohort)"])),
    prop = officer::fp_text(font.family = font_family, font.size = 8.5)
  )
))
doc <- officer::body_add_fpar(doc, value = officer::fpar(
  officer::ftext(
    sprintf("**Chikungunya-death %% is among the subset with death status known per the Gate 3.1-corrected outcome definition (N=%s overall; %s unknown/unresolved overall, excluded from this denominator). Condition categories are not mutually exclusive (a case may have more than one of DM/HTN/CKD, so rows do not sum to the overall row).",
            fmt_n(tbl$death_known_n[tbl$condition=="Overall (core-three eligible cohort)"]),
            fmt_n(nrow(eligible) - tbl$death_known_n[tbl$condition=="Overall (core-three eligible cohort)"])),
    prop = officer::fp_text(font.family = font_family, font.size = 8.5)
  )
))

out_file <- file.path(out_dir, "table_core3_confirmed_hosp_death_by_condition.docx")
print(doc, target = out_file)
message("[45] Written: ", out_file)
