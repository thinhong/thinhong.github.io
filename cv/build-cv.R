#!/usr/bin/env Rscript
# =====================================================================
# cv/build-cv.R  -  Build Thinh Ong's master CV (HTML + PDF) from the
# Google Sheet + Zotero, in the Scholarly design.
#
# RUN from the project root (the folder that holds code/ and cv/):
#     Rscript cv/build-cv.R
#
# PREREQUISITES (the same setup your current CV already uses):
#   * R packages: googlesheets4, dplyr, tidyr, stringr, lubridate, httr, jsonlite
#   * Google auth + ZOTERO_API_KEY / ZOTERO_USER_ID / ZOTERO_COLLECTION_KEY
#     in .Renviron  (already configured for your old cv.qmd)
#   * For the PDF only:  pip install weasyprint
#     and either an internet connection (so WeasyPrint can fetch Source Serif 4
#     from Google Fonts) or the Source Serif 4 font installed on your machine.
#
# OUTPUT:  cv/cv.html   and, if WeasyPrint is available,  cv/cv.pdf
# The design lives in cv/cv-template.html; this script only fills in the data.
# =====================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stringr); library(lubridate)
})

if (!file.exists("code/cv.R"))
  stop("Run this from the project root, e.g.  Rscript cv/build-cv.R")

## Reuse the existing fetch helpers: Google auth, read_sheet, Zotero functions,
## api_key, base_url, process_authors.
source("code/cv.R")

cv_url   <- "https://docs.google.com/spreadsheets/d/1ecqMkclPn-lhzgGHw2UTueiKkjTxuoAvJjpFKs3Jtm4/edit?usp=sharing"
template <- "cv/cv-template.html"
out_html <- "cv/cv.html"
out_pdf  <- "cv/cv.pdf"

read_tab <- function(sheet) suppressMessages(read_sheet(ss = cv_url, sheet = sheet))

## ---------------- small HTML helpers ----------------
esc <- function(x){
  x <- ifelse(is.na(x), "", as.character(x))
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;",  x, fixed = TRUE)
  x <- gsub(">", "&gt;",  x, fixed = TRUE)
  x
}
strip_scheme <- function(u) sub("/+$", "", sub("^https?://", "", u))
bold_name <- function(x) gsub("Ong T([A-Z]*)", "<b>Ong T\\1</b>", x)   # bold my name
lead_bold <- function(s){            # wrap the text before the first comma in <span class="t">
  s <- esc(s); pos <- regexpr(",", s, fixed = TRUE)
  ifelse(pos > 0,
         paste0('<span class="t">', substr(s, 1, pos - 1), '</span>', substr(s, pos, nchar(s))),
         paste0('<span class="t">', s, '</span>'))
}
section     <- function(title, inner)
  sprintf('<section class="cv-sec">\n  <h2 class="cv-h">%s</h2>\n  %s\n</section>', title, inner)
table_plain <- function(rows)
  sprintf('<table class="cv-table"><tbody>\n%s\n</tbody></table>', paste(rows, collapse = "\n"))

## ---------------- sections ----------------
build_education <- function(){
  d <- read_tab("edu")                                   # columns in order: date, degree, school
  rows <- sprintf('<tr><td class="c-when">%s</td><td><span class="t">%s</span>, %s</td></tr>',
                  esc(d[[1]]), esc(d[[2]]), esc(d[[3]]))
  section("Education", table_plain(rows))
}

build_experience <- function(){
  d <- read_tab("employ")
  full <- paste0(d$job, ", ", d$aff)
  rows <- sprintf('<tr><td class="c-when">%s</td><td>%s</td></tr>', esc(d$date), lead_bold(full))
  section("Experience", table_plain(rows))
}

build_grants <- function(){
  d <- read_tab("grants") |>
    mutate(my_role = factor(my_role, levels = c("PI", "Co-PI", "Co-I")),
           outcome_year = year(outcome)) |>
    arrange(my_role, desc(outcome))
  total <- sum(d$budget_total_usd, na.rm = TRUE)
  lead  <- sprintf('<p class="cv-lead">Secured $%s in research grants since %s.</p>',
                   format(total, big.mark = ",", trim = TRUE), min(d$outcome_year, na.rm = TRUE))
  amt   <- paste0("$", format(d$budget_total_usd, big.mark = ",", trim = TRUE))
  rows  <- sprintf(
    '<tr><td class="c-when">%s</td><td class="c-role">%s</td><td class="c-amt">%s</td><td><span class="t">%s</span> <span class="cv-meta">%s</span></td></tr>',
    esc(d$period), esc(as.character(d$my_role)), esc(amt), esc(d$title), esc(d$sponsor))
  section("Grants", paste0(lead, "\n", table_plain(rows)))
}

build_awards <- function(){
  d <- read_tab("awards") |> arrange(desc(year))
  rows <- sprintf('<tr><td class="c-when">%s</td><td>%s</td></tr>', esc(d$date), lead_bold(d$description))
  section("Awards", table_plain(rows))
}

build_team <- function(){
  d <- read_tab("team") |>
    mutate(year = as.numeric(year),
           role = factor(role, levels = c("RA", "Intern")),
           next_dest = ifelse(is.na(next_dest), "", next_dest)) |>
    arrange(role, next_dest != "", desc(year))
  thead <- '<thead><tr><th class="c-when">Tenure</th><th class="c-role-s">Role</th><th class="c-name">Name</th><th>Next destination</th></tr></thead>'
  rows  <- sprintf('<tr><td class="c-when">%s</td><td class="c-role-s">%s</td><td class="c-name">%s</td><td class="cv-meta">%s</td></tr>',
                   esc(d$period), esc(as.character(d$role)), esc(d$name), esc(d$next_dest))
  inner <- sprintf('<table class="cv-table">%s<tbody>\n%s\n</tbody></table>', thead, paste(rows, collapse = "\n"))
  section("Research team", inner)
}

build_policy <- function(){
  d <- read_tab("policy") |> arrange(desc(year))
  rows <- sprintf('<tr><td class="c-when">%s</td><td>%s</td></tr>', esc(d$date), esc(d$project))
  section("Policy engagement", table_plain(rows))
}

build_teaching <- function(){
  d <- read_tab("teaching") |>
    mutate(website = ifelse(is.na(website), "", website)) |>
    arrange(desc(year))
  groups <- c("Modelling" = "modelling", "Other courses" = "other", "Seminars" = "seminar")
  body <- ""
  for (label in names(groups)){
    g <- d |> filter(type == groups[[label]])
    if (nrow(g) == 0) next
    link <- ifelse(g$website != "",
                   sprintf(' <a href="%s">%s</a>', esc(g$website), esc(strip_scheme(g$website))), "")
    meta <- paste0(esc(g$note), ". ", esc(g$venue), ".", link)
    rows <- sprintf('<tr><td class="c-when">%s</td><td class="c-num">%s</td><td><span class="t">%s</span> <span class="cv-meta">%s</span></td></tr>',
                    esc(g$date), esc(g$attd), esc(g$name), meta)
    body <- paste0(body,
                   sprintf('<tr class="cv-subgroup"><td colspan="3">%s</td></tr>\n', label),
                   paste(rows, collapse = "\n"), "\n")
  }
  thead <- '<thead><tr><th class="c-when">Year</th><th class="c-num">People</th><th>Course</th></tr></thead>'
  inner <- sprintf('<table class="cv-table">%s<tbody>\n%s</tbody></table>', thead, body)
  section("Teaching", inner)
}

build_software <- function(){
  d <- read_tab("software") |> arrange(desc(year))
  link <- sprintf(' <a href="%s">%s</a>', esc(d$url_github), esc(strip_scheme(d$url_github)))
  rows <- sprintf('<tr><td class="c-when">%s</td><td><span class="t">%s.</span> <span class="cv-meta">%s.</span>%s</td></tr>',
                  esc(d$year), esc(d$name), esc(d$summary), link)
  section("Software", table_plain(rows))
}

build_publications <- function(){
  p <- fetch_zotero_items(api_key, base_url)
  p <- p |> filter(itemType == "journalArticle")
  p$authors <- sapply(p$creators, function(cr) if (is.data.frame(cr)) process_authors(cr) else NA)
  parsed <- suppressWarnings(ymd(p$date))
  p <- p |>
    mutate(
      pdate    = parsed,
      pyear    = ifelse(is.na(parsed), substr(p$date, 1, 4), format(parsed, "%Y")),
      journal  = ifelse(journalAbbreviation == "" | is.na(journalAbbreviation), publicationTitle, journalAbbreviation),
      vp       = ifelse(issue == "" | issue == "null" | is.na(issue),
                        paste0(volume, ": ", pages), paste0(volume, "(", issue, "):", pages))
    ) |>
    arrange(desc(pdate))
  authors <- bold_name(esc(str_trim(p$authors)))
  ref <- sprintf('%s. %s. <i>%s</i>. %s;%s. <a href="https://doi.org/%s">doi</a>',
                 authors, esc(p$title), esc(p$journal), esc(p$pyear), esc(p$vp), esc(p$DOI))
  items <- paste(sprintf('<li>%s</li>', ref), collapse = "\n")
  section("Publications", sprintf('<ol class="cv-pubs">\n%s\n</ol>', items))
}

## ---------------- assemble ----------------
message("Fetching data and building sections ...")
sections <- paste(
  build_education(), build_experience(), build_grants(),  build_awards(),
  build_team(),      build_policy(),     build_teaching(), build_software(),
  build_publications(),
  sep = "\n\n")

tmpl <- paste(readLines(template, warn = FALSE), collapse = "\n")
html <- sub("{{SECTIONS}}", sections, tmpl, fixed = TRUE)

tmp <- tempfile(fileext = ".html")
writeLines(html, tmp, useBytes = TRUE)
file.copy(tmp, out_html, overwrite = TRUE); unlink(tmp)
message("Wrote ", out_html)

## ---------------- optional PDF via WeasyPrint ----------------
invisible(tryCatch(
  system2("weasyprint", c(shQuote(out_html), shQuote(out_pdf)), stdout = TRUE, stderr = TRUE),
  error = function(e) NULL))
if (file.exists(out_pdf)) {
  message("Wrote ", out_pdf)
} else {
  message("PDF skipped. Install WeasyPrint (pip install weasyprint), or just open ",
          out_html, " in your browser and Save as PDF.")
}
