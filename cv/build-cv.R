#!/usr/bin/env Rscript
# =====================================================================
# cv/build-cv.R - build the CV (cv/cv.html + cv/cv.pdf) from the Google Sheet
# and Zotero.
#
# The data comes from R/site-data.R, the same data layer the website pages use,
# so the CV and the site always show the same facts. This script only turns that
# data into the CV:
#   cv/cv-template.html   the design, and the header typed by hand (name, role,
#                         contacts, research interests)
#   this file             every section below the header
#   cv/fonts/             the two typefaces (EB Garamond, Manrope), as files of
#                         this site, so a build needs no network for fonts
#
# Run it from the project root, whenever you want a new CV:
#
#     Rscript cv/build-cv.R          # fetch fresh data, rebuild cv.html + cv.pdf
#     bash deploy.sh                 # or: refresh data, rebuild the CV only if
#                                    # something changed, render, publish
#
# It is NOT wired into Quarto. There is deliberately no `pre-render:` hook in
# _quarto.yml, so rendering or previewing a page can never start a CV build.
#
# To change the CV:
#   the order of the sections, or their names   the list `cv_sections` below
#   what a row of a section says                 its build_*() function
#   how anything looks                           cv/cv-template.html
#
# Environment variables:
#   CV_SKIP=1        do nothing at all
#   CV_FORCE=1       rebuild and fetch again, whatever the state
#   CV_IF_CHANGED=1  skip the build when the data, the template, the fonts and
#                    this script are the same as at the last build (deploy.sh)
#   CV_SKIP_PDF=1    rebuild cv.html only (accepting a possibly stale pdf)
#   CV_BUILD=1       only meaningful if the pre-render hook is ever restored
#
# Requirements:
#   * R packages: stringr and pagedown, plus what R/site-data.R needs to fetch
#     (googlesheets4, httr, jsonlite, dplyr, lubridate)
#   * Chrome or Edge, version 131 or newer (pagedown prints the PDF with it, and
#     the footer of each sheet needs that version)
#   * ZOTERO_API_KEY / ZOTERO_USER_ID / ZOTERO_COLLECTION_KEY in .Renviron
#   * Google sign-in only if the Sheet is not link-viewable
# =====================================================================

## ---------------- fast gate: is a build even wanted? (runs before anything loads) ----------------
# Belt and braces. _quarto.yml has no pre-render hook, so in normal use this script
# only runs when you run it. The gate stays so that, if the hook is ever restored, a
# render still cannot quietly rebuild the CV.
# "Quarto is running me" = ANY QUARTO_PROJECT_* variable is set. Do not test
# QUARTO_PROJECT_DIR: Quarto does not set it for pre-render scripts, and a test on
# the wrong name fails OPEN (it rebuilt the CV on every render).
in_quarto  <- any(grepl("^QUARTO_PROJECT_", names(Sys.getenv())))
want_build <- nzchar(Sys.getenv("CV_BUILD")) || nzchar(Sys.getenv("CV_FORCE"))
if (nzchar(Sys.getenv("CV_SKIP"))) {
  message("cv: CV_SKIP is set, skipping.")
  quit(save = "no")
}
if (in_quarto && !want_build) {
  message("cv: not rebuilt (decoupled from render). Run  Rscript cv/build-cv.R  or set CV_BUILD=1 to build.")
  quit(save = "no")
}

suppressPackageStartupMessages(library(stringr))

## ---------------- config ----------------
# The Sheet address, the tab names, the Scholar profile and the cache live in
# R/site-data.R (shared with the website). Only CV settings stay here.
template   <- "cv/cv-template.html"
out_html   <- "cv/cv.html"
out_pdf    <- "cv/cv.pdf"
font_dir   <- "cv/fonts"
stamp_file <- "cv/.cv-built"          # signature of the last successful build (not in git)
own_name   <- "Ong T"                 # surname + first initial(s): set in bold in the reference list

# Short forms used in the Sheet, written in full on the CV: a reader should not
# have to know what "Co-I" means. A role that is not listed is shown as typed.
role_words <- c("PI" = "Principal investigator", "Co-PI" = "Co-principal investigator",
                "Co-I" = "Co-investigator", "RA" = "Research assistant")

# Columns each tab must provide. Checked up front, so a renamed column fails with a
# clear message and not with a cryptic error deep inside a build_*() function.
# 'edu' is absent on purpose: build_education() falls back to the column order.
required_cols <- list(
  employ     = c("date", "job", "aff"),
  grants     = c("period", "my_role", "budget_total_usd", "title", "sponsor", "outcome"),
  awards     = c("date", "description"),
  team       = c("period", "role", "name", "next_dest", "year"),
  policy     = c("date", "project"),
  engagement = c("period", "title", "detail"),
  teaching   = c("date", "attd", "name", "note", "venue", "website", "type"),
  talks      = c("date", "type", "title", "event"),
  service    = c("period", "category", "role", "name"),
  software   = c("year", "name", "summary", "url_github")
)
# Optional columns: filled with NA when the Sheet does not have them yet. (Those of
# `awards`, which are note, id, url and link_text, are handled by awards_clean() in
# R/site-data.R.) `link_text` sets the wording of a row's link ("News article").
optional_cols <- list(talks      = c("location", "url", "link_text"),
                      service    = c("location", "url", "link_text"),
                      engagement = c("url", "link_text"),
                      teaching   = "link_text",
                      software   = "link_text")

if (!file.exists("_quarto.yml") || !file.exists(template))
  stop("Run this from the project root, e.g.  Rscript cv/build-cv.R", call. = FALSE)

# The shared data layer: site_data(), grants_clean(), awards_clean(), and the small
# text helpers esc(), md_inline(), to_date() and usd() that the pages use too.
source("R/site-data.R")
scholar_url <- site_config$scholar_url

## ---------------- data-refresh policy (past the gate, so a build is wanted) ----------------
full_render <- nzchar(Sys.getenv("QUARTO_PROJECT_RENDER_ALL"))
forced      <- nzchar(Sys.getenv("CV_FORCE"))
have_out    <- file.exists(out_html) && file.exists(out_pdf)

## ---------------- small data helpers ----------------
# a column as trimmed text: NA becomes ""
nz <- function(x) { x <- trimws(as.character(x)); x[is.na(x)] <- ""; x }
# the same, and the placeholders that Zotero leaves in an empty field become "" too
clean <- function(x) { x <- nz(x); x[x %in% c("null", "0")] <- ""; x }
ensure_cols <- function(df, cols) {
  for (cl in cols) if (!cl %in% names(df)) df[[cl]] <- NA_character_
  df
}
has_rows <- function(d) is.data.frame(d) && nrow(d) > 0
# several pieces of one sentence, the empty ones left out: joined(c("a", "", "b"), ", ") is "a, b"
joined <- function(..., sep = " ") {
  m <- cbind(...)
  apply(m, 1, function(p) paste(p[nzchar(p)], collapse = sep))
}
# Fail early and clearly if the Sheet has been re-columned under us
check_columns <- function(tabs) {
  bad <- character(0)
  for (tb in names(required_cols)) {
    miss <- setdiff(required_cols[[tb]], names(tabs[[tb]]))
    if (length(miss))
      bad <- c(bad, sprintf("  %-10s missing: %s", tb, paste(miss, collapse = ", ")))
  }
  if (length(bad))
    stop("cv: the Google Sheet is missing columns the CV needs:\n",
         paste(bad, collapse = "\n"),
         "\nRename them in the Sheet, or update `required_cols` in cv/build-cv.R.",
         call. = FALSE)
}
# A grouped section only shows the groups it knows, so say it loudly when a row
# carries a value that would otherwise drop off the CV without a word.
warn_unmapped <- function(d, col, known, tab) {
  extra <- setdiff(unique(nz(d[[col]])), c(known, ""))
  if (length(extra))
    warning(sprintf("cv: '%s' tab has %s value(s) that match no group, so those rows are NOT on the CV: %s",
                    tab, col, paste(extra, collapse = ", ")), call. = FALSE)
  invisible(NULL)
}

## ---------------- sorting ----------------
# Newest first, for a column of dates, years or ranges of years. Understands Date
# columns, "2024-10-05", "2024-10" and "2024". A range such as "2022-2026" sorts on
# its LATEST year: an entry that runs to 2026 is more recent than one that ended in
# 2025, so current work stays near the top. Ties: the later start first ("2025"
# above "2024-2025" above "2024"), then the order of the Sheet.
#   by = "end"    (default) most recent activity first: courses, talks, policy work
#   by = "start"  most recently STARTED first: employment, where a long side role
#                 (2018-2023) must not outrank a post begun later (2021-2022)
sort_by_when <- function(df, col, by = c("end", "start")) {
  by <- match.arg(by)
  if (!col %in% names(df)) return(df)
  v <- df[[col]]
  if (inherits(v, "Date") || inherits(v, "POSIXt")) {
    k1 <- as.numeric(as.Date(v)); k2 <- k1
  } else {
    s   <- as.character(v)
    yrs <- str_extract_all(s, "\\d{4}")
    end <- vapply(yrs, function(x) if (length(x)) max(as.numeric(x)) else NA_real_, numeric(1))
    beg <- vapply(yrs, function(x) if (length(x)) min(as.numeric(x)) else NA_real_, numeric(1))
    mo  <- suppressWarnings(as.numeric(str_match(s, "^\\d{4}-(\\d{1,2})$")[, 2]))
    mo[is.na(mo) | mo < 1 | mo > 12] <- 6      # no month given: the middle of the year
    if (by == "start") { k1 <- beg * 12 + mo; k2 <- end }
    else               { k1 <- end * 12 + mo; k2 <- beg }
  }
  df[order(k1, k2, decreasing = TRUE, na.last = TRUE), , drop = FALSE]
}
# plain newest-first sort on a column of years, NA last
sort_by_year <- function(df, col) {
  if (!col %in% names(df)) return(df)
  df[order(suppressWarnings(as.numeric(df[[col]])), decreasing = TRUE, na.last = TRUE), , drop = FALSE]
}
# the place of each value in a fixed order; a value that is not listed comes last
rank_in <- function(x, levels) { r <- match(nz(x), levels); r[is.na(r)] <- length(levels) + 1L; r }
# just the year, whatever the cell holds ("2026-03-01", "2026-03", "2026")
year_of <- function(x) { y <- str_extract(nz(x), "\\d{4}"); y[is.na(y)] <- ""; y }
# The date of a publication as Zotero holds it: "2026-04-02", "2026/03/23",
# "2021-8-05", "February 2026" or "2026". A month without a day counts as its first
# day, a year alone as the middle of that year.
pub_date <- function(x) {
  s <- nz(x)
  d <- to_date(s)
  mon <- str_match(s, "^([A-Za-z]+)\\.?\\s+(\\d{4})$")
  m   <- pmatch(tolower(substr(mon[, 2], 1, 3)), tolower(month.abb), duplicates.ok = TRUE)
  fix <- is.na(d) & !is.na(m)
  d[fix] <- as.Date(sprintf("%s-%02d-01", mon[fix, 3], m[fix]))
  yr  <- year_of(s)
  fix <- is.na(d) & nzchar(yr)
  d[fix] <- as.Date(paste0(yr[fix], "-06-30"))
  list(date = d, year = ifelse(is.na(d), yr, format(d, "%Y")))
}

## ---------------- text ----------------
# Close a fragment with a full stop, unless it already ends in sentence punctuation
# (a title such as "Is this a good journal?" must not become "journal?.").
end_dot <- function(x) {
  s <- nz(x)
  ifelse(nzchar(s) & !grepl("[.?!]$", s), paste0(s, "."), s)
}
# the opposite, for a piece that gets its full stop when it is joined to others
no_dot <- function(x) sub("\\.$", "", nz(x))
# Typographic quotes for the straight ones a keyboard types: it's -> it's with a
# curled apostrophe, 'word' -> curled single quotes, "word" -> curled double quotes.
# Works on text that esc() has already made safe, and only on the text between tags,
# so an address inside href="..." is never touched.
curly <- function(x) {
  one <- function(s) {
    if (!nzchar(s) || !grepl("&#39;|&quot;", s)) return(s)
    part <- strsplit(s, "(?<=>)|(?=<)", perl = TRUE)[[1]]
    txt  <- !startsWith(part, "<")
    p <- part[txt]
    p <- gsub("(?<=[[:alnum:]])&#39;(?=[[:alnum:]])", "\u2019", p, perl = TRUE)   # inside a word
    p <- gsub("^&#39;(?=s\\b)", "\u2019", p, perl = TRUE)                        # 's right after a tag
    p <- gsub("(^|[\\s(\\[])&#39;", "\\1\u2018", p, perl = TRUE)                 # opens a quote
    p <- gsub("&#39;", "\u2019", p, fixed = TRUE)                                # the rest close one
    p <- gsub("(^|[\\s(\\[])&quot;", "\\1\u201c", p, perl = TRUE)
    p <- gsub("&quot;", "\u201d", p, fixed = TRUE)
    part[txt] <- p
    paste(part, collapse = "")
  }
  vapply(nz(x), one, character(1), USE.NAMES = FALSE)
}
# A plain cell of the Sheet, ready for the page.
tx <- function(x) curly(esc(x))

# Links. Every link looks the same: the link colour and an underline, so a link is
# easy to spot on paper and without colour.
# A link shows a short LABEL, not an address: the CV is read on a screen, so a link
# only has to be clickable, and a printed address is noise.
a_link <- function(href, label) {
  href <- nz(href); label <- nz(label)
  ifelse(nzchar(href) & nzchar(label),
         sprintf('<a class="cv-link" href="%s" target="_blank" rel="noopener">%s</a>', esc(href), tx(label)),
         "")
}
# The label of a link. In this order: the row's own `link_text` from the Sheet (not
# every talk link is slides: it may be news coverage or a recording), then the host
# where it says more than a generic word, then the default of the section.
link_label <- function(u, default = "Website", override = NULL) {
  h <- sub("/.*$", "", sub("^www\\.", "", sub("^https?://", "", nz(u))))
  out <- rep(default, length(h))
  out[grepl("^github\\.com$", h)]  <- "GitHub"
  out[grepl("shinyapps\\.io$", h)] <- "Shiny app"
  if (!is.null(override)) { ov <- nz(override); out[nzchar(ov)] <- ov[nzchar(ov)] }
  out[!nzchar(h)] <- ""
  out
}
# A free-text cell of the Sheet: **bold**, *italic* and [label](https://...) work
# (md_inline() in R/site-data.R), quotes are curled, and its links get the look above.
md <- function(x) {
  x <- curly(md_inline(x))
  x <- gsub('<a href="(mailto:[^"]+)">(.*?)</a>', '<a class="cv-link" href="\\1">\\2</a>', x, perl = TRUE)
  gsub('<a href="(https?://[^"]+)">(.*?)</a>',
       '<a class="cv-link" href="\\1" target="_blank" rel="noopener">\\2</a>', x, perl = TRUE)
}
# "PI" in the Sheet becomes "Principal investigator" on the CV
role_full <- function(x) {
  x <- nz(x)
  tx(ifelse(x %in% names(role_words), unname(role_words[x]), x))
}
# "Research assistant" in the middle of a sentence: a small first letter, but only for
# a plain word ("PhD student" keeps its capital)
lower_first <- function(x) ifelse(grepl("^[A-Z][a-z]+(\\s|$)", x), paste0(tolower(substr(x, 1, 1)), substr(x, 2, nchar(x))), x)
# bold the first part of "Research assistant, OUCRU, Vietnam" (up to the first comma)
lead_bold <- function(s) {
  s    <- nz(s)
  pos  <- regexpr(",", s, fixed = TRUE)
  lead <- ifelse(pos > 0, substr(s, 1, pos - 1), s)
  rest <- ifelse(pos > 0, substr(s, pos, nchar(s)), "")
  paste0('<span class="cv-t">', md(lead), '</span>', md(rest))
}
# my own name in bold wherever it stands in an author list
bold_name <- function(x) gsub(paste0("(\\b", own_name, "[A-Z]*)"), "<strong>\\1</strong>", x)
# never break a line inside "Pan-ngum" or "1060-1068"
keep_whole <- function(x) gsub("([^\\s,;<>]+-[^\\s,;<>]+)", '<span class="cv-nw">\\1</span>', x, perl = TRUE)

## ---------------- the building blocks of a section ----------------
# Every section is the same kind of list, so the whole CV reads in one way:
#   row_list()   date left, entry right. `fig` adds one figure at the right end of
#                each row (an amount, a number of people).
#   sub_head()   a small heading inside a section ("Invited talks")
row_list <- function(when, what, fig = NULL)
  sprintf('<ul class="cv-list" role="list">\n%s\n</ul>',
          paste(sprintf('<li class="cv-row%s"><span class="cv-when">%s</span><div class="cv-what">%s</div>%s</li>',
                        if (is.null(fig)) "" else " cv-row--fig", tx(when), what,
                        if (is.null(fig)) "" else sprintf('<span class="cv-fig">%s</span>', fig)), collapse = "\n"))
sub_head <- function(label) sprintf('<h3 class="cv-sub">%s</h3>', tx(label))
# A section whose rows fall into named groups (talks, service): one small heading
# and one list per group, in the order given, a group without rows left out.
grouped_lists <- function(d, col, groups, tab, one_group) {
  warn_unmapped(d, col, groups, tab)
  parts <- character(0)
  for (label in names(groups)) {
    g <- d[nz(d[[col]]) == groups[[label]], , drop = FALSE]
    if (nrow(g)) parts <- c(parts, sub_head(label), one_group(g))
  }
  if (length(parts)) paste(parts, collapse = "\n") else NULL
}

## ---------------- sections ----------------
# Each build_*() takes its tab and returns list(body = <html>, note = <html or "">),
# or NULL when there is nothing to show (the section is then left out).

build_education <- function(d) {
  if (!has_rows(d)) return(NULL)
  if (!all(c("date", "degree", "school") %in% names(d))) {   # fall back to the documented column order
    warning("cv: 'edu' tab is missing date/degree/school column names, using column order", call. = FALSE)
    names(d)[1:3] <- c("date", "degree", "school")
  }
  d <- sort_by_when(d, "date")
  list(body = row_list(d$date, sprintf('<span class="cv-t">%s</span>, %s', tx(d$degree), tx(d$school))))
}

build_experience <- function(d) {
  if (!has_rows(d)) return(NULL)
  d <- sort_by_when(d, "date", by = "start")   # a post begun in 2021 leads one begun in 2018
  list(body = row_list(d$date, lead_bold(paste0(nz(d$job), ", ", nz(d$aff)))))
}

build_grants <- function(d) {
  if (!has_rows(d)) return(NULL)
  # budget, outcome date, total and first year come from grants_clean() in
  # R/site-data.R, the function the About page uses, so both show one total
  g <- grants_clean(d)
  d <- g$rows
  d <- d[order(rank_in(d$my_role, c("PI", "Co-PI", "Co-I")), -as.numeric(d$odate), na.last = TRUE), , drop = FALSE]
  # "Title. Funder (in italics). Principal investigator."  and the amount at the right end
  role <- role_full(d$my_role)
  what <- joined(md(end_dot(d$title)),
                 ifelse(nzchar(nz(d$sponsor)), sprintf('<span class="cv-mi">%s</span>', md(end_dot(d$sponsor))), ""),
                 ifelse(nzchar(role), sprintf('<span class="cv-m">%s.</span>', role), ""))
  note <- if (is.finite(g$total) && g$total > 0)
    sprintf("%s secured%s", esc(usd(g$total)), if (is.finite(g$since)) paste(" since", g$since) else "") else ""
  list(body = row_list(d$period, what, fig = esc(usd(d$budget))), note = note)
}

build_policy <- function(d) {
  if (!has_rows(d)) return(NULL)
  d <- sort_by_when(d, "date")
  list(body = row_list(d$date, sprintf('<span class="cv-ink">%s</span>', md(d$project))))
}

build_engagement <- function(d) {
  if (!has_rows(d)) return(NULL)
  d <- sort_by_when(ensure_cols(d, optional_cols$engagement), "period")
  # bold title, then the sentence that carries the scale and the result
  what <- joined(sprintf('<span class="cv-t">%s</span>', tx(end_dot(d$title))),
                 sprintf('<span class="cv-m">%s</span>', md(end_dot(d$detail))),
                 a_link(d$url, link_label(d$url, "Project page", d$link_text)))
  list(body = row_list(d$period, what))
}

build_service <- function(d) {
  if (!has_rows(d)) return(NULL)
  d <- sort_by_when(ensure_cols(d, optional_cols$service), "period")
  groups <- c("Conference and workshop organisation" = "organising",
              "Networks and leadership"              = "network",
              "Editorial roles"                      = "editorial",
              "Peer review"                          = "review",
              "Professional memberships"             = "membership")
  body <- grouped_lists(d, "category", groups, "service", function(g) {
    rest <- joined(md(g$name), md(g$location), sep = ", ")          # "Name, Location" after the bold role
    what <- paste0(sprintf('<span class="cv-t">%s</span>', md(g$role)),
                   sprintf('<span class="cv-m">%s</span>', ifelse(nzchar(rest), paste0(", ", rest, "."), ".")))
    row_list(g$period, joined(what, a_link(g$url, link_label(g$url, "Website", g$link_text))))
  })
  if (is.null(body)) NULL else list(body = body)
}

build_team <- function(d) {
  if (!has_rows(d)) return(NULL)
  dest <- nz(d$next_dest)
  # research assistants before interns; in each group, those with a next step first, then the newest
  d <- d[order(rank_in(d$role, c("RA", "Intern")), !nzchar(dest), -suppressWarnings(as.numeric(d$year)), na.last = TRUE), , drop = FALSE]
  # "Name, research assistant. Next: PhD programme at ..."
  role <- lower_first(role_full(d$role))
  dest <- nz(d$next_dest)
  what <- joined(paste0(sprintf('<span class="cv-t">%s</span>', tx(d$name)), ifelse(nzchar(role), paste0(", ", role, "."), "")),
                 ifelse(nzchar(dest), sprintf('<span class="cv-m">Next: %s</span>', md(end_dot(dest))), ""))
  list(body = row_list(d$period, what))
}

build_awards <- function(d) {
  # Title, detail, note, links and the order come from awards_clean() in
  # R/site-data.R, the function the About page uses, so both list the same awards
  # in the same order. The title is `description` up to its first comma. A row
  # whose `id` has a scan in img/awards/ gets a "Certificate" link to that file on
  # the website, and `url` adds a link to a page on another site. The photo of the
  # ceremony is for the website only.
  a <- awards_clean(d)
  if (!has_rows(a)) return(NULL)
  base <- site_url()
  # one link for each scan: "Certificate", or "Certificate 2013-14, 2015-16" for several
  cert <- vapply(a$cert, function(f) {
    if (!length(f) || !nzchar(base)) return("")
    w <- award_link_words(f, "Certificate")
    paste(a_link(paste0(base, "/", f), w$text), collapse = ", ")
  }, character(1))
  more <- a_link(a$url, ifelse(a$is_file, link_label(a$url, "Certificate", a$link_text),
                                          link_label(a$url, "Website", a$link_text)))
  links  <- joined(cert, more)
  linked <- nzchar(links)
  # a full stop closes a part only when another part follows it
  follows <- nzchar(a$note) | linked
  title  <- ifelse(follows & !nzchar(a$detail), end_dot(a$title), a$title)
  detail <- ifelse(follows, end_dot(a$detail), a$detail)
  main <- paste0('<span class="cv-t">', md(title), '</span>', ifelse(nzchar(detail), paste0(", ", md(detail)), ""))
  note <- ifelse(nzchar(a$note), sprintf('<span class="cv-m">%s</span>', md(ifelse(linked, end_dot(a$note), a$note))), "")
  list(body = row_list(a$date, joined(main, note, links)))
}

build_teaching <- function(d) {
  if (!has_rows(d)) return(NULL)
  # sorted on `date`, the column shown, so the order on the page always matches the dates on the page
  d <- sort_by_when(ensure_cols(d, optional_cols$teaching), "date")
  groups <- c("Modelling" = "modelling", "Other courses" = "other")   # seminars live in the `talks` tab
  body <- grouped_lists(d, "type", groups, "teaching", function(g) {
    about <- joined(md(no_dot(g$note)), md(no_dot(g$venue)), sep = ". ")   # "What it covers. Where."
    what  <- joined(sprintf('<span class="cv-t">%s</span>', tx(end_dot(g$name))),
                    ifelse(nzchar(about), sprintf('<span class="cv-m">%s.</span>', about), ""),
                    a_link(g$website, link_label(g$website, "Course page", g$link_text)))
    n <- nz(g$attd)                                                         # the people who attended, at the right end
    row_list(g$date, what, fig = ifelse(nzchar(n), sprintf('<span class="cv-m">%s %s</span>', tx(n), ifelse(n == "1", "person", "people")), ""))
  })
  if (is.null(body)) return(NULL)
  list(body = body)
}

build_talks <- function(d) {
  if (!has_rows(d)) return(NULL)
  d <- sort_by_when(ensure_cols(d, optional_cols$talks), "date")
  groups <- c("Invited talks" = "invited", "Contributed talks" = "oral", "Posters" = "poster")
  body <- grouped_lists(d, "type", groups, "talks", function(g) {
    # title in normal text, then "Event, Place." with the event in italics
    ev    <- md(g$event)
    where <- joined(ifelse(nzchar(ev), sprintf("<em>%s</em>", ev), ""), md(g$location), sep = ", ")
    what  <- joined(sprintf('<span class="cv-ink">%s</span>', md(end_dot(g$title))),
                    ifelse(nzchar(where), sprintf('<span class="cv-m">%s.</span>', where), ""),
                    a_link(g$url, link_label(g$url, "Slides", g$link_text)))
    row_list(year_of(g$date), what)
  })
  if (is.null(body)) NULL else list(body = body)
}

build_software <- function(d) {
  if (!has_rows(d)) return(NULL)
  d <- sort_by_year(ensure_cols(d, optional_cols$software), "year")
  what <- joined(sprintf('<span class="cv-t">%s</span>', tx(end_dot(d$name))),
                 sprintf('<span class="cv-m">%s</span>', md(end_dot(d$summary))),
                 a_link(d$url_github, link_label(d$url_github, "Package site", d$link_text)))
  list(body = row_list(d$year, what))
}

build_publications <- function(p) {
  if (!has_rows(p)) return(NULL)
  p <- p[!is.na(p$itemType) & p$itemType == "journalArticle", , drop = FALSE]   # preprints and chapters are left out on purpose
  if (!nrow(p)) return(NULL)
  when <- pub_date(p$date)
  ord  <- order(when$date, decreasing = TRUE, na.last = TRUE)
  p <- p[ord, , drop = FALSE]; year <- when$year[ord]

  # The journal by its full name. Zotero holds an abbreviation for some records
  # only, so the abbreviation is used just when the full name is missing: a list
  # that mixes "BMJ Glob Health" and "Emerging Infectious Diseases" reads as careless.
  journal <- clean(p$publicationTitle)
  journal <- ifelse(nzchar(journal), journal, clean(p$journalAbbreviation))
  vol <- clean(p$volume); iss <- clean(p$issue); pg <- clean(p$pages)
  core  <- ifelse(nzchar(vol), paste0(vol, ifelse(nzchar(iss), paste0("(", iss, ")"), "")), "")
  where <- joined(core, pg, sep = ":")                      # "17(4):1060-1068", or what there is of it
  cite  <- joined(year, where, sep = ";")                   # "2026;17(4):1060-1068"
  doi   <- clean(p$DOI)
  # AMA style: Authors. Title. Journal. Year;Volume(Issue):Pages. doi:10.xxxx
  ref <- joined(
    bold_name(keep_whole(tx(end_dot(p$authors)))),
    tx(end_dot(p$title)),
    ifelse(nzchar(journal), sprintf("<em>%s</em>.", tx(journal)), ""),
    ifelse(nzchar(cite), sprintf('<span class="cv-nw">%s.</span>', esc(cite)), ""),
    ifelse(nzchar(doi), sprintf('<span class="cv-nw">doi:<a class="cv-doi" href="https://doi.org/%s" target="_blank" rel="noopener">%s</a></span>',
                                esc(doi), esc(doi)), ""))
  n <- nrow(p)
  # The year stands in the date column, as in every other section. The number of
  # papers is in the note beside the heading.
  list(body = row_list(nz(year), ref),
       note = sprintf("%d journal article%s. Metrics on %s", n, if (n == 1) "" else "s", a_link(scholar_url, "Google Scholar")))
}

## ---------------- the CV, top to bottom ----------------
# The order is deliberate: first what is rare at this career stage (funded as PI,
# policy impact, field leadership, people supervised), then contribution, then the
# scholarly record. To reorder the CV, reorder these lines. To rename a section,
# change its title. `id` is the anchor of the section (cv.html#publications).
# A section without rows is left out.
cv_sections <- list(
  list(id = "education",    title = "Education",                       tab = "edu",        build = build_education),
  list(id = "experience",   title = "Experience",                      tab = "employ",     build = build_experience),
  list(id = "funding",      title = "Research funding",                tab = "grants",     build = build_grants),
  list(id = "policy",       title = "Policy and public health impact", tab = "policy",     build = build_policy),
  list(id = "engagement",   title = "Public engagement",               tab = "engagement", build = build_engagement),
  list(id = "service",      title = "Service and leadership",          tab = "service",    build = build_service),
  list(id = "supervision",  title = "Supervision and mentoring",       tab = "team",       build = build_team),
  list(id = "awards",       title = "Awards and honours",              tab = "awards",     build = build_awards),
  list(id = "teaching",     title = "Teaching and training",           tab = "teaching",   build = build_teaching),
  list(id = "talks",        title = "Presentations",                   tab = "talks",      build = build_talks),
  list(id = "software",     title = "Software",                        tab = "software",   build = build_software),
  list(id = "publications", title = "Peer-reviewed publications",      tab = "pubs",       build = build_publications)
)
# The hue of the short rule above each heading: the first takes the logo blue, the rest
# cycle the site's four hues (STYLE-GUIDE.md, "The two colour rules").
section_hue <- function(i) if (i == 1) "bl" else c("te", "ma", "cy", "in")[(i - 2) %% 4 + 1]

section_html <- function(i, s, built) {
  note <- if (is.null(built$note) || !nzchar(built$note)) "" else sprintf('<p class="cv-note">%s</p>', built$note)
  sprintf(paste0('<section class="cv-sec hue-%s" id="%s" aria-labelledby="h-%s">\n',
                 '<header class="cv-sec__head"><h2 class="cv-h" id="h-%s">%s</h2>%s</header>\n%s\n</section>'),
          section_hue(i), s$id, s$id, s$id, tx(s$title), note, built$body)
}

## ---------------- data ----------------
# Run by hand: fetch fresh data. Run with CV_IF_CHANGED (deploy.sh, right after
# R/update-data.R): use what is saved, and only fetch if it is older than a day.
if_changed <- nzchar(Sys.getenv("CV_IF_CHANGED")) && !forced
refresh    <- (full_render || !in_quarto || forced) && !if_changed
dat <- site_data(if (refresh) "force" else "auto", sources = c("tabs", "pubs"))
for (s in c("tabs", "pubs")) {
  age <- .sd_hours(dat$fetched[[s]])
  if (!is.null(dat[[s]]) && age > site_config$ttl_hours)
    message("cv: *** USING STALE CACHED DATA (", .sd_labels[[s]], ", ",
            format(dat$fetched[[s]], "%Y-%m-%d"), ") - the published CV may be out of date. ***")
}
if (is.null(dat$tabs) || is.null(dat$pubs)) {
  if (have_out) {
    message("cv: *** KEEPING the existing cv.html / cv.pdf (no data available). They may be STALE. ***")
    quit(save = "no")
  }
  stop("cv: no data and no cache - cannot build the CV.", call. = FALSE)
}
check_columns(dat$tabs)

# Signature of everything the CV is made from. With CV_IF_CHANGED set, an unchanged
# signature means the existing cv.html / cv.pdf are already right, so the heavy part
# (Chrome printing the PDF) is skipped.
cv_signature <- function() {
  tmp <- tempfile(); on.exit(unlink(tmp))
  # the names of the award files count too: a new scan adds a link without any change in the Sheet
  saveRDS(list(tabs = dat$tabs, pubs = dat$pubs, award_files = sort(list.files(award_dir)),
               fonts = sort(list.files(font_dir))), tmp, compress = FALSE)
  paste(tools::md5sum(c(tmp, template, "cv/build-cv.R", "R/site-data.R")), collapse = "-")
}
signature <- cv_signature()
if (if_changed && have_out && file.exists(stamp_file) &&
    identical(readLines(stamp_file, n = 1, warn = FALSE), signature)) {
  message("cv: up to date (data, template, fonts and script unchanged since the last build). Not rebuilt.")
  quit(save = "no")
}

## ---------------- assemble ----------------
message("cv: building sections ...")
parts <- character(0)
for (s in cv_sections) {
  d <- if (identical(s$tab, "pubs")) dat$pubs else dat$tabs[[s$tab]]
  built <- s$build(d)
  if (is.null(built)) { message("cv: no rows for '", s$title, "', section left out."); next }
  parts <- c(parts, section_html(length(parts) + 1L, s, built))
}
if (!length(parts)) stop("cv: every section is empty - nothing to build.", call. = FALSE)

tmpl <- paste(readLines(template, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
if (lengths(regmatches(tmpl, gregexpr("{{SECTIONS}}", tmpl, fixed = TRUE))) != 1)
  stop("cv-template.html must hold the {{SECTIONS}} placeholder exactly once (in the body, not in a comment).", call. = FALSE)
# Every font file the template names must be there: a missing one would stop the
# PDF step with a message that does not say what to do.
fonts   <- unique(unlist(str_extract_all(tmpl, '(?<=url\\("fonts/)[^"]+')))
missing <- fonts[!file.exists(file.path(font_dir, fonts))]
if (length(missing))
  stop("cv: ", length(missing), " font file(s) named in cv-template.html are not in ", font_dir, "/: ",
       paste(utils::head(missing, 3), collapse = ", "), if (length(missing) > 3) " ..." else "",
       "\nRestore the folder from git (git checkout -- cv/fonts).", call. = FALSE)
today <- Sys.Date()
html  <- tmpl
html  <- gsub("{{UPDATED}}", paste(month.name[as.integer(format(today, "%m"))], format(today, "%Y")), html, fixed = TRUE)
html  <- gsub("{{UPDATED_ISO}}", format(today, "%Y-%m"), html, fixed = TRUE)
html  <- sub("{{SECTIONS}}", paste(parts, collapse = "\n\n"), html, fixed = TRUE)
left  <- unique(unlist(str_extract_all(html, "\\{\\{[A-Z_]+\\}\\}")))
if (length(left)) warning("cv: the template holds placeholders this script does not fill: ", paste(left, collapse = ", "), call. = FALSE)

# written to a temporary file first, so a failure never leaves half a cv.html
tmp <- tempfile(fileext = ".html")
con <- file(tmp, open = "wb", encoding = "native.enc")       # "wb": the same line ends on every system
writeLines(enc2utf8(html), con, sep = "\n", useBytes = TRUE)
close(con)
ok <- file.copy(tmp, out_html, overwrite = TRUE); unlink(tmp)
if (!ok) stop("cv: could not write ", out_html, call. = FALSE)
message("cv: wrote ", out_html, " (", length(parts), " sections)")

## ---------------- PDF via Chrome (pagedown) ----------------
if (nzchar(Sys.getenv("CV_SKIP_PDF"))) {
  message("cv: CV_SKIP_PDF is set - cv.pdf NOT rebuilt",
          if (file.exists(out_pdf)) " and may now be stale." else ".")
} else {
  if (!requireNamespace("pagedown", quietly = TRUE))
    stop("cv: the PDF needs the 'pagedown' package. Run  renv::install(\"pagedown\"); renv::snapshot()  ",
         "or set CV_SKIP_PDF=1 to build the HTML only.", call. = FALSE)
  before <- if (file.exists(out_pdf)) file.mtime(out_pdf) else as.POSIXct("1970-01-01", tz = "UTC")
  # The page size, the margins and the footer of each sheet ("Thinh Ong, curriculum
  # vitae, updated ...  Page 1 of 7") all come from the @page rule in the template,
  # so Chrome's own header and footer are switched off. Chrome also writes the tags
  # and the bookmarks (one per section), which help a screen reader and a long read.
  # `outline = FALSE` switches off pagedown's own bookmarks: on a machine that has
  # Ghostscript they would be added by rewriting the whole file, which drops the tags.
  # Two things to know about chrome_print(): it serves the folder cv/ over a local
  # address, and it stops on any file the page asks for and does not get. So
  # everything cv.html loads must sit inside cv/ (the fonts do).
  ok <- tryCatch({
    pagedown::chrome_print(out_html, out_pdf, outline = FALSE,
                           options = list(printBackground = TRUE, preferCSSPageSize = TRUE,
                                          displayHeaderFooter = FALSE,
                                          generateTaggedPDF = TRUE, generateDocumentOutline = TRUE))
    TRUE
  }, error = function(e) { message("cv: chrome_print failed - ", conditionMessage(e)); FALSE })
  if (ok && file.exists(out_pdf) && file.mtime(out_pdf) > before) {
    message("cv: wrote ", out_pdf)
  } else {
    stop("cv: cv.pdf was NOT regenerated. Publishing now would ship a stale or missing PDF. ",
         "Check that Chrome and 'pagedown' are available, or rerun with CV_SKIP_PDF=1 if you accept that.", call. = FALSE)
  }
}
# record what this build was made from (only reached when every step above worked)
if (!nzchar(Sys.getenv("CV_SKIP_PDF"))) writeLines(signature, stamp_file)
message("cv: done.")
