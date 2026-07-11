#!/usr/bin/env Rscript
# =====================================================================
# cv/build-cv.R - Build the CV (cv/cv.html + cv/cv.pdf) from the Google
# Sheet + Zotero, in the Scholarly design.
#
# This is wired as a Quarto pre-render script (_quarto.yml) but is DECOUPLED
# from rendering: by default it does NOTHING during a render or preview, so
# editing pages never rebuilds the CV. Rebuild it deliberately instead:
#     Rscript cv/build-cv.R                 # standalone, always rebuilds
#     CV_BUILD=1 quarto publish gh-pages    # publish a freshly built CV
#
# When it runs:
#   * During ANY quarto render / preview: skips instantly unless CV_BUILD=1
#     (or CV_FORCE=1) is set, so normal editing never touches the CV.
#   * With CV_BUILD=1 on a whole-site render/publish: refetch + rebuild both
#     files. On network failure it falls back to the cache with a loud warning.
#   * Manual run (Rscript cv/build-cv.R): always rebuilds with fresh data.
#
# Environment variables:
#   CV_BUILD=1     let a quarto render/publish rebuild the CV (off by default)
#   CV_FORCE=1     force a rebuild + fresh fetch (also implies CV_BUILD)
#   CV_SKIP=1      skip the CV even on a manual run
#   CV_SKIP_PDF=1  rebuild cv.html only (accepting a possibly stale pdf)
#
# Requirements:
#   * R packages: googlesheets4, dplyr, stringr, lubridate, httr,
#     jsonlite, pagedown  (pagedown is NOT in renv yet:
#     run  renv::install("pagedown"); renv::snapshot()  once)
#   * Chrome or Edge installed (pagedown uses it to print the PDF)
#   * ZOTERO_API_KEY / ZOTERO_USER_ID / ZOTERO_COLLECTION_KEY in .Renviron
#   * Google auth is only needed if the Sheet is not link-viewable;
#     in that case the cached gargle token for my_email is used.
# =====================================================================

## ---------------- fast gate: is a build even wanted? (runs before anything loads) ----------------
# The CV build is DECOUPLED from normal Quarto renders. As a pre-render hook this
# script exits instantly and changes nothing, so editing or previewing pages never
# rebuilds the CV. Rebuild it on purpose in one of these ways:
#     Rscript cv/build-cv.R                 # standalone, always rebuilds
#     CV_BUILD=1 quarto render              # rebuild while rendering the site
#     CV_BUILD=1 quarto publish gh-pages    # publish a freshly built CV
in_quarto  <- nzchar(Sys.getenv("QUARTO_PROJECT_DIR"))
want_build <- nzchar(Sys.getenv("CV_BUILD")) || nzchar(Sys.getenv("CV_FORCE"))
if (nzchar(Sys.getenv("CV_SKIP"))) {
  message("cv: CV_SKIP is set, skipping.")
  quit(save = "no")
}
if (in_quarto && !want_build) {
  message("cv: not rebuilt (decoupled from render). Run  Rscript cv/build-cv.R  or set CV_BUILD=1 to build.")
  quit(save = "no")
}

suppressPackageStartupMessages({
  library(googlesheets4); library(dplyr); library(stringr); library(lubridate)
})

## ---------------- config ----------------
cv_url     <- "https://docs.google.com/spreadsheets/d/1ecqMkclPn-lhzgGHw2UTueiKkjTxuoAvJjpFKs3Jtm4/edit?usp=sharing"
tab_names  <- c("edu", "employ", "grants", "awards", "team", "policy", "teaching", "software")
template   <- "cv/cv-template.html"
out_html   <- "cv/cv.html"
out_pdf    <- "cv/cv.pdf"
cache_file <- "cv/.cv-cache.rds"
cache_ttl  <- 24                      # hours
my_email   <- "ongphucthinh@gmail.com"

if (!file.exists("_quarto.yml") || !file.exists(template))
  stop("Run this from the project root, e.g.  Rscript cv/build-cv.R")

## ---------------- data-refresh policy (past the gate, so a build is wanted) ----------------
# in_quarto was set at the gate above. full_render tells a whole-site render or
# publish (fresh fetch) apart from a single-file render (reuse cached data unless
# CV_FORCE). have_out is used only to keep existing files if a fetch yields nothing.
full_render <- nzchar(Sys.getenv("QUARTO_PROJECT_RENDER_ALL"))
forced      <- nzchar(Sys.getenv("CV_FORCE"))
have_out    <- file.exists(out_html) && file.exists(out_pdf)

## ---------------- data fetch (cached) ----------------
`%or%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a[1])) b else a

# read_sheet returns list-columns for mixed-type cells; flatten to character
flatten_cols <- function(df) {
  df[] <- lapply(df, function(col) {
    if (!is.list(col)) return(col)
    vapply(col, function(v) {
      if (is.null(v) || length(v) == 0 || all(is.na(v))) NA_character_ else as.character(v[[1]])
    }, character(1))
  })
  df
}

fetch_tabs <- function() {
  read_one <- function(s) flatten_cols(suppressMessages(read_sheet(ss = cv_url, sheet = s)))
  gs4_deauth()                                       # works if the Sheet is link-viewable
  tabs <- try(setNames(lapply(tab_names, read_one), tab_names), silent = TRUE)
  if (inherits(tabs, "try-error")) {
    message("cv: public read failed, signing in to Google instead ...")
    gs4_auth(my_email)
    tabs <- setNames(lapply(tab_names, read_one), tab_names)
  }
  tabs
}

# "LastName Initials" for every author; institutional authors keep their name
process_authors <- function(creators) {
  a <- creators[creators$creatorType == "author", , drop = FALSE]
  if (nrow(a) == 0) return(NA_character_)
  one <- vapply(seq_len(nrow(a)), function(i) {
    ln <- if ("lastName" %in% names(a)) a$lastName[i] else NA
    if (is.na(ln %or% NA)) return((if ("name" %in% names(a)) a$name[i] else NA) %or% "")
    ini <- paste0(substr(unlist(strsplit(a$firstName[i] %or% "", "[- ]")), 1, 1), collapse = "")
    trimws(paste(ln, ini))
  }, character(1))
  paste(one[nzchar(one)], collapse = ", ")
}

ensure_cols <- function(df, cols) {
  for (cl in cols) if (!cl %in% names(df)) df[[cl]] <- NA_character_
  df
}

fetch_pubs <- function() {
  key <- Sys.getenv("ZOTERO_API_KEY"); usr <- Sys.getenv("ZOTERO_USER_ID"); col <- Sys.getenv("ZOTERO_COLLECTION_KEY")
  if (!nzchar(key) || !nzchar(usr) || !nzchar(col))
    stop("ZOTERO_API_KEY / ZOTERO_USER_ID / ZOTERO_COLLECTION_KEY missing from .Renviron")
  url <- sprintf("https://api.zotero.org/users/%s/collections/%s/items", usr, col)
  pages <- list(); start <- 0
  repeat {                                            # Zotero caps limit at 100: paginate
    r <- httr::GET(url, query = list(format = "json", limit = 100, start = start),
                   httr::add_headers("Zotero-API-Key" = key), httr::timeout(60))
    if (httr::status_code(r) != 200) stop("Zotero API returned ", httr::status_code(r))
    d <- jsonlite::fromJSON(httr::content(r, "text", encoding = "UTF-8"))$data
    if (is.null(d) || !is.data.frame(d) || nrow(d) == 0) break
    pages[[length(pages) + 1]] <- d
    if (nrow(d) < 100) break
    start <- start + 100
  }
  z <- dplyr::bind_rows(pages)
  if (nrow(z) == 0) stop("the Zotero collection came back empty")
  z <- ensure_cols(z, c("itemType", "title", "date", "journalAbbreviation",
                        "publicationTitle", "volume", "issue", "pages", "DOI"))
  z$authors <- if ("creators" %in% names(z)) {
    vapply(z$creators, function(cr)
      if (is.data.frame(cr) && nrow(cr) > 0) process_authors(cr) else NA_character_,
      character(1))
  } else NA_character_
  z[, c("itemType", "authors", "title", "date", "journalAbbreviation",
        "publicationTitle", "volume", "issue", "pages", "DOI")]
}

get_data <- function(refresh) {
  cache <- if (file.exists(cache_file)) readRDS(cache_file) else NULL
  if (!refresh && !is.null(cache)) {
    age <- as.numeric(difftime(Sys.time(), cache$fetched_at, units = "hours"))
    if (age < cache_ttl) {
      message(sprintf("cv: using cached data (%.1f h old). CV_FORCE=1 for a fresh fetch.", age))
      return(cache)
    }
  }
  message("cv: fetching the Google Sheet + Zotero ...")
  dat <- tryCatch(
    list(fetched_at = Sys.time(), tabs = fetch_tabs(), pubs = fetch_pubs()),
    error = function(e) { message("cv: FETCH FAILED - ", conditionMessage(e)); NULL })
  if (!is.null(dat)) { saveRDS(dat, cache_file); return(dat) }
  if (!is.null(cache)) {
    message("cv: *** USING STALE CACHED DATA from ", format(cache$fetched_at),
            " - the published CV may be out of date. ***")
    return(cache)
  }
  NULL
}

## ---------------- small HTML helpers ----------------
esc <- function(x) {
  x <- ifelse(is.na(x), "", as.character(x))
  x <- gsub("&", "&amp;",  x, fixed = TRUE)
  x <- gsub("<", "&lt;",   x, fixed = TRUE)
  x <- gsub(">", "&gt;",   x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  gsub("'", "&#39;", x, fixed = TRUE)
}
# Inline mini-markdown for FREE-TEXT cells (policy, grants, awards, teaching,
# software, team next-destination). Escapes real HTML FIRST, then allows a tiny,
# safe markup set so a Google-Sheet cell can carry emphasis and links:
#   **bold**      -> <strong>              *italic*  -> <em>
#   [label](url)  -> <a href="url">label</a>   (only http / https / mailto URLs)
# Anything that is not one of these exact patterns is left as typed, so a stray
# "*" or "(2020-2023)" in ordinary prose is safe. Vectorised over a column.
# Convention: a URL may not contain a space or a ")"; put bold INSIDE the link
# label, e.g.  [**top ten achievements of 2024**](https://example.com/x).
md_inline <- function(x) {
  x <- esc(x)                                                              # 1) neutralise real HTML
  x <- gsub("\\[([^\\]]+)\\]\\((https?://[^) ]+|mailto:[^) ]+)\\)",
            '<a href="\\2">\\1</a>', x, perl = TRUE)                       # 2) [label](url)
  x <- gsub("\\*\\*([^*]+)\\*\\*", "<strong>\\1</strong>", x, perl = TRUE) # 3) **bold**
  gsub("\\*([^*]+)\\*", "<em>\\1</em>", x, perl = TRUE)                    # 4) *italic* (after bold)
}
clean <- function(x) {                       # NA, "", "null", "0" -> "" (metadata placeholders)
  x <- ifelse(is.na(x), "", as.character(x))
  ifelse(x %in% c("null", "0"), "", x)
}
to_date <- function(x) {
  if (inherits(x, "Date"))   return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  suppressWarnings(ymd(substr(as.character(x), 1, 10)))
}
strip_scheme <- function(u) sub("/+$", "", sub("^https?://", "", u))
bold_name    <- function(x) gsub("\\bOng T([A-Z]*)", "<b>Ong T\\1</b>", x)
lead_bold    <- function(s) {                # bold the lead (before the first comma); allow inline markup in the rest
  s <- as.character(s); s[is.na(s)] <- ""
  pos  <- regexpr(",", s, fixed = TRUE)      # split the RAW text, then md_inline each half
  lead <- ifelse(pos > 0, substr(s, 1, pos - 1), s)
  rest <- ifelse(pos > 0, substr(s, pos, nchar(s)), "")
  paste0('<span class="t">', md_inline(lead), '</span>', md_inline(rest))
}
section <- function(title, inner)
  sprintf('<section class="cv-sec">\n  <h2 class="cv-h">%s</h2>\n  %s\n</section>', title, inner)
table_plain <- function(rows)
  sprintf('<table class="cv-table"><tbody>\n%s\n</tbody></table>', paste(rows, collapse = "\n"))
arr_desc <- function(df, col) {              # numeric-aware descending sort, NAs last
  if (!col %in% names(df)) return(df)
  v <- suppressWarnings(as.numeric(df[[col]]))
  if (all(is.na(v))) v <- df[[col]]
  df[order(v, decreasing = TRUE, na.last = TRUE), , drop = FALSE]
}

## ---------------- sections ----------------
build_education <- function(d) {
  if (all(c("date", "degree", "school") %in% names(d))) {
    when <- d$date; degree <- d$degree; school <- d$school
  } else {                                   # fall back to the documented column order
    warning("cv: 'edu' tab is missing date/degree/school column names, using column order")
    when <- d[[1]]; degree <- d[[2]]; school <- d[[3]]
  }
  rows <- sprintf('<tr><td class="c-when">%s</td><td><span class="t">%s</span>, %s</td></tr>',
                  esc(when), esc(degree), esc(school))
  section("Education", table_plain(rows))
}

build_experience <- function(d) {
  full <- paste0(d$job, ", ", d$aff)
  rows <- sprintf('<tr><td class="c-when">%s</td><td>%s</td></tr>', esc(d$date), lead_bold(full))
  section("Experience", table_plain(rows))
}

build_grants <- function(d) {
  d$budget <- suppressWarnings(as.numeric(d$budget_total_usd))
  d$odate  <- to_date(d$outcome)
  d$role   <- factor(d$my_role, levels = c("PI", "Co-PI", "Co-I"))
  d <- d[order(d$role, -as.numeric(d$odate), na.last = TRUE), , drop = FALSE]
  total <- sum(d$budget, na.rm = TRUE)
  since <- suppressWarnings(min(year(d$odate), na.rm = TRUE))
  lead  <- sprintf('<p class="cv-lead">Secured $%s in research grants%s.</p>',
                   format(round(total), big.mark = ",", trim = TRUE, scientific = FALSE),
                   if (is.finite(since)) paste0(" since ", since) else "")
  amt <- ifelse(is.na(d$budget), "",
                paste0("$", format(round(d$budget), big.mark = ",", trim = TRUE, scientific = FALSE)))
  rows <- sprintf(
    '<tr><td class="c-when">%s</td><td class="c-role">%s</td><td class="c-amt">%s</td><td><span class="t">%s.</span> <span class="cv-meta">%s.</span></td></tr>',
    esc(d$period), esc(as.character(d$role)), esc(amt), md_inline(d$title), md_inline(d$sponsor))
  section("Grants", paste0(lead, "\n", table_plain(rows)))
}

build_awards <- function(d) {
  d <- arr_desc(d, "year")
  rows <- sprintf('<tr><td class="c-when">%s</td><td>%s</td></tr>', esc(d$date), lead_bold(d$description))
  section("Awards", table_plain(rows))
}

build_team <- function(d) {
  d <- d |>
    mutate(year = suppressWarnings(as.numeric(year)),
           role = factor(role, levels = c("RA", "Intern")),
           next_dest = ifelse(is.na(next_dest), "", next_dest)) |>
    arrange(role, next_dest != "", desc(year))
  thead <- '<thead><tr><th class="c-when">Tenure</th><th class="c-role-s">Role</th><th class="c-name">Name</th><th>Next destination</th></tr></thead>'
  rows  <- sprintf('<tr><td class="c-when">%s</td><td class="c-role-s">%s</td><td class="c-name">%s</td><td class="cv-meta">%s</td></tr>',
                   esc(d$period), esc(as.character(d$role)), esc(d$name), md_inline(d$next_dest))
  inner <- sprintf('<table class="cv-table">%s<tbody>\n%s\n</tbody></table>', thead, paste(rows, collapse = "\n"))
  section("Research team", inner)
}

build_policy <- function(d) {
  d <- arr_desc(d, "date")
  rows <- sprintf('<tr><td class="c-when">%s</td><td>%s</td></tr>', esc(d$date), md_inline(d$project))
  section("Policy engagement", table_plain(rows))
}

build_teaching <- function(d) {
  d <- d |> mutate(website = ifelse(is.na(website), "", website))
  d <- arr_desc(d, "year")
  groups <- c("Modelling" = "modelling", "Other courses" = "other", "Seminars" = "seminar")
  body <- ""
  for (label in names(groups)) {
    g <- d |> filter(type == groups[[label]])
    if (nrow(g) == 0) next
    link <- ifelse(g$website != "",
                   sprintf(' <a href="%s">%s</a>', esc(g$website), esc(strip_scheme(g$website))), "")
    meta <- mapply(function(n, v) {          # join non-empty parts, no dangling dots
      parts <- c(n, v); parts <- parts[nzchar(parts)]
      if (length(parts) == 0) "" else paste0(paste(parts, collapse = ". "), ".")
    }, md_inline(g$note), md_inline(g$venue), USE.NAMES = FALSE)
    rows <- sprintf('<tr><td class="c-when">%s</td><td class="c-num">%s</td><td><span class="t">%s</span> <span class="cv-meta">%s</span></td></tr>',
                    esc(g$date), esc(g$attd), esc(g$name), paste0(meta, link))
    body <- paste0(body,
                   sprintf('<tr class="cv-subgroup"><td colspan="3">%s</td></tr>\n', label),
                   paste(rows, collapse = "\n"), "\n")
  }
  thead <- '<thead><tr><th class="c-when">Year</th><th class="c-num">People</th><th>Course</th></tr></thead>'
  inner <- sprintf('<table class="cv-table">%s<tbody>\n%s</tbody></table>', thead, body)
  section("Teaching", inner)
}

build_software <- function(d) {
  d <- arr_desc(d, "year")
  url  <- ifelse(is.na(d$url_github), "", d$url_github)
  link <- ifelse(nzchar(url),
                 sprintf(' <a href="%s">%s</a>', esc(url), esc(strip_scheme(url))), "")
  rows <- sprintf('<tr><td class="c-when">%s</td><td><span class="t">%s.</span> <span class="cv-meta">%s.</span>%s</td></tr>',
                  esc(d$year), esc(d$name), md_inline(d$summary), link)
  section("Software", table_plain(rows))
}

build_publications <- function(p) {
  p <- p |> filter(itemType == "journalArticle")   # note: preprints/chapters are excluded on purpose
  if (nrow(p) == 0) stop("no journal articles found in the Zotero data")
  pdate <- to_date(p$date)
  pyear <- ifelse(is.na(pdate), str_extract(p$date, "[0-9]{4}"), format(pdate, "%Y"))
  pyear <- ifelse(is.na(pyear), "", pyear)
  # sort key: real date, else mid-year of the extracted year, else bottom
  key <- pdate
  key[is.na(key)] <- suppressWarnings(ymd(paste0(pyear[is.na(key)], "-06-30")))
  ord <- order(key, decreasing = TRUE, na.last = TRUE)
  p <- p[ord, , drop = FALSE]; pyear <- pyear[ord]

  jr  <- clean(p$journalAbbreviation)
  jr  <- ifelse(nzchar(jr), jr, clean(p$publicationTitle))
  vol <- clean(p$volume); iss <- clean(p$issue); pg <- clean(p$pages)
  core <- ifelse(nzchar(vol), paste0(vol, ifelse(nzchar(iss), paste0("(", iss, ")"), "")), "")
  vp   <- ifelse(nzchar(core) & nzchar(pg), paste0(core, ":", pg),
                 ifelse(nzchar(core), core, pg))
  tail <- ifelse(nzchar(vp), paste0(esc(pyear), ";", esc(vp), "."), paste0(esc(pyear), "."))
  doi  <- clean(p$DOI)
  doi_a <- ifelse(nzchar(doi),                       # AMA style: doi:10.xxxx (identifier linked, no space)
                  sprintf(' doi:<a href="https://doi.org/%s">%s</a>', esc(doi), esc(doi)), "")
  authors <- bold_name(esc(str_trim(p$authors)))
  ref   <- sprintf('%s. %s. <i>%s</i>. %s%s', authors, esc(p$title), esc(jr), tail, doi_a)
  items <- paste(sprintf('<li>%s</li>', ref), collapse = "\n")
  section("Publications", sprintf('<ol class="cv-pubs">\n%s\n</ol>', items))
}

## ---------------- assemble ----------------
refresh <- full_render || !in_quarto || forced
dat <- get_data(refresh)
if (is.null(dat)) {
  if (have_out) {
    message("cv: *** KEEPING the existing cv.html / cv.pdf (no data available). They may be STALE. ***")
    quit(save = "no")
  }
  stop("cv: no data and no cache - cannot build the CV.")
}

message("cv: building sections ...")
sections <- paste(
  build_education(dat$tabs$edu),     build_experience(dat$tabs$employ),
  build_grants(dat$tabs$grants),     build_awards(dat$tabs$awards),
  build_team(dat$tabs$team),         build_policy(dat$tabs$policy),
  build_teaching(dat$tabs$teaching), build_software(dat$tabs$software),
  build_publications(dat$pubs),
  sep = "\n\n")

tmpl <- paste(readLines(template, warn = FALSE), collapse = "\n")
if (!grepl("{{SECTIONS}}", tmpl, fixed = TRUE))
  stop("cv-template.html is missing the {{SECTIONS}} placeholder")
html <- sub("{{SECTIONS}}", sections, tmpl, fixed = TRUE)
html <- sub("{{UPDATED}}",
            paste(month.name[as.integer(format(Sys.Date(), "%m"))], format(Sys.Date(), "%Y")),
            html, fixed = TRUE)

# write to a temp file first so a failure never leaves a half-written cv.html
tmp <- tempfile(fileext = ".html")
writeLines(html, tmp, useBytes = TRUE)
file.copy(tmp, out_html, overwrite = TRUE); unlink(tmp)
message("cv: wrote ", out_html)

## ---------------- PDF via Chrome (pagedown) ----------------
if (nzchar(Sys.getenv("CV_SKIP_PDF"))) {
  message("cv: CV_SKIP_PDF is set - cv.pdf NOT rebuilt",
          if (file.exists(out_pdf)) " and may now be stale." else ".")
} else {
  if (!requireNamespace("pagedown", quietly = TRUE))
    stop("cv: the PDF needs the 'pagedown' package. Run  renv::install(\"pagedown\"); renv::snapshot()  ",
         "or set CV_SKIP_PDF=1 to build the HTML only.")
  before <- if (file.exists(out_pdf)) file.mtime(out_pdf) else as.POSIXct("1970-01-01", tz = "UTC")
  ok <- tryCatch({
    pagedown::chrome_print(out_html, out_pdf,
                           options = list(printBackground = TRUE, preferCSSPageSize = TRUE))
    TRUE
  }, error = function(e) { message("cv: chrome_print failed - ", conditionMessage(e)); FALSE })
  if (ok && file.exists(out_pdf) && file.mtime(out_pdf) > before) {
    message("cv: wrote ", out_pdf)
  } else {
    stop("cv: cv.pdf was NOT regenerated. Publishing now would ship a stale or missing PDF. ",
         "Check that Chrome and 'pagedown' are available, or rerun with CV_SKIP_PDF=1 if you accept that.")
  }
}
message("cv: done.")
