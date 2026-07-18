#!/usr/bin/env Rscript
# =====================================================================
# cv/build-cv.R - Build the CV (cv/cv.html + cv/cv.pdf) from the Google
# Sheet + Zotero, in the Scholarly design.
#
# This is NOT wired into Quarto. There is deliberately no `pre-render:` hook in
# _quarto.yml, so rendering or previewing a page can never start a CV build. You
# run this yourself, whenever you actually want a new CV:
#
#     Rscript cv/build-cv.R          # rebuild cv.html + cv.pdf from fresh data
#     quarto publish gh-pages        # then publish (ships whatever was built)
#
# So the publish flow is two commands, on purpose. An earlier version tried to be
# clever and hang the build off Quarto's pre-render hook, gated by an environment
# variable. That gate silently failed open and rebuilt the CV on every single
# render (see the note on QUARTO_PROJECT_* at the gate below), which is exactly
# the problem this file no longer has.
#
# Environment variables:
#   CV_SKIP=1      do nothing at all
#   CV_FORCE=1     rebuild + refetch even if the cache is fresh
#   CV_SKIP_PDF=1  rebuild cv.html only (accepting a possibly stale pdf)
#   CV_BUILD=1     only meaningful if the pre-render hook is ever restored
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
# Belt and braces. _quarto.yml has no pre-render hook, so in normal use nothing
# below ever fires and this script only runs when you run it. The gate is kept so
# that if the hook is ever restored, a render still cannot quietly rebuild the CV.
#
# Detect "Quarto is running me" by the presence of ANY QUARTO_PROJECT_* variable.
# Do NOT test QUARTO_PROJECT_DIR: Quarto does not set it for pre-render scripts.
# The documented set is QUARTO_PROJECT_RENDER_ALL / _OUTPUT_DIR / _INPUT_FILES /
# _OUTPUT_FILES / _SCRIPT_PROGRESS / _SCRIPT_QUIET, so matching the prefix catches
# all of them and any future sibling. Testing the wrong name silently fails OPEN,
# which is what made every render rebuild the CV.
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

suppressPackageStartupMessages({
  library(googlesheets4); library(dplyr); library(stringr); library(lubridate)
})

## ---------------- config ----------------
cv_url     <- "https://docs.google.com/spreadsheets/d/1ecqMkclPn-lhzgGHw2UTueiKkjTxuoAvJjpFKs3Jtm4/edit?usp=sharing"
tab_names  <- c("edu", "employ", "grants", "awards", "team", "policy", "teaching",
                "talks", "service", "software")
template   <- "cv/cv-template.html"
out_html   <- "cv/cv.html"
out_pdf    <- "cv/cv.pdf"
cache_file <- "cv/.cv-cache.rds"
cache_ttl  <- 24                      # hours
my_email   <- "ongphucthinh@gmail.com"
own_name   <- "Ong T"                 # surname + leading initial(s); bolded in the reference list
scholar_url <- "https://scholar.google.com/citations?user=PkfvVs0AAAAJ&hl=en"

# Columns each tab must provide. Checked up front so a renamed column fails with a
# clear message instead of a cryptic error deep inside a build_* function.
# 'edu' is deliberately absent: build_education() falls back to column order.
required_cols <- list(
  employ   = c("date", "job", "aff"),
  grants   = c("period", "my_role", "budget_total_usd", "title", "sponsor", "outcome"),
  awards   = c("date", "description"),
  team     = c("period", "role", "name", "next_dest", "year"),
  policy   = c("date", "project"),
  teaching = c("date", "attd", "name", "note", "venue", "website", "type"),
  talks    = c("date", "type", "title", "event"),
  service  = c("period", "category", "role", "name"),
  software = c("year", "name", "summary", "url_github")
)
# optional columns: filled in with NA if the Sheet does not have them yet
# `link_text` overrides the wording of a row's link ("News article", "Recording", ...)
optional_cols <- list(talks    = c("location", "url", "link_text"),
                      service  = c("location", "url", "link_text"),
                      teaching = "link_text",
                      software = "link_text")

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
# Close a fragment with a full stop, unless it already ends in sentence punctuation.
# Stops a title like "Is this a good journal?" coming out as "journal?."
end_dot <- function(x) {
  s <- ifelse(is.na(x), "", as.character(x))
  ifelse(nzchar(s) & !grepl("[.?!]\\s*$", s), paste0(s, "."), s)
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
# Links show a short, consistent LABEL rather than a raw address. This CV is read
# on screen, so a link only has to be clickable; a printed address is noise, and a
# truncated one is worse because it looks typeable but is not. The label is taken
# from the host where that says more than a generic word, otherwise each section
# passes its own default. The href always keeps the full URL.
# `override` is the optional per-row `link_text` column from the Sheet and wins over
# everything: not every talk link is slides (it may be news coverage, a recording or
# a programme), and no rule based on the URL can know that. Leave the cell empty to
# take the host-aware or section default.
link_label <- function(u, default = "Website", override = NULL) {
  s <- strip_scheme(ifelse(is.na(u), "", u))
  h <- sub("/.*$", "", sub("^www\\.", "", s))
  out <- rep(default, length(h))
  out[grepl("^github\\.com$", h)]  <- "GitHub"
  out[grepl("shinyapps\\.io$", h)] <- "Shiny app"
  if (!is.null(override)) {
    ov <- ifelse(is.na(override), "", trimws(as.character(override)))
    out[nzchar(ov)] <- ov[nzchar(ov)]
  }
  out[!nzchar(h)] <- ""
  out
}
# bold my own name wherever it appears in an author list; driven by `own_name`
# above so a change in how Zotero stores the name is a one-line edit
bold_name    <- function(x) gsub(paste0("(\\b", own_name, "[A-Z]*)"), "<b>\\1</b>", x)
lead_bold    <- function(s) {                # bold the lead (before the first comma); allow inline markup in the rest
  s <- as.character(s); s[is.na(s)] <- ""
  pos  <- regexpr(",", s, fixed = TRUE)      # split the RAW text, then md_inline each half
  lead <- ifelse(pos > 0, substr(s, 1, pos - 1), s)
  rest <- ifelse(pos > 0, substr(s, pos, nchar(s)), "")
  paste0('<span class="t">', md_inline(lead), '</span>', md_inline(rest))
}
section <- function(title, inner)
  sprintf('<section class="cv-sec">\n  <h2 class="cv-h">%s</h2>\n  %s\n</section>', title, inner)
# Column templates. Widths live here and in the matching col.w-* CSS, so the same
# column comes out the same width in every section no matter how many columns a
# given table has. A <colgroup> must be the first child of <table>.
CG_WHEN   <- '<colgroup><col class="w-when"><col></colgroup>'
CG_GRANTS <- '<colgroup><col class="w-when"><col class="w-col2"><col class="w-amt"><col></colgroup>'
CG_TEAM   <- '<colgroup><col class="w-when"><col class="w-col2"><col class="w-name"><col></colgroup>'
CG_COUNT  <- '<colgroup><col class="w-when"><col class="w-col2"><col></colgroup>'

table_plain <- function(rows, cg = CG_WHEN)
  sprintf('<table class="cv-table">%s<tbody>\n%s\n</tbody></table>', cg, paste(rows, collapse = "\n"))
arr_desc <- function(df, col) {              # numeric-aware descending sort, NAs last
  if (!col %in% names(df)) return(df)
  v <- suppressWarnings(as.numeric(df[[col]]))
  if (all(is.na(v))) v <- df[[col]]
  df[order(v, decreasing = TRUE, na.last = TRUE), , drop = FALSE]
}
# Newest-first sort for a column holding calendar dates rather than plain years.
# Builds a numeric year*12+month key, so "2024-9" correctly outranks "2024-10"
# (a plain text sort gets that backwards). Understands Date columns, "2024-10-05",
# "2024-10" and "2024". For a RANGE like "2022-2026" it sorts on the LATEST year,
# which is the CV convention: an entry running to 2026 is more recent than one
# that ended in 2025, so ongoing work stays near the top.
# `by` picks which end of a range leads:
#   "end"   (default) most recent activity first, right for one-off dated things
#           like courses, talks, awards and policy work.
#   "start" most recently STARTED first, right for employment, where a long side
#           role (2018-2023) should not outrank a post begun later (2021-2022).
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
    mo[is.na(mo) | mo < 1 | mo > 12] <- 6      # unknown/other month: sort mid-year
    if (by == "start") { k1 <- beg * 12 + mo; k2 <- end }   # start leads, longer run wins ties
    else               { k1 <- end * 12 + mo; k2 <- beg }   # end leads, later start wins ties
  }
  # Two keys, both descending. Without the tie-break "2024-2025" and "2025" score
  # the same and keep Sheet order, which reads as unsorted on the page; ranking the
  # later start first puts 2025 above 2024-2025 above 2024.
  df[order(k1, k2, decreasing = TRUE, na.last = TRUE), , drop = FALSE]
}
# Fail early and clearly if the Sheet has been re-columned under us
check_columns <- function(tabs) {
  bad <- character(0)
  for (tb in names(required_cols)) {
    miss <- setdiff(required_cols[[tb]], names(tabs[[tb]]))
    if (length(miss))
      bad <- c(bad, sprintf("  %-9s missing: %s", tb, paste(miss, collapse = ", ")))
  }
  if (length(bad))
    stop("cv: the Google Sheet is missing columns the CV needs:\n",
         paste(bad, collapse = "\n"),
         "\nRename them in the Sheet, or update `required_cols` in cv/build-cv.R.",
         call. = FALSE)
}
# display just the year, whatever the cell holds ("2026-03-01", "2026-03", "2026")
year_of <- function(x) {
  s <- ifelse(is.na(x), "", as.character(x))
  ifelse(nzchar(s), str_extract(s, "\\d{4}"), "")
}
# a grouped section only renders the categories it knows about, so shout if a row
# carries a value that would otherwise drop off the CV silently
warn_unmapped <- function(d, col, known, tab) {
  if (!col %in% names(d)) return(invisible(NULL))
  extra <- setdiff(unique(d[[col]][!is.na(d[[col]])]), known)
  if (length(extra))
    warning(sprintf("cv: '%s' tab has %s value(s) that match no group, so those rows are NOT on the CV: %s",
                    tab, col, paste(extra, collapse = ", ")), call. = FALSE)
  invisible(NULL)
}
# shared renderer for the grouped sections (teaching, talks, service): emits a
# subgroup header row then its rows, skipping empty groups
grouped_table <- function(body, thead = "", cg = CG_WHEN) {
  sprintf('<table class="cv-table">%s%s<tbody>\n%s</tbody></table>', cg, thead, body)
}
subgroup_row <- function(label, cols)
  sprintf('<tr class="cv-subgroup"><td colspan="%d">%s</td></tr>\n', cols, label)

## ---------------- sections ----------------
build_education <- function(d) {
  if (all(c("date", "degree", "school") %in% names(d))) {
    d <- sort_by_when(d, "date")             # newest first, like every other section
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
  # employment ranks on the START year: a role begun in 2021 leads one begun in 2018,
  # even if that older one ran on longer
  d <- sort_by_when(d, "date", by = "start")
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
    # title in normal text, funder italic (the amount and role already carry the weight here)
    '<tr><td class="c-when">%s</td><td class="c-role">%s</td><td class="c-amt">%s</td><td>%s <span class="cv-meta-i">%s</span></td></tr>',
    esc(d$period), esc(as.character(d$role)), esc(amt),
    md_inline(end_dot(d$title)), md_inline(end_dot(d$sponsor)))
  section("Research funding", paste0(lead, "\n", table_plain(rows, CG_GRANTS)))
}

build_awards <- function(d) {
  d <- sort_by_when(d, "date")   # sort on the column that is actually displayed
  rows <- sprintf('<tr><td class="c-when">%s</td><td>%s</td></tr>', esc(d$date), lead_bold(d$description))
  section("Awards and honours", table_plain(rows))
}

build_team <- function(d) {
  d <- d |>
    mutate(year = suppressWarnings(as.numeric(year)),
           role = factor(role, levels = c("RA", "Intern")),
           next_dest = ifelse(is.na(next_dest), "", next_dest)) |>
    arrange(role, next_dest != "", desc(year))
  thead <- '<thead><tr><th class="c-when">Tenure</th><th class="c-role">Role</th><th class="c-name">Name</th><th>Next destination</th></tr></thead>'
  rows  <- sprintf('<tr><td class="c-when">%s</td><td class="c-role">%s</td><td class="c-name">%s</td><td class="cv-meta">%s</td></tr>',
                   esc(d$period), esc(as.character(d$role)), esc(d$name), md_inline(d$next_dest))
  section("Supervision and mentoring", grouped_table(paste0(paste(rows, collapse = "\n"), "\n"), thead, CG_TEAM))
}

build_policy <- function(d) {
  d <- sort_by_when(d, "date")
  rows <- sprintf('<tr><td class="c-when">%s</td><td>%s</td></tr>', esc(d$date), md_inline(d$project))
  section("Policy and public health impact", table_plain(rows))
}

build_teaching <- function(d) {
  d <- ensure_cols(d, optional_cols$teaching)
  d <- d |> mutate(website = ifelse(is.na(website), "", website))
  # sort on 'date', the column shown in the Year cell, so the visible order always
  # matches the visible dates ('year' could disagree with it)
  d <- sort_by_when(d, "date")
  # seminars now live in the 'talks' tab, so this is courses only
  groups <- c("Modelling" = "modelling", "Other courses" = "other")
  warn_unmapped(d, "type", groups, "teaching")
  body <- ""
  for (label in names(groups)) {
    g <- d |> filter(type == groups[[label]])
    if (nrow(g) == 0) next
    link <- ifelse(g$website != "",
                   sprintf(' <a href="%s">%s</a>', esc(g$website),
                           esc(link_label(g$website, "Course page", g$link_text))), "")
    meta <- mapply(function(n, v) {          # join non-empty parts, no dangling dots
      parts <- c(n, v); parts <- parts[nzchar(parts)]
      if (length(parts) == 0) "" else paste0(paste(parts, collapse = ". "), ".")
    }, md_inline(g$note), md_inline(g$venue), USE.NAMES = FALSE)
    # course name bold and closed with a full stop, so it reads apart from the note
    rows <- sprintf('<tr><td class="c-when">%s</td><td class="c-num">%s</td><td><span class="t">%s</span> <span class="cv-meta">%s</span></td></tr>',
                    esc(g$date), esc(g$attd), esc(end_dot(g$name)), paste0(meta, link))
    body <- paste0(body, subgroup_row(label, 3), paste(rows, collapse = "\n"), "\n")
  }
  thead <- '<thead><tr><th class="c-when">Year</th><th class="c-num">People</th><th>Course</th></tr></thead>'
  section("Teaching and training", grouped_table(body, thead, CG_COUNT))
}

build_talks <- function(d) {
  d <- ensure_cols(d, optional_cols$talks)
  d <- sort_by_when(d, "date")
  groups <- c("Invited talks" = "invited", "Contributed talks" = "oral", "Posters" = "poster")
  warn_unmapped(d, "type", groups, "talks")
  body <- ""
  for (label in names(groups)) {
    g <- d[!is.na(d$type) & d$type == groups[[label]], , drop = FALSE]
    if (nrow(g) == 0) next
    url  <- ifelse(is.na(g$url), "", g$url)
    link <- ifelse(nzchar(url),
                   sprintf(' <a href="%s">%s</a>', esc(url),
                           esc(link_label(url, "Slides", g$link_text))), "")
    # "Event, Location." with the event italic and no dangling comma
    meta <- mapply(function(e, l) {
      ev <- if (nzchar(e)) paste0('<span class="cv-meta-i">', e, '</span>') else ""
      parts <- c(ev, l); parts <- parts[nzchar(parts)]
      if (length(parts) == 0) "" else paste0(paste(parts, collapse = ", "), ".")
    }, md_inline(g$event), md_inline(g$location), USE.NAMES = FALSE)
    # talk title in normal text, event italic
    rows <- sprintf('<tr><td class="c-when">%s</td><td>%s <span class="cv-meta">%s</span>%s</td></tr>',
                    esc(year_of(g$date)), md_inline(end_dot(g$title)), meta, link)
    body <- paste0(body, subgroup_row(label, 2), paste(rows, collapse = "\n"), "\n")
  }
  section("Presentations", grouped_table(body))
}

build_service <- function(d) {
  d <- ensure_cols(d, optional_cols$service)
  d <- sort_by_when(d, "period")
  groups <- c("Conference and workshop organisation" = "organising",
              "Networks and leadership"              = "network",
              "Editorial roles"                      = "editorial",
              "Peer review"                          = "review",
              "Professional memberships"             = "membership")
  warn_unmapped(d, "category", groups, "service")
  body <- ""
  for (label in names(groups)) {
    g <- d[!is.na(d$category) & d$category == groups[[label]], , drop = FALSE]
    if (nrow(g) == 0) next
    url  <- ifelse(is.na(g$url), "", g$url)
    link <- ifelse(nzchar(url),
                   sprintf(' <a href="%s">%s</a>', esc(url),
                           esc(link_label(url, "Website", g$link_text))), "")
    rest <- mapply(function(n, l) {          # ", Name, Location." after the bold role
      parts <- c(n, l); parts <- parts[nzchar(parts)]
      if (length(parts) == 0) "." else paste0(", ", paste(parts, collapse = ", "), ".")
    }, md_inline(g$name), md_inline(g$location), USE.NAMES = FALSE)
    rows <- sprintf('<tr><td class="c-when">%s</td><td><span class="t">%s</span><span class="cv-meta">%s</span>%s</td></tr>',
                    esc(g$period), md_inline(g$role), rest, link)
    body <- paste0(body, subgroup_row(label, 2), paste(rows, collapse = "\n"), "\n")
  }
  section("Service and leadership", grouped_table(body))
}

build_software <- function(d) {
  d <- ensure_cols(d, optional_cols$software)
  d <- arr_desc(d, "year")
  url  <- ifelse(is.na(d$url_github), "", d$url_github)
  link <- ifelse(nzchar(url),
                 sprintf(' <a href="%s">%s</a>', esc(url),
                         esc(link_label(url, "Package site", d$link_text))), "")
  # package name bold
  rows <- sprintf('<tr><td class="c-when">%s</td><td><span class="t">%s</span> <span class="cv-meta">%s</span>%s</td></tr>',
                  esc(d$year), esc(end_dot(d$name)), md_inline(end_dot(d$summary)), link)
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
  # tidy the punctuation the fixed template leaves behind when a field is missing
  ref <- gsub("<i>\\s*</i>\\.\\s*", "", ref)   # no journal: drop the empty slot
  ref <- gsub("\\.\\s*\\.", ".", ref)          # ". ."  -> "."
  ref <- gsub("\\.\\s*;", ";", ref)            # ". ;"  -> ";"   (missing year)
  ref <- gsub("^\\s*\\.\\s*", "", ref)         # no authors: drop the leading dot
  ref <- gsub("[ ]{2,}", " ", ref)             # collapse doubled spaces
  items <- paste(sprintf('<li>%s</li>', ref), collapse = "\n")
  # Count DOWN (27, 26, ... 1) so the newest paper carries the highest number.
  # The visible number comes from a CSS counter seeded here at n+1 (each <li>
  # increments it by -1), because the plain `reversed` attribute is honoured by
  # browsers but not by every PDF engine. `reversed` is kept for semantics.
  lead <- sprintf(
    '<p class="cv-lead">%d peer-reviewed journal articles. Citation metrics and the full list on <a href="%s">Google Scholar</a>.</p>',
    nrow(p), esc(scholar_url))
  section("Peer-reviewed publications",
          sprintf('%s\n<ol class="cv-pubs" reversed style="counter-reset:pub %d">\n%s\n</ol>',
                  lead, nrow(p) + 1L, items))
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

check_columns(dat$tabs)

message("cv: building sections ...")
# Order is deliberate: lead with what is rare at this career stage (funded as PI,
# policy impact, field leadership, people supervised), then contribution, then the
# scholarly record. Reordering the CV = reordering these lines, nothing else.
sections <- paste(
  build_education(dat$tabs$edu),                 # who I am
  build_experience(dat$tabs$employ),
  build_grants(dat$tabs$grants),                 # independence and impact
  build_policy(dat$tabs$policy),
  build_service(dat$tabs$service),
  build_team(dat$tabs$team),
  build_awards(dat$tabs$awards),
  build_teaching(dat$tabs$teaching),             # contribution and craft
  build_talks(dat$tabs$talks),
  build_software(dat$tabs$software),
  build_publications(dat$pubs),                  # the record
  sep = "\n\n")

tmpl <- paste(readLines(template, warn = FALSE), collapse = "\n")
if (!grepl("{{SECTIONS}}", tmpl, fixed = TRUE))
  stop("cv-template.html is missing the {{SECTIONS}} placeholder")
updated_txt <- paste(month.name[as.integer(format(Sys.Date(), "%m"))], format(Sys.Date(), "%Y"))
html <- sub("{{SECTIONS}}", sections, tmpl, fixed = TRUE)

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
  # Running page numbers. Chrome does not implement CSS @page margin boxes
  # (@bottom-center), so the footer has to come from Chrome's own template. It
  # renders in a separate context with no site CSS and no web fonts, hence the
  # inline styling and web-safe serif. headerTemplate is blanked to suppress
  # Chrome's default date + title line. The footer sits in the @page bottom margin,
  # so keep that margin at 15mm or more or the footer gets clipped.
  # Identity left, updated date centred, page number right. A three-cell table is
  # used rather than floats because Chrome centres a floated middle element
  # unreliably in this context.
  # Two cells only: identity + date on the left, page number on the right. With
  # just two cells each one aligns to its own outer edge, so no centring maths and
  # no table-layout tricks are needed. box-sizing:border-box is still essential:
  # "width:100%" PLUS "padding:0 17mm" in the default content-box mode makes the
  # div 34mm WIDER than the page, which shifts everything right and pushes the
  # page number clean off the sheet.
  footer_html <- sprintf(
    paste0('<div style="width:100%%; box-sizing:border-box; padding:0 17mm;',
           ' font-size:9pt; font-family:Georgia,serif; color:#5f5952;">',
           '<table style="width:100%%; border-collapse:collapse;"><tr>',
           '<td style="text-align:left; white-space:nowrap;">',
           'Thinh Ong, Curriculum Vitae, updated %s</td>',
           '<td style="text-align:right; white-space:nowrap;">Page <span class="pageNumber"></span>',
           ' of <span class="totalPages"></span></td>',
           '</tr></table></div>'),
    updated_txt)
  ok <- tryCatch({
    pagedown::chrome_print(out_html, out_pdf,
                           options = list(printBackground = TRUE, preferCSSPageSize = TRUE,
                                          displayHeaderFooter = TRUE,
                                          headerTemplate = "<span></span>",
                                          footerTemplate = footer_html))
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
