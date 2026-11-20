# =====================================================================
# R/site-data.R - one data layer for the website and the CV.
#
# Three sources are fetched here, in one place, and saved in one cache:
#   tabs     the Google Sheet (cv_url): edu, employ, grants, awards, team,
#            policy, engagement, teaching, talks, service, software
#   pubs     the Zotero collection (the publication list)
#   scholar  Google Scholar (total citations and h-index)
#
# Who reads it:
#   index.qmd, about/, research/, software/   the figures, the funding list,
#                                             the package table
#   teaching/ (through R/teaching.R)          the course cards and the facts
#                                             on the course pages
#   cv/build-cv.R                             every section of the CV
# So a number on the website and the same number on the CV always come from
# the same copy of the data.
#
# When the data is refreshed:
#   * by itself, the first time a page is rendered after a source is older
#     than `ttl_hours` (one fetch then serves the whole render),
#   * when you run   Rscript R/update-data.R   (deploy.sh runs it first),
#   * when you run   Rscript cv/build-cv.R     by hand.
# Not during  bash deploy.sh quick : it renders from the saved copy only.
# A failed fetch never breaks a render. The last saved copy of that source is
# used, and the fetch is not tried again for `retry_hours`.
#
# The cache (data/site-data.rds) is NOT in git, because it holds the whole
# Sheet, including columns that are not published anywhere.
#
# Use:
#   source("R/site-data.R")          # from the project root
#   source("../R/site-data.R")       # from a page one folder down
#   dat <- site_data()               # list(tabs, pubs, scholar, fetched, tried)
#   num <- site_numbers(dat)         # the headline figures
#   g   <- site_grants(dat)          # the funding list, cleaned
#   a   <- site_awards(dat)          # the awards, cleaned and newest first
#   h   <- awards_home(a)            # the ones for the Home page
#   r   <- award_rows_html(a)        # their rows as HTML, with the links to their files
# =====================================================================

## ---------------- config (the only place these live) ----------------
site_config <- list(
  cv_url      = "https://docs.google.com/spreadsheets/d/1ecqMkclPn-lhzgGHw2UTueiKkjTxuoAvJjpFKs3Jtm4/edit?usp=sharing",
  tab_names   = c("edu", "employ", "grants", "awards", "team", "policy", "engagement",
                  "teaching", "talks", "service", "software"),
  my_email    = "ongphucthinh@gmail.com",   # only used if the Sheet is not link-viewable
  scholar_id  = "PkfvVs0AAAAJ",
  cache       = "data/site-data.rds",       # relative to the project root
  ttl_hours   = 24,                         # a source older than this is refreshed
  retry_hours = 1                           # after a failed fetch, wait this long before trying again
)
site_config$scholar_url <- sprintf("https://scholar.google.com/citations?user=%s&hl=en",
                                   site_config$scholar_id)

## ---------------- small shared helpers ----------------
# The project root: the nearest folder at or above the working directory that
# holds _quarto.yml. Pages are rendered from their own folder, scripts from the
# root, so every path in this file is built from here.
site_root <- function() {
  d <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  repeat {
    if (file.exists(file.path(d, "_quarto.yml"))) return(d)
    up <- dirname(d)
    if (identical(up, d)) stop("site-data: no _quarto.yml found at or above ", getwd(), call. = FALSE)
    d <- up
  }
}

# HTML-escape a character vector (NA becomes "").
esc <- function(x) {
  x <- ifelse(is.na(x), "", as.character(x))
  x <- gsub("&", "&amp;",  x, fixed = TRUE)
  x <- gsub("<", "&lt;",   x, fixed = TRUE)
  x <- gsub(">", "&gt;",   x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  gsub("'", "&#39;", x, fixed = TRUE)
}
# Inline mini-markdown for FREE-TEXT cells of the Sheet (policy, grants, awards,
# teaching, software, team next-destination). Escapes real HTML FIRST, then
# allows a tiny, safe markup set so a cell can carry emphasis and links:
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
# A cell that may hold a Date, a date-time or text such as "2024-09-30".
to_date <- function(x) {
  if (inherits(x, "Date"))   return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  s <- substr(as.character(x), 1, 10)
  if (requireNamespace("lubridate", quietly = TRUE)) return(suppressWarnings(lubridate::ymd(s)))
  suppressWarnings(as.Date(s, format = "%Y-%m-%d"))
}
# "$237,930"
usd <- function(x) {
  ifelse(is.na(x), "", paste0("$", format(round(x), big.mark = ",", trim = TRUE, scientific = FALSE)))
}

# The address of the site, from _quarto.yml: "https://drthinhong.com".
site_url <- function() {
  qy <- readLines(file.path(site_root(), "_quarto.yml"), warn = FALSE)
  m  <- regmatches(qy, regexec("^\\s*site-url:\\s*[\"']?([^\"'#[:space:]]+)", qy))
  m  <- m[lengths(m) == 2]
  if (!length(m)) return("")
  sub("/+$", "", m[[1]][2])
}
# A link from the Sheet, made ready for a page. A link to a file of this site
# ("https://drthinhong.com/img/awards/x.jpg") becomes a path from the page
# ("../img/awards/x.jpg"), so it also works in  quarto preview  and before the
# file is published. Any other link is returned as it is. `up` is the way from
# the page to the project root: "../" for a page one folder down, "" for Home.
site_href <- function(url, up = "../") {
  base <- site_url()
  if (!nzchar(base)) return(url)
  own <- startsWith(url, paste0(base, "/"))
  ifelse(own, paste0(up, substring(url, nchar(base) + 2)), url)
}

# Progress lines and warnings from this file. They go to stderr with cat(), not
# message(): knitr would print a message INTO the page being rendered, while
# stderr only shows in the terminal.
.sd_say <- function(...) cat("site-data: ", ..., "\n", sep = "", file = stderr())

## ---------------- fetchers (one per source) ----------------
# read_sheet returns list-columns for mixed-type cells; flatten to character
.sd_flatten_cols <- function(df) {
  df[] <- lapply(df, function(col) {
    if (!is.list(col)) return(col)
    vapply(col, function(v) {
      if (is.null(v) || length(v) == 0 || all(is.na(v))) NA_character_ else as.character(v[[1]])
    }, character(1))
  })
  df
}

.sd_fetch_tabs <- function() {
  if (!requireNamespace("googlesheets4", quietly = TRUE)) stop("the googlesheets4 package is not installed")
  cfg <- site_config
  read_one <- function(s)
    .sd_flatten_cols(suppressMessages(googlesheets4::read_sheet(ss = cfg$cv_url, sheet = s)))
  googlesheets4::gs4_deauth()                         # works if the Sheet is link-viewable
  tabs <- try(stats::setNames(lapply(cfg$tab_names, read_one), cfg$tab_names), silent = TRUE)
  if (inherits(tabs, "try-error")) {
    .sd_say("public read of the Sheet failed, signing in to Google instead ...")
    googlesheets4::gs4_auth(cfg$my_email)
    tabs <- stats::setNames(lapply(cfg$tab_names, read_one), cfg$tab_names)
  }
  tabs
}

# "LastName Initials" for every author; institutional authors keep their name
.sd_authors <- function(creators) {
  `%or%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a[1])) b else a
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

.sd_fetch_pubs <- function() {
  for (pkg in c("httr", "jsonlite", "dplyr"))
    if (!requireNamespace(pkg, quietly = TRUE)) stop("the ", pkg, " package is not installed")
  # A page is rendered from its own folder, where R does not read the project's
  # .Renviron. Load it from the root so the Zotero keys are always found.
  if (!nzchar(Sys.getenv("ZOTERO_API_KEY"))) {
    renviron <- file.path(site_root(), ".Renviron")
    if (file.exists(renviron)) readRenviron(renviron)
  }
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
  cols <- c("itemType", "title", "date", "journalAbbreviation",
            "publicationTitle", "volume", "issue", "pages", "DOI")
  z <- dplyr::bind_rows(pages)
  if (nrow(z) == 0) stop("the Zotero collection came back empty")
  for (cl in cols) if (!cl %in% names(z)) z[[cl]] <- NA_character_
  z$authors <- if ("creators" %in% names(z)) {
    vapply(z$creators, function(cr)
      if (is.data.frame(cr) && nrow(cr) > 0) .sd_authors(cr) else NA_character_,
      character(1))
  } else NA_character_
  z[, c("itemType", "authors", cols[-1])]
}

.sd_fetch_scholar <- function() {
  if (!requireNamespace("httr", quietly = TRUE) || !requireNamespace("xml2", quietly = TRUE))
    stop("the httr and xml2 packages are needed")
  resp <- httr::GET(
    site_config$scholar_url,
    httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36"),
    httr::timeout(12)
  )
  if (httr::status_code(resp) != 200) stop("Google Scholar returned ", httr::status_code(resp))
  doc  <- xml2::read_html(httr::content(resp, as = "text", encoding = "UTF-8"))
  # The summary table holds six numbers in td.gsc_rsb_std, in this order:
  #   citations(all), citations(since), h(all), h(since), i10(all), i10(since)
  vals <- xml2::xml_text(xml2::xml_find_all(doc, "//td[@class='gsc_rsb_std']"))
  vals <- suppressWarnings(as.integer(gsub("\\D", "", vals)))
  vals <- vals[!is.na(vals)]
  if (length(vals) < 3 || vals[1] <= 0) stop("could not read the numbers on the Scholar page")
  list(citations = vals[1], hindex = vals[3])
}

.sd_fetchers <- list(tabs = .sd_fetch_tabs, pubs = .sd_fetch_pubs, scholar = .sd_fetch_scholar)
.sd_labels   <- c(tabs = "Google Sheet", pubs = "Zotero", scholar = "Google Scholar")

## ---------------- the cache ----------------
.sd_path <- function() file.path(site_root(), site_config$cache)

.sd_read <- function(path = .sd_path()) {
  empty <- list(version = 1L, tabs = NULL, pubs = NULL, scholar = NULL, fetched = list(), tried = list())
  if (!file.exists(path)) return(empty)
  out <- tryCatch(readRDS(path), error = function(e) NULL)
  if (!is.list(out)) return(empty)
  for (n in c("fetched", "tried")) if (!is.list(out[[n]])) out[[n]] <- list()
  out
}

# Written to a temporary file first, so a failure never leaves half a cache.
.sd_write <- function(dat, path = .sd_path()) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  tmp <- tempfile(tmpdir = dirname(path), fileext = ".rds")
  ok <- tryCatch({ saveRDS(dat, tmp); file.copy(tmp, path, overwrite = TRUE) }, error = function(e) FALSE)
  unlink(tmp)
  invisible(isTRUE(ok))
}

.sd_hours <- function(since, now = Sys.time()) {
  if (is.null(since) || length(since) != 1 || is.na(since)) return(Inf)
  as.numeric(difftime(now, since, units = "hours"))
}

## ---------------- the one entry point ----------------
# refresh:
#   "auto"   (default) refresh a source that is missing or older than ttl_hours,
#            unless a fetch already failed in the last retry_hours
#   "never"  only read what is saved
#   "force"  fetch now, whatever the age
# sources: which of "tabs", "pubs", "scholar" this call may refresh. All three
#          are always returned if they are in the cache.
site_data <- function(refresh = c("auto", "never", "force"),
                      sources = c("tabs", "pubs", "scholar"), quiet = FALSE) {
  refresh <- match.arg(refresh)
  say  <- function(...) if (!quiet) .sd_say(...)
  path <- .sd_path()
  dat  <- .sd_read(path)
  # `bash deploy.sh quick` sets SITE_DATA_REFRESH=never: pages are rendered from
  # the saved copy, with no network call, even when that copy is older than a day.
  if (refresh == "auto" && identical(Sys.getenv("SITE_DATA_REFRESH"), "never") && file.exists(path))
    refresh <- "never"
  if (refresh == "never") return(dat)

  cfg <- site_config; changed <- FALSE
  for (s in intersect(sources, names(.sd_fetchers))) {
    fresh <- !is.null(dat[[s]]) && .sd_hours(dat$fetched[[s]]) < cfg$ttl_hours
    if (refresh == "auto" && (fresh || .sd_hours(dat$tried[[s]]) < cfg$retry_hours)) next
    say("fetching ", .sd_labels[[s]], " ...")
    dat$tried[[s]] <- Sys.time(); changed <- TRUE
    # messages and warnings of the fetching packages are dropped, for the same
    # reason as above: inside a page chunk they would be printed into the page
    got <- tryCatch(suppressWarnings(suppressMessages(.sd_fetchers[[s]]())), error = function(e) {
      why <- strsplit(conditionMessage(e), "\n", fixed = TRUE)[[1]][1]   # first line is enough
      say("could not refresh ", .sd_labels[[s]], ": ", why); NULL })
    if (!is.null(got)) {
      dat[[s]] <- got; dat$fetched[[s]] <- dat$tried[[s]]
    } else if (!is.null(dat[[s]])) {
      say("*** using the saved copy of ", .sd_labels[[s]], " from ",
          format(dat$fetched[[s]], "%Y-%m-%d %H:%M"), ". It may be out of date. ***")
    } else {
      say("*** no saved copy of ", .sd_labels[[s]], " yet. ***")
    }
  }
  if (changed) .sd_write(dat, path)
  dat
}

## ---------------- what the pages ask for ----------------
# The grants tab, cleaned once, so the CV and the About page agree on every
# figure. Takes the tab itself (a data frame) and returns
#   rows   the tab plus `budget` (numeric USD) and `odate` (date of the outcome)
#   total  the sum of all budgets
#   since  the earliest outcome year
grants_clean <- function(d) {
  if (!is.data.frame(d) || nrow(d) == 0)
    return(list(rows = NULL, total = NA_real_, since = NA_integer_))
  d$budget <- suppressWarnings(as.numeric(d$budget_total_usd))
  d$odate  <- to_date(d$outcome)
  yrs <- suppressWarnings(as.integer(format(d$odate, "%Y")))
  list(rows  = d,
       total = sum(d$budget, na.rm = TRUE),
       since = if (all(is.na(yrs))) NA_integer_ else min(yrs, na.rm = TRUE))
}
site_grants <- function(dat = site_data()) grants_clean(dat$tabs$grants)

# The awards tab, cleaned once, so the CV and the About page list the same
# awards, in the same order, with the same words. Takes the tab itself (a data
# frame) and returns one row per award, newest first:
#   date       as typed in the Sheet ("2026", "2022-2026")
#   title      the name of the award: `description` up to its first comma
#   detail     the rest of `description` (who gave it, where), or ""
#   note       one more sentence, from the optional column `note`, or ""
#   home       TRUE when the optional column `home` holds a mark (an x, or a
#              ticked box): the award is also shown on the Home page
#   id         the short name of the award's files, from the optional column
#              `id` ("2026-commendation"), or ""
#   cert       the scans of the certificate, as paths from the project root
#              ("img/awards/2026-commendation-certificate.jpg"): usually one,
#              an empty set when there is none
#   photo      the photos of the ceremony, the same way
#   url        a full link (https://...) to a page about the award on another
#              site, from the optional column `url`, or ""
#   link_text  the wording of that link, from the optional column `link_text`, or ""
#   is_file    TRUE when `url` ends in a picture or PDF extension
# `cert` and `photo` are typed nowhere: award_files() below finds them by their
# file name, so the Sheet never holds a long address.
# Order: the latest year in `date` first; for the same latest year, the later
# start first ("2026" before "2022-2026"); rows that still tie keep the order
# of the Sheet. This is the rule of sort_by_when() in cv/build-cv.R.
# Returns NULL when the tab is missing or empty.
awards_clean <- function(d) {
  if (!is.data.frame(d) || nrow(d) == 0 || !all(c("date", "description") %in% names(d)))
    return(NULL)
  txt <- function(col) {                     # a column as trimmed text, "" where empty or absent
    v <- if (col %in% names(d)) trimws(as.character(d[[col]])) else rep("", nrow(d))
    v[is.na(v)] <- ""
    v
  }
  desc <- txt("description")
  pos  <- regexpr(",", desc, fixed = TRUE)
  out  <- data.frame(
    date      = txt("date"),
    title     = trimws(ifelse(pos > 0, substr(desc, 1, pos - 1), desc)),
    detail    = trimws(ifelse(pos > 0, substr(desc, pos + 1, nchar(desc)), "")),
    note      = txt("note"),
    id        = tolower(txt("id")),
    home      = !tolower(txt("home")) %in% c("", "false", "no", "n", "0"),
    url       = txt("url"),
    link_text = txt("link_text"),
    stringsAsFactors = FALSE)
  files     <- award_files(out$id)
  out$cert  <- files$cert
  out$photo <- files$photo
  out$url[!grepl("^https?://[^[:space:]\"<>]+$", out$url)] <- ""    # full links only
  out$is_file <- grepl("\\.(jpe?g|png|webp|gif|pdf)([?#].*)?$", out$url, ignore.case = TRUE)
  yrs <- regmatches(out$date, gregexpr("[0-9]{4}", out$date))
  end <- vapply(yrs, function(x) if (length(x)) max(as.numeric(x)) else NA_real_, numeric(1))
  beg <- vapply(yrs, function(x) if (length(x)) min(as.numeric(x)) else NA_real_, numeric(1))
  mon <- regmatches(out$date, regexec("^[0-9]{4}-([0-9]{1,2})$", out$date))
  mo  <- vapply(mon, function(x) if (length(x) == 2) as.numeric(x[2]) else NA_real_, numeric(1))
  mo[is.na(mo) | mo < 1 | mo > 12] <- 6
  out <- out[order(end * 12 + mo, beg, decreasing = TRUE, na.last = TRUE), , drop = FALSE]
  out <- out[nzchar(out$title), , drop = FALSE]                     # a blank row of the Sheet is not an award
  rownames(out) <- NULL
  out
}
site_awards <- function(dat = site_data()) awards_clean(dat$tabs$awards)
# The awards for the Home page: the rows with a mark in the column `home`, in
# the same order as everywhere else. With no mark at all, the `n` newest, so
# the block on Home is never empty. Takes the result of awards_clean().
awards_home <- function(a, n = 3) {
  if (is.null(a) || nrow(a) == 0) return(a)
  if (any(a$home)) a[a$home, , drop = FALSE] else utils::head(a, n)
}

# The files of the awards. An award can have a scan of its certificate and a
# photo of the ceremony. Nothing about them is typed in the Sheet except a short
# `id` ("2026-commendation": small letters, numbers and hyphens). The files sit
# in img/awards/, and each name is that id plus the kind of file:
#   img/awards/<id>-certificate.jpg     (.jpg .jpeg .png .webp or .pdf)
#   img/awards/<id>-photo.jpg           (.jpg .jpeg .png or .webp)
# An award with several files of one kind adds a short label to each name, for
# example the year (small letters, numbers and hyphens):
#   img/awards/<id>-certificate-2013-14.jpg
#   img/awards/<id>-certificate-2015-16.jpg
# A file that is not there gives no link, so an award can have any number of
# files of each kind, or none. Takes the ids and returns two columns, `cert` and
# `photo`. Each holds, for every id, the paths of its files from the project
# root, in the order of their labels and named by them ("" for a file without a
# label). An award without files gets an empty set.
# A file in the folder that fits no id would show nowhere, so it is reported in
# the terminal: it means a typing mistake in the id or in the file name.
award_dir <- "img/awards"
award_files <- function(id) {
  pics <- c("jpg", "jpeg", "png", "webp")
  bad  <- nzchar(id) & !grepl("^[a-z0-9]+(-[a-z0-9]+)*$", id)
  if (any(bad))
    .sd_say("*** `awards` tab: an id may only hold small letters, numbers and hyphens. Not used: ",
            paste(id[bad], collapse = ", "), " ***")
  id[bad] <- ""
  have <- list.files(file.path(site_root(), award_dir))
  have <- have[grepl(paste0("\\.(", paste(c(pics, "pdf"), collapse = "|"), ")$"), have, ignore.case = TRUE)]
  low  <- tolower(have)
  find <- function(kind, ext) lapply(id, function(i) {
    if (!nzchar(i)) return(character(0))
    stem <- paste0(i, "-", kind)                                    # an id holds no character that is special here
    hit  <- grepl(paste0("^", stem, "(-[a-z0-9]+)*\\.(", paste(ext, collapse = "|"), ")$"), low)
    lab  <- sub("^-", "", substring(sub("\\.[a-z0-9]+$", "", low[hit]), nchar(stem) + 1))
    ord  <- if (length(lab) && all(grepl("^[0-9]+$", lab))) order(as.numeric(lab)) else order(lab)
    stats::setNames(paste0(award_dir, "/", have[hit])[ord], lab[ord])
  })
  out <- data.frame(row.names = seq_along(id))
  out$cert  <- find("certificate", c(pics, "pdf"))
  out$photo <- find("photo", pics)
  lost <- setdiff(have, basename(unlist(c(out$cert, out$photo))))
  if (length(lost))
    .sd_say("*** ", award_dir, "/ holds ", length(lost), if (length(lost) == 1) " file" else " files",
            " that no award uses: ", paste(lost, collapse = ", "),
            ". Name a file <id>-certificate or <id>-photo, with the id of its row in the `awards` tab. ***")
  dup  <- unique(id[nzchar(id) & duplicated(id)])
  if (length(dup))
    .sd_say("*** `awards` tab: the same id is on more than one row: ", paste(dup, collapse = ", "), " ***")
  out
}
# The words of the links to the files of one kind of one award (one element of
# `cert` or `photo`). One file: "Certificate". Several files: "Certificate
# 2013-14", then "2015-16", "2016-17" (the label of each file; a file without a
# label gets its number). Returns `text`, what each link shows, and `name`, the
# full words of each link ("Certificate 2015-16") for a screen reader.
award_link_words <- function(files, word) {
  n   <- length(files)
  lab <- if (is.null(names(files))) rep("", n) else names(files)
  if (n > 1) lab[!nzchar(lab)] <- as.character(seq_len(n))[!nzchar(lab)]
  name <- trimws(paste(word, lab))
  list(text = ifelse(seq_len(n) == 1, name, lab), name = name)
}

# The rows of a list of awards, as HTML for a `.pg-rows.pg-rows--dated` block: one
# '<div class="pg-row">' per award, with its date, title, detail, note and the
# links to its files. About and Home both call it, so an award reads the same on
# both pages. Takes the result of awards_clean() or awards_home().
#   up      the path from the page to the project root: "../" for a page in a
#           folder (About), "" for Home
#   indent  the spaces in front of each row, so the page source stays tidy
# The links are not typed in the Sheet: they show for the files that exist
# (award_files). A link to a picture carries data-lb, so it opens in the picture
# viewer on the page (script in _theme/site.html). All the pictures of one award
# carry its id, so the reader can go from one to the next. The two caption lines
# are the title and "Certificate, 2026" (or the file's own label). A PDF gets no
# data-lb and opens in a new tab. `url` adds a link to a page about the award.
award_rows_html <- function(a, up = "../", indent = "        ") {
  if (is.null(a) || nrow(a) == 0) return(character(0))
  upper1 <- function(x) paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))   # the detail starts its own block
  arrow  <- '<svg viewBox="0 0 24 24" aria-hidden="true"><path d="M7 17L17 7M8 7h9v9"/></svg>'
  # One link of a row. `text` is what shows; `name` is what a screen reader says, and it also
  # names the award. `lb` holds the extra attributes of a link that opens in the viewer.
  tl     <- function(href, text, name, i, lb = "") ifelse(nzchar(href), sprintf(
    '<a class="tl" href="%s" target="_blank" rel="noopener"%s aria-label="%s">%s%s</a>',
    esc(href), lb, esc(paste0(name, ": ", a$title[i], ", ", a$date[i])), esc(text), arrow), "")
  # The links to the files of one kind, for every award. One file gives "Certificate"; several
  # give "Certificate 2013-14", "2015-16", ... (award_link_words).
  files  <- function(set, word) vapply(seq_along(set), function(i) {
    f <- set[[i]]
    if (!length(f)) return("")
    w  <- award_link_words(f, word)
    lb <- ifelse(grepl("\\.(jpe?g|png|webp)$", f, ignore.case = TRUE), sprintf(
      ' data-lb="%s" data-lb-title="%s" data-lb-meta="%s"', esc(a$id[i]), esc(a$title[i]),
      esc(ifelse(w$name == word, paste0(word, ", ", a$date[i]), w$name))), "")
    paste(tl(paste0(up, f), w$text, w$name, i, lb), collapse = "")
  }, character(1))
  more   <- ifelse(nzchar(a$link_text), a$link_text, ifelse(a$is_file, "Certificate", "Read more"))
  links  <- paste0(files(a$cert, "Certificate"), files(a$photo, "Photo"),
                   tl(site_href(a$url, up), more, more, seq_len(nrow(a))))
  detail <- paste0(md_inline(upper1(a$detail)),
                   ifelse(nzchar(a$note), paste0("<small>", md_inline(a$note), "</small>"), ""),
                   ifelse(nzchar(links), paste0('<span class="pg-links">', links, "</span>"), ""))
  paste0(indent, '<div class="pg-row">\n',
         indent, '  <span class="r-years">', esc(a$date), '</span>\n',
         indent, '  <span class="r-title">', md_inline(a$title), '</span>\n',
         ifelse(nzchar(detail), paste0(indent, '  <span class="r-detail">', detail, '</span>\n'), ""),
         indent, '</div>')
}

# The headline figures. A value is NA when its source has never been fetched,
# and a page leaves that figure out instead of showing a made-up number.
site_numbers <- function(dat = site_data()) {
  int1 <- function(x) if (is.null(x) || length(x) != 1 || is.na(x)) NA_integer_ else as.integer(x)
  g <- site_grants(dat)
  list(
    citations     = int1(dat$scholar$citations),
    hindex        = int1(dat$scholar$hindex),
    # journal articles only, the same rule as the publication list on the CV
    articles      = if (is.data.frame(dat$pubs)) sum(dat$pubs$itemType == "journalArticle", na.rm = TRUE) else NA_integer_,
    software      = if (is.data.frame(dat$tabs$software)) nrow(dat$tabs$software) else NA_integer_,
    grants        = if (is.null(g$rows)) NA_integer_ else nrow(g$rows),
    funding_total = g$total,
    funding_since = g$since
  )
}

# One highlighted figure for a page head: '<div class="pg-stat">...</div>',
# or "" when the value is not known.
stat_html <- function(label, n, hue = NULL) {
  if (is.null(n) || length(n) != 1 || is.na(n)) return("")
  sprintf('<div class="pg-stat%s"><span class="pg-stat-label">%s</span><span class="pg-stat-num">%s</span></div>',
          if (is.null(hue)) "" else paste0(" hue-", hue), esc(label),
          format(n, big.mark = ",", trim = TRUE, scientific = FALSE))
}
