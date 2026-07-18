# Renders the Software page as an editorial index using the shared page system:
# a two-column hero, soft category panels with package rows, and a sticky
# contents nav. The shell (hero, panels, contents nav) uses the shared `pg-`
# classes defined in theme.scss; the package-row internals use `sw-` classes.

# --- helpers ---------------------------------------------------------------

# safe non-empty scalar check (handles NULL / NA / "")
.sw_nz <- function(x) {
  if (is.null(x)) return(FALSE)
  if (length(x) != 1) return(FALSE)
  if (is.na(x)) return(FALSE)
  xx <- as.character(x)
  !is.na(xx) && nzchar(xx)
}

# slugify a category name into a stable anchor id
.sw_slug <- function(s) {
  s <- tolower(trimws(as.character(s)))
  s <- gsub("[^a-z0-9]+", "-", s)
  gsub("^-+|-+$", "", s)
}

# inline SVG icons (stroke uses currentColor)
.sw_icon <- function(kind) {
  switch(kind,
    globe  = "<svg viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='1.7' stroke-linecap='round' stroke-linejoin='round'><circle cx='12' cy='12' r='9'/><path d='M3 12h18M12 3c2.5 2.5 2.5 15 0 18M12 3c-2.5 2.5-2.5 15 0 18'/></svg>",
    github = "<svg viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='1.7' stroke-linecap='round' stroke-linejoin='round'><path d='M9 19c-4 1.5-4-2.5-6-3m12 5v-3.5c0-1 .1-1.4-.5-2 2.8-.3 5.5-1.4 5.5-6a4.6 4.6 0 0 0-1.3-3.2 4.3 4.3 0 0 0-.1-3.2s-1-.3-3.4 1.3a11.6 11.6 0 0 0-6 0C6.3 2.3 5.3 2.6 5.3 2.6a4.3 4.3 0 0 0-.1 3.2A4.6 4.6 0 0 0 3.9 9c0 4.6 2.7 5.7 5.5 6-.4.4-.5.9-.5 1.7V21'/></svg>",
    app    = "<svg viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='1.7' stroke-linecap='round' stroke-linejoin='round'><rect x='3' y='4' width='18' height='16' rx='2'/><path d='M3 9h18M8 14l2 2 4-4'/></svg>",
    cran   = "<svg viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='1.7' stroke-linecap='round' stroke-linejoin='round'><path d='M21 16V8l-9-5-9 5v8l9 5 9-5z'/><path d='M3.3 7.5L12 12l8.7-4.5M12 12v9'/></svg>",
    paper  = "<svg viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='1.7' stroke-linecap='round' stroke-linejoin='round'><path d='M6 2h9l5 5v15H6z'/><path d='M14 2v6h6M9 13h6M9 17h6'/></svg>",
    ""
  )
}

# choose a label + icon for the primary link from its URL
.sw_primary <- function(url) {
  if (!.sw_nz(url)) return(list(label = NA, icon = ""))
  if (grepl("github.com", url, fixed = TRUE))   return(list(label = "GitHub",   icon = .sw_icon("github")))
  if (grepl("shinyapps.io", url, fixed = TRUE)) return(list(label = "Open app", icon = .sw_icon("app")))
  list(label = "Website", icon = .sw_icon("globe"))
}

# bold the site owner's name in an author string
.sw_bold_author <- function(s) {
  s <- gsub("Ong T", "<strong>Ong T</strong>", s, fixed = TRUE)
  s <- gsub("T Ong", "<strong>T Ong</strong>", s, fixed = TRUE)
  s
}

# map a language name to a colour-class key (colours live in theme.scss)
.sw_lang_class <- function(lang) {
  l <- tolower(trimws(as.character(lang)))
  if (l %in% c("r")) "r"
  else if (l %in% c("c++", "cpp", "cxx")) "cpp"
  else if (l %in% c("c")) "c"
  else if (l %in% c("bash", "shell", "sh", "zsh")) "bash"
  else if (l %in% c("python", "py")) "py"
  else if (l %in% c("julia")) "julia"
  else if (l %in% c("javascript", "js", "typescript", "ts")) "js"
  else if (l %in% c("stan")) "stan"
  else "default"
}

.sw_lang_badge <- function(lang) {
  if (!.sw_nz(lang)) return("")
  paste0("<span class='sw-lang sw-lang--", .sw_lang_class(lang), "'>", lang, "</span>")
}

# logo plate, with a monogram fallback when no logo is set
.sw_logo <- function(z, i) {
  has_logo <- .sw_nz(z$logo[i]) && !grepl("/NA$", z$logo[i])
  if (has_logo) {
    paste0("<div class='sw-logo'><img src='", z$logo[i], "' alt='", z$name[i], " logo' loading='lazy' decoding='async'></div>")
  } else {
    paste0("<div class='sw-logo sw-logo--mono'>", substr(z$name[i], 1, 2), "</div>")
  }
}

# a single package row
.sw_row <- function(z, i) {
  # coverage badge (codecov image) shown as metadata next to the languages
  cov <- ""
  if (.sw_nz(z$url_codecov[i]) && .sw_nz(z$token_codecov[i])) {
    cov <- paste0("<a class='sw-cov' href='", z$url_codecov[i],
                  "' target='_blank' rel='noopener' aria-label='Code coverage'><img src='",
                  z$token_codecov[i], "' alt='code coverage' loading='lazy'></a>")
  }
  # action links
  acts <- "<div class='sw-actions'>"
  pr <- .sw_primary(z$url_github[i])
  if (!is.na(pr$label)) {
    acts <- paste0(acts, "<a class='sw-lnk sw-lnk--primary' href='", z$url_github[i],
                   "' target='_blank' rel='noopener'>", pr$icon, "<span>", pr$label, "</span></a>")
  }
  if (.sw_nz(z$url_cran[i])) {
    acts <- paste0(acts, "<a class='sw-lnk' href='", z$url_cran[i],
                   "' target='_blank' rel='noopener'>", .sw_icon("cran"), "<span>CRAN</span></a>")
  }
  if (.sw_nz(z$url_paper[i])) {
    acts <- paste0(acts, "<a class='sw-lnk' href='", z$url_paper[i],
                   "' target='_blank' rel='noopener'>", .sw_icon("paper"), "<span>Paper</span></a>")
  }
  acts <- paste0(acts, "</div>")

  paste0(
    "<article class='sw-row'>",
    .sw_logo(z, i),
    "<div class='sw-main'>",
    "<div class='sw-name-row'><span class='sw-name'>", z$name[i], "</span>",
    "<span class='sw-langs'>", .sw_lang_badge(z$lang1[i]), .sw_lang_badge(z$lang2[i]), "</span>",
    cov,
    "</div>",
    "<p class='sw-summary'>", z$summary[i], "</p>",
    "<p class='sw-authors'>Authors: ", .sw_bold_author(z$authors[i]), "</p>",
    "</div>",
    acts,
    "</article>"
  )
}

# --- data ------------------------------------------------------------------

# Read the software table from Google Sheets, with a 24h on-disk cache so a
# render never has to authenticate or hit the network when the cache is fresh.
# Mirrors the Scholar cache in code/scholar.R:
#   * a fresh cache (< max_age_hours) is used directly, no gs4_auth, no fetch,
#   * a failed or blocked fetch falls back to the last cached copy,
# so the build stays fast and never breaks on a Google outage or an expired
# token. The cache refreshes automatically once it passes max_age_hours.
# Returns the sheet as a data frame.
get_software_sheet <- function(cache = ".software-cache.rds",
                               max_age_hours = 24,
                               email = "ongphucthinh@gmail.com",
                               ss = "https://docs.google.com/spreadsheets/d/1ecqMkclPn-lhzgGHw2UTueiKkjTxuoAvJjpFKs3Jtm4/edit?usp=sharing",
                               sheet = "software") {

  .fresh <- function() {
    if (!file.exists(cache)) return(FALSE)
    age <- as.numeric(difftime(Sys.time(), file.info(cache)$mtime, units = "hours"))
    is.finite(age) && age < max_age_hours
  }
  .read_cache <- function() {
    if (!file.exists(cache)) return(NULL)
    out <- tryCatch(readRDS(cache), error = function(e) NULL)
    if (is.data.frame(out) && nrow(out) > 0) out else NULL
  }

  # 1) a fresh cache wins: no gs4_auth, no network (keeps render + preview fast)
  if (.fresh()) {
    cached <- .read_cache()
    if (!is.null(cached)) return(cached)
  }

  # 2) try Google Sheets
  fetched <- tryCatch({
    if (!requireNamespace("googlesheets4", quietly = TRUE)) stop("googlesheets4 missing")
    googlesheets4::gs4_auth(email = email)
    googlesheets4::read_sheet(ss = ss, sheet = sheet)
  }, error = function(e) NULL)

  if (is.data.frame(fetched) && nrow(fetched) > 0) {
    tryCatch(saveRDS(fetched, cache), error = function(e) NULL)
    return(fetched)
  }

  # 3) stale cache as a last resort, so a failed fetch never breaks the build
  stale <- .read_cache()
  if (!is.null(stale)) return(stale)

  stop("software sheet: no data fetched and no cache available")
}

# --- main ------------------------------------------------------------------

gen_software_table <- function(cv_sheet) {
  z <- cv_sheet
  cats <- unique(z$category)
  ncat <- length(cats)
  out <- ""

  # open shell + two-column hero
  out <- paste0(
    out,
    "<section class='pg'>",
    "<div class='pg-hero'>",
    "<div class='pg-hero__head'>",
    "<span class='pg-eyebrow'>Programming</span>",
    "<h1 class='pg-title'>Software <span class='pg-title-thin'>&amp; Packages</span></h1>",
    "</div>",
    "<div class='pg-hero__aside'>",
    "<p class='pg-lead'>Open-source R packages and tools for infectious disease modelling, biostatistics, and bioinformatics.</p>",
    "<div class='pg-stats'>",
    "<div class='pg-stat'><span class='pg-stat-num' data-to='", nrow(z), "'>", nrow(z), "</span><span class='pg-stat-label'>Open-source tools</span></div>",
    "<span class='pg-sep'></span>",
    "<div class='pg-stat'><span class='pg-stat-num' data-to='", ncat, "'>", ncat, "</span><span class='pg-stat-label'>Research areas</span></div>",
    "</div>",
    "</div>",
    "</div>"
  )

  # body grid: main column + sticky contents nav
  out <- paste0(out, "<div class='pg-body'><main class='pg-main'>")

  for (k in seq_along(cats)) {
    cat_name <- cats[k]
    tmp  <- z[z$category == cat_name, ]
    n    <- nrow(tmp)
    word <- if (n == 1) " tool" else " tools"
    slug <- .sw_slug(cat_name)
    num  <- sprintf("%02d", k)

    out <- paste0(out,
      "<section class='pg-panel' id='cat-", slug, "'>",
      "<div class='pg-sechead'><div>",
      "<span class='pg-num'>[", num, "]</span>",
      "<h2 class='pg-sectitle'>", cat_name, "</h2>",
      "</div><span class='pg-count'>", n, word, "</span></div>",
      "<div class='sw-rows'>")
    for (i in 1:n) out <- paste0(out, .sw_row(tmp, i))
    out <- paste0(out, "</div></section>")
  }

  out <- paste0(out, "</main>")

  # sticky contents nav
  out <- paste0(out, "<nav class='pg-toc' aria-label='Categories on this page'><p class='pg-toc-h'>On this page</p>")
  for (k in seq_along(cats)) {
    cat_name <- cats[k]
    slug <- .sw_slug(cat_name)
    num  <- sprintf("%02d", k)
    active <- if (k == 1) " class='active'" else ""
    out <- paste0(out,
      "<a href='#cat-", slug, "'", active, "><span class='tn'>", num,
      "</span><span class='tt'>", cat_name, "</span></a>")
  }
  out <- paste0(out, "</nav>")

  out <- paste0(out, "</div></section>")

  cat(out)
}
