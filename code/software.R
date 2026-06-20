# Renders the Software page as a grouped index list with category filters.
# Output classes are namespaced with `sw-` and styled in theme.scss.

# --- helpers ---------------------------------------------------------------

# safe non-empty scalar check (handles NULL / NA / "")
.sw_nz <- function(x) {
  if (is.null(x)) return(FALSE)
  if (length(x) != 1) return(FALSE)
  if (is.na(x)) return(FALSE)
  xx <- as.character(x)
  !is.na(xx) && nzchar(xx)
}

# inline SVG icons (stroke uses currentColor)
.sw_icon <- function(kind) {
  switch(kind,
    globe  = "<svg viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='1.7' stroke-linecap='round' stroke-linejoin='round'><circle cx='12' cy='12' r='9'/><path d='M3 12h18M12 3c2.5 2.5 2.5 15 0 18M12 3c-2.5 2.5-2.5 15 0 18'/></svg>",
    github = "<svg viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='1.7' stroke-linecap='round' stroke-linejoin='round'><path d='M9 19c-4 1.5-4-2.5-6-3m12 5v-3.5c0-1 .1-1.4-.5-2 2.8-.3 5.5-1.4 5.5-6a4.6 4.6 0 0 0-1.3-3.2 4.3 4.3 0 0 0-.1-3.2s-1-.3-3.4 1.3a11.6 11.6 0 0 0-6 0C6.3 2.3 5.3 2.6 5.3 2.6a4.3 4.3 0 0 0-.1 3.2A4.6 4.6 0 0 0 3.9 9c0 4.6 2.7 5.7 5.5 6-.4.4-.5.9-.5 1.7V21'/></svg>",
    app    = "<svg viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='1.7' stroke-linecap='round' stroke-linejoin='round'><rect x='3' y='4' width='18' height='16' rx='2'/><path d='M3 9h18M8 14l2 2 4-4'/></svg>",
    cran   = "<svg viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='1.7' stroke-linecap='round' stroke-linejoin='round'><path d='M21 16V8l-9-5-9 5v8l9 5 9-5z'/><path d='M3.3 7.5L12 12l8.7-4.5M12 12v9'/></svg>",
    paper  = "<svg viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='1.7' stroke-linecap='round' stroke-linejoin='round'><path d='M6 2h9l5 5v15H6z'/><path d='M14 2v6h6M9 13h6M9 17h6'/></svg>",
    tools  = "<svg viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='1.8' stroke-linecap='round' stroke-linejoin='round'><path d='M21 16V8l-9-5-9 5v8l9 5 9-5z'/><path d='M3.3 7.5L12 12l8.7-4.5M12 12v9'/></svg>",
    areas  = "<svg viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='1.8' stroke-linecap='round' stroke-linejoin='round'><path d='M12 2 2 7l10 5 10-5-10-5z'/><path d='M2 12l10 5 10-5M2 17l10 5 10-5'/></svg>",
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
    paste0("<div class='sw-logo'><img src='", z$logo[i], "' alt='", z$name[i], " logo' loading='lazy'></div>")
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
                  z$token_codecov[i], "' alt='code coverage'></a>")
  }
  # action links
  acts <- "<div class='sw-actions'>"
  pr <- .sw_primary(z$url_github[i])
  if (!is.na(pr$label)) {
    acts <- paste0(acts, "<a class='sw-lnk sw-lnk--primary' href='", z$url_github[i],
                   "' target='_blank' rel='noopener'>", pr$icon, pr$label, "</a>")
  }
  if (.sw_nz(z$url_cran[i])) {
    acts <- paste0(acts, "<a class='sw-lnk' href='", z$url_cran[i],
                   "' target='_blank' rel='noopener'>", .sw_icon("cran"), "CRAN</a>")
  }
  if (.sw_nz(z$url_paper[i])) {
    acts <- paste0(acts, "<a class='sw-lnk' href='", z$url_paper[i],
                   "' target='_blank' rel='noopener'>", .sw_icon("paper"), "Paper</a>")
  }
  acts <- paste0(acts, "</div>")

  paste0(
    "<div class='sw-row'>",
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
    "</div>"
  )
}

# --- main ------------------------------------------------------------------

gen_software_table <- function(cv_sheet) {
  z <- cv_sheet
  cats <- unique(z$category)
  out <- ""

  # Header with a stat pill (the navbar marks the active section)
  out <- paste0(
    out,
    "<header class='sw-head'>",
    "<h1 class='sw-h1'>Tools I build</h1>",
    "<p class='sw-intro'>Open-source R packages and tools for research and the wider scientific community.</p>",
    "<div class='sw-stats'>",
    "<div class='sw-stat'><span class='sw-stat-num' data-to='", nrow(z), "'>", nrow(z), "</span><span class='sw-stat-label'>Open-source tools</span></div>",
    "<span class='sw-sep'></span>",
    "<div class='sw-stat'><span class='sw-stat-num' data-to='", length(cats), "'>", length(cats), "</span><span class='sw-stat-label'>Research areas</span></div>",
    "</div>",
    "</header>"
  )

  # Filter chips
  out <- paste0(out, "<div class='sw-filters'>")
  out <- paste0(out, "<button class='sw-filter-btn active' data-filter='all'>All</button>")
  for (cat in cats) {
    out <- paste0(out, "<button class='sw-filter-btn' data-filter=\"", cat, "\">", cat, "</button>")
  }
  out <- paste0(out, "</div>")

  # One section per category (the filter shows / hides whole sections)
  for (cat in cats) {
    tmp <- z[z$category == cat, ]
    word <- if (nrow(tmp) == 1) " tool" else " tools"
    out <- paste0(out, "<section class='sw-cat' data-cat=\"", cat, "\">",
                  "<div class='sw-cat-h'><h2>", cat, "</h2><span class='sw-n'>", nrow(tmp), word,
                  "</span><span class='sw-rule'></span></div>")
    for (i in 1:nrow(tmp)) {
      out <- paste0(out, .sw_row(tmp, i))
    }
    out <- paste0(out, "</section>")
  }

  cat(out)
}
