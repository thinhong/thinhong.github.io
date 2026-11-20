# Renders the Software page with the shared page system (theme "Galerie"):
# a page head, an "On this page" row, one numbered part per category, and one
# row per package. The data is the `software` tab of the Sheet, read through
# R/site-data.R. The shell uses the shared `pg-` classes from _theme/theme.scss;
# the package rows use `sw-` classes. Each category takes one hue, in the fixed
# order cyan, indigo, teal, mauve (the page's own hue first).

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

# choose a label for the primary link from its URL
.sw_primary <- function(url) {
  if (!.sw_nz(url)) return(NA)
  if (grepl("github.com", url, fixed = TRUE))   return("GitHub")
  if (grepl("shinyapps.io", url, fixed = TRUE)) return("Open app")
  "Website"
}

# sentence case for a heading from the sheet ("Infectious Disease Modelling" ->
# "Infectious disease modelling"). Words in capitals (acronyms) are left alone.
.sw_sentence <- function(s) {
  w <- strsplit(as.character(s), " ", fixed = TRUE)[[1]]
  if (length(w) > 1) {
    tc <- grepl("^[A-Z][a-z]+$", w)
    tc[1] <- FALSE
    w[tc] <- tolower(w[tc])
  }
  paste(w, collapse = " ")
}

# bold the site owner's name in an author string
.sw_bold_author <- function(s) {
  s <- gsub("Ong T", "<strong>Ong T</strong>", s, fixed = TRUE)
  s <- gsub("T Ong", "<strong>T Ong</strong>", s, fixed = TRUE)
  s
}

.sw_lang_badge <- function(lang) {
  if (!.sw_nz(lang)) return("")
  paste0("<span class='sw-lang'>", lang, "</span>")
}

# logo plate, with a monogram fallback when no logo is set
.sw_logo <- function(z, i) {
  has_logo <- .sw_nz(z$logo[i]) && !grepl("/NA$", z$logo[i])
  if (has_logo) {
    paste0("<div class='sw-logo'><img src='", z$logo[i], "' alt='", z$name[i], " logo' width='76' height='76' loading='lazy' decoding='async'></div>")
  } else {
    paste0("<div class='sw-logo sw-logo--mono'>", substr(z$name[i], 1, 2), "</div>")
  }
}

# one text link in the actions column
.sw_link <- function(label, url) {
  paste0("<a class='tl' href='", url, "' target='_blank' rel='noopener'>", label, "</a>")
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
  if (!is.na(pr)) acts <- paste0(acts, .sw_link(pr, z$url_github[i]))
  if (.sw_nz(z$url_cran[i]))  acts <- paste0(acts, .sw_link("CRAN", z$url_cran[i]))
  if (.sw_nz(z$url_paper[i])) acts <- paste0(acts, .sw_link("Paper", z$url_paper[i]))
  acts <- paste0(acts, "</div>")

  paste0(
    "<article class='sw-row'>",
    .sw_logo(z, i),
    "<div class='sw-main'>",
    "<div class='sw-name-row'><h3 class='sw-name'>", z$name[i], "</h3>",
    "<span class='sw-langs'>", .sw_lang_badge(z$lang1[i]), .sw_lang_badge(z$lang2[i]), "</span>",
    cov,
    "</div>",
    "<p class='sw-summary'>", z$summary[i], "</p>",
    "<p class='sw-authors'>", .sw_bold_author(z$authors[i]), "</p>",
    "</div>",
    acts,
    "</article>\n"
  )
}

# --- logos -----------------------------------------------------------------

# Path of a package logo, as seen from the software/ page. The Sheet names the
# full-size file in img/software/ (some are several MB). If a small copy with the
# same name exists in img/software/thumbs/ (a .png, about 230px), the page uses
# that one, so it loads fast. Without a small copy the full-size file is used.
software_logo <- function(logo, dir = "../img/software") {
  vapply(as.character(logo), function(f) {
    if (is.na(f) || !nzchar(f)) return(NA_character_)
    thumb <- file.path(dir, "thumbs", paste0(tools::file_path_sans_ext(f), ".png"))
    if (file.exists(thumb)) thumb else file.path(dir, f)
  }, character(1), USE.NAMES = FALSE)
}

# --- main ------------------------------------------------------------------

gen_software_table <- function(cv_sheet) {
  z <- cv_sheet
  cats <- unique(z$category)
  ncat <- length(cats)
  hues <- rep(c("cy", "in", "te", "ma"), length.out = ncat)   # page hue first
  slugs <- vapply(cats, .sw_slug, character(1), USE.NAMES = FALSE)
  arrow <- "<svg viewBox='0 0 24 24' aria-hidden='true'><path d='M7 17L17 7M8 7h9v9'/></svg>"

  # page head
  out <- paste0(
    "<div class='pg'>\n",
    "<header class='pg-head'>",
    "<div class='pg-head__main'>",
    "<p class='pg-eyebrow rise'>Programming</p>",
    "<h1 class='pg-title rise d1'>Software &amp; packages</h1>",
    "<div class='pg-stats'>",
    "<div class='pg-stat'><span class='pg-stat-label'>Open-source tools</span><span class='pg-stat-num'>", nrow(z), "</span></div>",
    "<div class='pg-stat'><span class='pg-stat-label'>Research areas</span><span class='pg-stat-num'>", ncat, "</span></div>",
    "</div>",
    "</div>",
    "<div class='pg-head__aside rise d2'>",
    "<p class='pg-lead'>Open-source R packages and tools for infectious disease modelling, biostatistics, and bioinformatics.</p>",
    "<div class='pg-links'><a class='tl' href='https://github.com/thinhong' target='_blank' rel='noopener'>GitHub profile", arrow, "</a></div>",
    "</div>",
    "</header>\n"
  )

  # "On this page" row
  out <- paste0(out, "<nav class='pg-toc' aria-label='On this page'><span class='pg-toc-h'>On this page</span>")
  for (k in seq_along(cats)) {
    out <- paste0(out, "<a class='hue-", hues[k], "' href='#cat-", slugs[k], "'>", .sw_sentence(cats[k]), "</a>")
  }
  out <- paste0(out, "</nav>\n")

  # one numbered part per category
  for (k in seq_along(cats)) {
    tmp  <- z[z$category == cats[k], ]
    n    <- nrow(tmp)
    word <- if (n == 1) " tool" else " tools"
    out <- paste0(out,
      "<section class='pg-part hue-", hues[k], "' id='cat-", slugs[k], "'>",
      "<div class='pg-part__head'><div class='pg-part__line'>",
      "<div class='pg-part__name'><span class='pg-num'>", sprintf("%02d", k), "</span>",
      "<h2 class='pg-sectitle'>", .sw_sentence(cats[k]), "</h2></div>",
      "<span class='pg-secsub'>", n, word, "</span>",
      "</div></div>\n",
      "<div class='pg-rows'>\n")
    for (i in 1:n) out <- paste0(out, .sw_row(tmp, i))
    out <- paste0(out, "</div></section>\n")
  }

  # next page
  out <- paste0(out,
    "<nav class='pg-next hue-ma' aria-label='Next page'><a href='../blog/index.html'>",
    "<span class='nx-main'><span class='nx-label'>Next page</span><span class='nx-title'>Blog</span></span>",
    "<span class='nx-text'>A place to think out loud about disease modelling, code and the work of science.</span>",
    arrow, "</a></nav>\n</div>")

  # a fenced raw block, so pandoc passes the markup through untouched
  cat("```{=html}\n", out, "\n```\n", sep = "")
}
