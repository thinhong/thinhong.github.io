# =====================================================================
# R/teaching.R - the Teaching page and the course pages, built from the Sheet.
#
# The `teaching` tab of the Google Sheet has one row per run of a course. The
# CV lists every row (cv/build-cv.R). The website shows the courses: one card
# per course on the Teaching page, and on each course page the line above the
# title, the facts under the head and the "More in ..." cards. So a date, a
# place or a link is typed once, in the Sheet, and the CV and the site agree.
#
# The website reads these columns of the `teaching` tab, beside the ones the
# CV reads (type, date, name, attd, note, venue, website). They are optional,
# and the CV does not read them:
#   id        short name of the course: small letters, numbers and hyphens
#             ("r4babies"). Only a row with an id is on the website, so a row
#             without one is on the CV only. Rows with the same id are runs of
#             one course and share one card. When teaching/<part>/<id>/ holds a
#             page (teaching/others/r4babies/index.qmd), the card opens it;
#             otherwise it opens `website`.
#   web_name  the name on the website, when it is not `name` (the CV name)
#   format    the kind of course: "Short course", "Lecture series" ...
#   section   the part of the Teaching page: modelling, r or skills
#   tags      a few topics, separated by commas
#   with      who taught it, for the course page; empty means "Thinh Ong"
# The `talks` tab feeds the part "Seminars and talks" the same way: a talk with
# an `id` is on the Teaching page (columns id, web_name, format, tags, with).
#
# What is not in the Sheet: the parts of the page (teaching_parts) and the
# icon of each course (teaching_icons). A course with no icon here gets a plain
# page icon.
#
# Order: newest first in every part, the rule of the CV (latest year, then the
# later start). The date shown is the `date` cell as typed ("2024-2025"); a
# talk shows its month ("Sep 2023").
#
# Use, from a page (the first path goes up to the project root):
#   source("../R/site-data.R"); source("../R/teaching.R")
#   tc <- teaching_courses()                             # one row per course
#   cat(html_block(teaching_parts_html(tc)))             # the Teaching page
#   cat(html_block(course_meta_html(tc, "r4babies")))    # a course page: above the title
#   cat(html_block(course_facts_html(tc, "r4babies")))   #   the facts under the head
#   cat(html_block(course_more_html(tc, "r4babies")))    #   "More in R programming"
# =====================================================================

if (!exists("site_data", mode = "function"))
  stop("Source R/site-data.R before R/teaching.R", call. = FALSE)

## ---------------- the parts of the Teaching page ----------------
# In this order, with the hue cycle of the style guide (the page's own teal first).
teaching_parts <- data.frame(
  key   = c("modelling", "r", "skills", "seminars"),
  title = c("Disease modelling", "R programming", "Transferable skills", "Seminars & talks"),
  label = c("Disease modelling", "R programming", "Transferable skills", "Seminars and talks"),
  more  = c("More in disease modelling", "More in R programming", "More in transferable skills",
            "More seminars and talks"),
  hue   = c("te", "ma", "cy", "in"),
  unit  = c("course", "course", "course", "talk"),
  stringsAsFactors = FALSE)

## ---------------- the icons ----------------
# The inside of each card icon (an SVG of 120 x 80). Shapes take tone classes,
# not colours (see "Icons on card plates" in STYLE-GUIDE.md).
teaching_icons <- c(
  "idm-intro" = paste0(
    '<circle class="l" cx="24" cy="40" r="14"/><circle class="l2" cx="60" cy="40" r="14"/>',
    '<circle class="l3" cx="96" cy="40" r="14"/>',
    '<path class="fs" stroke-width="2" d="M40.5 36l3.5 4-3.5 4M76.5 36l3.5 4-3.5 4"/>'),
  "idm4b" = paste0(
    '<path class="l3" d="M10 66C30 66 36 14 50 14C70 14 80 66 110 66Z"/>',
    '<path class="s" stroke-width="2.6" d="M10 66C30 66 36 14 50 14C70 14 80 66 110 66"/>',
    '<path class="fs" stroke-width="1.6" d="M8 66h104"/><circle class="f" cx="50" cy="14" r="4.2"/>'),
  "s4bvn" = paste0(
    '<path class="l3" d="M10 64C40 64 44 16 60 16C76 16 80 64 110 64Z"/>',
    '<path class="s" stroke-width="2.6" d="M10 64C40 64 44 16 60 16C76 16 80 64 110 64"/>',
    '<path class="fs" stroke-width="1.6" stroke-dasharray="3 4" d="M60 8v60"/>',
    '<path class="fs" stroke-width="1.6" d="M8 64h104"/>'),
  "epr" = paste0(
    '<rect class="l3" x="16" y="54" width="7" height="12" rx="1.5"/><rect class="l3" x="26" y="50" width="7" height="16" rx="1.5"/>',
    '<rect class="l3" x="36" y="56" width="7" height="10" rx="1.5"/><rect class="l3" x="46" y="48" width="7" height="18" rx="1.5"/>',
    '<rect class="l3" x="56" y="52" width="7" height="14" rx="1.5"/><rect class="l" x="66" y="36" width="7" height="30" rx="1.5"/>',
    '<rect class="l" x="76" y="22" width="7" height="44" rx="1.5"/><rect class="l" x="86" y="40" width="7" height="26" rx="1.5"/>',
    '<rect class="l3" x="96" y="50" width="7" height="16" rx="1.5"/>',
    '<path class="fs" stroke-width="1.6" stroke-dasharray="3 4" d="M8 44h104"/>',
    '<path class="fs" stroke-width="1.6" d="M8 66h104"/><circle class="f" cx="79.5" cy="13" r="3.6"/>'),
  "r4babies" = paste0(
    '<rect class="l3" x="16" y="48" width="16" height="18" rx="2"/><rect class="l2" x="40" y="38" width="16" height="28" rx="2"/>',
    '<rect class="l1" x="64" y="28" width="16" height="38" rx="2"/><rect class="l" x="88" y="16" width="16" height="50" rx="2"/>',
    '<path class="fs" stroke-width="2" d="M18 38L48 28L72 18L100 6"/><path class="fs" stroke-width="1.6" d="M10 66h100"/>'),
  "oucru-r-training" = paste0(
    '<rect class="l3" x="14" y="8" width="92" height="64" rx="7"/><circle class="f" cx="25" cy="19" r="2.8"/>',
    '<circle class="f" cx="34" cy="19" r="2.8" opacity=".6"/><circle class="f" cx="43" cy="19" r="2.8" opacity=".35"/>',
    '<path class="s" stroke-width="3.2" d="M25 34h30M25 44h50M33 54h36M25 64h22"/>'),
  "r-cafe" = paste0(
    '<path class="l" d="M34 34h44v14a22 22 0 0 1-44 0z"/><path class="s" stroke-width="3.2" d="M78 38h5a8 8 0 0 1 0 16h-7"/>',
    '<path class="fs" stroke-width="2" d="M26 74h60"/>',
    '<path class="fs" stroke-width="2" d="M46 26c-3-4 3-6 0-10M56 26c-3-4 3-6 0-10M66 26c-3-4 3-6 0-10"/>'),
  "aw4b" = paste0(
    '<rect class="l3" x="30" y="8" width="48" height="64" rx="5"/><path class="s" stroke-width="3.2" d="M40 24h28M40 34h28M40 44h16"/>',
    '<circle class="l" cx="80" cy="56" r="13"/><path class="bs" stroke-width="2.6" d="M74 56l4.5 4.5L86.5 52"/>'),
  "is-this-a-good-journal" = paste0(
    '<rect class="l3" x="26" y="8" width="48" height="64" rx="5"/><path class="s" stroke-width="3.2" d="M36 24h28M36 34h28M36 44h14"/>',
    '<circle class="b" cx="76" cy="50" r="13"/><circle class="fs" stroke-width="3" cx="76" cy="50" r="13"/>',
    '<path class="fs" stroke-width="4" d="M85.5 59.5l9 9"/>')
)
# for a course that has no icon above: a plain page
teaching_icon_default <- paste0(
  '<rect class="l3" x="30" y="8" width="60" height="64" rx="5"/>',
  '<path class="s" stroke-width="3.2" d="M41 26h38M41 37h38M41 48h24"/>')

teaching_icon <- function(id) {
  inner <- if (id %in% names(teaching_icons)) teaching_icons[[id]] else teaching_icon_default
  paste0('<svg class="ic" viewBox="0 0 120 80" aria-hidden="true">', inner, "</svg>")
}

## ---------------- reading the Sheet ----------------
# A column of a tab as trimmed text, "" where the cell is empty or the column is missing.
.tc_col <- function(d, col) {
  v <- if (col %in% names(d)) d[[col]] else rep(NA_character_, nrow(d))
  if (inherits(v, "POSIXt") || inherits(v, "Date")) v <- format(as.Date(v), "%Y-%m-%d")
  v <- trimws(as.character(v))
  v[is.na(v) | v == "NA"] <- ""
  v
}
# The date of a talk: a date cell, or text such as "2023-09-25" or "25/09/2023".
.tc_talk_date <- function(x) {
  if (inherits(x, "Date"))   return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  s <- trimws(as.character(x))
  d <- suppressWarnings(as.Date(substr(s, 1, 10), format = "%Y-%m-%d"))
  d[is.na(d)] <- suppressWarnings(as.Date(s[is.na(d)], format = "%d/%m/%Y"))
  d
}
# Years in a date label: the latest and the earliest ("2024-2025" -> 2025, 2024).
.tc_years <- function(x, f) {
  yrs <- regmatches(x, gregexpr("[0-9]{4}", x))
  vapply(yrs, function(y) if (length(y)) f(as.numeric(y)) else NA_real_, numeric(1))
}

# Every run with an id, from both tabs: one row per row of the Sheet.
.tc_runs <- function(dat) {
  out <- list()
  te <- dat$tabs$teaching
  if (is.data.frame(te) && nrow(te) > 0) {
    date <- .tc_col(te, "date")
    out$te <- data.frame(
      kind = "course", row = seq_len(nrow(te)), id = tolower(.tc_col(te, "id")),
      date = date, when = date, name = .tc_col(te, "name"), web_name = .tc_col(te, "web_name"),
      format = .tc_col(te, "format"), section = tolower(.tc_col(te, "section")),
      type = tolower(.tc_col(te, "type")), tags = .tc_col(te, "tags"), with = .tc_col(te, "with"),
      venue = .tc_col(te, "venue"), website = .tc_col(te, "website"),
      end = .tc_years(date, max), beg = .tc_years(date, min), mo = 6,
      stringsAsFactors = FALSE)
  }
  tk <- dat$tabs$talks
  if (is.data.frame(tk) && nrow(tk) > 0 && "date" %in% names(tk)) {
    d  <- .tc_talk_date(tk$date)
    mo <- as.integer(format(d, "%m")); yr <- as.integer(format(d, "%Y"))
    out$tk <- data.frame(
      kind = "talk", row = 1000L + seq_len(nrow(tk)), id = tolower(.tc_col(tk, "id")),
      date = ifelse(is.na(d), "", paste(month.abb[mo], yr)),     # month.abb: English in every locale
      when = ifelse(is.na(d), "", paste(month.name[mo], yr)),
      name = .tc_col(tk, "title"), web_name = .tc_col(tk, "web_name"),
      format = .tc_col(tk, "format"), section = "seminars", type = "talk",
      tags = .tc_col(tk, "tags"), with = .tc_col(tk, "with"),
      venue = .tc_col(tk, "event"), website = .tc_col(tk, "url"),
      end = yr, beg = yr, mo = ifelse(is.na(mo), 6, mo),
      stringsAsFactors = FALSE)
  }
  if (!length(out)) return(NULL)
  runs <- do.call(rbind, unname(out))
  runs <- runs[nzchar(runs$id), , drop = FALSE]
  bad  <- !grepl("^[a-z0-9]+(-[a-z0-9]+)*$", runs$id)
  if (any(bad))
    .sd_say("*** `teaching`/`talks` tab: an id may only hold small letters, numbers and hyphens. ",
            "Not on the website: ", paste(unique(runs$id[bad]), collapse = ", "), " ***")
  runs[!bad, , drop = FALSE]
}

# The page of a course: "others/r4babies" when teaching/others/r4babies/index.qmd
# exists, otherwise "".
.tc_page <- function(id) {
  root <- file.path(site_root(), "teaching")
  hit  <- Sys.glob(file.path(root, "*", id, "index.qmd"))
  if (!length(hit)) return("")
  substring(dirname(hit[1]), nchar(root) + 2)
}

## ---------------- one row per course ----------------
# Columns: id, kind ("course" or "talk"), section, name, format, tags, with,
# venue, date (for cards), when (for the course page), url (the newest run's
# link), page (see .tc_page), links (a list: one link per run when the runs have
# their own links, named by their dates), and the sort keys end, beg, mo, row.
teaching_courses <- function(dat = site_data()) {
  runs <- .tc_runs(dat)
  # A saved copy of the Sheet from before the website columns: fetch it once.
  if ((is.null(runs) || !nrow(runs)) && !identical(Sys.getenv("SITE_DATA_REFRESH"), "never")) {
    .sd_say("the saved copy of the Sheet has no course with an `id`: fetching the Sheet again ...")
    runs <- .tc_runs(site_data("force", sources = "tabs"))
  }
  if (is.null(runs) || !nrow(runs))
    stop("No course in the `teaching` or `talks` tab has an `id`, so the Teaching page would be ",
         "empty. Check the Sheet, then run  Rscript R/update-data.R  from the project root.", call. = FALSE)

  known <- teaching_parts$key[teaching_parts$key != "seminars"]
  odd   <- runs$kind == "course" & !runs$section %in% known
  if (any(odd)) {
    .sd_say("*** `teaching` tab: `section` should be one of ", paste(known, collapse = ", "),
            ". Put in a part by its `type`: ", paste(unique(runs$id[odd]), collapse = ", "), " ***")
    runs$section[odd] <- ifelse(runs$type[odd] == "modelling", "modelling", "skills")
  }

  first <- function(x) { x <- x[nzchar(x)]; if (length(x)) x[1] else "" }
  ids   <- unique(runs$id)
  rows  <- lapply(ids, function(i) {
    r <- runs[runs$id == i, , drop = FALSE]
    r <- r[order(r$end * 12 + r$mo, r$beg, decreasing = TRUE, na.last = TRUE), , drop = FALSE]   # newest run first
    old_first <- rev(seq_len(nrow(r)))                                                          # for "2025, 2026"
    lab  <- function(x) paste(unique(x[old_first][nzchar(x[old_first])]), collapse = ", ")
    has  <- nzchar(r$website)
    own  <- r[has, , drop = FALSE]
    links <- if (nrow(own) > 1 && length(unique(own$website)) > 1)
      stats::setNames(own$website[rev(seq_len(nrow(own)))], own$date[rev(seq_len(nrow(own)))]) else character(0)
    data.frame(
      id = i, kind = r$kind[1], section = first(r$section),
      name = if (nzchar(first(r$web_name))) first(r$web_name) else first(r$name),
      format = first(r$format), tags = first(r$tags),
      with = if (nzchar(first(r$with))) first(r$with) else "Thinh Ong",
      venue = first(r$venue), date = lab(r$date), when = lab(r$when),
      url = first(r$website), page = .tc_page(i),
      end = r$end[1], beg = r$beg[1], mo = r$mo[1], row = min(r$row),
      links = I(list(links)), stringsAsFactors = FALSE)
  })
  tc <- do.call(rbind, rows)
  tc <- tc[order(-tc$end * 12 - tc$mo, -tc$beg, tc$row, na.last = TRUE), , drop = FALSE]
  rownames(tc) <- NULL
  lost <- tc$id[!nzchar(tc$page) & !nzchar(tc$url)]
  if (length(lost))
    .sd_say("*** no page in teaching/<part>/<id>/ and no `website` in the Sheet, so the card is not a link: ",
            paste(lost, collapse = ", "), " ***")
  tc
}

## ---------------- the pieces of HTML ----------------
# Raw HTML for a page chunk with  output: asis  (nothing for an empty piece).
html_block <- function(x) {
  x <- paste(x[nzchar(x)], collapse = "\n")
  if (!nzchar(x)) return("")
  paste0("\n```{=html}\n", x, "\n```\n")
}

.tc_arrow <- '<svg viewBox="0 0 24 24" aria-hidden="true"><path d="M7 17L17 7M8 7h9v9"/></svg>'
.tc_count <- function(n, unit) paste(n, if (n == 1) unit else paste0(unit, "s"))

# One card. `up` is the way from the page to the teaching/ folder: "" on the
# Teaching page, "../../" on a course page. `wide` keeps the plate beside the text.
.tc_card <- function(c, up = "", wide = FALSE, indent = "      ") {
  page  <- c$page
  href  <- if (nzchar(page)) paste0(up, page, "/index.html") else c$url
  new   <- if (!nzchar(page) && nzchar(href)) ' target="_blank" rel="noopener"' else ""
  meta  <- paste0('<span class="meta">',
                  if (nzchar(c$format)) paste0("<b>", esc(c$format), "</b>") else "",
                  if (nzchar(c$date)) paste0("<span>", esc(c$date), "</span>") else "", "</span>")
  tags  <- if (nzchar(c$tags)) paste0(indent, '    <span class="tags">', esc(c$tags), "</span>\n") else ""
  plate <- paste0(indent, '  <span class="plate">', teaching_icon(c$id), "</span>\n")
  links <- c$links[[1]]
  cls   <- paste0("card-g crs", if (wide) " card-g--wide" else "")

  if (length(links) > 1) {
    # Runs with links of their own: the title is the main link (it covers the card)
    # and one link per run sits under the tags (.card-g--links in _theme/theme.scss).
    label <- if (nzchar(page)) "" else paste0(' aria-label="', esc(paste0(c$name, ": ", names(links)[length(links)])), '"')
    yrs <- paste0(indent, '      <a class="tl" href="', esc(links), '" target="_blank" rel="noopener" aria-label="',
                  esc(paste0(names(links), ": ", c$name)), '">', esc(names(links)), .tc_arrow, "</a>\n", collapse = "")
    return(paste0(
      indent, '<div class="', cls, ' card-g--links">\n', plate,
      indent, '  <span class="txt">\n',
      indent, '    ', meta, "\n",
      indent, '    <a class="nm card-g__main" href="', esc(href), '"', new, label, ">", esc(c$name), "</a>\n",
      tags,
      indent, '    <span class="pg-links">\n', yrs, indent, "    </span>\n",
      indent, "  </span>\n",
      indent, "</div>"))
  }
  open  <- if (nzchar(href)) paste0('<a class="', cls, '" href="', esc(href), '"', new, ">") else paste0('<div class="', cls, '">')
  close <- if (nzchar(href)) "</a>" else "</div>"
  paste0(
    indent, open, "\n", plate,
    indent, '  <span class="txt">\n',
    indent, '    ', meta, "\n",
    indent, '    <span class="nm">', esc(c$name), "</span>\n",
    tags,
    indent, "  </span>\n",
    indent, close)
}

# One part of the Teaching page. A part with one course shows a wide card.
.tc_part <- function(p, num, cs, up = "") {
  wide  <- nrow(cs) == 1
  cards <- vapply(seq_len(nrow(cs)), function(k) .tc_card(cs[k, , drop = FALSE], up, wide, if (wide) "    " else "      "), character(1))
  paste0(
    '  <section class="pg-part hue-', p$hue, '" id="', p$key, '">\n',
    '    <div class="pg-part__head">\n',
    '      <div class="pg-part__line">\n',
    '        <div class="pg-part__name"><span class="pg-num">', sprintf("%02d", num), '</span><h2 class="pg-sectitle">',
    esc(p$title), "</h2></div>\n",
    '        <span class="pg-secsub">', .tc_count(nrow(cs), p$unit), "</span>\n",
    "      </div>\n",
    "    </div>\n",
    if (wide) paste0(cards, "\n") else paste0('    <div class="cards">\n', paste(cards, collapse = "\n"), "\n    </div>\n"),
    "  </section>")
}

# The Teaching page below its head: the "On this page" row and one numbered part
# per section that has a course. Two neighbouring parts with one course each sit
# side by side (.pg-duo), as wide cards.
teaching_parts_html <- function(tc, up = "") {
  parts <- teaching_parts[teaching_parts$key %in% tc$section, , drop = FALSE]
  n     <- nrow(parts)
  if (!n) return("")
  cs  <- lapply(parts$key, function(k) tc[tc$section == k, , drop = FALSE])
  one <- vapply(cs, nrow, integer(1)) == 1
  toc <- paste0(
    '  <nav class="pg-toc" aria-label="On this page">\n',
    '    <span class="pg-toc-h">On this page</span>\n',
    paste0('    <a class="hue-', parts$hue, '" href="#', parts$key, '">', esc(parts$label), "</a>\n", collapse = ""),
    "  </nav>")
  out <- toc; k <- 1
  while (k <= n) {
    if (one[k] && k < n && one[k + 1]) {
      out <- c(out, '  <div class="pg-duo">', .tc_part(parts[k, ], k, cs[[k]], up),
               .tc_part(parts[k + 1, ], k + 1, cs[[k + 1]], up), "  </div>")
      k <- k + 2
    } else {
      out <- c(out, .tc_part(parts[k, ], k, cs[[k]], up))
      k <- k + 1
    }
  }
  paste(out, collapse = "\n")
}

# The course of a page, or a clear error when the id is not in the Sheet.
.tc_course <- function(tc, id) {
  i <- match(id, tc$id)
  if (is.na(i))
    stop(sprintf("No course with the id \"%s\" in the `teaching` or `talks` tab of the Sheet. ", id),
         "Add the id there, or fix the id in this page.", call. = FALSE)
  tc[i, , drop = FALSE]
}
.tc_part_of <- function(c) teaching_parts[match(c$section, teaching_parts$key), , drop = FALSE]

# A course page: the line above the title (part, format, date).
course_meta_html <- function(tc, id, indent = "      ") {
  c <- .tc_course(tc, id); p <- .tc_part_of(c)
  paste0(indent, '<p class="pg-metaline rise"><span class="pill hue-', p$hue, '">', esc(p$label), "</span>",
         if (nzchar(c$format)) paste0("<span>", esc(c$format), "</span>") else "",
         if (nzchar(c$when)) paste0("<span>", esc(c$when), "</span>") else "", "</p>")
}

# A course page: the facts under the head (When, Where, With, Format, Topics).
course_facts_html <- function(tc, id, indent = "  ") {
  c <- .tc_course(tc, id)
  f <- c(When = c$when, Where = c$venue, With = c$with, Format = c$format, Topics = c$tags)
  f <- f[nzchar(f)]
  paste0(indent, '<section class="facts" aria-label="Course details">\n',
         paste0(indent, '  <div class="fact"><span>', names(f), "</span><span>", esc(f), "</span></div>\n", collapse = ""),
         indent, "</section>")
}

# A course page: the other courses of its part ("More in R programming"), or
# nothing when it is the only one. `up` is the way from the page to teaching/.
course_more_html <- function(tc, id, up = "../../") {
  c <- .tc_course(tc, id); p <- .tc_part_of(c)
  cs <- tc[tc$section == c$section & tc$id != id, , drop = FALSE]
  if (!nrow(cs)) return("")
  cards <- vapply(seq_len(nrow(cs)), function(k) .tc_card(cs[k, , drop = FALSE], up), character(1))
  paste0(
    '  <section class="pg-part hue-', p$hue, ' pg-part--plain">\n',
    '    <div class="pg-part__head">\n',
    '      <div class="pg-part__line">\n',
    '        <div class="pg-part__name"><h2 class="pg-sectitle">', esc(p$more), "</h2></div>\n",
    '        <span class="pg-secsub">', .tc_count(nrow(cs), p$unit), "</span>\n",
    "      </div>\n",
    "    </div>\n",
    '    <div class="cards">\n', paste(cards, collapse = "\n"), "\n    </div>\n",
    "  </section>")
}
