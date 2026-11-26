#!/usr/bin/env Rscript
# =====================================================================
# R/render-plan.R - work out which pages need rendering.
#
#     Rscript --vanilla R/render-plan.R          # plan for what changed
#     Rscript --vanilla R/render-plan.R full     # plan a render of every page
#     Rscript --vanilla R/render-plan.R plan     # only say what would happen
#     Rscript --vanilla R/render-plan.R done     # after a render: save the record
#     Rscript --vanilla R/render-plan.R check    # is docs/ still what was recorded?
#
# deploy.sh runs this, renders what the plan says, then runs it with `done`,
# and with `check` after publishing.
# You rarely run it by hand, except with `plan` (or  bash deploy.sh plan)  to
# see what the next deploy would render.
#
# Why: `quarto render` rebuilds every page, even when one blog post changed.
# This script compares every source file with a record of the last render
# (data/render-state.rds, not in git) and lists only the pages that depend on
# a changed file. Quarto can render one page or one folder into an existing
# docs/ folder: it then also updates the blog list, the search index and the
# sitemap. So the result is the same site as a full render.
#
# The record also holds a checksum of every file in docs/. `quarto preview`
# writes into docs/ too, and it shows drafts: after a preview, the blog list in
# docs/ can name a draft post. So a page in docs/ that is no longer the one the
# last deploy made is rendered again, even if its source did not change.
#
# The rules, in the order they are tried for each changed file:
#   1. Notes and tools (README.md, STYLE-GUIDE.md, deploy.sh, the page
#      templates, the CV builder)             -> nothing to render
#   2. Site-wide files (_quarto.yml, _theme/, _extensions/, the logo)
#                                             -> every page
#   3. A published file was deleted           -> every page (cleans docs/)
#   4. R code (R/, renv.lock) or the data in data/site-data.rds
#                                             -> every page with an R chunk
#   5. A folder's _metadata.yml               -> every page in that folder
#   6. A list template (.ejs)                 -> the pages beside it
#   7. A page (.qmd)                          -> that page
#   8. A file in a page's folder (image, .bib, data)
#                                             -> that page
#   9. Any other file (img/, js/, cv/cv.html, cv/cv.pdf, cv/fonts/, ...):
#      named in the site settings             -> every page
#      otherwise                              -> copied into docs/ if it is
#                                                published; pages that name it
#                                                are rendered, unless it is a
#                                                file that pages only link to
#                                                (.html .pdf .js .css ...)
#      in a folder the R code lists by itself (img/awards/: award files are
#      found by name)                         -> every page with an R chunk
# A full render also happens when there is no record yet, docs/ is incomplete,
# Quarto was updated, a file in docs/ that is not a page was changed by hand,
# or more than half of the pages need rendering anyway.
#
# Limit: the script knows a page changed, not what changed in it. A page that
# shows text taken from another page is only covered for Quarto listings
# (the blog list). If you add such a link by hand, run  bash deploy.sh full.
#
# Output, in data/ (not in git):
#   render-plan.txt         line 1: full, partial or none. Then one target
#                           per line (a page or a folder) for `quarto render`.
#   render-state.next.rds   the checksums taken before the render. `done` adds
#                           the checksums of docs/ and saves both as
#                           render-state.rds. If a render fails, `done` never
#                           runs, the old record stays, and the next run tries
#                           the same pages again.
#
# Base R only, so it runs with --vanilla (no renv start-up).
# =====================================================================

if (!file.exists("_quarto.yml"))
  stop("Run this from the project root:  Rscript --vanilla R/render-plan.R", call. = FALSE)

args    <- commandArgs(trailingOnly = TRUE)
dry_run <- "plan" %in% args
forced  <- "full" %in% args
finish  <- "done" %in% args

state_file <- "data/render-state.rds"
next_file  <- "data/render-state.next.rds"
plan_file  <- "data/render-plan.txt"
data_cache <- "data/site-data.rds"       # the cache written by R/site-data.R

say <- function(...) cat("    ", ..., "\n", sep = "")

## ---------------- what Quarto is set up to do ----------------
qy <- readLines("_quarto.yml", warn = FALSE)
out_dir <- trimws(sub("^\\s*output-dir:\\s*", "", grep("^\\s*output-dir:", qy, value = TRUE)[1]))
if (is.na(out_dir) || !nzchar(out_dir)) out_dir <- "_site"

# every file in docs/ and its checksum
out_sums <- function() {
  f <- list.files(out_dir, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  setNames(unname(tools::md5sum(file.path(out_dir, f))), f)
}

if (finish) {                            # the render worked: save the record
  if (!file.exists(next_file)) stop("no plan to finish: run the plan step first", call. = FALSE)
  state <- readRDS(next_file)
  state$out <- out_sums()
  saveRDS(state, state_file)
  unlink(next_file)
  quit(save = "no")
}

if ("check" %in% args) {                 # after publishing: did docs/ change meanwhile?
  was <- tryCatch(readRDS(state_file)$out, error = function(e) NULL)
  is  <- out_sums()
  off <- union(setdiff(names(is), names(was)), names(was)[is.na(is[names(was)]) | is[names(was)] != was])
  if (length(off)) {
    say("*** ", out_dir, "/ changed while the site was being published: ",
        paste(utils::head(off, 4), collapse = ", "), if (length(off) > 4) " ..." else "")
    say("*** A `quarto preview` may be running. A preview shows drafts, so a draft may now be public.")
    say("*** Stop the preview and run  bash deploy.sh  again.")
  }
  quit(save = "no")
}

# the `resources:` list of _quarto.yml: files and folders Quarto copies as they are
resources <- character(0)
i <- grep("^\\s*resources:\\s*$", qy)[1]
if (!is.na(i)) for (l in qy[-seq_len(i)]) {
  if (grepl("^\\s*(#|$)", l)) next
  m <- regmatches(l, regexec("^\\s+-\\s+[\"']?([^\"'#]+?)[\"']?\\s*(#.*)?$", l))[[1]]
  if (length(m) < 2) break
  resources <- c(resources, sub("/$", "", trimws(m[2])))
}
in_resources <- function(f) any(f == resources | startsWith(f, paste0(resources, "/")))

## ---------------- every source file and its checksum ----------------
# Folders that hold no sources: outputs, caches, package libraries, drafts.
skip_top <- c(out_dir, ".quarto", ".git", ".github", ".vscode", ".positai", ".Rproj.user",
              "renv", "data", "design-mockups", "site_libs", "_freeze", "_site", "node_modules")
# Files that editors, R and Quarto leave behind while they work.
junk <- paste0("(^|/)(\\.DS_Store|Thumbs\\.db|desktop\\.ini|\\.Rhistory|\\.RData|\\.Ruserdata|\\.Renviron)$",
               "|\\.quarto_ipynb$|\\.knit\\.md$|\\.rmarkdown$|~$|\\.swp$|(^|/)[^/]+_(files|cache)/")

top   <- setdiff(list.files(".", all.files = TRUE, no.. = TRUE), skip_top)
dirs  <- top[dir.exists(top)]
files <- c(top[!dir.exists(top)],
           unlist(lapply(dirs, list.files, recursive = TRUE, all.files = TRUE, full.names = TRUE, no.. = TRUE)))
files <- sort(gsub("\\\\", "/", files))
files <- files[!grepl(junk, files)]
now   <- tools::md5sum(files)
names(now) <- files

# Pages: Quarto renders every .qmd, except under names that start with _ or .
is_page <- function(f) grepl("\\.qmd$", f) & !grepl("(^|/)[_.]", f)
pages   <- files[is_page(files)]
page_text <- lapply(setNames(pages, pages), readLines, warn = FALSE)
r_pages   <- pages[vapply(page_text, function(t) any(grepl("^```\\{r", t, useBytes = TRUE)), logical(1))]
names_it  <- function(name) pages[vapply(page_text, function(t) any(grepl(name, t, fixed = TRUE, useBytes = TRUE)), logical(1))]
pages_in  <- function(dir) if (dir == ".") pages else pages[startsWith(pages, paste0(dir, "/"))]
pages_at  <- function(dir) pages[dirname(pages) == dir]

# Is a file named in the site settings (so it can matter to every page) or in the R code?
# Comments and the `resources:` list do not count: they name files without using them.
settings <- files[grepl("^_quarto[^/]*\\.yml$|(^|/)_metadata\\.yml$", files)]
settings_text <- unlist(lapply(settings, function(f) {
  t    <- sub("(^|\\s)#.*$", "", readLines(f, warn = FALSE), useBytes = TRUE)
  item <- sub("/$", "", trimws(gsub("[\"']", "", sub("^\\s*-\\s+", "", t))))
  t[!(grepl("^\\s*-\\s", t) & item %in% resources)]
}))
r_code_text <- sub("#.*$", "", unlist(lapply(files[grepl("^R/.*\\.[Rr]$", files)], readLines, warn = FALSE)), useBytes = TRUE)
in_text <- function(name, text) any(grepl(name, text, fixed = TRUE, useBytes = TRUE))

# The data the pages show, without the fetch times (they change at every fetch).
data_sig <- function() {
  if (!file.exists(data_cache)) return(NA_character_)
  d <- tryCatch(readRDS(data_cache), error = function(e) NULL)
  if (is.null(d)) return(NA_character_)
  tmp <- tempfile(); on.exit(unlink(tmp))
  saveRDS(d[c("tabs", "pubs", "scholar")], tmp, compress = FALSE)
  unname(tools::md5sum(tmp))
}

quarto_version <- Sys.getenv("QUARTO_VERSION", "")
state <- list(version = 1L, quarto = quarto_version, files = now, data = data_sig(), out = NULL)

## ---------------- compare with the last render ----------------
old <- if (file.exists(state_file)) tryCatch(readRDS(state_file), error = function(e) NULL)
full_why <- character(0)                 # reasons for a full render
render   <- list()                       # page -> why it is rendered
folders  <- list()                       # folder -> why all of it is rendered
copy     <- character(0)                 # shared files to copy into docs/
quiet    <- character(0)                 # changed files that need nothing

add_pages <- function(p, why) for (x in p) if (is.null(render[[x]])) render[[x]] <<- why

# Would Quarto publish this shared file? Yes if it is a listed resource, if it is
# already in docs/, or if a rendered page links to it (for example a new package
# logo whose name comes from the Sheet). The link is matched on the whole path,
# so img/software/a.png is not confused with img/software/thumbs/a.png.
html_text <- NULL
published <- function(f) {
  if (in_resources(f) || file.exists(file.path(out_dir, f))) return(TRUE)
  if (is.null(html_text)) {
    html <- list.files(out_dir, pattern = "\\.html$", recursive = TRUE, full.names = TRUE)
    html_text <<- unlist(lapply(html, readLines, warn = FALSE))
  }
  in_text(f, html_text)
}

needed <- file.path(out_dir, c("index.html", "search.json", "sitemap.xml", "site_libs"))
if (forced) {
  full_why <- "you asked for a full render"
} else if (is.null(old) || !identical(old$version, 1L) || is.null(old$out)) {
  full_why <- "there is no record of an earlier render"
} else if (!all(file.exists(needed))) {
  full_why <- paste0(out_dir, "/ is missing or incomplete")
} else if (nzchar(quarto_version) && !identical(old$quarto, quarto_version)) {
  full_why <- sprintf("Quarto changed (%s, was %s)", quarto_version, old$quarto)
} else {
  was     <- old$files
  added   <- setdiff(names(now), names(was))
  removed <- setdiff(names(was), names(now))
  both    <- intersect(names(now), names(was))
  edited  <- both[is.na(now[both]) | is.na(was[both]) | now[both] != was[both]]
  changed <- data.frame(file = c(edited, added, removed),
                        how  = rep(c("edited", "new", "deleted"), c(length(edited), length(added), length(removed))),
                        stringsAsFactors = FALSE)

  no_effect <- paste0("^(README|STYLE-GUIDE)\\.md$|^(deploy|render)\\.sh$|^\\.git[a-z]+$|\\.Rproj$",
                      "|^\\.nojekyll$|^_publish\\.yml$|^_page-template\\.qmd$",
                      "|(^|/)_template[^/]*/|^cv/(?!cv\\.(html|pdf)$|fonts/)|^R/(render-plan|update-data)\\.R$")
  site_wide <- "^_quarto[^/]*\\.yml$|^_theme/|^_extensions/|^_brand\\.yml$|^_variables\\.yml$|^logo-icon\\.svg$"
  # files a page only links to: a new copy in docs/ is enough, the page itself does not change
  link_only <- "\\.(html?|pdf|js|mjs|css|map|woff2?|ttf|otf|ico|zip|docx?|pptx?|xlsx?|mp4|webm|mp3)$"

  for (k in seq_len(nrow(changed))) {
    f <- changed$file[k]; how <- changed$how[k]; label <- sprintf("%s (%s)", f, how)
    if (grepl(no_effect, f, perl = TRUE)) { quiet <- c(quiet, f); next }
    if (grepl(site_wide, f)) { full_why <- c(full_why, paste0("site-wide file: ", label)); next }
    if (how == "deleted" && (is_page(f) || file.exists(file.path(out_dir, f)))) {
      full_why <- c(full_why, paste0("published file deleted: ", f)); next
    }
    if (grepl("^R/|^renv\\.lock$|^\\.Rprofile$", f)) { add_pages(r_pages, paste0("R code: ", label)); next }
    if (basename(f) == "_metadata.yml") {
      if (dirname(f) == ".") full_why <- c(full_why, paste0("site-wide file: ", label))
      else folders[[dirname(f)]] <- paste0("folder settings: ", label)
      next
    }
    if (grepl("\\.ejs$", f)) { add_pages(pages_at(dirname(f)), paste0("list template: ", label)); next }
    if (is_page(f)) { if (how != "deleted") add_pages(f, how); next }
    if (grepl("(^|/)_", f)) {              # a private file: only matters if a page names it
      users <- names_it(basename(f))
      if (length(users)) add_pages(users, paste0("uses ", label)) else quiet <- c(quiet, f)
      next
    }
    # the nearest folder, going up, that holds a page
    d <- dirname(f)
    while (d != "." && !length(pages_at(d))) d <- dirname(d)
    if (d != ".") {
      if (length(pages_in(d)) > length(pages_at(d))) folders[[d]] <- paste0("shared file of the folder: ", label)
      else add_pages(pages_at(d), paste0("its file: ", label))
      next
    }
    # no page folder above it: a shared file (img/, js/, cv/cv.html) or a loose file
    if (in_text(basename(f), settings_text)) { full_why <- c(full_why, paste0("named in the site settings: ", label)); next }
    users <- if (grepl(link_only, f, ignore.case = TRUE)) character(0) else names_it(basename(f))
    if (in_text(basename(f), r_code_text)) users <- union(users, r_pages)
    # A folder the R code lists by itself. No page names these files: the certificate and
    # the photo of an award (img/awards/) are found by their file name, so a new file
    # there changes the pages that the R code builds.
    if (in_text(paste0('"', dirname(f), '"'), r_code_text)) users <- union(users, r_pages)
    if (length(users)) add_pages(users, paste0("uses ", label))
    if (how != "deleted" && published(f)) copy <- c(copy, f) else if (!length(users)) quiet <- c(quiet, f)
  }

  if (!identical(old$data, state$data))
    add_pages(r_pages, "the data changed (Sheet, Zotero or Scholar)")

  # docs/ against the record: what a preview or a hand edit changed since the last deploy
  out_now <- out_sums(); out_was <- old$out
  both    <- intersect(names(out_now), names(out_was))
  touched <- c(both[out_now[both] != out_was[both]], setdiff(names(out_now), names(out_was)), setdiff(names(out_was), names(out_now)))
  redo <- function(page) add_pages(page, paste0("its page in ", out_dir, "/ is not the one from the last deploy (preview?)"))
  for (g in touched) {
    page  <- sub("\\.html$", ".qmd", g)
    owner <- sub("_files/.*$", ".qmd", g)          # figures a page made: about/index_files/...
    if (grepl("\\.html$", g) && page %in% pages) redo(page)
    else if (grepl("_files/", g) && owner %in% pages) redo(owner)
    else if (g %in% c("search.json", "sitemap.xml", "listings.json")) next   # Quarto rewrites these from the pages it renders
    else if (g %in% names(now) && g %in% names(out_now) && identical(unname(now[g]), unname(out_now[g]))) next   # a copy that equals its source
    else if (g %in% copy) next                                              # about to be copied again
    else if (startsWith(g, "site_libs/") && !g %in% names(out_was)) next    # a library file a preview added: harmless
    else full_why <- c(full_why, paste0(out_dir, "/", g, " was changed outside the deploy"))
  }
}

## ---------------- the plan ----------------
for (d in names(folders)) for (p in pages_in(d)) if (is.null(render[[p]])) render[[p]] <- folders[[d]]
if (!length(full_why) && length(render) > length(pages) / 2)
  full_why <- sprintf("%d of %d pages changed, so one full render is quicker", length(render), length(pages))

if (length(full_why)) {
  mode <- "full"; targets <- character(0)
} else if (!length(render)) {
  mode <- "none"; targets <- character(0)
} else {
  mode <- "partial"
  loose   <- setdiff(names(render), unlist(lapply(names(folders), pages_in)))
  targets <- c(names(folders), loose)
  # A list page (the blog list) and its posts. Quarto 1.4 and later render the
  # list by themselves when one of its posts is rendered, so the list is left
  # out of the targets then. Older versions get it as a target of its own.
  lists  <- pages[vapply(page_text, function(t) any(grepl("^listing:", t, useBytes = TRUE)), logical(1))]
  modern <- !nzchar(quarto_version) || numeric_version(sub("[^0-9.].*$", "", quarto_version)) >= "1.4"
  for (l in lists) {
    posts <- setdiff(loose[startsWith(loose, paste0(dirname(l), "/"))], lists)
    if (!length(posts)) next
    targets <- if (modern) setdiff(targets, l) else union(targets, l)
  }
}

cat("\n")
if (mode == "full") {
  say("Plan: render every page (", length(pages), "). Why:")
  for (w in utils::head(unique(full_why), 6)) say("  - ", w)
  if (length(unique(full_why)) > 6) say("  - and ", length(unique(full_why)) - 6, " more")
} else if (mode == "none") {
  say("Plan: nothing to render. No page depends on a changed file.")
} else {
  say("Plan: render ", length(render), " of ", length(pages), " pages.")
  for (p in names(render)) say("  - ", p, "   <- ", render[[p]])
  for (l in setdiff(lists, names(render))) if (any(startsWith(setdiff(names(render), lists), paste0(dirname(l), "/"))))
    say("  - ", l, "   <- the list that shows these posts")
}
if (mode != "full" && length(copy)) say("Copy into ", out_dir, "/: ", paste(copy, collapse = ", "))
if (length(quiet)) say("Changed, nothing to render: ", paste(utils::head(quiet, 8), collapse = ", "),
                       if (length(quiet) > 8) sprintf(" and %d more", length(quiet) - 8))
cat("\n")

if (dry_run) { say("(plan only: nothing was written)"); quit(save = "no") }

## ---------------- write the plan, copy shared files ----------------
dir.create("data", showWarnings = FALSE)
if (mode != "full") for (f in copy) {
  to <- file.path(out_dir, f)
  dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
  if (!file.copy(f, to, overwrite = TRUE)) stop("could not copy ", f, " into ", out_dir, "/", call. = FALSE)
}
con <- file(plan_file, "wb")             # "wb": plain line ends on Windows too, for bash
writeLines(c(mode, targets), con, sep = "\n")
close(con)
saveRDS(state, next_file)
