# CV

This folder builds the CV from live data (Google Sheet + Zotero) into a styled
HTML page and a matching PDF. The build is wired into Quarto as a pre-render
script, so publishing the site always ships a fresh CV. There is no manual
step to remember.

## Files

- `build-cv.R` - the generator. Fetches the data (with a 24h cache in
  `.cv-cache.rds`), fills the template, writes `cv.html`, then prints `cv.pdf`
  with Chrome via the pagedown package.
- `cv-template.html` - the design (Scholarly style) and the header text (name,
  tagline, summary, contact line). Edit this to change the look or wording.
  The footer date is stamped automatically at build time.
- `cv.html` / `cv.pdf` - generated output. Not tracked in git, never edit by
  hand. cv.html has a screen-only "Download PDF / Print" bar.

## One-time setup

pagedown is not in renv yet. In R, from the project root:

```r
renv::install("pagedown")
renv::snapshot()
```

Chrome must be installed; pagedown uses it to print the PDF. (Edge also
works if you set the PAGEDOWN_CHROME environment variable to msedge.exe.)
Zotero keys stay in `.Renviron` as before. Google sign-in is only used if the
Sheet is not link-viewable (the script tries an anonymous read first).

## Update the content

- Sections (education, experience, grants, awards, research team, policy,
  teaching, software): edit the Google Sheet.
- Publications: edit the Zotero collection (journal articles only, and the
  fetch paginates, so more than 100 items is fine).
- Name, tagline, summary, contact line: edit the header block in
  `cv-template.html`.

## How it runs

- `quarto render`, `quarto publish gh-pages`, and the first render of
  `quarto preview` rebuild the CV with freshly fetched data.
- Incremental preview renders skip the CV when both output files exist.
- Manual rebuild from the project root: `Rscript cv/build-cv.R`
- If a fetch fails, the script falls back to the last cached data with a loud
  warning; it refuses to silently ship a stale PDF.

Escape hatches: `CV_SKIP=1` (skip entirely), `CV_FORCE=1` (rebuild + refetch
no matter what), `CV_SKIP_PDF=1` (HTML only).

## Publish

```
quarto publish gh-pages
```

That is all: the pre-render step rebuilds the CV, then Quarto copies
`cv/cv.html` and `cv/cv.pdf` (listed under `resources` in `_quarto.yml`) into
the site. The CV buttons on the home, About, and Research pages point to
`cv/cv.html`, live at https://drthinhong.com/cv/cv.html. The old
`/about/cv.html` URL redirects there.
