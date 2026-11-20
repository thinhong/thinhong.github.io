# CV

This folder builds the CV into a styled HTML page (`cv.html`) and a matching PDF
(`cv.pdf`). The data comes from `R/site-data.R`, the same data layer the website
uses (Google Sheet + Zotero), so the CV and the site always show the same facts.

## Files

- `build-cv.R` - the generator. Reads the shared data, fills the template, writes
  `cv.html`, then prints `cv.pdf` with Chrome through the pagedown package.
- `cv-template.html` - the design (Scholarly style) and the header text (name,
  tagline, summary, contact line). Edit this to change the look or wording.
- `cv.html` / `cv.pdf` - generated output. Not tracked in git, never edit by
  hand. `cv.html` has a screen-only "Download PDF / Print" bar.
- `.cv-built` - a short signature of the last build (generated, not in git).

## Update the content

- Sections (education, experience, grants, awards, team, policy, engagement,
  teaching, talks, service, software): edit the Google Sheet.
- Publications: edit the Zotero collection (journal articles only, and the fetch
  paginates, so more than 100 items is fine).
- Name, tagline, summary, contact line: edit the header block in
  `cv-template.html`.

Free-text cells of the Sheet can carry `**bold**`, `*italic*` and
`[label](https://...)` links.

The `awards` tab also fills the Honours part of the About page. Write
`description` as "Name of the award, who gave it": the text before the first
comma is set in bold on the CV and is the title of the row on About. Optional
columns:

- `note`: one more sentence.
- `id`: a short name for the files of the award, such as `2026-commendation`.
  A scan saved as `img/awards/<id>-certificate.jpg` gets a "Certificate" link on
  the CV and on About, with no address typed in the Sheet. A photo saved as
  `img/awards/<id>-photo.jpg` is linked on About only. For several certificates
  of one award, add a label to each name: `<id>-certificate-2013-14.jpg`.
- `url` and `link_text`: a full link to a page about the award on another site,
  and the wording of that link.
- `home`: an `x` shows the award on the Home page as well. The CV does not use
  it.

## How it runs

There is no `pre-render` hook in `_quarto.yml`, so rendering or previewing a page
never builds the CV. It is built in two ways, both from the project root:

- `Rscript cv/build-cv.R` - fetch fresh data and rebuild now.
- `bash deploy.sh` - refreshes the data first, then rebuilds the CV only if the
  data, the template or the build script changed since the last build. If nothing
  changed, the Chrome step is skipped. (`bash deploy.sh quick` skips both steps.)

If a fetch fails, the script uses the last saved data with a loud warning. It
refuses to finish if the PDF was not regenerated.

Switches (environment variables):

| Variable | Effect |
|---|---|
| `CV_SKIP=1` | Do nothing |
| `CV_FORCE=1` | Fetch and rebuild, whatever the state |
| `CV_IF_CHANGED=1` | Skip the build if nothing changed (set by `deploy.sh`) |
| `CV_SKIP_PDF=1` | Build `cv.html` only |

## One-time setup

- The pagedown package: `renv::install("pagedown"); renv::snapshot()`.
- Chrome must be installed. Edge also works if the `PAGEDOWN_CHROME` environment
  variable points to `msedge.exe`.
- Zotero keys in `.Renviron`: `ZOTERO_API_KEY`, `ZOTERO_USER_ID`,
  `ZOTERO_COLLECTION_KEY`.
- Google sign-in is only used if the Sheet is not link-viewable (the script
  tries an anonymous read first).

Quarto copies `cv/cv.html` and `cv/cv.pdf` (listed under `resources` in
`_quarto.yml`) into the site. The CV links on the Home, About and Research pages
point to `cv/cv.html`, live at https://drthinhong.com/cv/cv.html.
