# CV

This folder builds the CV into a web page (`cv.html`) and a matching PDF
(`cv.pdf`). The data comes from `R/site-data.R`, the same data layer the website
uses (Google Sheet + Zotero), so the CV and the site always show the same facts.

## Files

- `build-cv.R` - the generator. Reads the shared data, builds every section,
  fills the template, writes `cv.html`, then prints `cv.pdf` with Chrome through
  the pagedown package.
- `cv-template.html` - the design, and the header text typed by hand (name,
  role, contacts, research interests). Edit this to change the look or the
  wording of the header.
- `fonts/` - the two typefaces as files of this site: EB Garamond (the text) and
  Manrope (name, headings, dates). Both are under the SIL Open Font License; the
  two licence files sit beside them. The files come from the Fontsource packages
  `@fontsource/eb-garamond` and `@fontsource/manrope`, version 5.3.0.
- `cv.html` / `cv.pdf` - generated output. Not tracked in git, never edit by
  hand.
- `.cv-built` - a short signature of the last build (generated, not in git).

## The design

An academic CV inside the website's frame, kept simple so it is easy to read.
The text is in EB Garamond, with italics for journals, events and funders. The
name, the section headings and the dates are in Manrope. Colour has one job: a
short rule above each heading, in one of the website's five hues. Links are blue
and underlined. `STYLE-GUIDE.md` has the rules (see "The CV").

Every section is the same kind of list: the date on the left, the entry on the
right. Funding and teaching end each row with one figure on the right (the
amount, the number of people). There are no tables, no section numbers and no
abbreviations: "PI" in the Sheet is written as "Principal investigator".

One file serves three uses:

- **Screen.** A small bar on top: a link back to the website, the light / dark
  button, Print and Download PDF. The page follows the light / dark choice made
  on the website, and a choice made here is kept for the website too.
- **Phone.** Below 600px the rows stack: the date sits above its entry, and a
  figure (amount, people) moves to the right end of the date line.
- **Paper and the PDF.** A4, always light. Each sheet has a footer with the name,
  the month of the build and "Page 1 of 7". The PDF has bookmarks (one per
  section) and tags for screen readers.

## Update the content

- Sections (education, experience, grants, awards, team, policy, engagement,
  teaching, talks, service, software): edit the Google Sheet.
- Publications: edit the Zotero collection (journal articles only, and the fetch
  paginates, so more than 100 items is fine). A journal is shown by its full
  name; the abbreviation is used only when Zotero has no full name.
- Name, role, contacts, research interests: edit the header block in
  `cv-template.html`.
- The order or the names of the sections: the list `cv_sections` in
  `build-cv.R`. A section without rows is left out.

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
  data, the template, the fonts or the build script changed since the last
  build. If nothing changed, the Chrome step is skipped. (`bash deploy.sh quick`
  skips both steps.)

If a fetch fails, the script uses the last saved data with a loud warning. It
refuses to finish if the PDF was not regenerated.

Switches (environment variables):

| Variable | Effect |
|---|---|
| `CV_SKIP=1` | Do nothing |
| `CV_FORCE=1` | Fetch and rebuild, whatever the state |
| `CV_IF_CHANGED=1` | Skip the build if nothing changed (set by `deploy.sh`) |
| `CV_SKIP_PDF=1` | Build `cv.html` only |

## When you edit the template

- **Everything `cv.html` loads must sit inside `cv/`.** To print the PDF,
  pagedown serves this folder alone, and it stops the build on any file the page
  asks for and does not get. A link such as `../logo-icon.svg` or a font file
  that is missing ends the build with "Failed to open ...". This is why the fonts
  live in `cv/fonts/` and the icon of the page is written into the template.
- **Keep the marker `{{SECTIONS}}` exactly once, in the body.** The script puts
  the sections there. `{{UPDATED}}` and `{{UPDATED_ISO}}` become the month of the
  build.
- **Sizes are in `em` of `.cv`.** One font size sets the whole scale: 18px on a
  screen, 17px on a phone, 12pt on paper. To make the PDF tighter or looser,
  change the 12pt in the print block.
- **The footer of each sheet comes from the `@page` rule** at the end of the
  styles. It needs the bottom margin of the page, so keep that margin at 15mm or
  more.

## One-time setup

- The pagedown package: `renv::install("pagedown"); renv::snapshot()`.
- Chrome must be installed, version 131 or newer (the footer of each sheet needs
  it). Edge also works if the `PAGEDOWN_CHROME` environment variable points to
  `msedge.exe`.
- Zotero keys in `.Renviron`: `ZOTERO_API_KEY`, `ZOTERO_USER_ID`,
  `ZOTERO_COLLECTION_KEY`.
- Google sign-in is only used if the Sheet is not link-viewable (the script
  tries an anonymous read first).

Quarto copies `cv/cv.html`, `cv/cv.pdf` and `cv/fonts/` (listed under `resources`
in `_quarto.yml`) into the site. The CV links on the Home, About and Research
pages point to `cv/cv.html`, live at https://drthinhong.com/cv/cv.html.
