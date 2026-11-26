# drthinhong.com

Source of my personal website, built with [Quarto](https://quarto.org) and R.

## Everyday commands

Run these from the project root.

| I want to | Command |
|---|---|
| Preview the site while I edit | `quarto preview` |
| Publish the site | `bash deploy.sh` |
| Publish a text-only change fast (a blog post) | `bash deploy.sh quick` |
| See what the next deploy would render | `bash deploy.sh plan` |
| Publish and render every page again | `bash deploy.sh full` |
| Pull fresh numbers now (Sheet, Zotero, Scholar) | `Rscript R/update-data.R` |
| Rebuild the CV now | `Rscript cv/build-cv.R` |

`bash deploy.sh` does everything in order: fresh data, the CV if its data
changed, a render of the pages that changed, then the push to `gh-pages`.
Stop `quarto preview` before you deploy.

## What a deploy renders

A deploy does not rebuild the whole site. `R/render-plan.R` compares every
source file with its record of the last render and renders only the pages that
depend on a changed file. Each run prints the plan and the reason for each page.

| What changed | What is rendered |
|---|---|
| A blog post (edited, new, or `draft` switched) | That post and the blog list |
| Any other page, or a file in its folder (picture, `.bib`) | That page |
| The data (Sheet, Zotero, Scholar) or code in `R/` | The pages built from data: Home, About, Research, Software, Teaching and the course pages |
| `blog/_metadata.yml` or `slides/_metadata.yml` | Every page in that folder |
| `blog/listing-mosaic.ejs` | The blog list |
| A shared file (`img/`, `js/`, the CV files) | Nothing. The file is copied into `docs/`. A picture also renders the pages that name it. |
| A certificate or a photo in `img/awards/` | The pages with figures. They build the award links from the files in that folder. |
| `_quarto.yml`, `_theme/`, `_extensions/`, the logo, a deleted page | Every page |
| Notes (`README.md`, `STYLE-GUIDE.md`, the templates) | Nothing |

Every page is also rendered when there is no record yet (the first run, or a
new computer), when Quarto was updated, or when more than half of the pages
changed. The search index and the sitemap are kept up to date by Quarto.

`quarto preview` writes into `docs/` and shows draft posts. The record holds a
checksum of every file in `docs/`, so a page that a preview changed is rendered
again at the next deploy, and a draft does not reach the site.

`bash deploy.sh quick` skips the data fetch and the CV check. Use it for a
change in text only. The figures on the site then stay as they were at the
last normal deploy. If the site ever looks out of date, run
`bash deploy.sh full` once.

## Where the numbers come from

Nothing is typed by hand on the pages. One file, `R/site-data.R`, fetches three
sources and saves them in one cache, `data/site-data.rds`:

| Source | Holds | Edit it in |
|---|---|---|
| Google Sheet (`cv_url`) | Education, jobs, grants, awards, team, policy, teaching, talks, service, software | The Sheet |
| Zotero collection | Publications | Zotero |
| Google Scholar | Citations, h-index | (read only) |

The website pages and the CV both read that cache, so they always agree.

| Shown on | Figure | From |
|---|---|---|
| Home | Citations, articles, packages | Scholar, Zotero, Sheet tab `software` |
| Home | Selected honours | Sheet tab `awards`, rows marked in the column `home` |
| Research | Citations, h-index | Scholar |
| About | Funding total and the list of grants | Sheet tab `grants` |
| About | The list of honours | Sheet tab `awards` |
| Software | The whole package table | Sheet tab `software` |
| Teaching | The course cards | Sheet tabs `teaching` and `talks`, rows with an `id` |
| Course pages | The line above the title, the facts, the "More in ..." cards | Sheet tabs `teaching` and `talks` |
| CV | Every section | Sheet and Zotero |

The cache refreshes in three ways:

1. By itself, the first time a page is rendered after a source is older than
   24 hours. One fetch then serves the whole render.
2. When `Rscript R/update-data.R` runs (`deploy.sh` runs it first).
3. When `Rscript cv/build-cv.R` runs by hand.

If a fetch fails, the last saved copy is used and a line in the terminal says
so. The cache is not in git, because it holds the whole Sheet. On a new
computer, run `Rscript R/update-data.R` once before the first render. It needs
the Zotero keys in `.Renviron` (`ZOTERO_API_KEY`, `ZOTERO_USER_ID`,
`ZOTERO_COLLECTION_KEY`).

## Folder map

```
_quarto.yml          site settings: menu, footer, fonts, what is rendered
index.qmd            Home
about/  research/  teaching/  software/  blog/  slides/     the pages (folder = URL)
cv/                  CV builder (build-cv.R, cv-template.html, fonts/); cv.html and cv.pdf are generated
R/                   R code shared by pages and the CV
  site-data.R          the one data layer (Sheet, Zotero, Scholar, cache)
  update-data.R        fetch fresh data now
  software.R           builds the Software page markup
  teaching.R           builds the Teaching cards and the facts on course pages
  render-plan.R        works out which pages a deploy must render
data/                generated, not in git: the data cache (site-data.rds) and
                     the record of the last render (render-state.rds)
_theme/              the look: theme.scss, small scripts loaded on every page, and
                     topic-hue.lua (gives each blog post its topic's colour)
js/                  viz.js, the figure kit for posts with interactive figures
img/                 images (affiliations, blog covers, package logos, award files)
STYLE-GUIDE.md       design rules and tokens. Read before changing a page.
_page-template.qmd   copy this to start a new page
deploy.sh            publish
docs/                the rendered site (generated, not in git)
renv/  renv.lock     the R package library for this project
```

Files and folders that start with `_` are never published. Only `.qmd` files are
rendered, so notes such as this file stay private to the repository.

## Common edits

- **New grant, award, talk, course or package:** add a row in the Sheet. The
  site picks it up within a day, or at once after `Rscript R/update-data.R`.
- **New course, or a new run of one:** add a row in the Sheet tab `teaching`, one
  row per run. The CV shows every row. To show it on Teaching, give it an `id`
  (small letters, numbers and hyphens) and fill `format`, `section` (`modelling`,
  `r` or `skills`) and `tags`, plus `web_name` when the site name differs from
  `name`. Rows with the same `id` share one card, with a link for each run. A
  course page is optional: copy one to `teaching/<part>/<id>/index.qmd` and set
  its `id`, and the card opens it. A new course gets a plain icon until one is
  added to `teaching_icons` in `R/teaching.R`. A talk shows under "Seminars and
  talks" when its row in the tab `talks` has an `id`.
- **New paper:** add it to the Zotero collection. To feature it on Research or
  Home, also add a row in `research/index.qmd` or `index.qmd`.
- **New package logo:** put the file named in the Sheet in `img/software/`. If it
  is larger than about 100 KB, also save a 230px PNG with the same name in
  `img/software/thumbs/`. The page then loads the small one.
- **New award:** add a row in the Sheet tab `awards`. The CV and the Honours part
  of About both read it. Write `description` as "Name of the award, who gave it":
  the text before the first comma is the title. `note` adds one more sentence.
  To show the award on Home too, put an `x` in the column `home`. With no `x`
  on any row, Home shows the three newest awards.
- **Certificate or photo of an award:** give the row a short `id` in the Sheet,
  for example `2026-commendation` (small letters, numbers and hyphens). Save the
  files in `img/awards/`, about 1400px wide, under these names:
  `2026-commendation-certificate.jpg` and `2026-commendation-photo.jpg`.
  No address is typed anywhere. About and Home show a "Certificate" and a
  "Photo" link for the files that exist, and the CV links to the certificate.
  On the site, a click opens the picture in a viewer on the page (a PDF opens
  in a new tab).
  An award can have both files, one or none. For several certificates (or
  photos) of one award, add a short label to each name, such as the year:
  `2013-annual-gpa-certificate-2013-14.jpg`. Each file then gets its own link.
  A file that fits no `id` is reported in the terminal at the next render.
- **New blog post:** copy `blog/_template/` (see "Blog drafting workflow" in
  `STYLE-GUIDE.md`).
- **New "Today I learned" note:** copy `blog/_template-til/`. The tag
  `today I learned` puts it in the TIL filter on the blog page (`/blog/#til`).
- **New page:** copy `_page-template.qmd`.
