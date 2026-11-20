# ThinhOng site style guide

This is the single reference for keeping the site visually consistent. Read it
before building or restyling a page. It is short on purpose.

The theme is called "Galerie", colour version C. In one sentence: white space,
thin type in one typeface, hairlines, and four matte colours plus the logo blue.

## How consistency is enforced

Two ideas do all the work:

1. **Design tokens.** Every colour, spacing, size and timing value lives once in
   `_theme/theme.scss` as a CSS variable (a token). Pages never hard-code these values;
   they reference the token. Change a token once and every page updates.

2. **A shared page system (`.pg-*`).** The page layout (head, "On this page" row,
   numbered parts, rows, cards, panels, next-page link) is defined once in
   `_theme/theme.scss`. Every page uses it, so pages match by construction.

Anything that only one page needs is namespaced to that page (`.hm-*` Home,
`.sw-*` Software, `.blg-*` Blog listing, `.ab-*` the About globe) and still uses
the shared tokens. Interactive figures in posts share one kit, `.viz-*` (see
"Interactive figures" below).

## Colour

### Neutral tokens

| Token | Light | Dark | Use for |
|---|---|---|---|
| `--bg` | #ffffff | #0b0c10 | Page background |
| `--ink` | #0c0e13 | #f4f5f7 | Titles, strong text, hairline accents |
| `--text` | #3d4149 | #c2c5cc | Body text |
| `--muted` | #666b76 | #969aa5 | Labels, dates, captions |
| `--hair` | #e1e3e8 | #262830 | Hairlines and borders |
| `--brand` | #2050bf | #8fb0ff | Logo blue, focus ring |
| `--ne-bg`, `--ne-fg` | #eceef2, #3d4149 | #1d2028, #c2c5cc | Neutral label |
| `--code-bg` | #f4f5f8 | #14161c | Code blocks |
| `--land` | #dfe2e8 | #23262f | Map land |

### The five hues

Each hue has three tones. Use the right tone for the job.

| Hue | Line `--xx` | Tint `--xx-bg` | Text `--xx-fg` | Dark line / tint / text |
|---|---|---|---|---|
| Logo blue `bl` | #2050bf | #e7eefb | #1a43a3 | #8fb0ff / #172446 / #c2d2ff |
| Indigo `in` | #5b6ee1 | #e6e9fb | #3a4ab5 | #9aa7f2 / #1c2148 / #c3cbf8 |
| Teal `te` | #1c9a81 | #dbf1ea | #136b59 | #5fd0b7 / #11332b / #93e2cf |
| Mauve `ma` | #b9668f | #f7e5ee | #8f3f66 | #e3a3c2 / #3a1f2d / #f0bcd4 |
| Cyan `cy` | #2a86b5 | #e0f0f8 | #1a5f84 | #7cc4e6 / #102c3c / #a9dcf3 |

- **Line tone**: rules, dots, icons and large figures. On white its contrast is
  about 3.5:1 to 4.4:1, so it is for graphics and large text only.
- **Tint tone**: backgrounds of panels, card plates and labels.
- **Text tone**: any small coloured text (contrast 4.5:1 or more on its tint and
  on the page background).

### Hue classes

A hue class sets three working variables: `--hue` (line), `--hue-bg` (tint) and
`--hue-fg` (text). Components only read these three, so one class recolours a
whole part.

`hue-bl`, `hue-in`, `hue-te`, `hue-ma`, `hue-cy`, and `hue-ne` (neutral).

### The two colour rules

1. **Each section owns one hue.** Set it on the page with `body-classes` in the
   front matter. It colours the title rule and marks the section in the menu.

   | Section | Hue class |
   |---|---|
   | Home, About | `hue-bl` |
   | Research | `hue-in` |
   | Teaching (and course pages) | `hue-te` |
   | Software | `hue-cy` |
   | Blog | `hue-ma` |

   Blog posts are the one exception: each post takes its topic's colour, and the
   menu still marks Blog in mauve (see "Colour in the blog").

2. **Inside a page, the numbered parts cycle the four hues** in the order
   indigo, teal, mauve, cyan, starting with the page's own hue. Put the hue class
   on each `.pg-part` and on its link in the "On this page" row.

No gold, no metallic colours, no gradients on surfaces, no shadows. Surfaces are
matte and flat. Affiliation and software logos keep their own full colours and
sit on a white tile in both modes.

Charts in posts use their own data colours, chosen so groups stay apart for
colour-blind readers (see "Figure colours" below). Pages never use them.

## Type

One typeface: **Manrope** (weights 200, 300, 400, 500, 600), loaded in
`_quarto.yml`. Hierarchy comes from size and weight only. There is no second
font, no uppercase labels and no letter-spaced labels. Headings use sentence case.

| Token | Value | Use for |
|---|---|---|
| `--fs-h1` | clamp(2.6rem, 6.3vw, 5.9rem) | Page title (weight 200). Not the Home headline: that is a full sentence, so it is smaller, clamp(1.9rem, 4.3vw, 3.75rem). |
| `--fs-h2` | clamp(1.9rem, 3.8vw, 3.1rem) | Part title (weight 300) |
| `--fs-h3` | clamp(1.25rem, 2.1vw, 1.7rem) | Row and card titles (weight 400) |
| `--fs-lead` | clamp(1.02rem, 1.35vw, 1.18rem) | Lead paragraph |
| `--fs-fig` | clamp(2.2rem, 3.4vw, 3rem) | Highlighted figures |
| `--fs-small` | 14.5px | Meta text, links in rows |
| `--fs-label` | 13px | Small labels above values |

**Highlighted figures** (citations, funding, counts) have one size for the whole
site, `--fs-fig` (48px on a desktop screen), weight 300, in the line tone, with a
small label. Use `.pg-stats > .pg-stat` (a 2px coloured rule above each figure) or
`.fig` (figure and label on one line). Do not make a figure larger than this.

## Layout and motion tokens

| Token | Value | Use for |
|---|---|---|
| `--page-max` | 1320px | Max width of the page shell |
| `--page-pad-x` | clamp(20px, 4vw, 56px) | Left and right page padding |
| `--part-gap` | clamp(48px, 7vw, 104px) | Space above each numbered part |
| `--stack-gap` | clamp(20px, 3vw, 36px) | Space between blocks inside a part |
| `--scroll-offset` | 96px | Anchor offset so headings clear the fixed menu |
| `--ease` | cubic-bezier(.22, .61, .36, 1) | The one easing curve |

### How blocks change with the screen width

A block never wraps piece by piece. Free wrapping (`flex-wrap`, or a grid with
`auto-fit`) lets one cell drop to the next line by itself at in-between widths,
for example the year and the arrow of a row sitting alone under the number.
So every block has a small set of fixed layouts and switches between them at a
set width. Inside one layout, each piece has its own column.

| Block | Layouts, from narrow to wide | Switches at |
|---|---|---|
| Works rows (`.pg-rows`) | title, then label, then source with the year at the right; title with an arrow, then label, source and year on one line in three columns; one line of six columns | list width 640px and 1000px |
| Dated rows (`.pg-rows--dated`, `--amt`) | stacked; years in a column with the detail under the title; one line | list width 520px and 830px |
| Software rows (`.sw-row`) | stacked; logo beside the text with the links under it; one line | list width 520px and 900px |
| "On this page" (`.pg-toc`) | 2 equal columns; 4 equal columns; one line | 700px and 900px |
| Part title line, next-page link, Home section links | stacked; one line | 640px (title line), 860px (links) |
| Panel | label above the text; label in a left column | 900px |
| Methods | 1, 2 or 4 columns | 600px and 1180px |
| Cards | picture left and text right, one card per line; 3 columns | 900px |
| Facts | list with the label left and the value right; one line of equal columns | 1000px |
| Home hero (`.hm-hero`) | a small photo with the name beside it, then the headline and the details; headline and details left, portrait right in a column of 230px to 330px | 700px |
| Home hero details (`.hm-info`) | stacked; 3 columns | hero text column 640px |
| Home figures | stacked; 3 columns | 600px |
| Home affiliations | label above the logos; label beside the logos. The logos are always one line and shrink together on small phones | 640px |
| Footer | name and links, then the email under them; one line | 720px |
| Blog filter, globe regions (`.scroll-x`) | always one line; it scrolls sideways when it does not fit | no switch |
| About globe (`.ab`) | globe, then the text under it; globe left (up to 540px wide, it stays in view while the text scrolls) and the text right | 900px |
| Figure toolbars (`.viz-toolbar`) | each group of controls on its own line; one line | card width 760px |

Rows, the Home hero and figure cards follow the width of their own box
(container queries on `.pg-rows`, `.hm-hero__main` and `.viz-card`), because the
same block can sit in a wide page or in a narrow column. The other blocks
follow the window width. Do not set `container-type` on `.pg` or `.pg-part`: it
would trap every `position: fixed` piece inside the part. (The full-screen globe
on About is safe either way: it opens in a `<dialog>`, above the page.)

Two more breakpoints: 992px (the menu collapses and the article contents list
hides) and 720px (the blog grid becomes one column).

When you add or change a block, check it at every width from 320px to 1920px,
not only at phone, tablet and laptop sizes. Drag the window edge slowly and
watch for a piece that drops to a new line by itself.

## The shared page system (`.pg-*`)

```
.pg                         page shell (max width, side padding)
  .pg-head                  page head
    .pg-head__main          left: eyebrow, title, optional figures
      .pg-eyebrow           small label above the title
      .pg-title             the page title (h1)
      .pg-stats > .pg-stat  optional figures (.pg-stat-label + .pg-stat-num)
    .pg-head__aside         right: lead text and links
      .pg-lead              one or two short sentences
      .pg-links > a.tl      underlined links with a small arrow
  .pg-toc                   "On this page" row (a.hue-xx per part)
  .pg-part.hue-xx[id]       a numbered part
    .pg-part__head          coloured rule, then the title line
      .pg-part__line
        .pg-part__name      .pg-num ("01") + h2.pg-sectitle
        .pg-secsub          optional short note on the right
    ...blocks...
  .pg-next.hue-xx           link to the next page
```

Part variants: `.pg-part--split` puts the heading on the left and the content
(`.pg-part__body`) on the right (used on About). `.pg-part--plain` has no number
(used on course pages). `.pg-duo` places two parts side by side.

Blocks that go inside a part:

| Block | Classes | Use for |
|---|---|---|
| Rows of works | `.pg-rows > .pg-row` with `.r-n`, `.r-title`, `.r-sub > (.r-pill, .r-meta, .r-year)`, `.r-arrow` | Lists of works. Use `<a class="pg-row">` when the row is a link. The label, the source and the year must sit inside `.r-sub`. Add `.pg-rows--ink` for a dark top line. Set `--col-pill`, `--col-meta` and `--col-year` on `.pg-rows` to change the column widths (defaults 186px, 230px, 92px). |
| Rows with dates | `.pg-rows.pg-rows--dated > .pg-row` with `.r-years`, `.r-title`, `.r-detail`; `.pg-rows--amt` adds `.r-amt` | Roles, degrees, grants, awards. One list holds one kind of row. A row can end with links to its own files or pages: `.pg-links > a.tl` at the end of `.r-detail` (the awards on About and Home, built by `award_rows_html()` in `R/site-data.R`). |
| Panel | `.panel` (add `.panel--tint` for a tinted ground) with `.panel__label`, `.panel__body`, `.panel__lead`, `.panel__text` | One large statement with support text |
| Small items | `.methods > .method` | Short titled items with a coloured rule |
| Cards | `.cards > a.card-g` with `.plate`, `.txt > .meta, .nm, .tags` | Courses and similar items. Three columns on a wide screen; below 900px each card is a line with the plate on the left. `.card-g--wide` keeps the plate beside the text at every width. A course with more than one link (one per year it ran) is a `div.card-g.card-g--links`: the title is the main link (`a.nm.card-g__main`) and opens from anywhere on the card, and `.pg-links > a.tl` under the tags holds the year links. |
| Pictures | `.frames > figure.frame` with `.mat > img` and `figcaption` | Photos on a tinted mat |
| Picture viewer | `a.tl` with `data-lb`, and `data-lb-title`, `data-lb-meta` for the two caption lines | A link to a picture (jpg, png or webp) that opens in a framed card over the dimmed page, in place of a new tab (the award files on About and Home). Links with the same `data-lb` value are one set: the reader moves through it with the arrow buttons, the arrow keys or a swipe. The page needs no other markup: the script in `_theme/site.html` builds the card (`.lb`) on the first click. Keep `href` and `target="_blank"` on the link, so it still works with no script and with Ctrl + click. A PDF gets no `data-lb` and opens in a new tab. |
| Facts | `.facts > .fact` | Label and value pairs: one line of equal columns on a wide screen (any number of facts), a list below 1000px |
| Figure | `.fig` with `.fig-num`, `.fig-label` | One highlighted figure |
| See more | `.more > .pg-rows + .pg-rows.more__rows[hidden] + button.more__btn` | A long list that shows its first rows and opens the rest in place (the grants on About). The button needs `aria-controls`, `data-open` and `data-close`; the script is in `_theme/site.html`. |
| Labels | `.pill.hue-xx`, wrapped in `.pills` when there are several | Short coloured labels |
| One-line scroller | `.scroll-x` on a row of buttons | A short row of buttons that must stay on one line (blog filter, map regions). It scrolls sideways when it does not fit, and an edge fades only while more is hidden on that side (script in `_theme/site.html`). |
| Links | `a.tl` (underlined link with arrow), `a.il` (link inside a sentence), `a.back` (back link at the top) | |

Icons on card plates are small inline SVGs (`viewBox="0 0 120 80"`, class `ic`).
Their shapes take tone classes instead of colours: `l`, `l1`, `l2`, `l3` (line
tone at four strengths), `f` (text tone fill), `s` and `fs` (strokes), `b` and
`bs` (page background). This keeps icons correct in both modes.

## Numbers and lists come from data

No figure is typed by hand on a page. Citations, the h-index, the article count,
the package count, the funding total, the list of grants and the list of
honours all come from
`R/site-data.R`, the same data layer the CV is built from (Google Sheet, Zotero,
Google Scholar). `README.md` explains how it refreshes.

The same holds for courses. The cards on Teaching, and on each course page the
line above the title, the facts and the "More in ..." cards, are built by
`R/teaching.R` from the `teaching` and `talks` tabs, the rows the CV lists. Only
the text of a course page (title, lead, links, photos) is typed in its `.qmd`.

In a page, read the data in an R chunk and print the markup:

```r
source("../R/site-data.R")       # "R/site-data.R" on the Home page
num <- site_numbers()            # citations, hindex, articles, software, grants, funding_total, funding_since
cat(stat_html("Citations", num$citations))
```

A figure with no data yet is left out (`stat_html()` returns an empty string).
Text from the Sheet goes through `md_inline()`, so `**bold**`, `*italic*` and
`[label](url)` in a cell work the same way as on the CV.

Files of an award follow one naming rule, so no address is typed in the Sheet.
A row of the `awards` tab has a short `id` (for example `2026-commendation`),
and its files are `img/awards/<id>-certificate.jpg` and
`img/awards/<id>-photo.jpg`, saved about 1400px wide. The links show for the
files that exist (`award_files()` in `R/site-data.R`). Several files of one
kind carry a short label, `<id>-certificate-2013-14.jpg`, and each gets its
own link. A picture opens in the picture viewer on the page, and all the
pictures of one award form one set there. A PDF opens in a new tab.
About and Home build these rows with one function, `award_rows_html()`.

## Building a new page

1. Copy `_page-template.qmd` to `your-section/index.qmd`.
2. Set `pagetitle`, `description` and `body-classes` (the page hue).
3. Fill in the eyebrow, title, lead and optional figures in the head.
4. Add one `.pg-part` per section, each with a unique `id` and a hue class in the
   cycle order. Add a matching link in the `.pg-toc` row.
5. Put anything that is unique to this page in the page's own `<style>` block
   with a short page prefix (for example `.xx-card`), and style it with tokens.
6. Add the page to the navbar in `_quarto.yml` if it should be linked. If it is a
   new section with its own hue, also add its folder to the "mark their own
   section in the menu" rule in `_theme/theme.scss`.

## Article pages (blog posts and long course notes)

Posts are plain Markdown and use Quarto's own title block, body and contents
list. `_theme/theme.scss` section 8 styles them: a 720px text column on the left and
the "On this page" list on the right (the list hides below 992px).

- The first callout in a post is the "in short" box. It takes the page tint.
  Callouts have no icon (`icon=false`). A folded callout (`collapse="true"`)
  shows only its title line.
- A blog post takes the colour of its topic (title rule, topic pill, "in short"
  box, contents dot, quotes). Nothing to set by hand: see "Colour in the blog".
- Wrap a reference list in `::: {.refs}` so long DOI links wrap on phones.

### Blog covers

Covers use style A, "line and wash". One SVG file per post in `img/blog-art/`,
used in two places: `image:` (the card on the blog page) and
`title-block-banner: "url(/img/blog-art/<file>.svg)"` (beside the post title).

Recipe for a new cover, so it matches the others:

- `viewBox="0 0 320 200"` with `preserveAspectRatio="xMidYMid slice"`.
- A flat ground in the tint of the post's topic group, and the focus dot in that
  group's line tone (see "Colour in the blog"): indigo #e6e9fb and #5b6ee1, teal
  #dbf1ea and #1c9a81, mauve #f7e5ee and #b9668f. Hollow circles are filled with
  the ground colour.
- One white disc at about 70% opacity, and one soft colour wash drawn with
  `mix-blend-mode: multiply`, in a different hue from the ground (#c7cef5,
  #b9dcee, #eac3d6 or #b3e0d5). Change the wash and the drawing from post to
  post, so posts in the same group still look different.
- Ink hairlines (#1f232b, 1 to 1.2px, `vector-effect: non-scaling-stroke`) that
  draw one simple idea from the post. One saturated dot as the focus point.
- A light paper grain (an `feTurbulence` filter inside the SVG file). Never put
  `filter: url(#id)` in page CSS, because Quarto's resource scanner fails on it.
- Covers keep the same colours in dark mode.

A post without `image:` gets the cover of its first topic on the blog page,
`cover-topic-<topic>.svg` (modelling, code, academia, productivity, reflections,
leadership). These follow the same group colours.

### Link preview pictures

When a link to the site is shared on LinkedIn, X or a chat app, the card shows
one picture. It must be a PNG or JPEG of about 1200 x 630. An SVG does not work.

- Pages use the site default, `img/og-default.png` (set once in `_quarto.yml`).
  A page with pictures in its body needs `image: /img/og-default.png` in its
  front matter, or Quarto picks the first picture on the page instead.
- A blog post with its own cover also needs a JPEG copy of that cover with the
  same name, for example `cover-supervise.jpg`, named in the two blocks
  `open-graph: image:` and `twitter-card: image:` (see `blog/_template/`). The
  page itself keeps the SVG. A post without these blocks uses the site default.
- The JPEG copy is drawn on a slightly wider canvas (ratio 1200:630) with the
  ground colour extended to both sides, so no part of the art is cut off.

### The blog listing template

`blog/listing-mosaic.ejs` builds the cards. Quarto passes its output through
pandoc, so three things break easily:

- Keep each card on **one line**. An indented line after a blank line becomes a
  code block.
- Do not write `--` inside the template (pandoc turns it into a dash). Class
  names there use one hyphen (`blg-lead`).
- Do not put inline `<svg>` in the template (pandoc escapes it). The arrow after
  "Read the post" is drawn in CSS.

The line above each card title has two groups: `.blg-tags` (the topic and kind
labels) and `.blg-when` (the date and the reading time). The line breaks only
between the two groups.

## Blog drafting workflow

Blog posts live in `blog/`, one folder per post (`blog/my-post/index.qmd`), and
the listing picks them up automatically. To write a post before it is public,
use Quarto's `draft` flag.

1. **Write with `draft: true`** in the post front matter. In `quarto preview` the
   post renders in full with a "Draft" banner and appears in your local listing.
   A published build (`quarto render`) drops it: no page, and nothing in the
   listing, navbar, search, sitemap or feed.
2. **Publish** by setting `draft: false` and running `bash deploy.sh` (or
   `bash deploy.sh quick`, which skips the data fetch). That is the only switch.
   The deploy renders the post and the blog list, not the whole site.
3. **Start from `blog/_template/`.** Copy that folder to `blog/your-slug/`; it
   already has `draft: true` and the standard front matter. For a Today I
   learned note, copy `blog/_template-til/` instead.
4. **Use `blog/_drafts/` as a holding pen** for raw ideas you do not want rendered
   even in preview. Quarto ignores any folder that starts with `_`.

One caveat: `quarto preview` always shows drafts, and it writes them into
`docs/`. The deploy renders such pages again, so a draft does not reach the
site, but stop the preview before you deploy. To see the site exactly as
visitors will, run `quarto render` and open the `docs/` output.

## Tags and "Today I learned" notes

A post's `categories` are its tags. Write them in lower case; the site shows
them in sentence case. The first tag is the topic (modelling, code, academia,
productivity, reflections or leadership): it gives the post its colour, the
default cover and a place in the filter.

A "Today I learned" note is a blog post whose second tag is `today I learned`:

```yaml
categories: [modelling, today I learned]
```

- Its card on the blog page gets a second pill, "Today I learned", drawn as an
  outline (it is a kind of post, not a topic, so it has no colour).
- The filter on the blog page has a "Today I learned" option. `/blog/#til`
  opens the blog page with only these notes, and `/blog/#modelling` with one topic.
- Quarto links each tag at the top of a post to `/blog/#category=<tag>`, and
  the blog page reads that too, so a reader can click a tag to see the others.
- A note can skip the cover: its card then shows the topic cover.

The topics, their labels and their colours are one list, `topics` in
`blog/_metadata.yml`. The cards (`blog/listing-mosaic.ejs`), the filter
(`blog/index.qmd`) and every post read it.

## Colour in the blog

In the blog, colour means topic. A reader can tell what a post is about before
reading its title, and the same colour means the same thing on the blog page,
in the filter and inside the post.

The site's five hues cannot each mark a topic: blue and indigo are almost the
same colour (and nearly identical in dark mode), cyan sits close to both, and
teal and mauve look alike to red-green colour-blind readers. So the topics form
three groups, each in a hue that stays clearly apart from the others, and each
tied to the part of the site it belongs with:

| Group | Hue | Topics | Like |
|---|---|---|---|
| Methods: how the science is done | `in` indigo | modelling, code | Research |
| Practice: how the work is done, alone and with others | `te` teal | academia, productivity, leadership | Teaching |
| Reflections: personal writing | `ma` mauve | reflections | the Blog itself |

The topic name is always written next to its colour, so no one has to rely on
colour alone.

What takes the topic's colour:

- On the blog page: the topic pill on each card, the card's cover (ground and
  focus dot), the title colour on hover and the arrow after "Read the post".
- In the filter: each topic looks like its pill; the chosen topic turns solid
  in its colour.
- In a post: the topic pill, the line above the title, the "in short" box, the
  dot in "On this page", quotes and selected text.

What does not:

- **Kinds of post** ("Today I learned") are outlined pills with no colour, on
  cards, in the filter and in posts. Colour is for topics only.
- **The menu** marks Blog in mauve on every blog page, and the blog page itself
  keeps mauve: that is the section, not a topic.
- **Figures** use their own data colours (see "Figure colours").

How it works: `topics` in `blog/_metadata.yml` gives each topic a group
(`hue: in`, `te` or `ma`) and a label. The cards and the filter read it, and
`_theme/topic-hue.lua` sets each post's colour from its first category when the
site renders. To move a topic to another group, change its `hue` there and
recolour its covers (ground and dot). A new topic needs a line in `topics` and a
default cover, `cover-topic-<topic>.svg`.

## Interactive figures

A post can have figures that run in the reader's browser, written in Observable
JS (`{ojs}` cells). `blog/equation-discovery/` is the full example. Every such
figure is built with the figure kit, `js/viz.js`, so it matches the site in
both modes. Its styles are section 9 of `_theme/theme.scss`.

1. Import the kit in the post's first OJS cell:

   ```js
   import { tokens, watchTheme, timeColor, timeGradient, motion, makeCard, button, segmented, legend, watchWidth } from "../../js/viz.js"
   ```

2. Read the colours in a cell of their own. The cell runs again when the reader
   switches light or dark, and every figure that uses it redraws:

   ```js
   VIZ = Generators.observe((notify) => watchTheme(() => notify(tokens())))
   ```

3. Build each figure as a card: `makeCard(title, hint)` gives `{ root, toolbar, body }`.
   Draw into `body` with d3, and redraw inside `watchWidth(root, ...)` so the
   figure fits any screen.
4. Put a wide figure in `::: {.viz-wide}`. It grows into the space before the
   contents list. Do not use Quarto's `.column-page` classes: they switch the
   article to Quarto's default grid.
5. In the front matter, add `execute: echo: false`, and set `reading-minutes:`
   by hand: Quarto counts the hidden code as reading text.

A toolbar breaks into lines by whole groups (a group of buttons, a labelled
choice, a readout), following the width of the card. Put related buttons in one
`viz-btns` group and a labelled choice in a `viz-field`, and do not add loose
controls between them.

Rules: never write a colour in a figure (use the names below); wrap every
animation length in `motion(ms)`; give each chart `class="viz-svg"`, and use
`viz-grid`, `viz-faint`, `viz-strong`, `viz-name`, `viz-small` and `viz-muted`
for its parts.

### Figure colours

Data colours are separate from the five page hues, because groups in a chart
must stay apart for colour-blind readers. They were checked with a palette
validator (Machado 2009 simulation, OKLab distance): every pair is at least 13
apart for protan and deutan vision and at least 15 apart for normal vision, and
every colour has at least 3:1 contrast with the page, in both modes. Change
them only together with a new check.

| Name in `VIZ` | Job | Light | Dark |
|---|---|---|---|
| `main` | The series the reader follows | #2050bf | #6488cc |
| `ghost` (also `attn`) | What drops out or is removed next | #c7d3ef | #28344c |
| `cat[0]` to `cat[4]` | Groups, always in this order | #2050bf, #b35d00, #33a587, #7e7bcc, #933860 | #6488cc, #ba630e, #3bab8d, #6f4bac, #a4345c |
| `time` | Time, early to late: one blue, five steps | #96b1e5 to #163986 | #3a5690 to #b9cef6 |
| `grey`, `faint` | Axes and data shown for comparison; context lines | #9aa0ab, #cfd3da | #5d626e, #2f333d |
| `ink`, `now`, `muted`, `rule`, `wash`, `bg` | Page tokens: `ink` and `now` are `--ink`; `muted`, `rule`, `wash` and `bg` are `--muted`, `--hair`, `--code-bg` and `--bg` | | |

The values are the `--viz-*` tokens in section 1 of `_theme/theme.scss`.

## The About globe

The Adventures part of About is a globe. The page's own script draws it on a
canvas, with no library. Its styles are the `.ab-*` block at the top of
`about/index.qmd` and its script is the last block of that file.

**Data.** Two files in `about/`. The R chunk at the top of the page puts both
into the page.

- `travel.json` has `home`, `regions` and `trips`. A trip is one object:
  `country`, `iso3`, `city`, `coords` (latitude, then longitude), `year` and
  `event`. A region is a `name` and a list of `iso3` codes. To add a trip, add
  one object to `trips`. A country that is new also goes into one region. A
  country that no region lists is shown under an "Elsewhere" tag.
- `world.geojson` has the country shapes (`ISO3`, `NAMEen`), with coordinates
  rounded to 2 decimals. Russia is one piece across the 180 degree line (its
  longitudes run past 180), so no cut shows on the globe.

**One level at a time.** No list grows long by itself. World lists the regions.
A region lists its countries (6, then "See all"). A country lists its visits
(the latest 3, then "See all N trips") and, above them, the visits per city.
The globe shows one dot per city, however many visits the city has.

**Gestures on a phone.** The page must stay easy to scroll past the globe.

- Asleep (the start): a swipe up or down on the globe scrolls the page. A swipe
  sideways, a press and hold (200 ms), a pinch or a double tap takes the globe
  and wakes it.
- Awake: every one-finger swipe moves the globe, and its frame turns dark. It
  sleeps again 4 seconds after the last touch, when a finger lands outside it,
  or when it leaves the screen. A quick stroke up or down still scrolls the page.
- Full screen: every swipe moves the globe. The whole block moves into a
  `<dialog>`, and an empty box keeps its place in the page.

**Mouse and keyboard.** Drag turns the globe. Ctrl or Cmd + scroll zooms, and a
plain scroll stays with the page. With the keyboard: arrow keys, +, - and 0.

**Speed.** A place follows the finger exactly, and north stays up. While the
globe moves it is drawn lighter: no border lines, and at most 2 device pixels
per CSS pixel. It is drawn in full again when it rests. Nothing is built while
the page loads: the script starts when the globe is near the screen, or when
the browser is idle. Keep these when you change the script, and test the change
on a real phone.

## Rules

- A `<section>` with an `id` that is a flex or grid box needs `:target::before { content: none; }`
  (see `.pg-part` in `_theme/theme.scss`). Quarto adds an empty block to the section a link
  jumps to, and in a flex or grid box that block shifts the content by one gap.
- Never hard-code a colour, spacing, size or timing value that a token exists
  for. Use the token.
- Shared layout belongs in the `.pg-*` system in `_theme/theme.scss`. Do not re-create
  it per page. If two pages need the same new piece, add it to the system once.
- Small coloured text uses the text tone (`--hue-fg`). Line tones are for
  graphics and large figures.
- One typeface, sentence case, no uppercase labels.
- Figures use `--fs-fig`. Nothing larger.
- Motion uses `--ease` and animates `opacity` and `transform` only. Hover effects
  go inside `@media (hover: hover)` so taps stay clean on touch screens, and every
  link or button also has a visible `:active` state for touch. Always honour
  `@media (prefers-reduced-motion: reduce)`.
- Every link and button has a touch target of at least 44px. A link inside a
  sentence is the one exception.
- Every image has `width` and `height` attributes (or a CSS `aspect-ratio`), so
  the page does not jump while it loads. Images below the first screen use
  `loading="lazy"`. Photos are saved at web size (about 1400px wide) before use.
  Logos are saved small: about 100px tall for the affiliation row on Home, and a
  230px PNG in `img/software/thumbs/` for each package logo.
- A block has fixed layouts and never wraps piece by piece (see "How blocks
  change with the screen width"). Check every width, not only three sizes.
- Each logo in the Home affiliation row carries its height as a number,
  `style="--hn: 32"`, next to its `width` and `height`. The row uses it to
  shrink all logos together on small phones.
- The Home hero stays modest. On a laptop (about 1190 x 690) the hero and the
  affiliations line fit in the first screen, and on a phone the photo is in
  view before any scrolling. Check both after a change to the hero.
- Both modes must work. Check light and dark before publishing.
- Do not use the long dash character. Use a comma, a colon or parentheses.

## Where things live

`README.md` has the full folder map and the everyday commands. The design files:

| File | Holds |
|---|---|
| `_theme/theme.scss` | All tokens, the navbar and footer, the `.pg-*` system, Home, Software rows, the blog listing, article pages and interactive figures |
| `_theme/theme-init.html`, `_theme/theme-toggle.html` | Light and dark mode (set before paint, then the button in the navbar) |
| `_theme/site.html` | Small scripts shared by every page (the "see more" button, the one-line scroller, the picture viewer) |
| `_theme/topic-hue.lua` | Gives each blog post its topic's colour when the site renders |
| `_quarto.yml` | Navbar, footer, the Manrope font link, giscus comments, what is rendered |
| `_page-template.qmd` | Copy-paste scaffold for a new page |
| `index.qmd` | Home (`.hm-*`) |
| `about/index.qmd` | About, with the globe CSS and script (`.ab-*`) |
| `about/travel.json`, `about/world.geojson` | The trips and regions, and the country shapes, for the About globe |
| `research/index.qmd` | Research |
| `teaching/index.qmd`, `R/teaching.R` | Teaching; the R code builds the parts and the cards from the Sheet, and the facts on course pages |
| `teaching/*/*/index.qmd` | Course pages (short pages) and course notes (article pages) |
| `software/index.qmd`, `R/software.R` | Software page; the R code builds the whole page from the Sheet |
| `R/site-data.R` | The data layer: every number and list that comes from the Sheet, Zotero or Scholar |
| `blog/index.qmd`, `blog/listing-mosaic.ejs` | Blog page and its card template |
| `blog/_metadata.yml` | Settings shared by all posts (contents list), and `topics`: each topic's colour group and label |
| `blog/_template/` | Copy-paste scaffold for a new post (`draft: true` preset) |
| `blog/_template-til/` | Copy-paste scaffold for a Today I learned note |
| `js/viz.js` | Figure kit for posts with interactive figures |
| `img/blog-art/` | Blog covers, one per post, plus one default per topic |

Reference pages: Research (rows and panels), Teaching (cards) and About (split
parts). When in doubt, match what they do.
