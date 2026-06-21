# ThinhOng site style guide

This is the single reference for keeping the site visually consistent. The goal
is that any page built from these rules looks like it belongs, and that the kind
of drift that made the Teaching and Software headers sit at different heights
cannot happen again.

Read this before building or restyling a page. It is short on purpose.

## How consistency is enforced

Two ideas do all the work:

1. **Design tokens.** Every spacing, size, radius and timing value lives once in
   `theme.scss` as a CSS variable (a token). Pages never hard-code these numbers;
   they reference the token. Change a token once and every page updates together.

2. **A shared page system (`.pg-*`).** The editorial page layout (hero, soft
   panels, sticky contents nav) is defined once in `theme.scss` under the `.pg-*`
   namespace. Teaching and Software both use it, so they are identical by
   construction. New pages use it too.

Anything that is unique to one page (the package rows on Software, the course
cards and modal on Teaching) is namespaced to that page (`.sw-*`, `.tq-*`) and
still references the shared tokens.

## Design tokens

Defined in `theme.scss` inside `:root` (light) and the dark override. Use the
token, never the raw value.

### Layout and spacing

| Token | Value | Use for |
|---|---|---|
| `--page-max` | 1200px | Max width of any page shell |
| `--page-pad-x` | clamp(18px, 4vw, 46px) | Left/right page padding |
| `--page-pad-bottom` | clamp(40px, 6vw, 56px) | Space below page content |
| `--hero-pad-top` | clamp(8px, 1.8vw, 18px) | Gap from navbar to the hero |
| `--hero-pad-bottom` | clamp(20px, 2.6vw, 30px) | Gap below the hero |
| `--hero-gap` | clamp(20px, 4vw, 56px) | Gap between hero columns |
| `--body-gap` | clamp(26px, 3.4vw, 48px) | Gap between main column and contents nav |
| `--stack-gap` | clamp(16px, 2vw, 22px) | Vertical gap between panels |
| `--panel-pad` | clamp(18px, 2.3vw, 28px) | Padding inside a soft panel |
| `--panel-radius` | 18px | Soft panel corner radius |
| `--card-radius` | 14px | Card / row corner radius |
| `--toc-w` | 196px | Width of the sticky contents nav |
| `--toc-top` | 92px | Sticky offset of the contents nav |
| `--scroll-offset` | 96px | scroll-margin so anchors clear the navbar |

### Type

| Token | Value | Use for |
|---|---|---|
| `--display-font` | "Bebas Neue", sans-serif | All display headings (h1, section titles, stats) |
| `--fs-eyebrow` | .74rem | The small uppercase label above a title |
| `--fs-h1` | clamp(2.3rem, 5vw, 4rem) | Page title |
| `--fs-h2` | clamp(1.4rem, 2.2vw, 1.9rem) | Section title |
| `--fs-lead` | clamp(.88rem, 1vw, .98rem) | Lead paragraph |

### Colour and motion

Colour tokens already existed and are unchanged: `--brand`, `--brand-soft`,
`--brand-strong`, `--heading`, `--text`, `--text-muted`, `--text-faint`,
`--border`, `--surface`, `--bg`, `--panel-bg`, `--card-shadow`, and so on. Each
has a light and a dark value, so using the token gives correct dark mode for free.

Motion uses one easing token, `--ease` (`cubic-bezier(.22,.61,.36,1)`). The older
`--e` is kept as an alias of `--ease` so existing page CSS keeps working; prefer
`--ease` in new code.

## The shared page system (`.pg-*`)

Anatomy of an editorial page:

```
.pg                       page shell (max-width, padding)
  .pg-hero                two-column hero
    .pg-hero__head        left: eyebrow + title (pinned to the top)
      .pg-eyebrow         small uppercase label
      .pg-title           the big display title
        .pg-title-thin    the faded part of the title
    .pg-hero__aside       right: lead text, and optional stats
      .pg-lead            one short descriptive sentence
      .pg-stats           optional counters
        .pg-stat > .pg-stat-num + .pg-stat-label
        .pg-sep           thin vertical divider between stats
  .pg-body                main column + sticky contents nav
    .pg-main              the stack of panels
      .pg-panel           a soft section panel (id = anchor target)
        .pg-sechead       section header row
          .pg-num         the "[01]" index
          .pg-sectitle    section title
          .pg-secsub      optional one-line subtitle
          .pg-count       optional right-aligned count
        ...page-specific content...
    .pg-toc               sticky contents nav
      .pg-toc-h           the "On this page" label
      a > .tn + .tt       a numbered link
```

Key behaviour, do not change without updating both pages:

- The hero is `align-items: start`, so **the title is always pinned to the top**.
  This is what keeps the title at the same height on every page no matter what
  the right column holds. This was the fix for the Teaching/Software mismatch.
- Panels fade in on scroll via `.pg.anim .pg-panel` (opacity only, which is cheap
  and safe with subgrid). Page-specific inner items add their own stagger.
- The contents nav collapses below 880px; the hero goes single column below 680px.

## Building a new page

1. Copy `_page-template.qmd` to `your-section/index.qmd`.
2. Set `pagetitle` and `description` in the front matter.
3. Fill in the eyebrow, title, lead, and optional stats in the hero.
4. Add one `.pg-panel` per section, each with a unique `id`, and put your content
   inside. Add a matching `.pg-toc` link for each panel.
5. Put anything visual that is unique to this page in the page's own `<style>`
   block with a short page prefix (for example `.xx-card`), and **style it with
   the tokens above**, not raw numbers.
6. Add the page to the navbar in `_quarto.yml` if it should be linked.

## Rules

- Never hard-code a spacing, size, radius or timing value that a token exists for.
  Use the token.
- Shared layout (hero, panels, contents nav, section heads) belongs in the
  `.pg-*` system in `theme.scss`. Do not re-create it per page. If two pages need
  the same new piece, add it to `.pg-*` once.
- Page-unique pieces are namespaced (`.sw-*`, `.tq-*`, etc.) and live with the
  page. They still use the tokens.
- Headings use `--display-font`. Body text uses Inter (the site default).
- Motion uses `--ease`. Hover effects go inside `@media (hover: hover)` so taps
  stay clean on touch. Always honour `@media (prefers-reduced-motion: reduce)`.
- Keep animations to `opacity` and `transform` so they stay smooth.
- Do not use the long dash character. Use a comma, a colon, or parentheses.

## Where things live

| File | Holds |
|---|---|
| `theme.scss` | All tokens, the `.pg-*` system, and the Software package-row styles (`.sw-*`) |
| `_page-template.qmd` | Copy-paste scaffold for a new editorial page |
| `teaching/index.qmd` | Teaching content + its course-card and modal CSS/JS (`.tq-*`) |
| `software/index.qmd` | Software page; pulls data and calls the generator |
| `code/software.R` | Generates the Software markup using `.pg-*` shell + `.sw-*` rows |
| `design-mockups/system-preview.html` | Standalone proof that the heroes align |

Reference pages: Teaching and Software are the two worked examples. When in
doubt, match what they do.
