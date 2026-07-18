# build-cv.R review, and how to fix the CV

A critical read of `cv/build-cv.R` plus `cv/cv-template.html`, focused on your three questions (inline links/bold, the DOI, the citation style) and then a full review.

## TL;DR

1. **Inline links and bold already work in the code, but not in your live CV yet.** The script already has a small `md_inline()` helper that turns `[label](url)` into a link and `**text**` into bold. Your published `cv.html` still shows the raw `[**top ten...**](...)` text because that file was built on 9 July, *before* the helper was added. You just need to rebuild (`Rscript cv/build-cv.R`).
2. **I added the one missing piece:** CSS so the bold and italic that `md_inline()` emits are pinned to the CV's exact weight, plus underlined links when printed. Without it the weight is left to each engine's font fallback rather than being explicit.
3. **The DOI change is correct.** `doi:10.xxxx` with the identifier linked is proper AMA style.
4. **Your citation style is a legitimate, reputable format** (essentially AMA / JAMA style). A few small, optional tweaks below.
5. **All refinements are now applied**, along with page numbers, reverse publication numbering, the Scholar link, and a retuned spacing scale. See section 4.

One thing to keep in mind throughout: R is not available in this environment, so I could not run the script. I verified the logic by reading it and the rendered output. Your rebuild on your own machine is the real test.

---

## 1. Inline links and bold (the generalisable solution)

### What is already there and why it is the right approach

The blocker was `esc()`. Every free-text value is HTML-escaped before it goes into the page, which is correct for safety but also turns any markup you type into literal characters. So raw markdown in a sheet cell shows up verbatim.

The clean, generalisable fix is a tiny "inline markdown" pass that runs on the free-text fields. That is exactly what `md_inline()` does (lines 191-197):

```r
md_inline <- function(x) {
  x <- esc(x)                                            # 1. neutralise real HTML first
  x <- gsub("\\[([^\\]]+)\\]\\((https?://[^) ]+|mailto:[^) ]+)\\)",
            '<a href="\\2">\\1</a>', x, perl = TRUE)     # 2. [label](url)
  x <- gsub("\\*\\*([^*]+)\\*\\*", "<strong>\\1</strong>", x, perl = TRUE) # 3. **bold**
  gsub("\\*([^*]+)\\*", "<em>\\1</em>", x, perl = TRUE)  # 4. *italic*
}
```

This is a good design, and I would keep it. The reasons it is the right generalisable choice:

- **You write normal markdown in the sheet.** No new syntax to remember. `[label](url)`, `**bold**`, `*italic*`.
- **It escapes first, then adds markup.** So real HTML in a cell can never inject anything, but your intended link and bold still come through.
- **It only allows `http`, `https`, and `mailto` links.** A cell can never smuggle in a `javascript:` link. This is a genuinely thoughtful safety detail, keep it.
- **It is vectorised**, so it runs on a whole column at once, same as `esc()`.

It is already wired into every free-text field: policy `project`, grant `title` and `sponsor`, award `description` (via `lead_bold`), teaching `note` and `venue`, software `summary`, and team `next_dest`. So your measles example will render correctly on the next build.

### How to use it in the sheet

Write plain markdown in the cell. Your real example works as-is:

```
Modelled measles seroprevalence and immunity gaps ... Recognised as one of the
city's [**top ten medical achievements of 2024**](https://medinet.hochiminhcity.gov.vn/...aspx).
```

- Link: `[visible text](https://...)`
- Bold: `**text**`
- Italic: `*text*`
- Bold link (your case): put the bold inside the label, `[**text**](https://...)`.

### The one thing that was missing (now fixed)

Your fonts only load EB Garamond weights 400, 500, and 600. A bare `<strong>` asks for 700, which is not loaded, so the result depends on each engine's font-fallback rule (Chrome, which prints your PDF, drops to the nearest available 600; other viewers may synthesize a slightly different weight). Rather than rely on that, I pinned it. Two CSS rules in `cv-template.html` make `md_inline()` output match the design exactly:

```css
.cv-table strong, .cv-pubs strong{font-weight:600; color:var(--ink)}
.cv-table em, .cv-pubs em{font-style:italic}
```

I also added, inside the print block, `.cv-table a, .cv-pubs a{text-decoration:underline}` so links are recognisable on paper (there is no hover on a printed page, and colour alone is weak, especially in black and white). If you dislike underlined links in the PDF, delete that one line.

### Limitations to be aware of (by design, low impact)

- **A URL cannot contain a space or a `)`.** This keeps the parser simple and unambiguous. If you ever need a link whose URL has a `)` (some Wikipedia URLs), replace it with `%29`. For example `Measles_(disease)` becomes `Measles_%28disease%29`.
- **A lone pair of single `*` in ordinary prose becomes italic.** Rare in a CV. If you literally need an asterisk, it is fine as long as there is not a second one in the same cell.
- **In `lead_bold` fields (awards, experience), a link should not straddle the first comma**, because those fields are split on the first comma before formatting. Policy, grants, teaching, and software have no such limit.

None of these affect your current content.

---

## 2. The DOI and the citation style

### The DOI change is correct

Old output was the literal word "doi" linked. The current code (lines 344-345) produces:

```
doi:10.3201/eid3110.250234        (with the identifier itself linked to https://doi.org/...)
```

That is the modern AMA convention: `doi:` prefix, no space, the DOI shown in full and clickable, no trailing period. Good.

### Is the citation style "correct" / reputable? Yes

Reconstructing the full format the script emits:

```
Ong TP, Nguyen VA, Smith J. Article title here. Emerg Infect Dis. 2025;31(10):250234. doi:10.3201/eid3110.250234
```

with the journal in italics and your own name in bold. This is **AMA style (the style JAMA and many biomedical journals use)**, which is one of the most reputable citation formats in medicine and epidemiology. Point by point:

- Authors as `Surname Initials`, comma-separated, no "and", no periods in initials. Correct.
- Article title in sentence case, ending in a period. Correct (this depends on how the title is stored in Zotero, see below).
- Journal abbreviated and *italicised*. This is the AMA choice. (Pure Vancouver / ICMJE is identical except it does **not** italicise the journal, so your italics make it AMA-flavoured rather than Vancouver. Both are reputable; just pick one and be consistent.)
- `Year;Volume(Issue):Pages.` Correct AMA punctuation.
- `doi:...` at the end. Correct.

So you can describe it honestly as "AMA style" on your CV, and it will read as a professional, journal-grade reference list.

### Small, optional refinements

- **Author truncation.** Strict AMA lists the first 3 (older editions: 6) authors then "et al." Your script lists **all** authors. On a CV that is usually the better choice, since it shows your exact authorship position, so I would keep listing all. Just know it is a deliberate, defensible deviation, not an error.
- **Title case.** The script uses the title exactly as Zotero stores it. AMA wants sentence case ("Measles resurgence in..." not "Measles Resurgence In..."). If your Zotero entries are in Title Case, the CV will be too. Fixing this at the source (Zotero) is cleaner than forcing it in code.
- **Journal abbreviation quality.** `journalAbbreviation` from Zotero is used, falling back to the full title. Consistency depends on your Zotero data having proper NLM abbreviations. Worth a spot-check.
- **Missing-metadata edge cases.** If an article ever has no journal, no year, or no authors, the fixed template `%s. %s. <i>%s</i>. %s%s` can leave a doubled period or a dangling `;`. It will not happen for normal journal articles, but there is a two-line guard in the patches section if you want to be safe.

---

## 3. Full critical review

### What is genuinely good

- **Single source of truth.** Everything comes from the Google Sheet and Zotero; the CV is a pure projection of that data. No hand-maintained duplicates to drift.
- **Atomic write.** `cv.html` is written to a temp file then copied over (lines 380-383), so a crash mid-build never leaves a half-written file.
- **Honest PDF verification.** After printing, it checks the PDF's modification time actually advanced, and *stops loudly* otherwise (lines 400-405). This is the right instinct: it refuses to silently ship a stale or missing PDF.
- **Resilient fetching.** 24-hour cache, `gs4_deauth()` first with an auth fallback, Zotero pagination for more than 100 items, and a clear "USING STALE CACHED DATA" warning if the network fails but a cache exists.
- **Safety.** `esc()` on all data, and `md_inline()` restricting link schemes. Good defensive posture for content that comes from a spreadsheet.
- **Graceful degradation.** `arr_desc()` and `ensure_cols()` mean a missing column downgrades a section rather than crashing the whole build. The `edu` tab even has a positional fallback.
- **The build is now decoupled from rendering** (the earlier `CV_BUILD` gate), so this heavy script no longer runs on every page edit.

### What is weak or risky (roughly in priority order)

1. **Your name is hard-coded as a fragile regex.** `bold_name <- function(x) gsub("\\bOng T([A-Z]*)", ...)` (line 208). If Zotero ever formats your name differently (a middle initial, a period, "Ong Phuc Thinh"), the bolding silently stops. It can also accidentally bold a different author whose name starts "Ong T". Better: derive it from one configurable variable. Patch below.
2. **Citation dangling punctuation on incomplete metadata** (see section 2). Low probability, easy guard.
3. **String-sorted dates can misorder.** `arr_desc()` sorts numerically if the whole column is numeric, else as text. For a `date` column like `2024-10` vs `2024-9`, text sorting puts `2024-10` before `2024-9`. Full ISO dates (`2024-10-01`) sort fine; bare years sort fine; mixed or single-digit months do not. Consider parsing to a real date before sorting the policy and awards tabs.
4. **No central schema check.** Each `build_*` function assumes specific column names (for example grants needs `budget_total_usd`, `my_role`, `outcome`, `period`, `title`, `sponsor`). Some are guarded, some are not; a renamed column can fail deep in a function with a cryptic error. A short "these columns must exist per tab" check up front would turn a confusing failure into a clear message.
5. **Eight separate `read_sheet()` calls.** One network round-trip per tab (line 93). It is fine because of the cache, but it is the main reason a cold build is slow. Not worth changing unless cold builds bother you.
6. **Tag inconsistency.** `md_inline()` emits semantic `<strong>` / `<em>`, while the publications use presentational `<b>` / `<i>`. Both look right now that the CSS covers all four, but for tidiness you could standardise on `<strong>`/`<em>` everywhere. Cosmetic.
7. **Only `journalArticle` items appear** (line 325). Preprints, chapters, and conference papers are silently excluded. This is commented as intentional; just flagging it in case you later want a "Preprints" subsection.
8. **`tail` shadows `base::tail`.** Harmless, but a slightly confusing variable name (line 342).

### Overall

This is a solid, careful script. The architecture (data to HTML template to Chrome-printed PDF) is sound, the safety and resilience details are better than most hand-rolled CV generators, and the two things you asked about are already essentially handled. The remaining items are refinements, not bugs, with the single exception that you must rebuild for any of it to appear.

---

## 4. What changed (all applied)

Everything in section 3 has been implemented, plus the four things you asked for afterwards.

**In `cv/build-cv.R`**

- `own_name <- "Ong T"` is now a config value, and `bold_name()` is built from it. Changing how Zotero stores your name is now a one-line edit.
- Citation punctuation guard in `build_publications()`: an empty journal slot is dropped, and `. .`, `. ;` and a leading `.` from a missing field are collapsed.
- `sort_by_when()`, a numeric year*12+month sort, replaces the text sort for the policy tab. It reads Date columns, `2024-10-05`, `2024-10`, `2024`, and ranges like `2022-2026`, so `2024-9` can no longer sort above `2024-10`.
- `required_cols` + `check_columns()` validate every tab up front. A renamed Sheet column now fails immediately with a message naming the tab and the missing columns, instead of erroring deep inside a build function.
- Publications are numbered **downwards from 27 to 1**, newest first. The number comes from a CSS counter seeded at `nrow(p) + 1`, not from the `reversed` attribute, because `reversed` is honoured by browsers but not by every PDF engine (verified: it silently fell back to 1, 2, 3 in testing).
- A Google Scholar lead line sits under the Publications heading, with the article count.
- The PDF now carries a **running footer on every page**: your name and the updated date on the left, `Page X of Y` on the right. This comes from Chrome's own `footerTemplate`, because Chrome does not implement CSS `@page` margin boxes (`@bottom-center`), which is the usual way to do this.

**In `cv/cv-template.html`**

- Spacing retuned (see below).
- `strong` / `em` rules so `md_inline()` output matches the design, with `a strong{color:inherit}` so a bold link keeps the link colour.
- Links underlined in print, since there is no hover on paper.
- The on-page footer is hidden in print, because the running footer replaces it.
- Publication markers are right-aligned so `27.` and `9.` end on the same edge.

### The spacing, measured

Your two reference points, from the PDFs themselves:

| | old (attached) | current | **new** |
|---|---|---|---|
| body size | 12pt | 12pt | 12pt (unchanged) |
| line-height | 1.14 (cramped) | 1.50 (airy) | **1.32** rows, 1.40 references |
| space above a heading | n/a | 27.7pt | **17.5pt** |
| space below a heading | n/a | 18.5pt | **11.3pt** |
| page margins T/B | 12.3 / 7.3mm | 19 / 19mm | **14 / 16mm** |

The type size is untouched, because both your versions already agreed on 12pt and shrinking it is the wrong lever. The density comes from leading, row padding and margins instead. Two deliberate choices:

- Short table rows get tighter leading (1.32) than the long wrapped references and the intro (1.40 to 1.42), because a long line needs more leading to track back to the next line. Uniform leading is the usual mistake here.
- The gap above a heading stays about 1.55x the gap below it, so each heading still visually belongs to the section beneath it. Old was 1.50, so the hierarchy is preserved, just at a smaller scale.

Measured effect, rendering identical content through the same engine: **+17% more content per page** and one page saved.

---

## What to do next

1. From the project root, run `Rscript cv/build-cv.R`.
2. Open `cv/cv.html` and check: the policy line shows a bold link, publications count 27 down to 1, the Scholar line is under the Publications heading, and DOIs read `doi:10.xxxx`.
3. Open `cv/cv.pdf` and check the page-number footer. This is the one item that could not be tested here, because it depends on Chrome and there is no Chrome in this environment. If the footer is missing or clipped, increase the bottom margin in the `@page` rule (currently 16mm); Chrome draws the footer inside that margin.

If it looks right, publish with `CV_BUILD=1 quarto publish gh-pages`.
