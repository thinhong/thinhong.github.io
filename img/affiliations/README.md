# Affiliation logos

Logos shown in the homepage hero strip (the "Affiliations" row). The files here are
tasteful placeholders so the layout works right away. Replace them with the official logos.

## How to swap in a real logo

1. Drop the official file in this folder, keeping the same filename:
   - `oxford.svg` (University of Oxford)
   - `oucru.svg` (Oxford University Clinical Research Unit)
   - `midsea.svg` (MIDSEA)
   - `vimc.svg` (Vaccine Impact Modelling Consortium)
2. That is it. The homepage already points at these paths, so the new logo appears on next build.

If you prefer PNG, save as e.g. `oxford.png` and update the matching `src` in `index.qmd`
(look for the `affil-logos` block).

## Format guidance (for the best result)

- Prefer SVG. It stays crisp at any size and scales cleanly on retina screens.
- If only PNG is available, export at 2x (about 320px wide) on a transparent background.
- Transparent background. The logo sits inside a white chip, so any padding baked into
  the file will look like extra space. Trim tight to the artwork.
- Any shape works. Logos are auto-fit to 30px tall inside the chip, so horizontal
  wordmark logos look best; tall crest-only logos will look small next to wide ones.
- Full colour is fine. Each logo sits on a white chip, so the real brand colours read
  well in both light and dark mode. No dark-mode version needed.

## Add or remove a logo

Edit the `affil-logos` block in `index.qmd`. Each logo is one line:

```html
<span class="logo-chip" title="University of Oxford"><img src="img/affiliations/oxford.svg" alt="University of Oxford"></span>
```

To make a logo clickable, wrap it in a link:

```html
<a class="logo-chip" href="https://www.ox.ac.uk/" title="University of Oxford"><img src="img/affiliations/oxford.svg" alt="University of Oxford"></a>
```

The chip styling (`.logo-chip`) and the strip layout live in `theme.scss` under the
"Affiliations" comment, if you want to tweak size, spacing, or shadow.
