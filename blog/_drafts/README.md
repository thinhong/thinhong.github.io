# _drafts (holding pen)

A place to park raw ideas that are not ready to be seen yet, not even in a local preview.

This folder starts with an underscore, so Quarto ignores everything inside it when it builds the site. Nothing here is rendered, listed, or published.

## The three stages of a post

1. **Raw idea, hidden even locally.** Start a folder in here, for example `blog/_drafts/my-idea/index.qmd`. It will not appear anywhere.

2. **Writing, visible locally only.** When you want to see it in context, move the folder up one level to `blog/my-idea/` and set `draft: true` in the front matter. Now `quarto preview` shows it (with a "Draft" banner) and it appears in your local mosaic, but a published build still leaves it out.

3. **Published.** Set `draft: false`, render, and push. It goes live.

## Starting from the template

The quickest way to begin at stage 2 is to copy `blog/_template/` to `blog/your-post-slug/`. The template already has `draft: true` and the standard front matter.
