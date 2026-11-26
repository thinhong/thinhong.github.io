#!/bin/bash
# Deploy the site: one command, four steps.
#
#   bash deploy.sh          everything (use this most of the time)
#   bash deploy.sh quick    for a text-only change, such as a blog post:
#                           skips steps 1 and 2, so no network and no CV check
#   bash deploy.sh full     everything, and render every page in step 3
#   bash deploy.sh plan     only say what step 3 would render. Changes nothing.
#
# 1. Fresh data.  R/update-data.R fetches the Google Sheet, Zotero and Google
#    Scholar and saves them in data/site-data.rds. The pages and the CV both read
#    that file, so every figure on the site is current at deploy time.
# 2. The CV, only if needed.  cv/build-cv.R rebuilds cv.html + cv.pdf when the
#    data, the template, the fonts or the script changed since the last build. If nothing
#    changed, the heavy step (Chrome printing the PDF) is skipped.
# 3. Render to docs/, only what changed.  R/render-plan.R compares every source
#    file with its record of the last render and lists the pages that depend on
#    a changed file. One edited blog post renders that post and the blog list,
#    not the whole site. A change to the theme or to _quarto.yml renders every
#    page. The rules are at the top of R/render-plan.R.
# 4. Publish docs/ with --no-render, so publishing is only a git push.
#
# Useful switches:
#   CV_SKIP=1 bash deploy.sh    leave the CV as it is (for example, no Chrome here)
#   CV_FORCE=1 bash deploy.sh   rebuild the CV even if nothing changed
#
# A failed fetch in step 1 does not stop the deploy: the last saved copy of that
# source is used and the script says so. It stops only if a source has no data
# at all.
#
# With `quick`, the figures on the site stay as they were at the last deploy
# that ran step 1. If something on the site ever looks out of date, run
# `bash deploy.sh full` once: it renders every page again.
#
# Stop `quarto preview` before you deploy. A preview writes into docs/ and shows
# draft posts. Step 3 repairs what an earlier preview left in docs/, but a
# preview that is still running can write again while the site is published.
set -euo pipefail

mode="${1:-}"
case "$mode" in
  ""|quick|full|plan) ;;
  *) echo "Usage: bash deploy.sh [quick|full|plan]"; exit 2 ;;
esac
if [ ! -f _quarto.yml ]; then echo "Run this from the project root."; exit 2; fi

# The Quarto version is part of the render record: after a Quarto update the
# files in docs/site_libs change, so every page must be rendered again.
export QUARTO_VERSION="$(quarto --version | tr -d '\r')"

if [ "$mode" = plan ]; then
  Rscript --vanilla R/render-plan.R plan
  exit 0
fi

started=$SECONDS
took() { echo "    (${1}: $((SECONDS - t0)) s)"; }

t0=$SECONDS
if [ "$mode" = quick ]; then
  echo "==> 1/4 Data: skipped (quick). The figures stay as they are."
  echo "==> 2/4 CV: skipped (quick)."
  # Pages read the saved data only, even if it is older than a day: no network.
  export SITE_DATA_REFRESH=never
else
  echo "==> 1/4 Refreshing data (Google Sheet, Zotero, Google Scholar) ..."
  Rscript R/update-data.R
  took "data"; t0=$SECONDS

  echo "==> 2/4 CV: rebuilding only if its data, template, fonts or script changed ..."
  CV_IF_CHANGED=1 Rscript cv/build-cv.R
  took "CV"; t0=$SECONDS
fi

echo "==> 3/4 Rendering to docs/: only what changed ..."
if [ "$mode" = full ]; then
  Rscript --vanilla R/render-plan.R full
else
  Rscript --vanilla R/render-plan.R
fi
plan="$(head -n 1 data/render-plan.txt | tr -d '\r')"
case "$plan" in
  none) ;;
  full) quarto render ;;
  partial)
    # One `quarto render` per target (a page or a folder), from line 2 of the
    # plan on. The file is read on its own channel (3), so quarto cannot
    # swallow the rest of the list.
    line=0
    while IFS= read -r target <&3 || [ -n "$target" ]; do
      line=$((line + 1))
      target="${target%$'\r'}"
      if [ "$line" -eq 1 ] || [ -z "$target" ]; then continue; fi
      echo "    quarto render $target"
      quarto render "$target"
    done 3< data/render-plan.txt
    ;;
  *) echo "Could not read data/render-plan.txt"; exit 1 ;;
esac
# The render worked: save the record (the source files and what is in docs/).
# If a render fails, the script stops above, the old record stays, and the next
# run tries the same pages again.
Rscript --vanilla R/render-plan.R done
took "render"; t0=$SECONDS

echo "==> 4/4 Publishing docs/ to gh-pages (no re-render) ..."
# Add --no-prompt to make this fully unattended once you have confirmed the
# gh-pages target once (Quarto records it in _publish.yml after the first run).
quarto publish gh-pages --no-render
took "publish"
# A running `quarto preview` writes into docs/ and shows drafts. If docs/ changed
# while it was being published, say so.
Rscript --vanilla R/render-plan.R check

echo "==> Done in $((SECONDS - started)) s. Site pushed to the gh-pages branch."
