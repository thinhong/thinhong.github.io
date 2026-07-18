#!/bin/bash
# Deploy the site.
#
# The slow part of "quarto publish gh-pages" was that publish re-renders the
# WHOLE site every time before pushing (18 pages + slides), and re-runs the
# Google Scholar and Google Sheets calls in the process. This script splits the
# two steps: render once here, then publish the already-built docs/ with
# --no-render, so publishing is just a git push and never touches the network.
#
# If the CV (cv/cv.html, cv/cv.pdf) changed, rebuild it first. It is kept
# separate on purpose because it is the heavy step (Zotero + Sheets + Chrome):
#   Rscript cv/build-cv.R
#
# Usage:  bash deploy.sh
set -euo pipefail

echo "==> Rendering site to docs/ ..."
quarto render

echo "==> Publishing docs/ to gh-pages (no re-render) ..."
# This is your usual command with --no-render added, so it stays interactive.
# Add --no-prompt to make it fully unattended once you have confirmed the
# gh-pages target once (Quarto records it in _publish.yml after the first run).
quarto publish gh-pages --no-render

echo "==> Done. Site pushed to the gh-pages branch."
