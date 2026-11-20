#!/usr/bin/env Rscript
# =====================================================================
# R/update-data.R - fetch fresh data now and say what changed.
#
#     Rscript R/update-data.R        # run from the project root
#
# Fetches the Google Sheet, the Zotero collection and Google Scholar through
# R/site-data.R and saves them in data/site-data.rds. The website pages and
# the CV both read that file, so after this command the next render and the
# next CV build use the new data.
#
# You rarely need to run it by hand:
#   * deploy.sh runs it first, every time (except  bash deploy.sh quick),
#   * a render refreshes any source that is older than a day by itself.
# Run it when you have just edited the Sheet or Zotero and want to preview the
# result at once.
#
# Exit status: 0 when every source has data (fresh, or a saved copy after a
# failed fetch, with a warning). 1 only when a source has no data at all,
# because pages that need it could not be built.
# =====================================================================

if (!file.exists("_quarto.yml") || !file.exists("R/site-data.R"))
  stop("Run this from the project root:  Rscript R/update-data.R", call. = FALSE)
source("R/site-data.R")

before <- site_numbers(site_data("never"))
dat    <- site_data("force")
after  <- site_numbers(dat)

## ---------------- report ----------------
show <- function(x, money = FALSE) {
  if (is.null(x) || length(x) != 1 || is.na(x)) return("none")
  if (money) usd(x) else format(x, big.mark = ",", trim = TRUE, scientific = FALSE)
}
rows <- list(
  c("Citations",              "citations",     FALSE),
  c("h-index",                "hindex",        FALSE),
  c("Peer-reviewed articles", "articles",      FALSE),
  c("Software packages",      "software",      FALSE),
  c("Grants",                 "grants",        FALSE),
  c("Research funding",       "funding_total", TRUE)
)
cat("\n")
for (r in rows) {
  money <- identical(r[3], "TRUE")
  old <- show(before[[r[2]]], money); new <- show(after[[r[2]]], money)
  cat(sprintf("  %-24s %s%s\n", r[1], new,
              if (identical(old, new)) "" else sprintf("   (was %s)", old)))
}
cat("\n")

missing <- character(0)
for (s in names(.sd_labels)) {
  if (is.null(dat[[s]])) {
    missing <- c(missing, .sd_labels[[s]])
    cat(sprintf("  %-15s NO DATA\n", .sd_labels[[s]]))
  } else {
    fresh <- .sd_hours(dat$fetched[[s]]) < 0.25
    cat(sprintf("  %-15s %s\n", .sd_labels[[s]],
                if (fresh) "fresh"
                else paste0("NOT refreshed, still the copy from ", format(dat$fetched[[s]], "%Y-%m-%d %H:%M"))))
  }
}
cat("\n")

if (length(missing)) {
  cat("update-data: no data for: ", paste(missing, collapse = ", "),
      ". Fix the cause printed above and run this again.\n", sep = "")
  quit(save = "no", status = 1)
}
