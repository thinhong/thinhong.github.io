# Pull Google Scholar citation indices (total citations + h-index) at render
# time. Designed to be safe for a static-site build:
#   * a 24h on-disk cache keeps previews fast and avoids hammering Scholar,
#   * a failed or blocked fetch falls back to the cache, then to hardcoded
#     values, so the build never breaks.
# Returns a list(citations = <int>, hindex = <int>).

get_scholar_indices <- function(id = "PkfvVs0AAAAJ",
                                cache = ".scholar-cache.json",
                                max_age_hours = 24,
                                fallback = list(citations = 313L, hindex = 8L)) {

  .read_cache <- function() {
    if (!file.exists(cache)) return(NULL)
    out <- tryCatch(jsonlite::fromJSON(cache), error = function(e) NULL)
    if (is.list(out) && !is.null(out$citations) && !is.null(out$hindex)) {
      return(list(citations = as.integer(out$citations),
                  hindex    = as.integer(out$hindex)))
    }
    NULL
  }

  .cache_fresh <- function() {
    if (!file.exists(cache)) return(FALSE)
    age <- as.numeric(difftime(Sys.time(), file.info(cache)$mtime, units = "hours"))
    is.finite(age) && age < max_age_hours
  }

  # 1) a fresh cache wins, no network needed (keeps quarto preview snappy)
  if (.cache_fresh()) {
    cached <- .read_cache()
    if (!is.null(cached)) return(cached)
  }

  # 2) try Google Scholar
  fetched <- tryCatch({
    if (!requireNamespace("httr", quietly = TRUE) ||
        !requireNamespace("xml2", quietly = TRUE)) stop("missing packages")
    url  <- sprintf("https://scholar.google.com/citations?user=%s&hl=en", id)
    resp <- httr::GET(
      url,
      httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36"),
      httr::timeout(12)
    )
    if (httr::status_code(resp) != 200) stop("bad status")
    doc  <- xml2::read_html(httr::content(resp, as = "text", encoding = "UTF-8"))
    # The summary table holds six numbers in td.gsc_rsb_std, in this order:
    #   citations(all), citations(since), h(all), h(since), i10(all), i10(since)
    vals <- xml2::xml_text(xml2::xml_find_all(doc, "//td[@class='gsc_rsb_std']"))
    vals <- suppressWarnings(as.integer(gsub("\\D", "", vals)))
    vals <- vals[!is.na(vals)]
    if (length(vals) < 3) stop("could not parse stats")
    list(citations = vals[1], hindex = vals[3])
  }, error = function(e) NULL)

  if (!is.null(fetched) && is.finite(fetched$citations) && fetched$citations > 0) {
    tryCatch(jsonlite::write_json(fetched, cache, auto_unbox = TRUE),
             error = function(e) NULL)
    return(fetched)
  }

  # 3) stale cache, then hardcoded fallback
  stale <- .read_cache()
  if (!is.null(stale)) return(stale)
  fallback
}
