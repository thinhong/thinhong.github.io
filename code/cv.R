library(googlesheets4)
library(httr)
library(jsonlite)

#### Google Sheets auth ####
gs4_auth("ongphucthinh@gmail.com")

#### Read Zotero collection ####
api_key        <- Sys.getenv("ZOTERO_API_KEY")
user_id        <- Sys.getenv("ZOTERO_USER_ID")
collection_key <- Sys.getenv("ZOTERO_COLLECTION_KEY")

base_url <- paste0("https://api.zotero.org/users/", user_id, "/collections/", collection_key, "/items")

# Fetch items from the Zotero collection
fetch_zotero_items <- function(api_key, base_url, limit = 100) {
  params   <- list(format = "json", limit = limit)
  response <- GET(base_url, query = params, add_headers("Zotero-API-Key" = api_key))
  if (status_code(response) != 200) {
    stop("Failed to fetch data: ", status_code(response))
  }
  items <- content(response, "text", encoding = "UTF-8")
  fromJSON(items)$data
}

# Format authors from the Zotero creators column as "LastName Initials"
process_authors <- function(creators_list) {
  if (length(creators_list) == 0) return(NA)
  authors <- creators_list[creators_list$creatorType == "author", ]
  formatted_authors <- sapply(seq_len(nrow(authors)), function(i) {
    last_name  <- authors$lastName[i]
    first_name <- authors$firstName[i]
    initials   <- paste0(substr(unlist(strsplit(first_name, "[- ]")), 1, 1), collapse = "")
    paste(last_name, initials)
  })
  paste(formatted_authors, collapse = ", ")
}
