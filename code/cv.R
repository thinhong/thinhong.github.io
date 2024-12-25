library(googlesheets4)
library(kableExtra)
library(httr)
library(jsonlite)

#### Make latex tables ####
gs4_auth("ongphucthinh@gmail.com")

make_italic <- function(text) {
  paste0("\\textit italicise{", text, " italicise}")
}

make_url <- function(text) {
  paste0("\\url urlhere{", text, " urlhere}")
}

# A4 papersize is 210x297mm
# Substract left=15mm and right=15mm we have 180mm left
make_table <- function(data, colwidths = NULL, italic = F, header = NULL, longtable = F, newline = F, url = F, margin = NULL) {
  kable_obj <- kbl(data,
                   format = "latex",
                   booktabs = T,
                   longtable = longtable,
                   col.names = header) |>
    kable_styling(full_width = T)
  
  # Set column widths if user enter colwidths
  if (!is.null(colwidths)) {
    # Input colwidths as percentage, generate widths in mm
    colwidths <- 170 * colwidths / sum(colwidths)
    colwidths <- paste0(colwidths, "mm")
    for (i in seq_along(colwidths)) {
      kable_obj <- kable_obj |> column_spec(i, width = colwidths[i])
    }
  }
  
  if (italic) {
    kable_obj <- gsub("\\\\textbackslash\\{\\}", "\\\\", kable_obj)
    kable_obj <- gsub(" italicise\\\\", "", kable_obj)
  }
  
  if (newline) {
    kable_obj <- gsub("\\\\textbackslash\\{\\}", "\\\\", kable_obj)
  }
  
  if (url) {
    kable_obj <- gsub("\\\\textbackslash\\{\\}", "\\\\", kable_obj)
    kable_obj <- gsub(" urlhere\\\\", "", kable_obj)
  }
  
  # Remove top and bottom lines
  gsub("\\\\toprule|\\\\bottomrule|\\\\midrule", "", kable_obj)
}

#### Read zotero collection ####

# Replace these with your actual Zotero details
api_key <- Sys.getenv("ZOTERO_API_KEY")
user_id <- Sys.getenv("ZOTERO_USER_ID")
collection_key <- Sys.getenv("ZOTERO_COLLECTION_KEY")

# Base URL for Zotero API
base_url <- paste0("https://api.zotero.org/users/", user_id, "/collections/", collection_key, "/items")

# Function to fetch items from the collection
fetch_zotero_items <- function(api_key, base_url, limit = 100) {
  
  # Set up query parameters
  params <- list(
    format = "json", # Return JSON format
    limit = limit    # Number of items to fetch per request (max 100)
  )
  
  # Make the API request
  response <- GET(base_url, query = params, add_headers("Zotero-API-Key" = api_key))
  
  # Check response status
  if (status_code(response) != 200) {
    stop("Failed to fetch data: ", status_code(response))
  }
  
  # Parse the JSON response
  items <- content(response, "text", encoding = "UTF-8")
  items_df <- fromJSON(items)
  
  return(items_df$data)
}

# Function to process authors from the creators column
process_authors <- function(creators_list) {
  # Check if creators_list is empty
  if (length(creators_list) == 0) return(NA)
  
  # Filter for authors only
  authors <- creators_list[creators_list$creatorType == "author", ]
  
  # Format names as "LastName Initials"
  formatted_authors <- sapply(1:nrow(authors), function(i) {
    last_name <- authors$lastName[i]
    first_name <- authors$firstName[i]
    
    # Convert given and middle names (including hyphenated names) to initials
    initials <- paste0(substr(unlist(strsplit(first_name, "[- ]")), 1, 1), collapse = "")
    
    # Combine last name with initials
    paste(last_name, initials)
  })
  
  # Combine authors into a single string separated by "; "
  authors_string <- paste(formatted_authors, collapse = ", ")
  
  return(authors_string)
}
