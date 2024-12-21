library(googlesheets4)
library(kableExtra)

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
