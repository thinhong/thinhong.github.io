gen_software_table <- function(cv_sheet) {
  z <- cv_sheet
  out <- c()
  for (category in unique(z$category)) {
    out <- paste0(out, "<h3>", category, "</h3>\n\n")
    tmp <- z[z$category == category, ]
    out <- paste0(out, "<table class='table'>")
    for (i in 1:nrow(tmp)) {
      out <- paste0(out, "\n  <tr>")
      # Name and Summary column
      out <- paste0(out,
                    "<td class='w-75'><strong>",
                    tmp$name[i],
                    "</strong><br>")
      out <- paste0(out, tmp$summary[i], "<br>")
      
      # Bold "Ong T" in authors
      author_text <- gsub("Ong T", "<strong>Ong T</strong>", tmp$author[i], fixed = TRUE)
      out <- paste0(out, "<em>Authors: ", author_text, "</em><br>")
      
      # Language badges
      lang1_badge <- ifelse(
        tmp$lang1[i] == "R",
        "bg-primary",
        ifelse(
          tmp$lang1[i] == "C++",
          "bg-warning",
          ifelse(tmp$lang1[i] == "bash", "bg-info", "bg-secondary")
        )
      )
      lang2_badge <- ifelse(
        tmp$lang2[i] == "R",
        "bg-primary",
        ifelse(
          tmp$lang2[i] == "C++",
          "bg-warning",
          ifelse(tmp$lang2[i] == "bash", "bg-info", "bg-secondary")
        )
      )
      
      out <- paste0(out,
                    "<span class='badge ",
                    lang1_badge,
                    "'>",
                    tmp$lang1[i],
                    "</span> ")
      out <- paste0(out,
                    "<span class='badge ",
                    lang2_badge,
                    "'>",
                    tmp$lang2[i],
                    "</span>")
      out <- paste0(out, "</td>")
      
      # Logo column with hyperlink
      out <- paste0(out,
                    "<td class='w-25'><a href='",
                    tmp$url_github[i],
                    "' target='_blank'>")
      out <- paste0(out,
                    "<img src='",
                    tmp$logo[i],
                    "' alt='Logo' style='height:120px;'></a></td>")
      
      out <- paste0(out, "</tr>")
    }
    out <- paste0(out, "\n</table>\n\n")
  }
  cat(out)
}