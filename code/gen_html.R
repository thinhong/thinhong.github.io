gen_software_table <- function(cv_sheet) {
  z <- cv_sheet
  out <- ""
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
      
      # Bold "T Ong" in authors
      author_text <- gsub("T Ong", "<strong>T Ong</strong>", tmp$author[i], fixed = TRUE)
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
      out <- paste0(
        out,
        "<span class='badge rounded-pill ",
        lang1_badge,
        "'>",
        tmp$lang1[i],
        "</span> "
      )
      
      if (!is.na(tmp$lang2[i])) {
        lang2_badge <- ifelse(
          tmp$lang2[i] == "R",
          "bg-primary",
          ifelse(
            tmp$lang2[i] == "C++",
            "bg-warning",
            ifelse(tmp$lang2[i] == "bash", "bg-info", "bg-secondary")
          )
        )
        out <- paste0(
          out,
          "<span class='badge rounded-pill ",
          lang2_badge,
          "'>",
          tmp$lang2[i],
          "</span> "
        )
      }
      
      # Add Paper badge if url_paper exists
      if (!is.na(tmp$url_paper[i])) {
        out <- paste0(
          out,
          "<a href='",
          tmp$url_paper[i],
          "' target='_blank'><span class='badge rounded-pill bg-light border border-2 border-dark text-dark' style='float:right;'>Paper</span></a>"
        )
      }
      
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
