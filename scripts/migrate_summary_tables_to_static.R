#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(stringr)
  library(glue)
})

files <- list.files("posts", pattern = "^index\\.qmd$", recursive = TRUE, full.names = TRUE)

pattern <- paste0(
  "```\\{r, echo=FALSE, warning=FALSE, message=FALSE\\}\\n",
  "race_date <- \"([0-9]{4}-[0-9]{2}-[0-9]{2})\"\\n\\n",
  "source\\('\\.\\./\\.\\./\\.\\./inst/aux_functions\\.R'\\)\\n\\n",
  "DT::datatable\\(get_summary_data\\(race_date\\),\\n",
  "\\s*rownames = FALSE,\\n",
  "\\s*filter = 'top',\\n",
  "\\s*selection = 'single',\\n",
  "\\s*escape = FALSE,\\n",
  "\\s*options = list\\(\\n",
  "\\s*paging =TRUE,\\n",
  "\\s*pageLength =\\s+10\\n",
  "\\s*\\)\\n",
  "\\)\\n",
  "```"
)

replacement <- "<!-- summary_race_date: \\1 -->\n{{< include summary_table.html >}}"

updated <- 0

for (path in files) {
  content <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  new_content <- stringr::str_replace(content, pattern, replacement)

  if (!identical(content, new_content)) {
    writeLines(new_content, path, useBytes = TRUE)
    updated <- updated + 1
    message(glue("Updated {path}"))
  }
}

message(glue("Updated {updated} files"))
