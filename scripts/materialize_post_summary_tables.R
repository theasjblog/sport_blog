#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(glue)
  library(stringr)
})

source("inst/aux_functions.R")

extract_match <- function(lines, pattern) {
  match <- stringr::str_match(lines, pattern)
  values <- as.vector(match[, -1, drop = FALSE])
  values <- values[!is.na(values) & nzchar(values)]

  if (length(values) == 0) {
    return(NA_character_)
  }

  values[[1]]
}

extract_race_id <- function(lines) {
  code_value <- extract_match(lines, 'race_id\\s*<-\\s*"([^"]+)"')
  if (!is.na(code_value) && nzchar(code_value)) {
    return(code_value)
  }

  extract_match(lines, 'summary_race_id:\\s*([^\\s>]+)')
}

extract_race_date <- function(lines) {
  code_value <- extract_match(lines, 'race_date\\s*<-\\s*"([0-9]{4}-[0-9]{2}-[0-9]{2})"')
  if (!is.na(code_value) && nzchar(code_value)) {
    return(code_value)
  }

  extract_match(lines, 'summary_race_date:\\s*([0-9]{4}-[0-9]{2}-[0-9]{2})')
}

extract_yaml_race_id <- function(lines) {
  extract_match(lines, '^race_id:\\s*"?(.*?)"?\\s*$')
}

extract_yaml_race_date <- function(lines) {
  extract_match(lines, '^race_date:\\s*"?(.*?)"?\\s*$')
}

escape_html <- function(text) {
  text <- gsub("&", "&amp;", text, fixed = TRUE)
  text <- gsub("<", "&lt;", text, fixed = TRUE)
  text <- gsub(">", "&gt;", text, fixed = TRUE)
  gsub('"', "&quot;", text, fixed = TRUE)
}

build_summary_table_html <- function(summary_data) {
  rows <- vapply(
    seq_len(nrow(summary_data)),
    function(i) {
      paste0(
        "<tr><td>",
        escape_html(summary_data[i, 1]),
        "</td><td>",
        summary_data[i, 2],
        "</td></tr>"
      )
    },
    character(1)
  )

  paste0(
    "<table class=\"table table-striped table-sm summary-table\">\n",
    "  <thead><tr><th>",
    escape_html(names(summary_data)[1]),
    "</th><th>",
    escape_html(names(summary_data)[2]),
    "</th></tr></thead>\n",
    "  <tbody>\n    ",
    paste(rows, collapse = "\n    "),
    "\n  </tbody>\n",
    "</table>\n"
  )
}

normalize_inputs <- function(args) {
  if (length(args) == 0) {
    return(list.files("posts", pattern = "^index\\.qmd$", recursive = TRUE, full.names = TRUE))
  }

  files <- c()

  for (arg in args) {
    if (dir.exists(arg)) {
      candidate <- file.path(arg, "index.qmd")
      if (file.exists(candidate)) {
        files <- c(files, candidate)
      } else {
        files <- c(files, list.files(arg, pattern = "^index\\.qmd$", recursive = TRUE, full.names = TRUE))
      }
    } else if (file.exists(arg)) {
      files <- c(files, arg)
    }
  }

  unique(files)
}

find_posts_with_summary <- function(paths) {
  files <- normalize_inputs(paths)

  data.frame(path = files, stringsAsFactors = FALSE) |>
    transform(
      content = I(lapply(path, readLines, warn = FALSE, encoding = "UTF-8"))
    ) |>
    transform(
      uses_summary = vapply(
        content,
        function(lines) {
          any(stringr::str_detect(lines, "get_summary_data\\(")) ||
            any(stringr::str_detect(lines, "summary_table\\.html")) ||
            any(stringr::str_detect(lines, "summary_race_(date|id):"))
        },
        logical(1)
      ),
      race_id = vapply(
        content,
        function(lines) {
          yaml_value <- extract_yaml_race_id(lines)
          if (!is.na(yaml_value) && nzchar(yaml_value)) {
            return(yaml_value)
          }

          code_or_comment <- extract_race_id(lines)
          if (!is.na(code_or_comment) && nzchar(code_or_comment)) {
            return(code_or_comment)
          }

          NA_character_
        },
        character(1)
      ),
      race_date = vapply(
        content,
        function(lines) {
          yaml_value <- extract_yaml_race_date(lines)
          if (!is.na(yaml_value) && nzchar(yaml_value)) {
            return(yaml_value)
          }

          code_or_comment <- extract_race_date(lines)
          if (!is.na(code_or_comment) && nzchar(code_or_comment)) {
            return(code_or_comment)
          }

          NA_character_
        },
        character(1)
      )
    ) |>
    subset(uses_summary, select = c(path, race_id, race_date))
}

write_summary_table <- function(path, race_id, race_date) {
  if (!is.na(race_id) && nzchar(race_id)) {
    summary_data <- get_summary_data_for_race_ids_remote(race_id)
  } else if (!is.na(race_date) && nzchar(race_date)) {
    summary_data <- get_summary_data_remote(race_date)
  } else {
    stop(glue("No race_id or race_date found in {path}"))
  }

  table_path <- file.path(dirname(path), "summary_table.html")
  writeLines(build_summary_table_html(summary_data), table_path, useBytes = TRUE)
  message(glue("Wrote {table_path}"))
}

args <- commandArgs(trailingOnly = TRUE)
posts <- find_posts_with_summary(args)

if (nrow(posts) == 0) {
  message("No posts found.")
  quit(save = "no", status = 0)
}

for (i in seq_len(nrow(posts))) {
  write_summary_table(posts$path[[i]], posts$race_id[[i]], posts$race_date[[i]])
}
