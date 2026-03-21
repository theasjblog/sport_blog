#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(glue)
  library(dplyr)
  library(hms)
  library(stringr)
})

source("inst/aux_functions.R")

jobs <- list(
  list(
    path = "posts/race_reports/2025-04-12_INDOOR_TRIATHLON_PROVINCIAL_CHAMPIONSHIP_-_RACE_3",
    event_dates = c("2025-04-12", "2025-02-22")
  ),
  list(
    path = "posts/race_reports/2025-05-04_INDOOR_TRIATHLON_PROVINCIAL_CHAMPIONSHIP_-_RACE_4",
    event_dates = c("2025-05-04", "2025-04-12", "2025-02-22")
  )
)

for (job in jobs) {
  analysis_data <- fetch_race_analysis_data_remote(job$event_dates)
  cache_dir <- file.path(job$path, "analysis_data")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  write.csv(analysis_data$race_metadata,
            file.path(cache_dir, "race_metadata.csv"),
            row.names = FALSE)
  write.csv(analysis_data$all_races,
            file.path(cache_dir, "all_races.csv"),
            row.names = FALSE)
  write.csv(analysis_data$activities_metadata,
            file.path(cache_dir, "activities_metadata.csv"),
            row.names = FALSE)
  write.csv(analysis_data$activities_data,
            file.path(cache_dir, "activities_data.csv"),
            row.names = FALSE)

  message(glue("Wrote CSV analysis cache in {cache_dir}"))
}
