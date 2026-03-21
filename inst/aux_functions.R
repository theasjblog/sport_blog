library(arrow)
library(dplyr)
library(hms)
library(stringr)
library(DT)
library(DBI)
library(duckdb)
library(glue)


get_current_post_dir <- function() {
  if (requireNamespace("knitr", quietly = TRUE)) {
    current_input <- knitr::current_input()
    if (!is.null(current_input) && nzchar(current_input)) {
      return(
        normalizePath(
          dirname(current_input),
          winslash = "/",
          mustWork = FALSE
        )
      )
    }
  }

  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}


get_analysis_cache_dir <- function() {
  file.path(get_current_post_dir(), "analysis_data")
}


read_analysis_cache_csv <- function(path) {
  data <- read.csv(
    path,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  if ("EVENT_DATE" %in% names(data)) {
    data$EVENT_DATE <- as.Date(data$EVENT_DATE)
  }

  data
}


fetch_race_analysis_data_remote <- function(event_dates) {
  con <- dbConnect(duckdb::duckdb())
  on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

  dbExecute(con, "INSTALL httpfs; LOAD httpfs;")

  event_dates_sql <- paste0("DATE '", event_dates, "'", collapse = ", ")

  query <- glue("
  SELECT *
  FROM read_parquet('https://storage.googleapis.com/blogs_josa/sport/parquets/races_metadata.parquet')
  WHERE EVENT_DATE IN ({event_dates_sql})
")

  race_metadata <- dbGetQuery(con, query)

  race_ids <- paste0("'", race_metadata$RACE_ID, "'", collapse = ", ")

  query <- glue("
  SELECT *
  FROM read_parquet('https://storage.googleapis.com/blogs_josa/sport/parquets/races_rankings.parquet')
  WHERE RACE_ID IN ({race_ids})
")

  all_races <- dbGetQuery(con, query)

  query <- glue("
  SELECT *
  FROM read_parquet('https://storage.googleapis.com/blogs_josa/sport/parquets/activities_metadata.parquet')
  WHERE RACE_ID IN ({race_ids})
")

  activities_metadata <- dbGetQuery(con, query)

  ids <- paste0("'", activities_metadata$ID, "'", collapse = ", ")

  query <- glue("
  SELECT *
  FROM read_parquet('https://storage.googleapis.com/blogs_josa/sport/parquets/activities_data.parquet')
  WHERE ID IN ({ids})
")

  activities_data <- dbGetQuery(con, query)

  list(
    race_metadata = race_metadata,
    all_races = all_races,
    activities_metadata = activities_metadata,
    activities_data = activities_data
  )
}


get_post_analysis_data <- function(event_dates) {
  analysis_cache_dir <- get_analysis_cache_dir()
  cache_files <- c(
    race_metadata = file.path(analysis_cache_dir, "race_metadata.csv"),
    all_races = file.path(analysis_cache_dir, "all_races.csv"),
    activities_metadata = file.path(analysis_cache_dir, "activities_metadata.csv"),
    activities_data = file.path(analysis_cache_dir, "activities_data.csv")
  )

  if (all(file.exists(cache_files))) {
    return(list(
      race_metadata = read_analysis_cache_csv(cache_files[["race_metadata"]]),
      all_races = read_analysis_cache_csv(cache_files[["all_races"]]),
      activities_metadata = read_analysis_cache_csv(cache_files[["activities_metadata"]]),
      activities_data = read_analysis_cache_csv(cache_files[["activities_data"]])
    ))
  }

  fetch_race_analysis_data_remote(event_dates)
}


build_summary_data <- function(race_metadata, race_rankings) {
  df <- race_metadata |>
    dplyr::left_join(
      race_rankings, by = 'RACE_ID'
    )

  final <- data.frame(
    "Race type" = paste0(df$EVENT_TYPE, " ", df$EVENT_DISTANCE),
    "Final time" = hms::as_hms(df$RESULT_FINAL),
    "Sport 1 time" = hms::as_hms(df$RESULT_SPORT_1),
    "T 1 time" = hms::as_hms(df$RESULT_T_1),
    "Sport 2 time" = hms::as_hms(df$RESULT_SPORT_2),
    "T 2 time" = hms::as_hms(df$RESULT_T_2),
    "Sport 3 Time" = hms::as_hms(df$RESULT_SPORT_3),
    "Overall ranking" = df$RANKING_OVERALL,
    "Gender ranking" = df$RANKING_GENDER,
    "Category ranking" = df$RANKING_CATEGORY
  )

  final <- t(final)

  final <- data.frame(
    col1 = gsub("\\.", " ", rownames(final)),
    col2 = final[,1],
    row.names = NULL
  ) |>
    filter(!is.na(col2)) |>
    filter(stringr::str_detect(col2, "NA/", negate = TRUE)) |>
    filter(col2 != "00:00:00") |>
    mutate(
      col2 = gsub("NA", "-", col2)
    )

  final <- final |>
    dplyr::bind_rows(
      data.frame(
        col1 = 'Activity details',
        col2 = paste0('<a href="https://asjblog.shinyapps.io/single_race_viewer/?_inputs_&selector=%22',
                      gsub(" ", "%20", df$RACE_ID),
                      '%22" target="_blank">Link</a>')
      )
    )

  colnames(final) <- c(substring(df$RACE_ID, 1, 10), final$col2[1])
  final[-1,]
}


get_summary_data_for_race_ids_remote <- function(race_ids) {
  con <- dbConnect(duckdb::duckdb())
  on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

  dbExecute(con, "INSTALL httpfs; LOAD httpfs;")

  race_ids_sql <- paste0("'", race_ids, "'", collapse = ", ")

  query <- glue("
  SELECT *
  FROM read_parquet('https://storage.googleapis.com/blogs_josa/sport/parquets/races_metadata.parquet')
  WHERE RACE_ID IN ({race_ids_sql})
")

  race_metadata <- dbGetQuery(con, query)

  athlete_name <- "ADRIAN JOSEPH"

  query <- glue("
  SELECT *
  FROM read_parquet('https://storage.googleapis.com/blogs_josa/sport/parquets/races_rankings.parquet')
  WHERE RACE_ID IN ({race_ids_sql})
    AND ATHLETE = '{athlete_name}'
")

  race_rankings <- dbGetQuery(con, query)

  build_summary_data(race_metadata, race_rankings)
}


get_summary_data_remote <- function(race_date){
  con <- dbConnect(duckdb::duckdb())
  on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

  dbExecute(con, "INSTALL httpfs; LOAD httpfs;")

  query <- glue("
  SELECT RACE_ID
  FROM read_parquet('https://storage.googleapis.com/blogs_josa/sport/parquets/races_metadata.parquet')
  WHERE EVENT_DATE = DATE '{race_date}'
")

  race_ids <- dbGetQuery(con, query)$RACE_ID
  dbDisconnect(con, shutdown = TRUE)

  get_summary_data_for_race_ids_remote(race_ids)
}


get_summary_data <- function(race_date = NULL, race_id = NULL){
  if (!is.null(race_id) && nzchar(race_id)) {
    return(get_summary_data_for_race_ids_remote(race_id))
  }

  if (!is.null(race_date) && nzchar(race_date)) {
    return(get_summary_data_remote(race_date))
  }

  stop("get_summary_data() requires either race_id or race_date")
}
