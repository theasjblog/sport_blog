library(arrow)
library(dplyr)
library(hms)
library(stringr)
library(DT)
library(DBI)
library(duckdb)
library(glue)


get_summary_data <- function(race_date){
  # Connect to DuckDB
  con <- dbConnect(duckdb::duckdb())
  
  # Enable HTTPFS extension to access remote files
  dbExecute(con, "INSTALL httpfs; LOAD httpfs;")
  
  query <- glue("
  SELECT *
  FROM read_parquet('https://storage.googleapis.com/blogs_josa/sport/parquets/races_metadata.parquet')
  WHERE EVENT_DATE = DATE '{race_date}'
")
  
  race_metadata <- dbGetQuery(con, query)
  
  
  race_ids <- race_metadata$RACE_ID
  race_ids_sql <- paste0("'", race_ids, "'", collapse = ", ")
  
  athlete_name <- "ADRIAN JOSEPH"
  
  query <- glue("
  SELECT *
  FROM read_parquet('https://storage.googleapis.com/blogs_josa/sport/parquets/races_rankings.parquet')
  WHERE RACE_ID IN ({race_ids_sql})
    AND ATHLETE = '{athlete_name}'
")
  
  df <- dbGetQuery(con, query)
  
  
  df <- df |>
    dplyr::left_join(
      race_metadata, by = 'RACE_ID'
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
                      '%22" target="_blank">Link</a>',
                      '</a>')
      )
    )
  
  colnames(final) <- c(substring(df$RACE_ID, 1, 10), final$col2[1])
  final <- final[-1,]
  
  return(final)
  
}
