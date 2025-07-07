library(arrow)
library(dplyr)
library(hms)
library(stringr)
library(DT)

get_summary_data <- function(df){
  final <- data.frame(
    "Race type" = paste0(df$event_type, " ", df$event_distance),
    "Final time" = hms::as_hms(df$result_total),
    "Sport 1 time" = hms::as_hms(df$result_sport_1),
    "T 1 time" = hms::as_hms(df$result_t_1),
    "Sport 2 time" = hms::as_hms(df$result_sport_2),
    "T 2 time" = hms::as_hms(df$result_t_2),
    "Sport 3 Time" = hms::as_hms(df$result_sport_3),
    
    
    "Overall ranking" = paste0(df$ranking_overall, "/", df$participants_overall),
    "Gender ranking" = paste0(df$ranking_gender, "/", df$participants_gender),
    "Category ranking" = paste0(df$ranking_category, "/", df$participants_category),
    
    "Recording device" = df$watch
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
        col2 = paste0('<a href=https://asjblog.shinyapps.io/race_shiny/?_inputs_&selector=%22',
                      df$event_date,
                      "%20-%20",
                      gsub(' ', "%20", df$event_name),
                      "%20-%20",
                      gsub(' ', "%20", df$event_type),
                      '%22 target="_blank">',
                      "Details", 
                      '</a>')
      )
    )
  
  colnames(final) <- c(df$event_name, final$col2[1])
  final <- final[-1,]
  
  
  
  return(final)
}

