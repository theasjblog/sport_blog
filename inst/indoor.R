library(hms)
library(dplyr)
library(arrow)
library(stringr)
library(tidyr)
race_name <- "2025-05-04_INDOOR_TRIATHLON_SERIES_RACE_4"
all_data <- readLines(file.path(
  'posts', race_name, 'data.txt'
))
ll <- length(all_data)
to_hms_from_min_sec <- function(time_str) {
  parts <- str_split_fixed(time_str, ":", 3)
  hours <- as.numeric(parts[, 1])
  minutes <- as.numeric(parts[, 2])
  seconds <- as.numeric(parts[, 3])
  
  total_seconds <- hours *3600 + minutes * 60 + seconds
  as_hms(total_seconds)
}

len_split <- length(stringr::str_split(all_data[1], ' ')[[1]])
all_data <- as.data.frame(all_data) |>
  separate(all_data, 
           into = paste0("V", 1:len_split), 
           sep = " ", remove = FALSE) |>
  rename(
    'gender' = 'V3',
    'swim_laps' = 'V5',
    'bike_distance' = 'V6',
    'run_laps' = 'V7',
    'swim_points' = 'V8',
    'bike_points' = 'V9',
    'run_points' = 'V10'
  ) |>
  mutate(
    category = paste0(gender, V4),
    swim_laps = as.numeric(swim_laps),
    bike_distance = as.numeric(bike_distance),
    run_laps = as.numeric(run_laps),
    swim_points = as.numeric(swim_points),
    bike_points = as.numeric(bike_points),
    run_points = as.numeric(run_points)
  ) |>
  rowwise() |>
  mutate(
    total_points = sum(swim_points, bike_points, run_points, na.rm = TRUE),
    names = trimws(toupper(paste0(V1, " ", V2)), 'both')
    ) |>
  ungroup() |>
  select(all_of(c(
    'names',
    'category',
    'gender',
    'swim_laps',
    'bike_distance',
    'run_laps',
    'swim_points',
    'bike_points',
    'run_points',
    'total_points'
  ))) |>
  arrange(desc(total_points)) |>
  mutate(
    overall_ranking = seq(1, ll)
  ) |>
  arrange(desc(swim_points)) |>
  mutate(
    overall_ranking_swim = seq(1, ll)
  ) |>
  arrange(desc(bike_points)) |>
  mutate(
    overall_ranking_bike = seq(1, ll)
  ) |>
  arrange(desc(run_points)) |>
  mutate(
    overall_ranking_run = seq(1, ll)
  )
  
all_data <- lapply(split(all_data, all_data$gender), function(d){
  all_n <- nrow(d)
  d <- d |>
    arrange(desc(total_points)) |>
    mutate(
      gender_ranking = seq(1, all_n)
    ) |>
    arrange(desc(swim_points)) |>
    mutate(
      gender_ranking_swim = seq(1, all_n)
    ) |>
    arrange(desc(bike_points)) |>
    mutate(
      gender_ranking_bike = seq(1, all_n)
    ) |>
    arrange(desc(run_points)) |>
    mutate(
      gender_ranking_run = seq(1, all_n)
    )
  d
}) |>
  dplyr::bind_rows()

all_data <- lapply(split(all_data, all_data$category), function(d){
  all_n <- nrow(d)
  d <- d |>
    arrange(desc(total_points)) |>
    mutate(
      category_ranking = seq(1, all_n)
    ) |>
    arrange(desc(swim_points)) |>
    mutate(
      category_ranking_swim = seq(1, all_n)
    ) |>
    arrange(desc(bike_points)) |>
    mutate(
      category_ranking_bike = seq(1, all_n)
    ) |>
    arrange(desc(run_points)) |>
    mutate(
      category_ranking_run = seq(1, all_n)
    )
  d
}) |>
  dplyr::bind_rows()

arrow::write_parquet(all_data, sink = file.path(
  'posts', race_name, 'full_results.parquet'
))
