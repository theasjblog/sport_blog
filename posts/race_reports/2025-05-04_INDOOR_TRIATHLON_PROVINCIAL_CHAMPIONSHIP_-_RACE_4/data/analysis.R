library(readr)
library(dplyr)
library(ggplot2)
library(FITfileR)

get_record <- function(file_name){
  
  df <- FITfileR::readFitFile(file.path('data', file_name))
  df <- FITfileR::getMessagesByType(df, 'record') |>
    filter(distance > 0) |>
    mutate(date_race =  gsub('_run.fit', '', file_name),
           Power = ifelse(distance > 50 & Power < 50, mean(Power, na.rm = TRUE), Power),
           pace = round(c(0, 1000/(diff(distance)*60)), 2))
  df <- df |>
    mutate(elapsed_time = seq(0, nrow(df) - 1))
  
  return(df)
}

join_records <- function(all_records){
  all_colnames <- lapply(all_records, function(d){colnames(d)})
  sameCols <- Reduce(intersect, all_colnames)
  
  all_records <- lapply(all_records, function(d){
    d <- d|>
      select(all_of(sameCols))
    d
  }) |>
    dplyr::bind_rows()
  
  return(all_records)
}

all_records <- lapply(c('2025-02-22_run.fit',
                        '2025-04-12_run.fit',
                        '2025-05-04_run.fit'), function(d){get_record(d)})

all_records <- join_records(all_records)


summary_run_records <- all_records |>
  group_by(date_race) |>
  summarize(
    mean_power = round(mean(Power), 0),
    mean_heart_rate = round(mean(heart_rate), 0),
    mean_ground_time = round(mean(`Ground Time`), 1),
    mean_cadence = round(mean(cadence), 0),
    split_pace_perc = round(100 * (mean(pace[seq(1, floor(n()/2))]) - mean(pace[seq(floor(n()/2)+1, n())])) / mean(pace[seq(1, floor(n()/2))]), 2),
    split_cadence_perc = round(100 * (mean(cadence[seq(1, floor(n()/2))]) - mean(cadence[seq(floor(n()/2)+1, n())])) / mean(cadence[seq(1, floor(n()/2))]), 2),
    split_power_perc = round(100 * (mean(Power[seq(1, floor(n()/2))]) - mean(Power[seq(floor(n()/2)+1, n())])) / mean(Power[seq(1, floor(n()/2))]), 2),
    split_ground_perc = round(100 * (mean(`Ground Time`[seq(1, floor(n()/2))]) - mean(`Ground Time`[seq(floor(n()/2)+1, n())])) / mean(`Ground Time`[seq(1, floor(n()/2))]), 2),
    distance = round(max(distance)/1000, 2),
    elapsed_time = round(max(elapsed_time)/60, 2),
    pace = round(elapsed_time/distance, 2),
    pace_char = sprintf("%d:%02d", floor(pace), round((pace - floor(pace)) * 60))
  ) |>
  select(all_of(c(
    'distance',
    'pace_char',
    'split_pace_perc',
    'mean_power',
    'split_power_perc',
    'mean_cadence',
    'split_cadence_perc',
    'mean_ground_time',
    'split_ground_perc',
    'mean_heart_rate'
  ))) |>
  rename(
    'Distance [km]' = 'distance',
    'Pace [min/km]' = 'pace_char',
    'Power [W]' = 'mean_power',
    'Heart Rate [bpm]' = 'mean_heart_rate',
    'Ground Time [ms]' = 'mean_ground_time',
    'Cadence [spm]' = 'mean_cadence',
    'Pace Variation [%]' = 'split_pace_perc',
    'Cadence Variation [%]' = 'split_cadence_perc',
    'Power Variation [%]' = 'split_power_perc',
    'Ground Time Variation [%]' = 'split_ground_perc',
  )


get_plot <- function(df, y_data){
  p <- ggplot(df) +
    aes(x = distance, y = .data[[y_data]], colour = date_race) +
    geom_line() +
    scale_color_hue(direction = 1) +
    theme_minimal() +
    ylab(y_data)
  
  return(p)
}

get_plot(all_records, 'Power')
get_plot(all_records, 'heart_rate')
get_plot(all_records, 'cadence')
get_plot(all_records, 'pace')




get_swim_data <- function(file_path, length_factor = 1){
  df <- read_csv(file.path("data", file_path),
                 show_col_types = FALSE) |>
    filter(!is.na(Distance)) |>
    mutate(date_race = gsub('_swim.csv', '', file_path), 
           Distance = Distance * length_factor,
           'Cumulative Time' = as.numeric(`Cumulative Time`/60),
           Time = as.numeric(Time/60),
           'Avg Pace' = as.numeric(`Avg Pace`/(60 * length_factor)),
           cum_distance = cumsum(Distance),
           strokes_per_minute = round(2 * 60 * `Total Strokes` / Time)) |>
    select(all_of(c(
      "Distance", 'cum_distance', "Time", "Cumulative Time", "Avg Pace", 
      "Total Strokes", 'strokes_per_minute', "date_race"   
    )))
  
  return(df)
}

all_swim_records_original <- get_swim_data('2025-02-22_swim.csv', 2) |>
  dplyr::bind_rows(get_swim_data('2025-04-12_swim.csv', 1)) |>
  dplyr::bind_rows(get_swim_data('2025-05-04_swim.csv', 1))



all_swim_records_normalized <-  all_swim_records_original |>
  filter(date_race != '2025-05-04') |>
  dplyr::bind_rows(all_swim_records_original |>
                     filter(date_race == '2025-05-04') |>
                     mutate(group_row  = c(rep(seq(1, 17), each = 2), 18)) |>
                     group_by(group_row) |>
                     summarize(`Total Strokes` = sum(`Total Strokes`),
                               Time = sum(Time),
                               Distance = unique(Distance),
                               cum_distance = max(cum_distance),
                               `Cumulative Time` = max(`Cumulative Time`),
                               `Avg Pace` = round(mean(`Avg Pace`)),
                               strokes_per_minute = mean(strokes_per_minute),
                               date_race = unique(date_race))) |>
  select(-c('group_row'))

summary_swim_records <- all_swim_records_normalized |>
  group_by(date_race) |>
  summarize(
    'Distance [m]' = max(cum_distance),
    'Time [sec/50m]' = round(mean(Time)),
    'Time Variation [%]' = round(100 * (mean(Time[seq(1, floor(n()/2))]) - mean(Time[seq(floor(n()/2)+1, n())])) / mean(Time[seq(1, floor(n()/2))]), 2),
    'Strokes Rate [spm]' = round(mean(strokes_per_minute)),
    'Strokes Rate Variation [%]' = round((100 * (mean(`Total Strokes`[seq(1, floor(n()/2))]) - mean(`Total Strokes`[seq(floor(n()/2)+1, n())])) / mean(`Total Strokes`[seq(1, floor(n()/2))])),2)
  )
  


ggplot(all_swim_records_normalized) +
  aes(x = cum_distance, y = Time, colour = date_race) +
  geom_step() +
  scale_color_hue(direction = 1) +
  theme_minimal()

official_records <- data.frame(
  date_race = c('2025-02-22', '2025-04-12', '2025-05-04'),
  swim_distance = c(825, 775, 887),
  bike_distance = c(9, 10.6, 10.6),
  run_distance = c(15*0.2, 16.5*0.2, 17*0.2),
  swim_points = c(330, 310, 355),
  bike_points = c(450, 530, 530),
  run_points = c(375, 413, 425),
  total_points = c(1150, 1253, 1310)
) |>
  mutate(
    swim_pace = round(100*15/swim_distance, 2),
    swim_pace_char = sprintf("%d:%02d", floor(swim_pace), round((swim_pace - floor(swim_pace)) * 60)),
    bike_speed = round(60*bike_distance/15, 2),
    run_pace = round(15/run_distance, 2),
    run_pace_char = sprintf("%d:%02d", floor(run_pace), round((run_pace - floor(run_pace)) * 60))
  ) |>
  select(all_of(c(
    'date_race',
    'swim_distance',
    'swim_pace_char',
    'swim_points',
    'bike_distance',
    'bike_speed',
    'bike_points',
    'run_distance',
    'run_pace_char',
    'run_points',
    'total_points'
  )))






