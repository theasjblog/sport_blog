library(dplyr)
all_races <- arrow::read_parquet('https://storage.googleapis.com/blogs_josa/sport/parquets/all_results.parquet') |>
  filter(event_date %in% c('2025-04-12', '2025-05-04', '2025-02-22'))

race_1 <- all_races |>
  filter(event_date  == '2025-02-22')

race_3 <- all_races |>
  filter(event_date  == '2025-04-12')

race_4 <- all_races |>
  filter(event_date  == '2025-05-04')

leaders <- race_4 |>
  arrange(desc(result_final)) |>
  mutate(overall_ranking = seq(1, nrow(race_4))) |>
  group_by(category) |>
  arrange(desc(result_final)) |>
  slice_head(n=1) |>
  ungroup() |>
  select(all_of(c(
    'names', 'category', 'result_final', 'overall_ranking'
  ))) |>
  mutate(ag_ranking = 1) |>
  rename(
    'Name' = 'names',
    'Age Group' = 'category',
    'Total Points' = 'result_final',
    'Age Group Ranking' = 'ag_ranking',
    'Overall Ranking' = 'overall_ranking'
  )
  

segments <- all_races |>
  filter(category == unique(category[names == "ADRIAN JOSEPH"])) |>
  group_by(event_date) |>
  arrange(desc(result_sport_1)) |>
  mutate(ag_ranking_swim = seq(1, n())) |>
  arrange(desc(result_sport_2)) |>
  mutate(ag_ranking_bike = seq(1, n())) |>
  arrange(desc(result_sport_3)) |>
  mutate(ag_ranking_run = seq(1, n())) |>
  arrange(desc(result_final)) |>
  mutate(ag_ranking = seq(1, n())) |>
  ungroup() |>
  filter(names == 'ADRIAN JOSEPH') |>
  arrange(event_date) |>
  mutate(event_date = c(1, 3, 4)) |>
  select(all_of(c(
    'names', 'event_date', 'ag_ranking_swim', 'ag_ranking_bike', 'ag_ranking_run', 'ag_ranking'
  ))) |>
  rename(
    'Name' = 'names',
    'Race [#]' = 'event_date',
    'Swim Ranking' = 'ag_ranking_swim',
    'Bike Ranking' = 'ag_ranking_bike',
    'Run Ranking' = 'ag_ranking_run',
    'Final Ranking' = 'ag_ranking'
  )
    

all_races <- arrow::read_parquet('https://storage.googleapis.com/blogs_josa/sport/parquets/record/2025-02-22.parquet') |>
  dplyr::bind_rows(arrow::read_parquet('https://storage.googleapis.com/blogs_josa/sport/parquets/record/2025-04-12.parquet')) |>
  dplyr::bind_rows(arrow::read_parquet('https://storage.googleapis.com/blogs_josa/sport/parquets/record/2025-05-04.parquet'))


swim_summary <- all_races |>
  filter(sport == 'swim') |>
  mutate(distance = ifelse(as.Date(timestamp) == as.Date('2025-02-22'), distance * 2, distance),
         event_date = as.Date(timestamp)) |>
  group_by(event_date) |>
  summarise(distance = max(distance, na.rm = TRUE)) |>
  ungroup() |>
  arrange(event_date) |>
  mutate(event_date = c(1,3,4)) |>
  rename(
    'Race [#]' = 'event_date',
    'Distance [m]' = 'distance'
    )

all_runs <- all_races |>
  filter(sport == 'run') |>
  group_by(as.Date(timestamp)) |>
  arrange(timestamp) |>
  mutate(elapsed_time = as.numeric((timestamp - min(timestamp))/60)) |>
  ungroup()

convert_to_pace <- function(decimal_time){
  minutes <- floor(decimal_time)
  seconds <- round((decimal_time - minutes)*60)
  
  sprintf("%d:%02d", minutes, seconds)
}

summary_runs <- all_runs |>
  mutate(pace_run = 1000/(60*speed),
         event_date = as.Date(timestamp)) |>
  group_by(event_date) |>
  arrange(timestamp) |>
  summarise(tot_distance = round(max(distance)/1000, 2),
            avg_pace = mean(pace_run),
            pace_var = 100* (mean(pace_run[floor(n()/2)+1:n()], na.rm = TRUE) - mean(pace_run[1:floor(n()/2)])) / avg_pace,
            avg_power = mean(Power, na.rm = TRUE),
            power_var = 100* (mean(Power[floor(n()/2)+1:n()], na.rm = TRUE) - mean(Power[1:floor(n()/2)])) / avg_power,
            avg_cadence = mean(cadence, na.rm = TRUE),
            cadence_var = 100* (mean(cadence[floor(n()/2)+1:n()], na.rm = TRUE) - mean(cadence[1:floor(n()/2)])) / avg_cadence,
            avg_hr = mean(heart_rate, na.rm = TRUE),
            hr_var = 100* (mean(heart_rate[floor(n()/2)+1:n()], na.rm = TRUE) - mean(heart_rate[1:floor(n()/2)])) / avg_hr
            ) |>
  mutate(avg_page = convert_to_pace(avg_pace)) |>
  ungroup() |>
  arrange(event_date) |>
  mutate(event_date = c(1,3,4)) |>
  rename(
    'Race [#]' = 'event_date',
    'Distance [Km]' = 'tot_distance',
    'Pace [min/Km]' = 'avg_pace',
    'Pace Variation [%]' = 'pace_var',
    'Power [W]' = 'avg_power',
    'Power Variation [%]' = 'power_var',
    'Cadence [spm]' = 'avg_cadence',
    'Cadence Variation [%]' = 'cadence_var',
    'Heart rate [bpm]' = 'avg_hr',
    'Heart rate Vartiation [%]' = 'hr_var'
  )



summary_runs_plot <- all_runs |>
  mutate(Pace = 1000/(60*speed),
         event_date = as.character(as.Date(timestamp))) |>
  select(all_of(c(
    'event_date',
    'Power',
    'cadence',
    'heart_rate',
    'elapsed_time',
    'Pace'
  ))) |>
  tidyr::pivot_longer(
    cols = c(Power, cadence, heart_rate, Pace),
    values_to = 'value'
  )

library(ggplot2)
ggplot(summary_runs_plot) +
  aes(x = elapsed_time, y = value, colour = event_date) +
  geom_line(alpha = 0.5) +
  geom_smooth(method = "loess", span = 0.1, se = FALSE) +
  scale_color_hue(direction = 1) +
  labs(
    x = "Elapsed time [min]",
    y = "",
    title = "Run metrics for the three races"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20L,
                              face = "bold"),
    axis.title.x = element_text(size = 15L,
                                face = "bold"),
    axis.text.y = element_text(size = 15L),
    axis.text.x = element_text(size = 15L),
    legend.text = element_text(size = 15L),
    legend.title = element_text(face = "bold",
                                size = 15L)
  ) +
  facet_wrap(vars(name), scales = 'free_y')



