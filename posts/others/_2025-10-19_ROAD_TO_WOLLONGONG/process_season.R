library(arrow)
library(readr)
library(dplyr)
library(stringr)
library(hms)

all_files <- lapply(list.files('2025_season', full.names = TRUE), function(d){
  readr::read_csv(d, show_col_types = FALSE) |>
    mutate(week_n = as.numeric(substring(basename(d), nchar(basename(d))-5, nchar(basename(d))-4)))
}) |>
  dplyr::bind_rows() |>
  rename(
    activity_type = "Activity Type",
    n_activities = Activities,
    total_distance_km = "Total Distance",
    max_distance_km = "Max Distance",
    moving_time = "Moving Time",
    avg_pace_min_km = "Average Pace",
    avg_hr = "Average Heart Rate",
    avg_run_cadence = "Average Run Cadence",
    avg_bike_cadence = "Average Bike Cadence",
    avg_power = "Average Power",
    vertical_oscillation_cm = "Vertical Oscillation",
    ground_contact_time_ms = "Ground Contact Time"
  ) |>
  mutate(
    total_distance_km = case_when(
      total_distance_km == "--" ~ NA,
      stringr::str_detect(total_distance_km, " km") ~ as.numeric(substring(total_distance_km, 1, nchar(total_distance_km)-3)),
      stringr::str_detect(total_distance_km, " m") ~ as.numeric(substring(total_distance_km, 1, nchar(total_distance_km)-3))/1000,
      .default = NA
    ),
    max_distance_km = case_when(
      max_distance_km == "--" ~ NA,
      stringr::str_detect(max_distance_km, " km") ~ as.numeric(substring(max_distance_km, 1, nchar(max_distance_km)-3)),
      stringr::str_detect(max_distance_km, " m") ~ as.numeric(substring(max_distance_km, 1, nchar(max_distance_km)-3))/1000,
      .default = NA
    ),
    moving_time = gsub(" h:m:s", "", moving_time),
    moving_time = case_when(
      stringr::str_count(moving_time, ":") == 2 & nchar(moving_time) == 8 ~ moving_time,
      stringr::str_count(moving_time, ":") == 2 & nchar(moving_time) == 7 ~ paste0("0", moving_time),
      stringr::str_count(moving_time, ":") == 1 & nchar(moving_time) == 5 ~ paste0("00:", moving_time),
      stringr::str_count(moving_time, ":") == 1 & nchar(moving_time) == 4 ~ paste0("00:0", moving_time),
      stringr::str_count(moving_time, ":") == 0 & nchar(moving_time) == 2 ~ paste0("00:00:", moving_time),
      stringr::str_count(moving_time, ":") == 0 & nchar(moving_time) == 1 ~ paste0("00:00:0", moving_time),
      .default = NA
    ),
    moving_time = hms::as_hms(moving_time),
    avg_pace_min_km = case_when(
      avg_pace_min_km == "--" ~ NA,
      avg_pace_min_km != "--" ~ gsub(" /km", "", avg_pace_min_km),
      .default = NA
    ),
    avg_pace_min_km = if_else(
      is.na(avg_pace_min_km),
      NA,
      as.numeric(sub(":.*", "", avg_pace_min_km)) + as.numeric(sub(".*:", "", avg_pace_min_km)) / 60
    ),
    avg_hr = case_when(
      avg_hr == "--" ~ NA,
      avg_hr != "--" ~ as.numeric(gsub(" bpm", "", avg_hr)),
      .default = NA
    ),
    avg_run_cadence = case_when(
      avg_run_cadence == "--" ~ NA,
      avg_run_cadence != "--" ~ as.numeric(gsub(" spm", "", avg_run_cadence)),
      .default = NA
    ),
    avg_bike_cadence = case_when(
      avg_bike_cadence == "--" ~ NA,
      avg_bike_cadence != "--" ~ as.numeric(gsub(" rpm", "", avg_bike_cadence)),
      .default = NA
    ),
    avg_power = case_when(
      avg_power == "--" ~ NA,
      avg_power != "--" ~ as.numeric(gsub(" W", "", avg_power)),
      .default = NA
    ),
    vertical_oscillation_cm = case_when(
      vertical_oscillation_cm == "--" ~ NA,
      vertical_oscillation_cm != "--" ~ as.numeric(gsub(" cm", "", vertical_oscillation_cm)),
      .default = NA
    ),
    ground_contact_time_ms = case_when(
      ground_contact_time_ms == "--" ~ NA,
      ground_contact_time_ms != "--" ~ as.numeric(gsub(" ms", "", ground_contact_time_ms)),
      .default = NA
    ),
    avg_cadence = ifelse(is.na(avg_bike_cadence), avg_run_cadence, avg_bike_cadence),
    moving_time = as.numeric(moving_time),
    avg_pace_or_speed = case_when(
      activity_type == "Cycling" ~ 60/avg_pace_min_km,
      activity_type == "Swimming" ~ avg_pace_min_km/10,
      .default = avg_pace_min_km
    )
  ) |>
  select(-c(
    "avg_bike_cadence",
    "avg_run_cadence",
    "avg_pace_min_km"
  ))




all_files |>
  filter(activity_type != "Other") |>
  group_by(activity_type) |>
  summarise(less_than_3 = sum(n_activities < 3),
            equal_or_more_than_3 = sum(n_activities >= 3),
            mean_week = mean(n_activities, na.rm = TRUE),
            tot_h = sum(moving_time, na.rm = TRUE)/3600,
            tot_distance = sum(total_distance_km, na.rm = TRUE))

all_files |>
  group_by(week_n) |>
  summarise(tot_sessions = sum(n_activities, na.rm = TRUE))

all_files |>
  summarise(
    tot_sessions = sum(n_activities),
    tot_time = hms::as_hms(sum(moving_time)),
    total_distance = sum(total_distance_km, na.rm = TRUE)
  )

library(ggplot2)
all_files|>
  filter(!(activity_type %in% c("Gym & Fitness Equipment", "Other"))) |>
  ggplot() +
  aes(x = week_n, y = avg_pace_or_speed) +
  geom_smooth(se = TRUE, colour = "#112446") +
  theme_minimal() +
  facet_wrap(vars(activity_type), scales = "free_y")

