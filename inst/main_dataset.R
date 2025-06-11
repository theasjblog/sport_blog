df <- data.frame(
  
  # 'event_name' = NA,
  # 'event_date' = NA,
  # 'event_type' = NA,
  # 'event_distance' = NA,
  'participants_overall' = NA,
  'participants_gender' = NA,
  'participants_category' = NA,
  'ranking_overall' = NA,
  'ranking_gender' = NA,
  'ranking_category' = NA,
  
  'time_total' = NA,
  'time_sport_1' = NA,
  'time_sport_2' = NA,
  'time_sport_3' = NA,
  'time_t_1' = NA,
  'time_t_2' = NA,
  
  'ranking_overall_sport_1' = NA,
  'ranking_overall_sport_2' = NA,
  'ranking_overall_sport_3' = NA,
  'ranking_overall_t_1' = NA,
  'ranking_overall_t_2' = NA,
  
  'ranking_category_sport_1' = NA,
  'ranking_category_sport_2' = NA,
  'ranking_category_sport_3' = NA,
  'ranking_category_t_1' = NA,
  'ranking_category_t_2' = NA,
  
  'ranking_gender_sport_1' = NA,
  'ranking_gender_sport_2' = NA,
  'ranking_gender_sport_3' = NA,
  'ranking_gender_t_1' = NA,
  'ranking_gender_t_2' = NA,
  
  'ranking_overall_after_sport_2' = NA,
  'ranking_overall_after_t_1' = NA,
  'ranking_overall_after_t_2' = NA,
  
  'ranking_category_after_sport_2' = NA,
  'ranking_category_after_t_1' = NA,
  'ranking_category_after_t_2' = NA,
  
  'ranking_gender_after_sport_2' = NA,
  'ranking_gender_after_t_1' = NA,
  'ranking_gender_after_t_2' = NA
  
)

all_files <- list.files('posts', full.names = TRUE, pattern = 'data.json', recursive = TRUE)

all_res <- lapply(all_files, function(d){
  print(d)
  d <- jsonlite::fromJSON(d)
  return(
    data.frame(
      event_name = ifelse(is.null(d$race), NA, d$race),
      event_date = as.Date(d$date),
      event_type = ifelse(is.null(d$type), NA, d$type),
      event_distance = ifelse(is.null(d$distance), NA, d$distance)
    )
  )
}) |>
  dplyr::bind_rows()


all_res <- all_res |>
  dplyr::bind_cols(df)



arrow::write_parquet(all_res, sink = 'main_dataset.parquet')
all_res <- arrow::read_parquet(file = 'main_dataset.parquet')


#######################
#######################
#######################
library(arrow)
library(dplyr)
library(jsonlite)

get_idx <- function(json_path, all_res){
  jj <- jsonlite::fromJSON(json_path)
  print(jj)
  idx <- which(all_res$event_name == jj$race & all_res$event_date == as.Date(jj$date))
  
  return(idx)
}



all_res <- arrow::read_parquet('main_dataset.parquet')
json_path <- file.choose()
parquet_path <- list.files(dirname(json_path), pattern = '.parquet', full.names = TRUE)
idx <- get_idx(json_path, all_res)

details <- arrow::read_parquet(parquet_path) |>
  mutate(names = trimws(toupper(names))) |>
  filter(
    as.numeric(time_hms) != 0
  )

all_res$ranking_overall[idx] <- details$overall_ranking[details$names == "ADRIAN JOSEPH"]
all_res$ranking_gender[idx] <- details$gender_ranking[details$names == "ADRIAN JOSEPH"]
all_res$ranking_category[idx] <- details$category_ranking[details$names == "ADRIAN JOSEPH"]

all_res$participants_overall[idx] <- nrow(details)

all_res$time_total[idx] <- as_hms("01:51:54")
all_res$time_total[idx] <- details$time_hms[details$names == "ADRIAN JOSEPH"]

arrow::write_parquet(all_res, sink = 'main_dataset.parquet')





overall <- details |>
  arrange(time_hms)

gender <- details |>
  filter(gender == details$gender[details$names == "ADRIAN JOSEPH"]) |>
  arrange(time_hms)

category <- details |>
  filter(category == details$category[details$names == "ADRIAN JOSEPH"]) |>
  arrange(time_hms)



overall_sport_1 <- details |>
  arrange(sport_1_time_hms)
overall_sport_2 <- details |>
  arrange(sport_2_time_hms)
overall_sport_3 <- details |>
  arrange(sport_3_time_hms)
overall_t_1 <- details |>
  arrange(t_1_time_hms)
overall_t_2 <- details |>
  arrange(t_2_time_hms)


gender_sport_1 <- gender |>
  arrange(sport_1_time_hms)
gender_sport_2 <- gender |>
  arrange(sport_2_time_hms)
gender_sport_3 <- gender |>
  arrange(sport_3_time_hms)
gender_t_1 <- gender |>
  arrange(t_1_time_hms)
gender_t_2 <- gender |>
  arrange(t_2_time_hms)

category_sport_1 <- category |>
  arrange(sport_1_time_hms)
category_sport_2 <- category |>
  arrange(sport_2_time_hms)
category_sport_3 <- category |>
  arrange(sport_3_time_hms)
category_t_1 <- category |>
  arrange(t_1_time_hms)
category_t_2 <- category |>
  arrange(t_2_time_hms)

overall_after_t_1 <- overall |>
  rowwise() |>
  mutate(tot_time = sum(sport_1_time_hms + t_1_time_hms)) |>
  ungroup() |>
  arrange(tot_time)
overall_after_sport_2 <- overall |>
  rowwise() |>
  mutate(tot_time = sum(sport_1_time_hms + t_1_time_hms + sport_2_time_hms)) |>
  ungroup() |>
  arrange(tot_time)
overall_after_t_2 <- overall |>
  rowwise() |>
  mutate(tot_time = sum(sport_1_time_hms + t_2_time_hms + sport_2_time_hms + t_2_time_hms)) |>
  ungroup() |>
  arrange(tot_time)


gender_after_t_1 <- gender |>
  rowwise() |>
  mutate(tot_time = sum(sport_1_time_hms + t_1_time_hms)) |>
  ungroup() |>
  arrange(tot_time)
gender_after_sport_2 <- gender |>
  rowwise() |>
  mutate(tot_time = sum(sport_1_time_hms + t_1_time_hms + sport_2_time_hms)) |>
  ungroup() |>
  arrange(tot_time)
gender_after_t_2 <- gender |>
  rowwise() |>
  mutate(tot_time = sum(sport_1_time_hms + t_2_time_hms + sport_2_time_hms + t_2_time_hms)) |>
  ungroup() |>
  arrange(tot_time)

category_after_t_1 <- category |>
  rowwise() |>
  mutate(tot_time = sum(sport_1_time_hms + t_1_time_hms)) |>
  ungroup() |>
  arrange(tot_time)
category_after_sport_2 <- category |>
  rowwise() |>
  mutate(tot_time = sum(sport_1_time_hms + t_1_time_hms + sport_2_time_hms)) |>
  ungroup() |>
  arrange(tot_time)
category_after_t_2 <- category |>
  rowwise() |>
  mutate(tot_time = sum(sport_1_time_hms + t_2_time_hms + sport_2_time_hms + t_2_time_hms)) |>
  ungroup() |>
  arrange(tot_time)


all_res$ranking_overall[idx] <- which(overall$names == "ADRIAN JOSEPH")
all_res$ranking_gender[idx] <- which(gender$names == "ADRIAN JOSEPH")
all_res$ranking_category[idx] <- which(category$names == "ADRIAN JOSEPH")

all_res$participants_overall[idx] <- nrow(overall)
all_res$participants_gender[idx] <- nrow(gender)
all_res$participants_category[idx] <- nrow(category)


all_res$time_total[idx] <- overall$time_hms[overall$names == "ADRIAN JOSEPH"]
all_res$time_sport_1[idx] <- overall$sport_1_time_hms[overall$names == "ADRIAN JOSEPH"]
all_res$time_sport_2[idx] <- overall$sport_2_time_hms[overall$names == "ADRIAN JOSEPH"]
all_res$time_sport_3[idx] <- overall$sport_3_time_hms[overall$names == "ADRIAN JOSEPH"]
all_res$time_t_1[idx] <- overall$t_1_time_hms[overall$names == "ADRIAN JOSEPH"]
all_res$time_t_2[idx] <- overall$t_2_time_hms[overall$names == "ADRIAN JOSEPH"]

all_res$ranking_overall_sport_1[idx] <- which(overall_sport_1$names == "ADRIAN JOSEPH")
all_res$ranking_overall_sport_2[idx] <- which(overall_sport_2$names == "ADRIAN JOSEPH")
all_res$ranking_overall_sport_3[idx] <- which(overall_sport_3$names == "ADRIAN JOSEPH")
all_res$ranking_overall_t_1[idx] <- which(overall_t_1$names == "ADRIAN JOSEPH")
all_res$ranking_overall_t_2[idx] <- which(overall_t_2$names == "ADRIAN JOSEPH")

all_res$ranking_gender_sport_1[idx] <- which(gender_sport_1$names == "ADRIAN JOSEPH")
all_res$ranking_gender_sport_2[idx] <- which(gender_sport_2$names == "ADRIAN JOSEPH")
all_res$ranking_gender_sport_3[idx] <- which(gender_sport_3$names == "ADRIAN JOSEPH")
all_res$ranking_gender_t_1[idx] <- which(gender_t_1$names == "ADRIAN JOSEPH")
all_res$ranking_gender_t_2[idx] <- which(gender_t_2$names == "ADRIAN JOSEPH")

all_res$ranking_category_sport_1[idx] <- which(category_sport_1$names == "ADRIAN JOSEPH")
all_res$ranking_category_sport_2[idx] <- which(category_sport_2$names == "ADRIAN JOSEPH")
all_res$ranking_category_sport_3[idx] <- which(category_sport_3$names == "ADRIAN JOSEPH")
all_res$ranking_category_t_1[idx] <- which(category_t_1$names == "ADRIAN JOSEPH")
all_res$ranking_category_t_2[idx] <- which(category_t_2$names == "ADRIAN JOSEPH")


all_res$ranking_overall_after_sport_2[idx] <- which(overall_after_sport_2$names == "ADRIAN JOSEPH")
all_res$ranking_overall_after_t_1[idx] <- which(overall_after_t_1$names == "ADRIAN JOSEPH")
all_res$ranking_overall_after_t_2[idx] <- which(overall_after_t_2$names == "ADRIAN JOSEPH")

all_res$ranking_gender_after_sport_2[idx] <- which(gender_after_sport_2$names == "ADRIAN JOSEPH")
all_res$ranking_gender_after_t_1[idx] <- which(gender_after_t_1$names == "ADRIAN JOSEPH")
all_res$ranking_gender_after_t_2[idx] <- which(gender_after_t_2$names == "ADRIAN JOSEPH")

all_res$ranking_category_after_sport_2[idx] <- which(category_after_sport_2$names == "ADRIAN JOSEPH")
all_res$ranking_category_after_t_1[idx] <- which(category_after_t_1$names == "ADRIAN JOSEPH")
all_res$ranking_category_after_t_2[idx] <- which(category_after_t_2$names == "ADRIAN JOSEPH")

arrow::write_parquet(all_res, sink = 'main_dataset.parquet')


