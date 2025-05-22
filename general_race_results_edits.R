library(hms)
library(dplyr)
library(arrow)
library(stringr)
race_name <- "2011-10-13_BUPA_GREAT_YORKSHIRE_RUN"
all_data <- readLines(file.path(
  'posts', race_name, 'tt.txt'
))


all_data <- data.frame(
  names = trimws(toupper(gsub('\\\t', ' ', all_data[seq(7,1064,7)])), 'both'),
  overall_ranking = as.numeric(all_data[seq(1,1064, 7)]),
  time_hms = hms::as_hms(all_data[seq(3,1064,7)]),
  bib = as.numeric(all_data[seq(5,1064,7)])
)
#all_data <- all_data[seq(2, length(all_data))]
all_data <- lapply(all_data, function(d){
  d <- stringr::str_split(d, '\t')[[1]]
  cat_pos <- d[3]
  if(nchar(cat_pos)>0){
    cat <- stringr::str_split(cat_pos, '/')[[1]][1]
    gender <- substring(cat,1,1)
    cat_pos <- as.numeric(stringr::str_split(cat_pos, '/')[[1]][2])
  } else {
    cat <- NA
    cat_pos <- NA
    gender <- NA
  }
  
  return(data_frame(
    names = toupper(d[2]),
    club = toupper(d[4]),
    category_ranking = cat_pos,
    category = toupper(cat),
    gender = toupper(gender),
    time_hms = hms::as_hms(gsub('\\.', ':', d[5]))
  ))
}) |>
  dplyr::bind_rows() |>
  arrange(time_hms) |>
  mutate(overall_ranking = seq(1, 4576))

all_data <- lapply(split(all_data, all_data$gender), function(d){
  all_n <- nrow(d)
  d <- d |>
    arrange(final_time) |>
    mutate(gender_ranking = seq(1, all_n))
  d
}) |>
  dplyr::bind_rows()


all_data <- lapply(split(all_data, all_data$category), function(d){
  all_n <- nrow(d)
  d <- d |>
    arrange(final_time) |>
    mutate(category_ranking = seq(1, all_n))
  d
}) |>
  dplyr::bind_rows() |>
  rename(
    'time_hms' = 'final_time'
  )


all_data <- data.frame(
  names = "ADRIAN JOSEPH",
  bib = 1704,
  time_hms = hms::as_hms("00:48:46"),
  overall_ranking = 877,
  gender_ranking = 809,
  category_ranking = 447
)
arrow::write_parquet(all_data, sink = file.path(
  'posts', race_name, 'full_results.parquet'
))
