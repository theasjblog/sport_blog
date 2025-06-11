library(hms)
library(dplyr)
library(arrow)
library(stringr)
library(tidyr)
race_name <- "2024-09-29_OAKVILLE_10K"
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

# len_split <- length(stringr::str_split(all_data[1], '\\\t')[[1]])
# all_data <- as.data.frame(all_data) |>
#   separate(all_data, 
#            into = paste0("V", 1:len_split), 
#            sep = "\t", remove = FALSE) |>
#   rename(
#     'names' = 'V4',
#     'bib' = 'V2',
#     'overall_ranking' = 'V4',
#     'gender_ranking' = 'V6',
#     'catergory_ranking' = 'V7'
#     
#   ) |>
#   mutate(
#     names = trimws(toupper(names)),
#     n_colons = str_count(V8, ":"),
#     time_norm = if_else(n_colons == 1, paste0("00:", V8), V8),
#     time_hms = to_hms_from_min_sec(time_norm)
#   ) |>
#   select(all_of(c(
#     'names',
#     'bib',
#     'overall_ranking',
#     'gender_ranking',
#     'catergory_ranking',
#     'time_hms'
#   )))


len_split <- length(stringr::str_split(all_data[1], '\\\t')[[1]])
all_data <- as.data.frame(all_data) |>
  separate(all_data, 
           into = paste0("V", 1:len_split), 
           sep = "\t", remove = FALSE) |>
  rename(
    'names' = 'V3',
    'gender' = 'V4',
    'category' = 'V10'
  ) |>
    mutate(
      category = case_when(
        names == 'Ryan Tyrrell' ~ 'M20 - 29',
        names %in% c('Chris Angelatos', 'Kadeem McPherson') ~ 'M30 - 39',
        names == 'Maria Weber' ~ 'F40 - 49',
        names == 'Amanda Kulikauskas' ~ 'F30 - 39',
        names == 'Tina Perilli' ~ 'F50 - 59',
        .default = category
      ),
    names = trimws(toupper(names), 'both'),
    # gender = case_when(
    #   category == 'Top3Male' ~ 'M',
    #   category == 'Top3Female' ~ 'F',
    #   .default = category
    # ),
    #gender = substr(category,2,2),
    
    # category = paste0(gender, V7),
    n_colons = str_count(V8, ":"),
    time_norm = if_else(n_colons == 1, paste0("00:", V8), V8),
    time_hms = to_hms_from_min_sec(time_norm)#,
    
    # n_colons = str_count(V7, ":"),
    # time_norm = if_else(n_colons == 1, paste0("00:", V7), V7),
    # sport_1_time_hms = to_hms_from_min_sec(time_norm),
    # 
    # n_colons = str_count(V8, ":"),
    # time_norm = if_else(n_colons == 1, paste0("00:", V8), V8),
    # t_1_time_hms = to_hms_from_min_sec(time_norm),
    # 
    # n_colons = str_count(V9, ":"),
    # time_norm = if_else(n_colons == 1, paste0("00:", V9), V9),
    # sport_2_time_hms = to_hms_from_min_sec(time_norm),
    # 
    # n_colons = str_count(V10, ":"),
    # time_norm = if_else(n_colons == 1, paste0("00:", V10), V10),
    # t_2_time_hms = to_hms_from_min_sec(time_norm),
    # 
    # n_colons = str_count(V11, ":"),
    # time_norm = if_else(n_colons == 1, paste0("00:", V11), V11),
    # sport_3_time_hms = to_hms_from_min_sec(time_norm)
  ) |>
  select(all_of(c(
    'names',
    'time_hms',
    'category',
    'gender'#,
    # 'sport_1_time_hms',
    # 't_1_time_hms',
    # 'sport_2_time_hms',
    # 't_2_time_hms',
    # 'sport_3_time_hms'
  ))) |>
  arrange(time_hms) |>
  mutate(
    overall_ranking = seq(1, ll)
  )# |>
  # arrange(sport_1_time_hms) |>
  # mutate(
  #   sport_1_ranking = seq(1, ll)
  # ) |>
  # arrange(t_1_time_hms) |>
  # mutate(
  #   t_1_ranking = seq(1, ll)
  # ) |>
  # arrange(sport_2_time_hms) |>
  # mutate(
  #   sport_2_ranking = seq(1, ll)
  # ) |>
  # arrange(t_2_time_hms) |>
  # mutate(
  #   t_2_ranking = seq(1, ll)
  # ) |>
  # arrange(sport_3_time_hms) |>
  # mutate(
  #   sport_3_ranking = seq(1, ll)
  # )
  
all_data <- as.data.frame(all_data) |>
  separate(all_data, 
           into = paste0("V", 1:len_split), 
           sep = "\t", remove = FALSE) |>
  rename(
    'names' = 'V3',
    'category' = 'V5',
  ) |>
  mutate(
    names = trimws(toupper(names), 'both'),
    # gender = case_when(
    #   category == 'Top3Male' ~ 'M',
    #   category == 'Top3Female' ~ 'F',
    #   .default = category
    # ),
    gender = substr(category, 2, 2),
    # n_colons = str_count(V3, ":"),
    # time_norm = if_else(n_colons == 1, paste0("00:", V3), V3),
    # time_hms = to_hms_from_min_sec(time_norm),
    
    n_colons = str_count(V7, ":"),
    time_norm = if_else(n_colons == 1, paste0("00:", V7), V7),
    sport_1_time_hms = to_hms_from_min_sec(time_norm),

    n_colons = str_count(V8, ":"),
    time_norm = if_else(n_colons == 1, paste0("00:", V8), V8),
    t_1_time_hms = to_hms_from_min_sec(time_norm),

    n_colons = str_count(V9, ":"),
    time_norm = if_else(n_colons == 1, paste0("00:", V9), V9),
    sport_2_time_hms = to_hms_from_min_sec(time_norm),

    n_colons = str_count(V10, ":"),
    time_norm = if_else(n_colons == 1, paste0("00:", V10), V10),
    t_2_time_hms = to_hms_from_min_sec(time_norm),

    n_colons = str_count(V11, ":"),
    time_norm = if_else(n_colons == 1, paste0("00:", V11), V11),
    sport_3_time_hms = to_hms_from_min_sec(time_norm)
  ) |>
  rowwise() |>
  mutate(time_hms = hms::as_hms(sum(c(sport_1_time_hms, sport_2_time_hms, sport_3_time_hms, t_1_time_hms, t_2_time_hms), na.rm = TRUE))) |>
  ungroup() |>
  select(all_of(c(
    'names',
    'time_hms',
    'category',
    'gender',
    'sport_1_time_hms',
    'sport_2_time_hms',
    'sport_3_time_hms',
    't_1_time_hms',
    't_2_time_hms'
  ))) |>
  arrange(time_hms) |>
  mutate(
    overall_ranking = seq(1, ll)
  ) #|>
#arrange(sport_1_time_hms) |>
# mutate(
#   sport_1_ranking = seq(1, ll)
# ) |>
# arrange(t_1_time_hms) |>
# mutate(
#   t_1_ranking = seq(1, ll)
# ) |>
# arrange(sport_2_time_hms) |>
# mutate(
#   sport_2_ranking = seq(1, ll)
# ) |>
# arrange(t_2_time_hms) |>
# mutate(
#   t_2_ranking = seq(1, ll)
# ) |>
# arrange(sport_3_time_hms) |>
# mutate(
#   sport_3_ranking = seq(1, ll)
# )



all_data <- all_data[all_data!= "NULL"]
ll <- length(all_data)


all_data_df <- data.frame(
  names = trimws(toupper(gsub('\\\t', ' ', all_data[seq(3,ll,4)])), 'both'),
  #overall_ranking = as.numeric(all_data[seq(1,1064, 7)]),
  #time_hms = hms::as_hms(all_data[seq(3,1064,7)]),
  bib = as.numeric(all_data[seq(2,ll,4)]),
  mixed = all_data[seq(4,ll,4)]
) |>
  separate(mixed, into = paste0("V", 1:7), sep = "\t", remove = FALSE) %>%
  mutate(
    gender = V1,
    category = paste0(V1, V3),
    time_hms = as_hms(V5)
  ) |>
  select(all_of(c(
    'names',
    'bib',
    'gender',
    'category',
    'time_hms'
  ))) |>
  arrange(time_hms) |>
  mutate(
    overall_ranking = seq(1, 773)
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
    arrange(time_hms) |>
    mutate(gender_ranking = seq(1, all_n))# |>
    # arrange(sport_1_time_hms) |>
    # mutate(
    #   sport_1_gender_ranking = seq(1, all_n)
    # ) |>
    # arrange(t_1_time_hms) |>
    # mutate(
    #   t_1_gender_ranking = seq(1, all_n)
    # ) |>
    # arrange(sport_2_time_hms) |>
    # mutate(
    #   sport_2_gender_ranking = seq(1, all_n)
    # ) |>
    # arrange(t_2_time_hms) |>
    # mutate(
    #   t_2_gender_ranking = seq(1, all_n)
    # ) |>
    # arrange(sport_3_time_hms) |>
    # mutate(
    #   sport_3_gender_ranking = seq(1, all_n)
    # )
  d
}) |>
  dplyr::bind_rows()


all_data <- lapply(split(all_data, all_data$category), function(d){
  all_n <- nrow(d)
  d <- d |>
    arrange(time_hms) |>
    mutate(category_ranking = seq(1, all_n))# |>
    # arrange(sport_1_time_hms) |>
    # mutate(
    #   sport_1_category_ranking = seq(1, all_n)
    # ) |>
    # arrange(t_1_time_hms) |>
    # mutate(
    #   t_1_category_ranking = seq(1, all_n)
    # ) |>
    # arrange(sport_2_time_hms) |>
    # mutate(
    #   sport_2_category_ranking = seq(1, all_n)
    # ) |>
    # arrange(t_2_time_hms) |>
    # mutate(
    #   t_2_category_ranking = seq(1, all_n)
    # ) |>
    # arrange(sport_3_time_hms) |>
    # mutate(
    #   sport_3_category_ranking = seq(1, all_n)
    # )
  d
}) |>
  dplyr::bind_rows()


all_data <- data.frame(
  names = "ADRIAN JOSEPH",
  time_hms = hms::as_hms("01:10:13"),
  overall_ranking = 35,
  gender_ranking = 30,
  category_ranking = 5,
  total_competitors = 347,
  total_in_category = 30,
  total_in_gender = 285,
  sport_1_time = hms::as_hms("00:14:37"),
  # t_1_time = hms::as_hms("00:02:10"),
  sport_2_time = hms::as_hms("00:33:31"),
  # t_2_time = hms::as_hms("00:01:25"),
  sport_3_time = hms::as_hms("00:22:05")#,
  # position_sequence_category = list(35, 35, 37, 38, 32),
  # position_sequence_gender = list(92, 97, 96, 105, 91),
  # position_sequence_overall = list(120, 123, 106, 116, 100)
)

arrow::write_parquet(all_data, sink = file.path(
  'posts', race_name, 'full_results.parquet'
))

all_data <- arrow::read_parquet(file.path('posts/2011-10-13_BUPA_GREAT_YORKSHIRE_RUN/full_results.parquet'))
a <- arrow::read_parquet(file.path('posts/2017-05-01_THAMES_TURBO_SPRINT_TRIATHLON/full_results.parquet'))
