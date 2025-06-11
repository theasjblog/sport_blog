
race_name <- "2024-07-07_BRACEBRIDGE_SPRINT_TRIATHLON"
lines <- readLines(file.path(
  'posts', race_name, 'data.txt'
))

idx <- 
  stringr::str_detect(lines, 'Barrie Triathlon', negate = TRUE) &
  lines != 'BARRIE, ON • CAN' &
  lines != 'AUG 10, 2024' &
  stringr::str_detect(lines, 'Place\tName', negate = TRUE) &
  stringr::str_detect(lines, ' out of ', negate = TRUE) &
  lines != '' &
  lines != 'Events' &
  lines != 'Sportstats' &
  lines != 'Shop' &
  lines != '0' &
  lines != 'Results' &
  lines != 'Watchlist' &
  lines != "© 2025 Sportstats Acquisitions LLC. All rights reserved." &
  lines != "Need Help?" &
  lines != "Help" &
  lines != "About" &
  lines != "Services" &

lines <- lines[idx]

results <- data.frame(
  names = lines[seq(3, length(lines), 12)],
  category = lines[seq(4, length(lines), 12)],
  time_hms = lines[seq(11, length(lines), 12)],
  sport_1_time = lines[seq(5, length(lines), 12)],
  sport_2_time = lines[seq(7, length(lines), 12)],
  sport_3_time = lines[seq(9, length(lines), 12)]
) |>
  mutate(
    names = trimws(toupper(names)),
    gender = substring(category, 1,1),
    category = sub(" Bib.*", "", category),
    time_hms = hms::as_hms(time_hms),
    sport_1_time = hms::as_hms(sport_1_time),
    sport_2_time = hms::as_hms(sport_2_time),
    sport_3_time = hms::as_hms(sport_3_time)
  ) |>
  filter(toupper(category) != "RELAY")
nn <- nrow(results)
results <- results |>
  arrange(time_hms) |>
  mutate(overall_ranking = seq(1, nn))
  
# results <- list()
# i <- 1
# last_time <- hms::as_hms("00:00:00")
# while (i <= length(lines)) {
#   line <- lines[i]
#   
#   if (grepl("^[0-9]+\\t$", line)) {
#     # Start of a new entry
#     position <- as.integer(gsub("\t", "", line))
#     
#     if (i + 1 <= length(lines) && lines[i + 1] != "Unknown") {
#       # Registered participant
#       name     <- lines[i + 1]
#       gender   <- lines[i + 2]
#       category <- lines[i + 3]
#       # Club is optional – we'll skip it
#       time_raw <- lines[i + 5]
#       time     <- as_hms(paste0("00:", time_raw))  # mm:ss to hms
#       
#       results[[length(results) + 1]] <- list(
#         position = position,
#         name = name,
#         gender = gender,
#         category = category,
#         time = time
#       )
#       
#       last_time <- time
#       i <- i + 6  # Advance past this competitor
#       
#     } else {
#       # Unregistered participant
#       name     <- "Unknown"
#       gender   <- NA
#       category <- NA
#       time     <- last_time + 1  # Add 1 second
#       
#       results[[length(results) + 1]] <- list(
#         position = position,
#         name = name,
#         gender = gender,
#         category = category,
#         time = time
#       )
#       
#       last_time <- time
#       i <- i + 3  # Skip "Unknown" and empty fields
#     }
#     
#   } else {
#     # Unexpected line, just skip
#     i <- i + 1
#   }
# }
# 
# n_runners <- length(results)
# # Convert to data.frame
# results <- results |>
#   dplyr::bind_rows() |>
#   rename(
#     'time_hms' = 'time',
#     'names' = 'name'
#   ) |>
#   select(all_of(c(
#     'names',
#     'time_hms',
#     'gender',
#     'category'
#   ))) |>
#   mutate(
#     names = trimws(toupper(names), 'both')
#   ) |>
#   arrange(time_hms) |>
#   mutate(overall_ranking = seq(1,n_runners))
# 
# results <- lapply(split(results, results$gender), function(d){
#   all_n <- nrow(d)
#   d <- d |>
#     arrange(time_hms) |>
#     mutate(gender_ranking = seq(1, all_n))
#   d
# }) |>
#   dplyr::bind_rows()
# 
# 
# results <- lapply(split(results, results$category), function(d){
#   all_n <- nrow(d)
#   d <- d |>
#     arrange(time_hms) |>
#     mutate(category_ranking = seq(1, all_n))
#   d
# }) |>
#   dplyr::bind_rows()


arrow::write_parquet(results, sink = file.path(
  'posts', race_name, 'full_results.parquet'
))


results <- arrow::read_parquet(file.path(
  'posts', race_name, 'full_results.parquet'
))
