rm(list = ls())


```{r, echo = FALSE, warning=FALSE, message=FALSE}
library(DT)
library(hms)
library(dplyr)
library(arrow)

dateIs <- "2014-04-06"
df <- arrow::read_parquet('https://storage.googleapis.com/blogs_josa/sport/parquets/main_dataset.parquet') |>
  filter(event_date == dateIs)

all_files <- gcs_list_objects("blogs_josa", prefix = "sport/parquets/record/") |>
  mutate(event_date = substr(basename(name), 1, 10)) |>
  filter(as.character(event_date) == dateIs)

results <- arrow::read_parquet(
  paste0('https://storage.googleapis.com/blogs_josa/', all_files$name)) |>
  mutate(pace = ifelse(speed > 0, 1000/(speed*60), speed),
         timeflow = cumsum(c(0, diff(timestamp)))/60) 

library(ggplot2)
ggplot(results) +
  aes(x = timeflow, y = pace) +
  geom_line(colour = 'gray') +
  geom_smooth(method = "loess", span = 0.1, se = FALSE, color = "red") +
  theme_minimal() +
  ggtitle("Pace") +
  labs(y = "Pace [min/km]", x = "Elapsed time [min]") +
  theme(
    plot.title = element_text(
      size = 18,        # Font size
      face = "bold",    # Bold text
      hjust = 0.5       # Center title (0 = left, 1 = right, 0.5 = center)
    ),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )


ggplot(results) +
  aes(x = timeflow, y = heart_rate) +
  geom_line(colour = 'gray') +
  geom_smooth(method = "loess", span = 0.1, se = FALSE, color = "red") +
  theme_minimal() +
  ggtitle("Heart Rate") +
  labs(y = "Heart Rate [bpm]", x = "Elapsed time [min]") +
  theme(
    plot.title = element_text(
      size = 18,        # Font size
      face = "bold",    # Bold text
      hjust = 0.5       # Center title (0 = left, 1 = right, 0.5 = center)
    ),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )

ggplot(results) +
  aes(x = timeflow, y = cadence) +
  geom_line(colour = 'gray') +
  geom_smooth(method = "loess", span = 0.1, se = FALSE, color = "red") +
  theme_minimal() +
  ggtitle("Cadence") +
  labs(y = "Cadence [spm]", x = "Elapsed time [min]") +
  theme(
    plot.title = element_text(
      size = 18,        # Font size
      face = "bold",    # Bold text
      hjust = 0.5       # Center title (0 = left, 1 = right, 0.5 = center)
    ),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )

library(leaflet)
leaflet(results) |>
  addTiles() |>  # Add default OpenStreetMap map tiles
  addPolylines(lng = ~position_long, lat = ~position_lat, color = "blue", weight = 3) |>
  addAwesomeMarkers(
    lng = ~position_long[1],
    lat = ~position_lat[1],
    label = "Start",
    icon = awesomeIcons(
      icon = "play",
      markerColor = "green"
    )
  ) %>%
  addAwesomeMarkers(
    lng = ~position_long[nrow(df)],
    lat = ~position_lat[nrow(df)],
    label = "End",
    icon = awesomeIcons(
      icon = "flag-checkered",
      markerColor = "red"
    )
  )



final <- data.frame(
  "Race type" = paste0(df$event_type, " ", df$event_distance),
  "Final time" = hms::as_hms(df$result_total),
  "Overall ranking" = paste0(df$ranking_overall, "/", df$participants_overall),
  "Gender ranking" = paste0(df$ranking_gender, "/", df$participants_gender),
  "Category ranking" = paste0(df$ranking_category, "/", df$participants_category)
)

final <- t(final)

final <- data.frame(
  col1 = gsub("\\.", " ", rownames(final)),
  col2 = final[,1], 
  row.names = NULL
) |>
  filter(!is.na(col2)) |>
  filter(col2 != "NA/NA") |>
  mutate(
    col2 = gsub("NA", "-", col2)
  )
  

colnames(final) <- c(df$event_name, final$col2[1])
final <- final[-1,]

datatable(
  final,
  rownames = FALSE,                 # Don't show row names
  options = list(
    #    dom = 't',                      # Show only the table body (no search, no pagination, no header)
    ordering = FALSE                # Optional: disable sorting
  )
)
```