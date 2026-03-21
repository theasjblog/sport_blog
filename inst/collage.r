# Install if needed
# install.packages("googleCloudStorageR")

library(googleCloudStorageR)

# Authenticate (opens browser for OAuth or use service account JSON)
gcs_auth(file.choose())

# Set your bucket name
bucket <- "blogs_josa"
prefix <- "sport_small/"   # GCS "folder" (actually just a prefix)

# List all objects under that folder
objs <- gcs_list_objects(bucket, prefix = prefix) |>
  filter(
    tools::file_ext(name) %in% c('jpeg', 'jpg', 'png')
  )

# Inspect results
head(objs$name)

# Download all files in loop
for (f in objs$name) {
  # extract just filename after last "/"
  localfile <- f
  if(!dir.exists(dirname(localfile))){
    dir.create(dirname(localfile), recursive = TRUE)
  }
  
  # download to current working directory
  gcs_get_object(object_name = f, 
                 bucket = bucket,
                 saveToDisk = localfile,
                 overwrite = TRUE)
}


# REGULAR LAYOUT
library(magick)

# Read all images (adjust pattern to match your files)
files <- c(
  list.files("sport_small", pattern = "\\.jpg$", full.names = TRUE, recursive = TRUE),
  list.files("sport_small", pattern = "\\.jpeg$", full.names = TRUE, recursive = TRUE),
  list.files("sport_small", pattern = "\\.png$", full.names = TRUE, recursive = TRUE)
  )


# Loop over files and save as PNG
for (f in files) {
  img <- try(image_read(f), silent = TRUE)  # try reading
  
  if (!inherits(img, "try-error")) {
    outfile <- file.path('converted2', dirname(f), paste0(tools::file_path_sans_ext(basename(f)), ".png"))
    if(!dir.exists(dirname(outfile))){
      dir.create(dirname(outfile), recursive = TRUE)
    }
    image_write(img, path = outfile, format = "png")
  } else {
    message("Skipping: ", f)
  }
}

files <- c(
  #list.files("collage/bibs", pattern = "\\.png$", full.names = TRUE, recursive = TRUE)#,
  list.files("collage/medals", pattern = "\\.png$", full.names = TRUE, recursive = TRUE)#,
 # list.files("converted2", pattern = "\\.png$", full.names = TRUE, recursive = TRUE)
  )

files_sample <- sample(files, 200, replace = FALSE)
# Read and resize them to the same size (e.g., 200x200)
imgs <- lapply(files_sample, image_read)
imgs_resized <- lapply(imgs, image_resize, "200x200")

row <- image_append(image_join(imgs_resized), stack = FALSE)

# Arrange vertically (if multiple rows)
# final <- image_append(image_join(list(row1, row2, ...)), stack = TRUE)

image_write(row, "collage_test.png")






library(magick)
library(dplyr)

make_collage <- function(files, ncol = 10, cell_size = "200x200!", outfile = "collage.png") {
  # Read and resize
  imgs <- lapply(files, image_read)
  imgs_resized <- lapply(imgs, image_resize, cell_size)
  
  # Split into rows
  rows <- split(imgs_resized, ceiling(seq_along(imgs_resized) / ncol))
  
  # Build each row by appending horizontally
  row_imgs <- lapply(rows, function(r) image_append(image_join(r), stack = FALSE))
  
  # Stack rows vertically
  final <- image_append(image_join(row_imgs), stack = TRUE) |>
    image_background("white") |>    # Remove alpha (transparent) if any
    image_convert("png")            # Force proper PNG encoding
  
  # Save result
  image_write(final, outfile)
  return(final)
}

# Example usage
files <- c(
  list.files("collage/bibs", pattern = "\\.png$", full.names = TRUE, recursive = TRUE),
  list.files("collage/medals", pattern = "\\.png$", full.names = TRUE, recursive = TRUE),
  list.files("collage/photos_one_item", pattern = "\\.png$", full.names = TRUE, recursive = TRUE)
)
files <- list.files(dirname(file.choose()), full.names = TRUE)
files <- data.frame(files = files) |>
  mutate(date = stringr::str_split(files, "/", simplify = TRUE)[,4]) |>
  mutate(date = substring(date, 1, 10),
         date = as.Date(date)) |>
  mutate(sourcePath = stringr::str_split(files, "/", simplify = TRUE)[,2]) |>
  arrange(date, sourcePath)

2015-11-21# last date book 1
2019-09-28# first date book 3

this_files <- files |>
  # filter(date>=as.Date("1900-01-01") & date <= as.Date("2015-11-21")) |>
  # filter(date>as.Date("2015-11-21") & date < as.Date("2019-09-28")) |>
  # filter(date>=as.Date("2019-09-28") & date <= as.Date("3019-09-08")) |>
  pull(files)
title_collage <- 'cover_1.png'
title_collage <- 'spine_1.png'


n_cols <- floor(length(this_files)^(1/2))
n_rows <- ceiling(length(this_files)/n_cols)

n_cols <- 2
n_rows <- 7


missing_to_square <- (n_rows * n_cols) - length(this_files)
if(missing_to_square > 0){
  this_files <- c(
    this_files,
    sample(this_files, missing_to_square, replace = FALSE)
  )
}

# this_files <-  sample(files, 
#         length(files),
#         replace = FALSE)

make_collage(this_files,
             ncol = n_cols,
             cell_size = "200x200!",
             outfile = title_collage)

