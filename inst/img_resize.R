a_raw <- c(
  list.files('../../../Downloads/gcs_down/sport', full.names = TRUE, recursive = TRUE, include.dirs = FALSE, pattern = 'jpg'),
  list.files('../../../Downloads/gcs_down/sport', full.names = TRUE, recursive = TRUE, include.dirs = FALSE, pattern = 'png'))

a_small <- c(
  list.files('../../../Downloads/gcs_down/sport_small', full.names = TRUE, recursive = TRUE, include.dirs = FALSE, pattern = 'jpg'),
  list.files('../../../Downloads/gcs_down/sport_small', full.names = TRUE, recursive = TRUE, include.dirs = FALSE, pattern = 'png'))

a_small <- c(list.files('../../../Downloads/gcs_down/sport_small', full.names = TRUE, recursive = TRUE, include.dirs = FALSE))


library(dplyr)
library(magick)

get_file_info <- function(a){
  # Get file info
  file_info <- file.info(a) |>
    mutate(size_kb = size / (1024^2),
           name = a) |>
    select(all_of(c('size_kb', 'name')))
  
  
  pxs <- lapply(file_info$name, function(d){
    # Read the image
    img <- image_read(d)
    
    # Get image info
    info <- image_info(img)
    
    df <- data.frame(width = info$width,
                     height = info$height,
                     name = d
    )
    df
  }) |>
    dplyr::bind_rows()
  
  file_info <- file_info |>
    left_join(pxs, by = 'name')
  
  return(file_info)
}

a_raw <- get_file_info(a_raw)
a_small <- get_file_info(a_small)
  
a_raw <- a_raw |>
  filter(!name %in% gsub("sport_small", "sport", a_small$name))


tmp_path <- '../../../Downloads/gcs_down/sport_tmp'
final_path <- '../../../Downloads/gcs_down/sport_small'

target_size_mb <- 0.5

file_info <- a_raw
#for(i in 1:10){
for(i in 1:nrow(file_info)){
  cat(i, "of", nrow(file_info), '\n')
  this_img_tmp <- file.path(tmp_path, 
                        stringr:::str_split(file_info$name[i], "/")[[1]][7],
                        stringr:::str_split(file_info$name[i], "/")[[1]][8],
                        stringr:::str_split(file_info$name[i], "/")[[1]][9])
  this_img_final <- file.path(final_path, 
                            stringr:::str_split(file_info$name[i], "/")[[1]][7],
                            stringr:::str_split(file_info$name[i], "/")[[1]][8],
                            stringr:::str_split(file_info$name[i], "/")[[1]][9])
  
  dir.create(dirname(this_img_tmp), recursive = TRUE)
  dir.create(dirname(this_img_final), recursive = TRUE)
  
  img <- image_read(file_info$name[i])
  if(max(c(file_info$width[i], file_info$height[i])) > 1200){
    ratio <- floor(100*1200/max(c(file_info$width[i], file_info$height[i])))
    # Resize to 50% of the original size
    img_resized <- image_scale(img, paste0(ratio, "%"))
  } else {
    img_resized <- img
  }
  
  
  for (q in seq(100, 10, by = -5)) {  # try quality 100 down to 10
    image_write(img_resized, path = this_img_tmp, quality = q)
    
    size_mb <- file.info(this_img_tmp)$size / (1024^2)
    
    #cat("Quality:", q, " Size:", size_mb, "KB\n")
    
    if (size_mb <= target_size_mb | q == 10) {
      file.copy(this_img_tmp, this_img_final, overwrite = TRUE)
      cat("copied", this_img_final, "\n\n")
      break
    }
  }
}





# Compress JPEG (quality 80 is good balance)
img_compressed <- image_write(img_resized, 
                              path = "path/to/your/image_resized.jpg", 
                              format = "jpeg", 
                              quality = 80) # lower = more compression

# Check new file size
file.info("path/to/your/image_resized.jpg")$size / 1024  # in KB
