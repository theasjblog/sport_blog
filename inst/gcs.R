install.packages("googleCloudStorageR")
library(googleCloudStorageR)

gcs_auth(file.choose())

gcs_global_bucket("blogs_josa")


a <- c(
  list.files(file.path('posts'), recursive = TRUE, pattern = '.png', full.names = TRUE),
  list.files(file.path('posts'), recursive = TRUE, pattern = '.jpg', full.names = TRUE),
  list.files(file.path('posts'), recursive = TRUE, pattern = '.mov', full.names = TRUE)
)
a <- a[stringr::str_detect(a, 'figure-html', TRUE)]
a <- a[stringr::str_detect(a, 'img_modal', FALSE)]


for(src_path in a){
  dest_path <- gsub('posts', 'sport', src_path)
  dest_path <- gsub('img_modal', 'img', dest_path)
  dest_path <- gsub('’', '_', dest_path)
  tryCatch({
    gcs_upload(src_path, name = dest_path) 
  }, error = function(msg){
    write(src_path, file = 'log.log', append = TRUE)
    write(dest_path, file = 'log.log', append = TRUE)
    write('******************', file = 'log.log', append = TRUE)
    })
  
}

all_failed <- readLines('log.log')
all_src <- all_failed[seq(1,507,3)]
all_dest <- all_failed[seq(2,507,3)]

for(i in 1:length(all_src)){
  src_path <- all_src[i]
  dest_path <- all_dest[i]
  tryCatch({
    gcs_upload(src_path, name = dest_path) 
  }, error = function(msg){
    write(src_path, file = 'log_2.log', append = TRUE)
    write(dest_path, file = 'log_2.log', append = TRUE)
    write('******************', file = 'log.log', append = TRUE)
  })
  
}






src_path <- 'posts/1993_TORNEO_CALCIO_S._MARTINO_DI_CASTROZZA/img_modal/01.png'
dest_path <- 'sport/1993_TORNEO_CALCIO_S._MARTINO_DI_CASTROZZA/img/01.png'
gcs_upload(src_path, name = dest_path)




a_sp <- stringr::str_split(a, '/')
a_sp <- lapply(a_sp, function(d){
  d <- as.data.frame(t(as.data.frame(d)))
  colnames(d) <- paste0('col_', seq(1, ncol(d)))
  if(ncol(d) != 4){
    print(d)
    print(ncol(d))
  }
  d
}) |>
  dplyr::bind_rows()

library(dplyr)
summ_a <- a_sp |>
  group_by(col_2, col_3) |>
  summarise(s_n = n()) |>
  group_by(col_2) |>
  summarize(diff_cols = diff(s_n))

b <- list.dirs(file.path('posts'), recursive = FALSE, full.names = FALSE)
b <- b[seq(2,length(b))]
b
for(i in b){
  all_match <- a[stringr::str_detect(a, i)]
}








