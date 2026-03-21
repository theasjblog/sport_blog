args <- commandArgs(trailingOnly = TRUE)

root_paths <- if (length(args) == 0) c("index.qmd", "posts") else args

all_files <- unlist(lapply(root_paths, function(path) {
  if (dir.exists(path)) {
    list.files(path, pattern = "\\.qmd$", recursive = TRUE, full.names = TRUE)
  } else if (file.exists(path)) {
    path
  } else {
    character()
  }
}), use.names = FALSE)

all_files <- unique(all_files)

pattern <- paste0(
  "(https://storage\\.googleapis\\.com/blogs_josa/sport/blog_assets/",
  "[^[:space:]\\\"')\\]>}]+/img/thumbnail/",
  "[^[:space:]\\\"')\\]>}]+)\\.(png|jpeg)"
)

for (file in all_files) {
  lines <- readLines(file, warn = FALSE, encoding = "UTF-8")
  updated <- gsub(pattern, "\\1.jpg", lines, perl = TRUE)

  if (!identical(lines, updated)) {
    writeLines(updated, file, useBytes = TRUE)
    cat("Updated", file, "\n")
  }
}
