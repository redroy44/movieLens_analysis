library(tidyverse)
library(lubridate)
library(stringr)

url <- "http://files.grouplens.org/datasets/movielens/"
dataset_small <- "ml-latest-small"
dataset_full <- "ml-latest"
data_folder <- "data"
archive_type <- ".zip"

# Choose dataset version
dataset <- dataset_small
dataset_zip <- paste0(dataset, archive_type)

# Download the data and unzip it
if (!file.exists(file.path(data_folder, dataset_zip))) {
  download.file(paste0(url, dataset_zip), file.path(data_folder, dataset_zip))
}
unzip(file.path(data_folder, dataset_zip), exdir = data_folder)

# Load the data
dataset_files <- c("movies", "ratings", "links", "tags")
suffix <- ".csv"

for (f in dataset_files) {
  path <- file.path(data_folder, dataset, paste0(f, suffix))
  assign(f, read.csv(path, header = T, stringsAsFactors = F))
  print(object.size(f),units="Gb")
}

# Clean ratings
ratings <- tbl_df(ratings) %>%
  mutate(rating = as.factor(rating), timestamp = as_datetime(timestamp))

# Clean movies
# TODO (no genres listed) -> NA
movies <- tbl_df(movies)
movies <-movies %>%
  mutate(title = str_trim(title)) %>% # trim whitespaces
  extract(title, c("title_tmp", "year"), regex = "^(.*) \\(([0-9 \\-]*)\\)$", remove = F) %>% # split title to title, year
  mutate(year = ifelse(str_length(year) > 4, as.integer(str_split(year, "-", simplify = T)[1]), as.integer(year))) %>% # for series take debut date
  mutate(title = ifelse(is.na(title_tmp), title, title_tmp)) %>% # replace title NA's with original title
  select(-title_tmp) # drop title1 column

# Check NA's
na_movies <- movies %>%
  filter(is.na(title) | is.na(year)) %>%
  print

# Clean tags
tags <- tbl_df(tags) %>%
  mutate(timestamp = as_datetime(timestamp))
