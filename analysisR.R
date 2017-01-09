# Load the packages -------------------------------------------------------
library(checkpoint)
checkpoint("2016-11-15")
library(tidyverse)
library(lubridate)
library(stringr)
library(rvest)
library(tidytext)
library(wordcloud)
library(doMC)
registerDoMC(cores = 8)
set.seed(29082012)


# Load the data -----------------------------------------------------------

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

# Load the files
dataset_files <- c("movies", "ratings", "links", "tags")
suffix <- ".csv"

for (f in dataset_files) {
  path <- file.path(data_folder, dataset, paste0(f, suffix))
  assign(f, read_csv(path))
  print(object.size(f),units="Mb") # Not working?
}


# Data cleaning -----------------------------------------------------------

# Clean ratings
ratings_df <- tbl_df(ratings) %>%
  mutate(rating = as.factor(rating), timestamp = as_datetime(timestamp))

# Clean movies
movies_df <- tbl_df(movies)
movies_df <- movies_df %>%
  mutate(title = str_trim(title)) %>% # trim whitespaces
  extract(title, c("title_tmp", "year"), regex = "^(.*) \\(([0-9 \\-]*)\\)$", remove = F) %>% # split title to title, year
  mutate(year = if_else(str_length(year) > 4, as.integer(str_split(year, "-", simplify = T)[1]), as.integer(year))) %>% # for series take debut date
  mutate(title = if_else(is.na(title_tmp), title, title_tmp)) %>% # replace title NA's with original title
  select(-title_tmp)  %>%# drop title1 column
  mutate(genres = if_else(genres == "(no genres listed)", `is.na<-`(genres), genres)) # generic function to turn (no genres listed) to NA

# Check NA's
na_movies <- movies_df %>%
  filter(is.na(title) | is.na(year)) %>%
  print

# Clean tags
tags_df <- tbl_df(tags) %>%
  mutate(timestamp = as_datetime(timestamp))


# Q1 ----------------------------------------------------------------------

# Genres popularity per year
genres_popularity <- movies_df %>%
  na.omit() %>%
  select(movieId, year, genres) %>%
  separate_rows(genres, sep = "\\|") %>%
  group_by(year, genres) %>%
  summarise(number = n())

# TODO turn to ggvis!
genres_popularity %>%
  filter(year > 1950) %>%
  ggplot(aes(x = year, y = number)) +
    geom_line(aes(group=genres, color=genres))


# Q2 ----------------------------------------------------------------------

# Tags for genres
genres_tags <- movies_df %>%
  na.omit() %>%
  select(movieId, year, genres) %>%
  separate_rows(genres, sep = "\\|") %>%
  left_join(tags_df, by = "movieId") %>%
  na.omit() %>%
  select(genres, tag) %>%
  group_by(genres) %>% 
  summarize(tag_list=unique(list(tag)))

# tidy_books %>%
#   anti_join(stop_words) %>%
#   count(word) %>%
#   with(wordcloud(word, n, max.words = 100))









