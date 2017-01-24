# Load the packages -------------------------------------------------------
library(checkpoint)
checkpoint("2016-11-15", auto.install.knitr=T)
library(tidyverse)
library(lubridate)
library(stringr)
library(rvest)
library(tidytext)
library(wordcloud)
library(doMC)
registerDoMC(cores = 4)
set.seed(29082012)

# genre rating histogram

# Print Version Information
version

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
unzip(file.path(data_folder, dataset_zip), exdir = data_folder, overwrite = F)

# Load the files
dataset_files <- c("movies", "ratings", "links", "tags")
suffix <- ".csv"

for (f in dataset_files) {
  path <- file.path(data_folder, dataset, paste0(f, suffix))
  assign(f, read_csv(path))
  print(paste(f, "object size is", format(object.size(get(f)),units="Mb")))
}


# Data cleaning -----------------------------------------------------------

# Clean ratings
glimpse(ratings)
ratings_df <- tbl_df(ratings) %>%
  mutate(timestamp = as_datetime(timestamp))

# Clean movies
# TODO make genres a factor?
glimpse(movies)
movies_df <- tbl_df(movies) %>%
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
glimpse(tags)
tags_df <- tbl_df(tags) %>%
  mutate(timestamp = as_datetime(timestamp))

summary(movies_df)

genres <- movies_df %>%
  separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarise(number = n()) %>%
  arrange(desc(number))


# Q0 ----------------------------------------------------------------------
# Number of movies per year/decade
movies_per_year <- movies_df %>%
  na.omit() %>%
  select(movieId, year) %>%
  group_by(year) %>%
  summarise(count = n())

# fill missing years
movies_per_year <- movies_per_year %>%
  complete(year = full_seq(year, 1), fill = list(count = 0))

# TODO turn to ggvis!
movies_per_year %>%
  #filter(year > 2010) %>%
  ggplot(aes(x = year, y = count)) +
  geom_line(color="blue")


# Q1 ----------------------------------------------------------------------

# Genres popularity per year
genres_popularity <- movies_df %>%
  na.omit() %>%
  select(movieId, year, genres) %>%
  separate_rows(genres, sep = "\\|") %>%
  mutate(genres = as.factor(genres)) %>%
  group_by(year, genres) %>%
  summarise(number = n()) %>%
  complete(year = full_seq(year, 1), genres, fill = list(number = 0))

# Most popular genres
genres_top <- genres_popularity %>%
  group_by(genres) %>%
  summarise(number = sum(number)) %>%
  arrange(desc(number)) %>%
  top_n(10, number)
  

# TODO turn to ggvis!
genres_popularity %>%
  filter(year > 1930) %>%
  filter(genres %in% genres_top$genres) %>%
  ggplot(aes(x = year, y = number)) +
    geom_area(aes(fill=genres), position = "fill") + 
    scale_fill_brewer(palette = "Paired") 


# Q2 ----------------------------------------------------------------------

# Tags for genres
genres_tags <- movies_df %>%
  na.omit() %>%
  select(movieId, year, genres) %>%
  separate_rows(genres, sep = "\\|") %>%
  inner_join(tags_df, by = "movieId") %>%
  select(genres, tag) %>%
  #distinct() %>%
  group_by(genres) %>%
  nest()

# plot wordcloud per genre
genre<-"Horror"
genres_tags %>%
  filter(genres == genre) %>%
  unnest() %>%
  mutate(tag = str_to_lower(tag, "en")) %>%
  anti_join(tibble(tag=c(tolower(genre)))) %>%
  count(tag) %>%
  with(wordcloud(tag, n, max.words = 50, colors=brewer.pal(8, "Dark2")))

# comparison.cloud?


# Q3 ----------------------------------------------------------------------

# average rating for a movie

avg_rating <- ratings_df %>%
  inner_join(movies_df, by = "movieId") %>%
  na.omit() %>%
  select(movieId, title, rating, year) %>%
  group_by(movieId, title, year) %>%
  summarise(count = n(), mean = mean(rating), min = min(rating), max = max(rating)) %>%
  ungroup() %>%
  arrange(count, desc(mean))

# Lower bound of Wilson score confidence interval for a Bernoulli parameter
# http://www.evanmiller.org/how-not-to-sort-by-average-rating.html
# movies with the same mean but more reviews get higher score
ci_lower <- function(pos, n, confidence) {
  z = qnorm(1-(1-confidence)/2)
  phat = pos
  return (phat + z*z/(2*n) - z * sqrt((phat*(1-phat)+z*z/(4*n))/n))/(1+z*z/n)
}

avg_rating <- avg_rating %>%
  mutate(score = ci_lower(mean/5, count, 0.95)) %>%
  arrange(desc(score))

# find best movie of a decade based on score
# heavily dependent on the number of reviews
best_per_decade <- avg_rating %>%
  filter(count > 2) %>% # at least the median number of reviews (3)
  mutate(decade = year  %/% 10 * 10) %>%
  arrange(year, desc(score)) %>%
  group_by(decade) %>%
  summarise(title = first(title), score = first(score), mean = first(mean), count = first(count))


# Q4 ----------------------------------------------------------------------

# avg_rating <- ratings_df %>%
#   inner_join(movies_df, by = "movieId") %>%
#   na.omit() %>%
#   select(movieId, title, rating, year) %>%
#   group_by(movieId, title, year) %>%
#   summarise(count = n(), mean = mean(rating), min = min(rating), max = max(rating)) %>%
#   ungroup() %>%
#   arrange(count, desc(mean))

genres_rating <- movies_df %>%
  na.omit() %>%
  select(movieId, year, genres) %>%
  inner_join(ratings_df, by = "movieId") %>%
  select(-timestamp, -userId) %>%
  mutate(decade = year  %/% 10 * 10) %>%
  separate_rows(genres, sep = "\\|") %>%
  group_by(year, genres) %>%
  summarise(count = n(), avg_rating = mean(rating)) %>%
  ungroup() %>%
  mutate(score = ci_lower(avg_rating/5, count, 0.95)) %>%
  arrange(year)

# TODO turn to ggvis!
a<- genres_rating %>%
  filter(genres %in% genres_top$genres) %>%
  ggplot(aes(x = year, y = score)) +
    geom_line(aes(group=genres, color=genres)) +
    geom_smooth(aes(group=genres, color=genres)) +
    facet_wrap(~genres)



# Web Scraping ------------------------------------------------------------
# source utility functions
source(file = "functions.R")

imdb_url = "http://www.imdb.com/title/tt"

imdb_df <- movies_df %>%
  inner_join(links, by = "movieId") %>%
  select(-tmdbId) %>%
  mutate(link = paste0(imdb_url, imdbId))

# Quick check
get_cast(c("http://www.imdb.com/title/tt0114709", "http://www.imdb.com/title/tt0076759"))
get_budget(c("http://www.imdb.com/title/tt0114709", "http://www.imdb.com/title/tt0076759"))
get_director(c("http://www.imdb.com/title/tt0114709", "http://www.imdb.com/title/tt0076759"))
get_time(c("http://www.imdb.com/title/tt0114709", "http://www.imdb.com/title/tt0076759"))

# Out of curiosity
#Need to optimize this to one function?
system.time({
imdb_df <- imdb_df %>%
  #top_n(100) %>%
  #mutate(time = get_time(link)) %>%
  mutate(director = get_director(link)) %>%
  #mutate(budget = get_budget(link)) %>% # Will be sparse. Or not?
  #mutate(cast = get_cast(link))
})
# Q5 ----------------------------------------------------------------------


imdb_df1 <- read_csv('imdb_df.csv')














