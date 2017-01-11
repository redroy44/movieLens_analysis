# Utility functions for IMDB web scraping

# Get movie cast ----------------------------------------------------------
get_cast <- function(link) {
  cast <- foreach(d=iter(link, by='row'), .combine=rbind) %dopar% {
    tmp <- d %>%
      read_html() %>%
      html_nodes("#titleCast .itemprop span") %>%
      html_text(trim = T) %>%
      paste(collapse="|")
  }
  rownames(cast) <- c()
  return(as.vector(cast))
}


# Get movie budget --------------------------------------------------------
get_budget <- function(link) {
  budget <- foreach(d=iter(link, by='row'), .combine=rbind) %dopar% {
    tmp <- d %>%
      read_html() %>%
      html_nodes(css='#titleDetails > .txt-block') %>%
      html_text(trim = T) %>%
      tibble() %>% filter(str_detect(., "Budget"))
    ifelse(length(tmp) == 0, NA, parse_number(unlist(tmp)))
  }
  rownames(budget) <- c()
  return(as.vector(budget))
}


# Get movie director ------------------------------------------------------
get_director <- function(link) {
  
  director <- foreach(d=iter(link, by='row'), .combine=rbind) %dopar% {
    tmp <- d %>%
      read_html() %>%
      html_nodes(css='.credit_summary_item >  span[itemprop="director"]') %>%
      html_text(trim = T) %>%
      str_replace(",", "") %>%
      paste(collapse="|")
  }
  rownames(director) <- c()
  return(as.vector(director))
}


# Get movie running time ------------------------------------------------  
get_time <- function(link) {
  
  time <- foreach(d=iter(link, by='row'), .combine=rbind) %dopar% {
    tmp <- d %>%
      read_html() %>%
      html_nodes(css='#titleDetails > .txt-block') %>%
      html_text(trim = T) %>%
      tibble() %>% filter(str_detect(., "Runtime"))
    ifelse(length(tmp) == 0, NA, parse_number(unlist(tmp)))
  }
  rownames(time) <- c()
  return(as.vector(time))
}
