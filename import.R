
library(fs)
library(glue)
library(here)
library(rtweet)
library(tidyverse)

try_get_timeline <- possibly(get_timeline, otherwise = NULL)

get_mc_tweets <- function(reg = "#malortcourt|#MalortCourt", 
                          max_iters = 10, 
                          n_per_pull = 3200, 
                          verbose = TRUE) {
  
  i <- 1
  m_id <- NULL
  out <- NULL
  
  while (i <= max_iters) {
    
    
    if (verbose) {
      message(glue("Starting iteration {i}."))
      if (i == max_iters) message(glue("Reached maximum number of pulls."))
    }
    
    this <- 
      try_get_timeline("ChicagoNemesis",
                       n = n_per_pull, 
                       max_id = m_id) 
    
    # max_id "returns results with an ID less than (that is, older than) or equal to 'max_id'", so should always be 1 row
    if (nrow(this) <= 1) { 
      if (verbose) message(glue("Done after iteration {i}."))
      return(out)
    }
    
    m_id <- 
      this %>% 
      filter(status_id == min(as.numeric(status_id))) %>% 
      pull(status_id)
    
    mcs <- 
      this %>% 
      filter(str_detect(text, reg))
    
    out <-
      out %>% 
      bind_rows(mcs)
    
    i <- i + 1
  }
  
  out
}

raw <- get_mc_tweets()

# Actual months malort court took place
month_year_dict <- 
  tribble(
    ~c_year, ~c_month,
    2015, 10,
    2016, 12,
    2017, 10, 
    2018, 10, 
    2019, 01
  )

tweets <-
  raw %>% 
  select(text, created_at, 
         favorite_count, retweet_count, 
         hashtags, media_url, status_id) %>% 
  rename(like_count = favorite_count) %>% 
  mutate(
    year = lubridate::year(created_at),
    month = lubridate::month(created_at)
  ) %>% 
  left_join(month_year_dict, 
            by = c("year" = "c_year")) %>%
  mutate(
    in_court =
      case_when(
        month == c_month ~ TRUE,
        TRUE ~ FALSE
      )
  ) %>% 
  select(-c_month) %>% 
  rowwise() %>% 
  mutate(hashtags = str_c(hashtags, collapse = ", "))


