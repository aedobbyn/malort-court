
library(fs)
library(glue)
library(here)
library(rtweet)
library(tidyverse)

try_get_timeline <- possibly(get_timeline, otherwise = NULL)

get_mc_tweets <- function(reg = "(#*)[Mm]alort(\\s*)[Cc]ourt",
                          filter_to_reg = TRUE,
                          max_id = Inf,
                          max_iters = 10,
                          n_per_pull = 3200,
                          verbose = TRUE) {
  i <- 1
  m_id <- NULL
  out <- NULL

  while (i <= max_iters && (is.null(out) || max(out$status_id <= max_id))) {
    if (verbose) {
      message(glue("Starting iteration {i}."))
      if (i == max_iters) message(glue("Reached maximum number of pulls."))
    }

    this <-
      try_get_timeline("ChicagoNemesis",
        n = n_per_pull,
        max_id = m_id
      )

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

tweets <-
  raw %>%
  arrange(created_at) %>%
  mutate(
    text = text %>% str_replace_all("&amp", "&")
  ) %>%
  select(
    text, created_at,
    favorite_count, retweet_count,
    hashtags, media_url, status_id
  ) %>%
  rename(like_count = favorite_count) %>%
  mutate(
    year = lubridate::year(created_at),
    month = lubridate::month(created_at),
    day = lubridate::day(created_at)
  ) %>%
  left_join(month_year_dict,
    by = c("year" = "c_year")
  ) %>%
  mutate(
    in_court =
      case_when(
        month == c_month ~ TRUE,
        TRUE ~ FALSE
      )
  ) %>%
  select(-c_month) %>%
  rowwise() %>%
  mutate(hashtags = str_c(hashtags, collapse = ", ")) %>%
  ungroup()


# Get actual month and day malort court took place by number of tweets tweeted
ymd_dict <-
  tweets %>%
  count(year, month, day, sort = TRUE) %>%
  distinct(year, .keep_all = TRUE) %>% # Keep day and month w top n tweets
  arrange(year, month, day)

first_tweet <-
  tweets %>%
  filter(
    str_detect(text, "begin #malortcourt|underway|Order|Emergency Hearing|#malortcourtpt2")
  ) %>%
  select(text, status_id, year, month)

last_tweet <-
  tweets %>%
  filter(
    str_detect(text, "ajourned|adjourned|wraps up|will be recorded")
  ) %>%
  select(text, status_id, year, month)

bounds <-
  ymd_dict %>%
  left_join(first_tweet %>% select(-text), by = c("year", "month")) %>%
  rename(
    beginning_status_id = status_id
  ) %>%
  left_join(last_tweet %>% select(-text), by = c("year", "month")) %>%
  rename(
    ending_status_id = status_id
  )


get_all_tweets <- function() {
  
  min_id <- min(bounds$beginning_status_id)
    
  while (min(out$status_id) > min_id) {
    
  }
}


get_more_tweets <- function(n_per_pull = 100) {
  
  out <- NULL
  
  for (i in seq(nrow(bounds))) {
    this <-
      try_get_timeline("ChicagoNemesis",
        n = n_per_pull,
        max_id = bounds$ending_status_id[i]
      ) %>% 
      filter(status_id <= bounds$ending_status_id[i])
    
    this_max_id <- max(this$status_id)
    
    while(this_max_id < bounds$beginning_status_id[i]) {
      this <-
        try_get_timeline("ChicagoNemesis",
                         n = n_per_pull,
                         max_id = bounds$ending_status_id[i]
        )
    }
    
    out <-
      out %>% 
      bind_rows(this)
  }
  
  out
}




