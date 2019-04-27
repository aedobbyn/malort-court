
library(fs)
library(glue)
library(here)
library(rtweet)
library(tidyverse)

try_get_timeline <- possibly(get_timeline, otherwise = NULL)

get_mc_tweets <- function(reg = "(#*)[Mm]alort(\\s*)[Cc]ourt",
                          filter_to_reg = TRUE,
                          min_id = 0,
                          max_id = Inf,
                          max_iters = 10,
                          n_per_pull = 3200,
                          verbose = TRUE) {
  i <- 1
  m_id <- NULL
  out <- NULL

  while (i <= max_iters && (is.null(out) || min(out$status_id >= min_id))) {
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
    
    if (filter_to_reg) {
      this <-
        this %>%
        filter(str_detect(text, reg))
    }

    out <-
      out %>%
      bind_rows(this)

    i <- i + 1
  }
  
  if (min_id > 0) {
    out <- 
      out %>% 
      filter(as.numeric(status_id) >= min_id)
  }
  
  if (is.finite(max_id)) {
    out <- 
      out %>% 
      filter(as.numeric(status_id) <= max_id)
  }

  out
}

raw <- get_mc_tweets()

clean_tweets <- function(tbl, status_as_numeric = FALSE) {
  out <- tbl %>% 
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
  
  if (status_as_numeric) {
    out <- 
      out %>% 
      mutate(
        status_id = as.numeric(status_id)
      )
  }
  out
}

tweets <-
  raw %>%
  clean_tweets()

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


id_boundaries <- 
  suppressWarnings({
    bounds %>% 
      rowwise() %>% 
      mutate(
        ineq = glue(">= {as.numeric(beginning_status_id)} & status_id <= {as.numeric(ending_status_id)}")
      ) %>% 
      ungroup() %>% 
      pull(ineq) %>% 
      str_c(collapse = ") || ( status_id ")
  })
  


full <- get_mc_tweets(filter_to_reg = FALSE, 
                      min_id = min(as.numeric(bounds$beginning_status_id)),
                      max_id = max(as.numeric(bounds$ending_status_id))
                      ) %>% 
  clean_tweets(status_as_numeric = TRUE) 


mc <-
  full %>% 
  filter(
    (status_id >= 650439476628451328 & status_id <= 650467989666435072) |
      ( status_id >= 806186436995256320 & status_id <= 810339253930618880) |
      ( status_id >= 921887456865275904 & status_id <= 921909940838596608) |
      ( status_id >= 1053704347891130368 & status_id <= 1053862055017467904) |
      ( status_id >= 1084277365851615232 & status_id <= 1084278626411900928)
)
