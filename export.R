
tweets_export <-
  mc %>%
  rowwise() %>%
  mutate(media_url = str_c(media_url, collapse = ", "))

dir_create("data")
write_csv(tweets_export, here("data", "tweets.csv"))
