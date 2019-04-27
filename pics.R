

pic_urls <-
  dat %>% 
  unnest(media_url) %>% 
  drop_na(media_url) %>% 
  select(media_url, status_id)

download_media <- function(tbl = pic_urls, dir = here("pics")) {
  if (!dir_exists(dir)) {
    dir_create(dir)
  }
  
  for (i in seq(nrow(tbl))) {
    download.file(tbl$media_url[i], destfile = glue("{dir}/{tbl$status_id[i]}.jpg"))
  }
  
  # map(tbl$media_url, download.file, destfile = glue("{tbl$status_id}.jpg"))
  # glue("{tbl$status_id}.jpg")
  
}
  
