
pics <-
  tweets %>%
  unnest(media_url) %>%
  drop_na(media_url) %>%
  select(media_url, status_id)

# File name is {tweet's status_id}-{tweet's url id}.{url's extension}
download_media <- function(tbl = pics, dir = here("pics"), destroy_existing = TRUE) {
  
  if (destroy_existing) {
    if (dir_exists(dir)) {
      dir_delete(dir)
    }
    
    dir_create(dir)
  } else {
    if (!dir_exists(dir)) {
      dir_create(dir)
    }
  }

  for (i in seq(nrow(tbl))) {
    url <- tbl$media_url[i]
    status_id <- tbl$status_id[i]

    # Get id and extension
    pic_id_ext <-
      url %>%
      str_extract("(media|img)/.+\\.[a-z]+") %>%
      str_remove("(media|img)/")

    if (is.na(pic_id_ext)) {
      message(glue("couldn't download: {url}, {status_id}"))
    }

    file_nm <- glue("{dir}/{status_id}-{pic_id_ext}")

    download.file(url,
      destfile = file_nm
    )
  }
}

download_media()
