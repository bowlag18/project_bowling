get_lyrics <- function(lyrics_url) {
  # Function: Scrape last.fm lyrics page for: artist, song,
  # and lyrics from a provided content link.
  # Return as a tibble/data.frame
  
  cat("Scraping song lyrics from:", lyrics_url, "\n")
  
  pacman::p_load(tidyverse, rvest) # install/ load package(s)
  
  url <- url(lyrics_url, "rb") # open url connection
  html <- read_html(url) # read and parse html as an xml object
  close(url) # close url connection
  
  artist <-
    html %>%
    html_element("a.header-new-crumb") %>%
    html_text()
  
  song <-
    html %>%
    html_element("h1.header-new-title") %>%
    html_text()
  
  lyrics <-
    html %>%
    html_elements("p.lyrics-paragraph") %>%
    html_text()
  
  cat("...one moment ")
  
  Sys.sleep(1) # sleep for 1 second to reduce server load
  
  song_lyrics <- tibble(artist, song, lyrics, lyrics_url)
  
  cat("... done! \n")
  
  return(song_lyrics)
}


write_content <- function(content, target_file) {
  # Function: Write the tibble content to disk. Create the directory if
  # it does not already exist.
  
  pacman::p_load(tidyverse) # install/ load packages
  
  target_dir <- dirname(target_file) # identify target file directory structure
  dir.create(path = target_dir, recursive = TRUE, showWarnings = FALSE) # create directory
  write_csv(content, target_file) # write csv file to target location
  
  cat("Content written to disk!\n")
}


get_genre_lyrics_urls <- function(last_fm_genre) {
  # Function: Scrapes a given last.fm genre title for top tracks in
  # that genre and then creates links to the lyrics pages for these tracks
  
  cat("Scraping top songs from:", last_fm_genre, "genre: \n")
  
  pacman::p_load(tidyverse, rvest) # install/ load packages
  
  # create web url for the genre listing page
  genre_listing_url <-
    paste0("https://www.last.fm/tag/", last_fm_genre, "/tracks")
  
  genre_lyrics_urls <-
    read_html(genre_listing_url) %>% # read raw html and parse to xml
    html_elements("td.chartlist-name a") %>% # isolate the track elements
    html_attr("href") %>% # extract the href attribute
    paste0("https://www.last.fm", ., "/+lyrics") # join the domain, relative artist path, and the post-pended /+lyrics to create an absolute URL
  
  return(genre_lyrics_urls)
}

get_lyrics_catch <- function(lyrics_url) {
  tryCatch(get_lyrics(lyrics_url),
           error = function(e) return(NULL)) # no, URL, return(NULL)/ skip
}


download_lastfm_lyrics <- function(last_fm_genre, target_file) {
  # Function: get last.fm lyric urls by genre and write them to disk
  
  if(!file.exists(target_file)) {
    cat("Downloading data.\n")
    
    get_genre_lyrics_urls(last_fm_genre) %>%
      map(get_lyrics_catch) %>%
      bind_rows() %>%
      mutate(genre = last_fm_genre) %>%
      write_content(target_file)
    
  } else {
    cat("Data already downloaded!\n")
  }
}
