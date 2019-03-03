Sys.setenv(SPOTIFY_CLIENT_ID = 'db9cfbe1f69f45d1a4bd641463a6c09e')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'cf422bc7e56a4d82a075c2ad60aa9672')
access_token <- get_spotify_access_token()

spotify_df <- get_artist_audio_features('Bruno')

str(spotify_df)

token <- '3YxcxGs0Y-acFzCzoJi8LeK8HeKRbW7yk54qVeLhUh7YktgxDrFf5Vvgk-YfbNJm'

library(httr)
genius_get_artists <- function(artist_name, n_results = 10) {
  baseURL <- 'https://api.genius.com/search?q=' 
  requestURL <- paste0(baseURL, gsub(' ', '%20', artist_name),
                       '&per_page=', n_results,
                       '&access_token=', token)
  
  res <- GET(requestURL) %>% content %>% .$response %>% .$hits
  
  map_df(1:length(res), function(x) {
    tmp <- res[[x]]$result$primary_artist
    list(
      artist_id = tmp$id,
      artist_name = tmp$name
    )
  }) %>% unique
}

genius_artists <- genius_get_artists('Bruno Mars')
genius_artists
genius_artists <- genius_artists[1,]

baseURL <- 'https://api.genius.com/artists/' 
requestURL <- paste0(baseURL, genius_artists$artist_id[1], '/songs')

track_lyric_urls <- list()
i <- 1
while (i > 0) {
  tmp <- GET(requestURL, query = list(access_token = token, per_page = 50, page = i)) %>% content %>% .$response
  track_lyric_urls <- c(track_lyric_urls, tmp$songs)
  if (!is.null(tmp$next_page)) {
    i <- tmp$next_page
  } else {
    break
  }
}

length(track_lyric_urls)

summary(track_lyric_urls[[1]])


library(rvest)

lyric_scraper <- function(url) {
  read_html(url) %>% 
    html_node('lyrics') %>% 
    html_text
}

genius_df <- map_df(1:length(track_lyric_urls), function(x) {
  library(googleway)
  # add in error handling
  lyrics <- try(lyric_scraper(track_lyric_urls[[x]]$url))
  if (class(lyrics) != 'try-error') {
    # strip out non-lyric text and extra spaces
    lyrics <- str_replace_all(lyrics, '\\[(Verse [[:digit:]]|Pre-Chorus [[:digit:]]|Hook [[:digit:]]|Chorus|Outro|Verse|Refrain|Hook|Bridge|Intro|Instrumental)\\]|[[:digit:]]|[\\.!?\\(\\)\\[\\],]', '')
    lyrics <- str_replace_all(lyrics, '\\n', ' ')
    lyrics <- str_replace_all(lyrics, '([A-Z])', ' \\1')
    lyrics <- str_replace_all(lyrics, ' {2,}', ' ')
    lyrics <- tolower(str_trim(lyrics))
  } else {
    lyrics <- NA
  }
  
  tots <- list(
    track_name = track_lyric_urls[[x]]$title,
    lyrics = lyrics
  )
  
  return(tots)
})

str(genius_df)


library(genius)

lyrics <- rep(NA, length(genius_df$track_name))
for (i in length(lyrics)){
  lyrics[i] <- genius_lyrics(artist = "Bruno Mars", song = genius_df$track_name[i])
}
lyrics

