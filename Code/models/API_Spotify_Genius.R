Sys.setenv(SPOTIFY_CLIENT_ID = 'db9cfbe1f69f45d1a4bd641463a6c09e')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'cf422bc7e56a4d82a075c2ad60aa9672')
access_token <- get_spotify_access_token()
library(spotifyr)
#beatles <- get_artist_audio_features('the beatles')


# https://github.com/ewenme/geniusr
Sys.setenv(GENIUS_API_TOKEN = '3YxcxGs0Y-acFzCzoJi8LeK8HeKRbW7yk54qVeLhUh7YktgxDrFf5Vvgk-YfbNJm')
genius_token()

library(geniusr)
library(dplyr)
library(tidytext)

# Get song search results for the term 'good morning'
gm_search <- search_song(search_term = "good morning") %>%
  # look for Kanye as the primary artist
  filter(artist_name == "Kanye West")





