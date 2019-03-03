Sys.setenv(SPOTIFY_CLIENT_ID = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx')
access_token <- get_spotify_access_token()
library(spotifyr)
#beatles <- get_artist_audio_features('the beatles')


# https://github.com/ewenme/geniusr
Sys.setenv(GENIUS_API_TOKEN = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx')
genius_token()

library(geniusr)
library(dplyr)
library(tidytext)

# Get song search results for the term 'good morning'
gm_search <- search_song(search_term = "good morning") %>%
  # look for Kanye as the primary artist
  filter(artist_name == "Kanye West")





