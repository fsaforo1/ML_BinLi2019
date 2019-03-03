

library(dplyr)
library(plotly)

dance_sum <- newdata %>%
  group_by(danceability) %>%
  summarize(Popularity = sum(popularity, na.rm = TRUE)) %>%
  ggplot(aes(x = danceability, y = Popularity)) +
  geom_point(size = 2, colour = "darkblue")


## Loudness
loud_sum <- newdata %>%
  group_by(loudness) %>%
  summarize(Popularity = sum(popularity, na.rm = TRUE)) %>%
  ggplot(aes(x = loudness, y = Popularity)) +
  geom_point(size = 2, colour = "darkblue")



## Valence
val_sum <- newdata %>%
  group_by(valence) %>%
  summarize(Popularity = sum(popularity, na.rm = TRUE)) %>%
  ggplot(aes(x = valence, y = Popularity)) +
  geom_point(size = 2, colour = "darkblue")


## instrumentalness
instrum_sum <- newdata %>%
  group_by(instrumentalness) %>%
  summarize(Popularity = sum(popularity, na.rm = TRUE)) %>%
  ggplot(aes(x = instrumentalness, y = Popularity)) +
  geom_point(size = 2, colour = "darkblue")



## energy
energy_sum <- newdata %>%
  group_by(energy) %>%
  summarize(Popularity = sum(popularity, na.rm = TRUE)) %>%
  ggplot(aes(x = energy, y = Popularity)) +
  geom_point(size = 2, colour = "darkblue")



## duration_ms
duration_sum <- newdata %>%
  group_by(duration_ms) %>%
  summarize(Popularity = sum(popularity, na.rm = TRUE)) %>%
  ggplot(aes(x = duration_ms, y = Popularity)) +
  geom_point(size = 2, colour = "darkblue")

