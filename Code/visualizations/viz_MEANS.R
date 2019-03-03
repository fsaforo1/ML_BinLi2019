
library(dplyr)
library(plotly)

dance_mean <- newdata %>%
  group_by(danceability) %>%
  summarize(Popularity = mean(popularity, na.rm = TRUE)) %>%
  ggplot(aes(x = danceability, y = Popularity)) +
  geom_point(size = 2, colour = "darkblue")


## Loudness
loud_mean <- newdata %>%
  group_by(loudness) %>%
  summarize(Popularity = mean(popularity, na.rm = TRUE)) %>%
  ggplot(aes(x = loudness, y = Popularity)) +
  geom_point(size = 2, colour = "darkblue")



## Valence
val_mean <- newdata %>%
  group_by(valence) %>%
  summarize(Popularity = mean(popularity, na.rm = TRUE)) %>%
  ggplot(aes(x = valence, y = Popularity)) +
  geom_point(size = 2, colour = "darkblue")


## instrumentalness
instrum_mean <- newdata %>%
  group_by(instrumentalness) %>%
  summarize(Popularity = mean(popularity, na.rm = TRUE)) %>%
  ggplot(aes(x = instrumentalness, y = Popularity)) +
  geom_point(size = 2, colour = "darkblue")



## energy
energy_mean <- newdata %>%
  group_by(energy) %>%
  summarize(Popularity = mean(popularity, na.rm = TRUE)) %>%
  ggplot(aes(x = energy, y = Popularity)) +
  geom_point(size = 2, colour = "darkblue")



## duration_ms
duration_mean <- newdata %>%
  group_by(duration_ms) %>%
  summarize(Popularity = mean(popularity, na.rm = TRUE)) %>%
  ggplot(aes(x = duration_ms, y = Popularity)) +
  geom_point(size = 2, colour = "darkblue")

