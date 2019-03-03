
#ggplot(aggregate(popularity ~ danceability, newdata, mean))

library(dplyr)
library(plotly)

danceKEY_mean <- newdata %>%
  group_by(danceability, KEY) %>%
  summarize(Popularity = mean(popularity, na.rm = TRUE)) %>%
  ggplot(aes(x = danceability, y = Popularity, colour = KEY)) +
  geom_point(size = 2, colour = "black") + geom_point(size = 2)

danceTEMPO_mean <- newdata %>%
  group_by(danceability, TEMPO) %>%
  summarize(Popularity = mean(popularity, na.rm = TRUE)) %>%
  ggplot(aes(x = danceability, y = Popularity, colour = TEMPO)) +
  geom_point(size = 2, colour = "black") + geom_point(size = 2)


danceTONE_mean <- newdata %>%
  group_by(danceability, tone) %>%
  summarize(Popularity = mean(popularity, na.rm = TRUE)) %>%
  ggplot(aes(x = danceability, y = Popularity, colour = tone)) +
  geom_point(size = 2, colour = "black") + geom_point(size = 2)


## Loudness
loudKEY_mean <- newdata %>%
  group_by(loudness, KEY) %>%
  summarize(Popularity = mean(popularity, na.rm = TRUE)) %>%
  ggplot(aes(x = loudness, y = Popularity, colour = KEY)) +
  geom_point(size = 2, colour = "black") + geom_point(size = 2)

loudTEMPO_mean <- newdata %>%
  group_by(loudness, TEMPO) %>%
  summarize(Popularity = mean(popularity, na.rm = TRUE)) %>%
  ggplot(aes(x = loudness, y = Popularity, colour = TEMPO)) +
  geom_point(size = 2, colour = "black") + geom_point(size = 2)


loudTONE_mean <- newdata %>%
  group_by(loudness, tone) %>%
  summarize(Popularity = mean(popularity, na.rm = TRUE)) %>%
  ggplot(aes(x = loudness, y = Popularity, colour = tone)) +
  geom_point(size = 2, colour = "black") + geom_point(size = 2)


## Valence
valKEY_mean <- newdata %>%
  group_by(valence, KEY) %>%
  summarize(Popularity = mean(popularity, na.rm = TRUE)) %>%
  ggplot(aes(x = valence, y = Popularity, colour = KEY)) +
  geom_point(size = 2, colour = "black") + geom_point(size = 2)

valTEMPO_mean <- newdata %>%
  group_by(valence, TEMPO) %>%
  summarize(Popularity = mean(popularity, na.rm = TRUE)) %>%
  ggplot(aes(x = valence, y = Popularity, colour = TEMPO)) +
  geom_point(size = 2, colour = "black") + geom_point(size = 2)


valTONE_mean <- newdata %>%
  group_by(valence, tone) %>%
  summarize(Popularity = mean(popularity, na.rm = TRUE)) %>%
  ggplot(aes(x = valence, y = Popularity, colour = tone)) +
  geom_point(size = 2, colour = "black") + geom_point(size = 2)


## instrumentalness
instrumKEY_mean <- newdata %>%
  group_by(instrumentalness, KEY) %>%
  summarize(Popularity = mean(popularity, na.rm = TRUE)) %>%
  ggplot(aes(x = instrumentalness, y = Popularity, colour = KEY)) +
  geom_point(size = 2, colour = "black") + geom_point(size = 2)

instrumTEMPO_mean <- newdata %>%
  group_by(instrumentalness, TEMPO) %>%
  summarize(Popularity = mean(popularity, na.rm = TRUE)) %>%
  ggplot(aes(x = instrumentalness, y = Popularity, colour = TEMPO)) +
  geom_point(size = 2, colour = "black") + geom_point(size = 2)


instrumTONE_mean <- newdata %>%
  group_by(instrumentalness, tone) %>%
  summarize(Popularity = mean(popularity, na.rm = TRUE)) %>%
  ggplot(aes(x = instrumentalness, y = Popularity, colour = tone)) +
  geom_point(size = 2, colour = "black") + geom_point(size = 2)


## energy
energyKEY_mean <- newdata %>%
  group_by(energy, KEY) %>%
  summarize(Popularity = mean(popularity, na.rm = TRUE)) %>%
  ggplot(aes(x = energy, y = Popularity, colour = KEY)) +
  geom_point(size = 2, colour = "black") + geom_point(size = 2)

energyTEMPO_mean <- newdata %>%
  group_by(energy, TEMPO) %>%
  summarize(Popularity = mean(popularity, na.rm = TRUE)) %>%
  ggplot(aes(x = energy, y = Popularity, colour = TEMPO)) +
  geom_point(size = 2, colour = "black") + geom_point(size = 2)


energyTONE_mean <- newdata %>%
  group_by(energy, tone) %>%
  summarize(Popularity = mean(popularity, na.rm = TRUE)) %>%
  ggplot(aes(x = energy, y = Popularity, colour = tone)) +
  geom_point(size = 2, colour = "black") + geom_point(size = 2)


## duration_ms
durationKEY_mean <- newdata %>%
  group_by(duration_ms, KEY) %>%
  summarize(Popularity = mean(popularity, na.rm = TRUE)) %>%
  ggplot(aes(x = duration_ms, y = Popularity, colour = KEY)) +
  geom_point(size = 2, colour = "black") + geom_point(size = 2)

durationTEMPO_mean <- newdata %>%
  group_by(duration_ms, TEMPO) %>%
  summarize(Popularity = mean(popularity, na.rm = TRUE)) %>%
  ggplot(aes(x = duration_ms, y = Popularity, colour = TEMPO)) +
  geom_point(size = 2, colour = "black") + geom_point(size = 2)


durationTONE_mean <- newdata %>%
  group_by(duration_ms, tone) %>%
  summarize(Popularity = mean(popularity, na.rm = TRUE)) %>%
  ggplot(aes(x = duration_ms, y = Popularity, colour = tone)) +
  geom_point(size = 2, colour = "black") + geom_point(size = 2)