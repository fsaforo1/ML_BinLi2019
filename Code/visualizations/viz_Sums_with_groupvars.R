library(dplyr)
library(plotly)

danceKEY_sum <- newdata %>%
  group_by(danceability, KEY) %>%
  summarize(Popularity = sum(popularity, na.rm = TRUE)) %>%
  ggplot(aes(x = danceability, y = Popularity, colour = KEY)) +
  geom_point(size = 2, colour = "black") + geom_point(size = 2)


danceTEMPO_sum <- newdata %>%
  group_by(danceability, TEMPO) %>%
  summarize(Popularity = sum(popularity, na.rm = TRUE)) %>%
  ggplot(aes(x = danceability, y = Popularity, colour = TEMPO)) +
  geom_point(size = 2, colour = "black") + geom_point(size = 2)


danceTONE_sum <- newdata %>%
  group_by(danceability, tone) %>%
  summarize(Popularity = sum(popularity, na.rm = TRUE)) %>%
  ggplot(aes(x = danceability, y = Popularity, colour = tone)) +
  geom_point(size = 2, colour = "black") + geom_point(size = 2)

## Loudness
loudKEY_sum <- newdata %>%
  group_by(loudness, KEY) %>%
  summarize(Popularity = sum(popularity, na.rm = TRUE)) %>%
  ggplot(aes(x = loudness, y = Popularity, colour = KEY)) +
  geom_point(size = 2, colour = "black") + geom_point(size = 2)

loudTEMPO_sum <- newdata %>%
  group_by(loudness, TEMPO) %>%
  summarize(Popularity = sum(popularity, na.rm = TRUE)) %>%
  ggplot(aes(x = loudness, y = Popularity, colour = TEMPO)) +
  geom_point(size = 2, colour = "black") + geom_point(size = 2)


loudTONE_sum <- newdata %>%
  group_by(loudness, tone) %>%
  summarize(Popularity = sum(popularity, na.rm = TRUE)) %>%
  ggplot(aes(x = loudness, y = Popularity, colour = tone)) +
  geom_point(size = 2, colour = "black") + geom_point(size = 2)


## Valence
valKEY_sum <- newdata %>%
  group_by(valence, KEY) %>%
  summarize(Popularity = sum(popularity, na.rm = TRUE)) %>%
  ggplot(aes(x = valence, y = Popularity, colour = KEY)) +
  geom_point(size = 2, colour = "black") + geom_point(size = 2)

valTEMPO_sum <- newdata %>%
  group_by(valence, TEMPO) %>%
  summarize(Popularity = sum(popularity, na.rm = TRUE)) %>%
  ggplot(aes(x = valence, y = Popularity, colour = TEMPO)) +
  geom_point(size = 2, colour = "black") + geom_point(size = 2)


valTONE_sum <- newdata %>%
  group_by(valence, tone) %>%
  summarize(Popularity = sum(popularity, na.rm = TRUE)) %>%
  ggplot(aes(x = valence, y = Popularity, colour = tone)) +
  geom_point(size = 2, colour = "black") + geom_point(size = 2)



## instrumentalness
instrumKEY_sum <- newdata %>%
  group_by(instrumentalness, KEY) %>%
  summarize(Popularity = sum(popularity, na.rm = TRUE)) %>%
  ggplot(aes(x = instrumentalness, y = Popularity, colour = KEY)) +
  geom_point(size = 2, colour = "black") + geom_point(size = 2)

instrumTEMPO_sum <- newdata %>%
  group_by(instrumentalness, TEMPO) %>%
  summarize(Popularity = sum(popularity, na.rm = TRUE)) %>%
  ggplot(aes(x = instrumentalness, y = Popularity, colour = TEMPO)) +
  geom_point(size = 2, colour = "black") + geom_point(size = 2)


instrumTONE_sum <- newdata %>%
  group_by(instrumentalness, tone) %>%
  summarize(Popularity = sum(popularity, na.rm = TRUE)) %>%
  ggplot(aes(x = instrumentalness, y = Popularity, colour = tone)) +
  geom_point(size = 2, colour = "black") + geom_point(size = 2)


## energy
energyKEY_sum <- newdata %>%
  group_by(energy, KEY) %>%
  summarize(Popularity = sum(popularity, na.rm = TRUE)) %>%
  ggplot(aes(x = energy, y = Popularity, colour = KEY)) +
  geom_point(size = 2, colour = "black") + geom_point(size = 2)

energyTEMPO_sum <- newdata %>%
  group_by(energy, TEMPO) %>%
  summarize(Popularity = sum(popularity, na.rm = TRUE)) %>%
  ggplot(aes(x = energy, y = Popularity, colour = TEMPO)) +
  geom_point(size = 2, colour = "black") + geom_point(size = 2)


energyTONE_sum <- newdata %>%
  group_by(energy, tone) %>%
  summarize(Popularity = sum(popularity, na.rm = TRUE)) %>%
  ggplot(aes(x = energy, y = Popularity, colour = tone)) +
  geom_point(size = 2, colour = "black") + geom_point(size = 2)


## duration_ms
durationKEY_sum <- newdata %>%
  group_by(duration_ms, KEY) %>%
  summarize(Popularity = sum(popularity, na.rm = TRUE)) %>%
  ggplot(aes(x = duration_ms, y = Popularity, colour = KEY)) +
  geom_point(size = 2, colour = "black") + geom_point(size = 2)

durationTEMPO_sum <- newdata %>%
  group_by(duration_ms, TEMPO) %>%
  summarize(Popularity = sum(popularity, na.rm = TRUE)) %>%
  ggplot(aes(x = duration_ms, y = Popularity, colour = TEMPO)) +
  geom_point(size = 2, colour = "black") + geom_point(size = 2)


durationTONE_sum <- newdata %>%
  group_by(duration_ms, tone) %>%
  summarize(Popularity = sum(popularity, na.rm = TRUE)) %>%
  ggplot(aes(x = duration_ms, y = Popularity, colour = tone)) +
  geom_point(size = 2, colour = "black") + geom_point(size = 2)
