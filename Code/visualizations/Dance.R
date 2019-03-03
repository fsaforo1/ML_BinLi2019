install.packages('magick')


library(tidyverse)
library(magick)



mini_thom <- image_read(str_glue('troll.gif'))


library(spotifyr)
library(tidyverse)

tots_thom <- map_df(c('Marshmellow', 'Drake', 'Travis Scott', 'Ariana Grande'), get_artist_audio_features)

non_studio_albums <- c('OK Computer OKNOTOK 1997 2017', 'TKOL RMX 1234567', 'In Rainbows Disk 2', 
                       'Com Lag: 2+2=5', 'I Might Be Wrong', 'The Eraser Rmxs')

tots_thom <- filter(tots_thom, !album_name %in% non_studio_albums) %>% 
  select(track_name, album_name, danceability, album_release_year) %>% 
  arrange(-danceability)

library(knitr)
library(kableExtra)
tots_thom %>% 
  kable() %>% 
  kable_styling() %>% 
  scroll_box(height = '500px')


library(ggridges)
library(lubridate)

# make label for plot
album_names_label <- tots_thom %>% 
  arrange(album_release_year) %>% 
  mutate(label = str_glue('{album_name} ({album_release_year})')) %>% 
  pull(label) %>% 
  unique

p <- ggplot(tots_thom, aes(x = danceability, y = as.character(album_release_year))) + 
  geom_density_ridges() +
  theme_ridges(center_axis_labels = TRUE, grid = FALSE, font_size = 6) +
  theme(plot.title = element_text(face = 'bold', size = 14, hjust = 1.25),
        plot.subtitle = element_text(size = 10, hjust = 1.1)) +
  ggtitle('Does danceability really matter?', 'Song Danceability by album - Marshmello, Drake, Travis Scott, DJ Snake, and Ariana Grande') +
  labs(x = 'Song danceability', y = '') +
  scale_x_continuous(breaks = c(0,.25,.5,.75,1)) +
  scale_y_discrete(labels = album_names_label)

ggsave(p, filename = str_glue('danceplot.png'), width = 5, height = 3)
background <- image_read(str_glue('danceplot.png'))
background

frames <- map(1:length(mini_thom), function(frame) {
  hjust <- 200+(100*frame) # <- this makes him move along the x axis
  image_composite(background, mini_thom[frame], offset = str_glue('+{hjust}+400'))
})


image_animate(image_join(frames), fps = 5, loop = 0)

library(rmarkdown); render('Play.Rmd')
