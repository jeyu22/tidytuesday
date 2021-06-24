library(png)
library(grid)
library(tidyverse)
library(tidytext)
library(ggwordcloud)
library(wordcloud)

friends <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends.csv')
friends_info <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends_info.csv')


top10 <- friends_info %>%
  arrange(desc(imdb_rating)) %>%
  group_by(season) %>%
  slice(1) %>%
  mutate(year = lubridate::year(air_date),
         sep = paste(season,"-",episode))

chandler <- friends %>%
  mutate(sep = paste(season,"-",episode))%>%
  filter(speaker == "Chandler Bing", sep %in% top10$sep)

lines <-chandler %>%
  group_by(sep) %>%
  sample_n(10)

ds <- friends %>%
  filter(speaker == "Chandler Bing") %>%
  unnest_tokens(ngram, text, token = "ngrams", n = 1) %>%
  anti_join(stop_words, by = c("ngram" = "word")) %>%
  count(season, ngram, sort = TRUE) %>%
  filter(!is.na(ngram)) 


ggplot(lines, aes(label = text), size = 5) +
  geom_text_wordcloud() +
  facet_wrap(~sep )

img <- readPNG(system.file("img", "Rlogo.png", package="png"))
g <- rasterGrob(img, interpolate=TRUE)

qplot(1:10, 1:10, geom="blank") +
  annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
  geom_point()






ggplot(top10, aes(x=year, y = 0, col=title)) +
  geom_point(aes(y = 0),size = 10)  + theme(axis.line.y=element_blank(),
                                          axis.text.y=element_blank(),
                                          axis.title.x=element_blank(),
                                          axis.title.y=element_blank(),
                                          axis.ticks.y=element_blank(),
                                          #axis.text.x =element_blank(),
                                          axis.ticks.x =element_blank(),
                                          axis.line.x =element_blank(),
                                          legend.title = element_blank(),
                                          plot.title = element_text(hjust = 0.5),
                                          legend.position = "none",
                                          panel.border = element_blank(),
                                          panel.grid.major = element_blank(),
                                          panel.grid.minor = element_blank(),
                                          plot.background = element_rect(fill = "#9C8CD4"),
                                          panel.background = element_rect(fill = "#9C8CD4",
                                                                          colour = "#9C8CD4",
                                                                          size = 0.5, linetype = "solid")  ) 

  