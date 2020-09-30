library(wordcloud)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(tidytext)
library(RColorBrewer)
library(imager)
library(ggimage)
library(gganimate)

options(scipen = 999)
taylor_swift_lyrics <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/taylor_swift_lyrics.csv")
sales <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/sales.csv")

sales[sales$title == "Folklore", "sales"] <- 1342473
sales[sales$title == "Folklore", "country"] <- "US"

sales <- sales %>%
  filter(country == "US", artist == "Taylor Swift") %>%
  arrange((title))


for (album in taylor_swift_lyrics$Album) {
  lyrics <- taylor_swift_lyrics %>%
    filter(Album == album) %>%
    select(Lyrics)

  lyrics <- lyrics %>%
    unnest_tokens(word, Lyrics) %>%
    count(word, sort = TRUE) %>%
    anti_join(stop_words) %>%
    filter(word != "ooh", word != "ha")


  fp <- file.path("~/tidytuesday/week40", paste0(album, ".png"))

  png(filename = fp, width = 2, height = 2, units = "in", res = 300)
  wordcloud(
    words = lyrics$word, freq = lyrics$n,
    max.words = 20, random.order = FALSE, rot.per = 0.35,
    colors = brewer.pal(3, "Dark2")
  )
  dev.off()
}

imgs <- data.frame(images = list.files("~/tidytuesday/week40"))

sales <- cbind(sales, imgs)


sales <- sales %>%
  mutate(yr = year(mdy(released)))

sales$title <- factor(sales$title, levels = c(
  "Taylor Swift",
  "Fearless",
  "Speak Now",
  "Red", "1989",
  "Reputation",
  "Lover",
  "Folklore"
))



plot <- sales %>%
  ggplot(aes(x = title, y = sales)) +
  geom_image(aes(image = file.path("~/tidytuesday/week40", paste0(images)), x = title, y = sales), size = .15) +
  theme_bw() +
  theme(
    panel.border = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")
  ) + 
  labs(title = "Most Common Words in Taylor Swift's Albums by Album Sales",
       x= "Album Name",
       y= "Number of Sales",
       subtitle = "Week 40 #TidyTuesday") +
  transition_states(title, wrap = FALSE) +
  shadow_mark() +
  enter_grow() +
  enter_fade() 


animate(plot)
anim_save("taylorswift.gif", animation = last_animation())
