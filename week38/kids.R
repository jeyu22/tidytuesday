library(geofacet)
library(tidyverse)
library(ggplot2)
library(hrbrthemes)

kids <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-15/kids.csv")

kids <- kids %>%
  mutate(variable = str_replace(kids$variable, "other_health", "health"))

kids <- kids %>%
  filter(variable == "health" | variable == "PK12ed")

ggplot(kids, aes(year, inf_adj_perchild, color = variable)) +
  geom_line() +
  facet_geo(~state) +
  xlab("Year(2000-2016)") +
  ylab("Amount Spent Per Child ($)") +
  theme_bw() +
  labs(
    title = "PK-12 and Healthcare Spending for Kids per State",
    subtitle = "Data from Urban Institute; Healthcare Excludes Medicare Spending ",
    color = "Variable"
  ) +
  theme(plot.background = element_rect(fill = "lightblue"))
