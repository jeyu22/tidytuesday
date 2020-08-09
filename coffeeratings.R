
library(tidyverse)
library(baguette)
library(hrbrthemes)
library(cowplot)
library(rpart)
library(rpart.plot)


coffee <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-07/coffee_ratings.csv')

coffee <- coffee %>%
  filter(total_cup_points != 0.00)
 
method <- ggplot(coffee %>% filter(!is.na(processing_method)), aes(x=processing_method, y=total_cup_points, fill=processing_method))+
   geom_violin()+
   labs(x="Processing Method", y="Total Cup Points", 
        title = "Processing Method", fill = "Method")+
   theme_ft_rc()

 
acidity <- ggplot(coffee, aes(x=acidity, y = total_cup_points))+geom_point()+
   labs(x="Acidity", y="Total Cup Points", 
        title = "Acidity")+
   theme_ft_rc()
 
sweet <- ggplot(coffee, aes(x=sweetness, y = total_cup_points))+geom_point()+
  labs(x="Sweetness", y="Total Cup Points", 
       title = "Sweetness")+
  theme_ft_rc()

species <- ggplot(coffee %>% group_by(species) %>% summarise(avg_cup = mean(total_cup_points)),
                  aes(x=species, y=avg_cup, fill=species))+
  geom_bar(stat="identity")+
  labs(x="Variety", y="Total Cup Points", 
       title = "Species", fill = "Species")+
  theme_ft_rc()
species


plot_grid(method, acidity, sweet, species)


##### extra modeling for 'fun'

bag_coffee <- bag_tree() %>% 
  set_engine("rpart", times = 100) %>% # 25 ensemble members 
  set_mode("regression") %>% 
  fit(total_cup_points ~ species + country_of_origin + processing_method + aroma + flavor + aftertaste +
        acidity+ body + balance + uniformity + clean_cup + sweetness + cupper_points + moisture + altitude_mean_meters,
      data    = coffee)

bag_coffee

coffee_tree <- rpart(
  formula = total_cup_points ~ species + country_of_origin + processing_method + aroma + flavor + aftertaste +
    acidity+ body + balance + uniformity + clean_cup + sweetness + cupper_points + moisture + altitude_mean_meters,
  data    = coffee,
  method  = "anova"
)

coffee_tree
rpart.plot(coffee_tree) 
