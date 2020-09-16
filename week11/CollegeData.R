library(tidyverse)
library(aplpack)

tuition_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/tuition_cost.csv')


salary_potential <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/salary_potential.csv')



colleges <- left_join(tuition_cost, salary_potential, by= c("name"="name"))

states_colleges <- colleges %>%
  group_by(state_code)%>%
  summarize_at(c("in_state_tuition","in_state_total","out_of_state_tuition","out_of_state_total",
                 "early_career_pay","mid_career_pay","make_world_better_percent",
                 "stem_percent"),mean,na.rm=TRUE) %>%
  filter(!is.na(state_code))

faces(states_colleges[,2:5],labels=states_colleges$state_code,main="College In-State and Out-of-State Tuition by State")
         
faces(states_colleges[,6:9],labels=states_colleges$state_code,main="College Outcomes (Career Pay) by State")


