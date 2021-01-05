library(dplyr)
library(tidyr)
library(ggplot2)

transit_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-05/transit_cost.csv')


summary(transit_cost)
glimpse(transit_cost)


sum(is.na(transit_cost))


#502 are not railrold, only 34 are railrold
transit_cost %>%
 replace_na(list(rr = 0)) %>%
  count(rr , sort = TRUE) %>%


#explore length variable
#length is right skewed
#most of lines are < 50 km
transit_cost %>%
  filter(length < 4000) %>%
   ggplot(aes(length)) +
   geom_histogram(color = 'green')


#there are line that have > 100 km length, I want to explore to what cities do they belong
#these cities has the longest lines in this dataset: Paris, Delhi, Chennai, Jeddah,Mecca, Doha and kuwait
transit_cost %>%
  select(city, length) %>%
  filter(length > 100)


#usually there is less than 10 stations for each tunnel, some have > 50
transit_cost %>%
  count(stations, sort = TRUE) %>%
  ggplot(aes(stations, n))+
  geom_point(color = 'red')


#some projects have longer tunnels but less stations 
transit_cost %>%
  select(city, length, stations) %>%
  group_by(length, stations) %>%
  count(sort = TRUE) %>%
  filter(stations > 50)


summary(transit_cost$real_cost)

cost_table <- transit_cost %>%
  count(real_cost, sort = TRUE)

#remove last 7 rows because of them are NA's and wrong values
transit_cost <- transit_cost[1:537, ]


