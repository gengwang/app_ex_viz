library(tidyverse)
library(purrr)
library(maps)
# library(scales)
# library(xts)
library(jsonlite)

cities.world <- dput(world.cities) %>% select(name, country.etc, lat, long) %>% filter(name %in% c("Chicago", "New York", "San Jose", "London", "Seattle", "San Francisco", "Houston")) %>% filter(country.etc %in% c("USA", "UK"))

write.csv(cities.world, file = './assets/6cites.csv')
