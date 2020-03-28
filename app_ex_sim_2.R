library(maps)
library(tidyverse)

data(world.cities)
summary(world.cities)

cities.sample <- world.cities %>% filter(pop >= 1277104)
write.csv(cities.sample, './assets/world_sites.csv')