# Generate a list of application health by sites (west and east)
library(statsr)
library(dplyr)
library(ggplot2)
library(jsonlite)
score = c(
    # west coast
    sample(1:3, 4, replace = T),
    sample(4:7, 80, replace = T),
    sample(8:10, 20, replace = T),
    rep(NA, 8),
    # east coast
    sample(1:3, 2, replace = T),
    sample(4:7, 42, replace = T),
    sample(8:10, 21, replace = T),
    rep(NA, 4)
  )

coast = c(rep("west", 112), rep("east", 69))

health_by_site <- data.frame(score, coast)

write.csv(health_by_site, 'assets/health_by_site.csv')