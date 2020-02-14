library(dplyr, warn.conflicts = FALSE)
library(ggplot2)

# Source: Dave Childers 1/23/20, 11:16 AM
# set the random seed for reproducibility
set.seed(999)
d <- tibble(
  # 'time' access -- can use a date sequence instead
  t = 1:100,
  # each observation has a 2% chance of being a jump
  # this is a parameter that can be varied
  is_jump = runif(100) < 0.02,
  # jumps are chosen from uniform distribution b/w -10 and 10
  jump_value = runif(n = 100, min = -10, max = 10),
  jump = ifelse(is_jump, jump_value, 0),
  # this is the 'mean' function w/o the noise
  mean = 390 + cumsum(jump) ,
  # add in some random noise
  noise = runif(n = 100, min = -2, max = 2),
  # this is the distribution
  obs = mean + noise
) %>% 
  select(t, mean, obs) %>%
  print

# plot the mean function, shows the jumps
ggplot(d, aes(x = t, y = mean)) +
  geom_line() +
  scale_y_continuous(limits = c(350, 410))

# plot the simulated series
ggplot(d, aes(x = t, y = obs)) +
  geom_line() +
  scale_y_continuous(limits = c(350, 410))
