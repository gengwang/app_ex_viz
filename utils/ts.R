library(tidyverse)
library(jsonlite)

# Return a seq of POSIXt objects (so we can use it in a ts)
# @start, @end: date time
# @by number in seconds
# For example, to sample very 5 min in two days  
# t <- ts_dt_data(start = ISOdatetime(2020,1,30,0,0,0), end = ISOdatetime(2020,2,1,0,0,0), by=(60*5))
# sample.size_total <- length(t)
ts_dt_data <- function(start, end, by) {
  seq(ISOdatetime(2020,1,30,0,0,0), ISOdatetime(2020,2,1,0,0,0), by)
}

# Return a sequence of numbers ranging from 1 ~ 10
seq_health_scores <- function(size = 24, start = 5, range = 2) {
  score_range <- factor(c(1:10), ordered = TRUE)
  ran_smoother(size=size, start = start, range = range) %>% sapply(min, max(score_range)) %>% sapply(max, min(score_range))
}

# Generate @size of random integers, starting with @start. This is a "smoother" alternative of sample function
# TODO: alternative way is to use a for-in loop to offset the prvious value by a random choice of -1 or 1
ran_smoother <- function(size=10, start=5, range=2) {
  # 1:3 %>% reduce(pile) will produce c(1, 3, 6)
  pile <- function(acc, d) {
    last <- acc %>% last() + d
    acc <- acc %>% append(last)
  }
  s <- sample((-1*range):range, size, replace=T)
  ran_smoother <- s %>% reduce(pile) + start
}

# Break an interger into a vector of several integers so that the sum of the latter equals the former
# for example, shatter(577) may return c(190, 204, 183) 
shatter <- function(x, size=3) {
  shatter <- sample(1:round(size), round(x), replace = T) %>% factor() %>%  summary()
}

# Return a seq of obs when w/ random jumps.
# Source: Dave Childers 1/23/20, 11:16 AM
ran_jump <- function(size = 100, mean = 390, jump = 10, noise = 2, variance = 0.02, seed) {
  # set the random seed for reproducibility
  # seed = 999
  if(!missing(seed)) {
    set.seed(seed)
  }
  t <- tibble(
    # 'time' access -- can use a date sequence instead
    t = 1:size,
    # each observation has a 2% chance of being a jump
    # this is a parameter that can be varied
    is_jump = runif(size) < variance,
    # jumps are chosen from uniform distribution b/w -@jumpand @jump
    jump_value = runif(n = size, min = -jump, max = jump),
    jump = ifelse(is_jump, jump_value, 0),
    # this is the 'mean' function w/o the noise
    mean = mean + cumsum(jump) ,
    # add in some random noise
    noise = runif(n = size, min = -noise, max = noise),
    # this is the distribution
    obs = mean + noise
  )
  t$obs
}

# generator: sample.health_scores <- sample.health_scores %>% mutate(cls = bin_10_to_3(score))
bin_10_to_3 <- function(x) {
  case_when(
    x <= 4 ~ "Poor",
    x <= 7 & x >4 ~ "Fair",
    x > 7 ~ "Good"
  )
}

# Plot a sequence
# TODO: Plot multiple line seq
plot_seq <- function(v) {
  ggplot(data = tibble(x=1:length(v), y = v), aes(x=x, y=y))+
    geom_line()
}

# Save an object to JSON file on disk
# save_json_file(qual_kpis, './assets/001.json')
save_json_file <- function(obj, path) {
  obj %>% toJSON() %>% write(path)
}