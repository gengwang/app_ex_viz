library(statsr)
library(dplyr)
library(ggplot2)
library(purrr)
# library(scales)
# library(xts)
library(jsonlite)

# Sample size we need for our viz: sample very 5 min in two days
score_range <- factor(c(1:10), ordered = TRUE)
t <- seq(ISOdatetime(2020,1,30,0,0,0), ISOdatetime(2020,2,1,0,0,0), by=(60*5))
sample.size_total <- length(t)
score <-  sample(1:10, sample.size_total, replace = T)
health_o_t <- data.frame(score = score, time = t)
health_o_t <- health_o_t %>% mutate(class = score %>% bin_10_to_3())
health_o_t <- health_o_t[c('time', 'score', 'class')]

# shatter an interger into several integers so that the sum of which equals the original number
# for example, shatter(577) may return c(190, 204, 183) 
shatter <- function(x, size=3) {
  shatter <- sample(1:round(size), round(x), replace = T) %>% factor() %>%  summary()
}
# generate a number of size of random integers, starting with start. This is a "smoother" alternative of sample function
ran_smoother <- function(start=5, size=10, range=2) {
  # 1:3 %>% reduce(pile) will produce c(1, 3, 6)
  pile <- function(acc, d) {
    last <- acc %>% last() + d
    acc <- acc %>% append(last)
  }
  s <- sample((-1*range):range, size, replace=T)
  ran_smoother <- s %>% reduce(pile) + start
}

bin_10_to_3 <- function(x) {
  case_when(
    x <= 4 ~ "Poor",
    x <= 7 & x >4 ~ "Fair",
    x > 7 ~ "Good"
  )
}

# manually generate some health scores for 2 days in an interval of 5 mins
n <- 24
h <- ran_smoother(size=n) %>% sapply(min, max(score_range)) %>% sapply(max, min(score_range))
s <- shatter(sample.size_total, n)
h_s <- data.frame(health = h, size=s)
make_fregments <- function(x, out) {
  out <- rep(x[1], x[2])
}

sample.health_scores <- apply(h_s, 1, make_fregments) %>% unlist()
sample.health_scores <- data.frame( time = t, score = sample.health_scores)
sample.health_scores <- sample.health_scores %>% mutate(cls = bin_10_to_3(score))
write.csv(sample.health_scores, './assets/001.csv', append = T)
write.csv(sample.health_scores, './assets/health_scores.csv')

# 12 clients by healh score (1:10)
sample2.clients_by_health_scores <- data.frame(index=1:12, score=1:10 %>% sample(12, replace=T))
sample2.clients_by_health_scores$score <-  c(6, 1, 1, 3, 7, 9, 5, 4, 9, 7, 1, 4)
sample2.clients_by_health_scores$score %>% table()

# # # # # # # # # # # # # # # # # # # # # # # 
# visualization
sample2.health_scores <- read.csv('./assets/health_scores_1.csv', stringsAsFactors = F)
sample2.health_scores <- sample2.health_scores %>% mutate(time = time %>% as.POSIXct())
p_score <- ggplot(data=sample2.health_scores, aes(x=time, y=score))+
  geom_line()
ggplot(data=sample2.health_scores, aes(x=time, y=cls))+
  geom_point()
# clients by health scores
ggplot(sample2.clients_by_health_scores, aes(x=score))+
       geom_histogram(bins = 10)
ggsave('001.pdf',plot = last_plot(), device = NULL, path = NULL,
       scale = 1, width = 248, height = 74, units = c("px"),
       dpi = 300, limitsize = TRUE)

json_health_o_t <- health_o_t %>% head(30) %>% jsonlite::toJSON()
cat(json_health_o_t)

