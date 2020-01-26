install.packages("wesanderson")
library(wesanderson)
library(gridExtra)
library(dplyr)
#library(purrr)
# Explore different statistics for describing changes of two time series, for example +20% from TA -> TB means .2 increase TA -> TB.

randomNumbers <- function(len, mean=0, sd=1) {
  len <- round(len/2)*2
  v <- rnorm(len/2, mean = mean, sd = sd)
  neg <- v %>% map(function(x) -x)
  randomNumbers <- sample(c(v, unlist(neg)))
}

pctChange <- function(x) {
  return (lead(x)/x - 1) * 100
}

wes_colors <- wes_palette(name="BottleRocket2")
#font_family <- X11Font("-cronyx-helvetica-%s-%s-*-*-%d-*-*-*-*-*-*-*")
font_family <- X11Font("-monotype-arial-%s-%s-*-*-%d-*-*-*-*-*-*-*")

# Get an intuition of what percentage change looks like
demo <- ts(c(-6:5), start=c(2019,1), deltat = 1/12)
demo <- data.frame(y=as.matrix(demo), date=time(demo))
demo <- demo %>% mutate(pct_change = pctChange(y))
demo <- demo %>% mutate(pct_change = lag(pct_change))
# Manually "shift" pct_change so it aligns with the delta
demo <- demo %>% mutate(pct_change = head(append(0, demo$pct_change), -1))

p_demo <- ggplot(data=demo, aes(x=date, y=y))+
  geom_point(color=wes_colors[5])+
  geom_line(aes(x=date, y=y), color=wes_colors[5])+
  geom_point(aes(y=pct_change, color=wes_colors[3]))+
  geom_line(aes(y=pct_change, color=wes_colors[3]))

p_demo

# Generate some data sets with the same mean (0) but variations
len = round(289/2)*2
a <- randomNumbers(len, mean=0, sd=.1)
b <- randomNumbers(len, mean=0, sd=6)
c <- sort(b)
da <- data.frame(x=1:len, y=a, pct_change=pctChange(a))
db <- data.frame(x=1:len, y=b, pct_change=pctChange(b))
dc <- data.frame(x=1:len, y=c, pct_change=pctChange(c))

# Calculate consecutive delta

# Plot charts

limit_y <- c(min(a, b) %>% -2 %>% round, 
             max(a, b) %>% +3 %>% round)

p_a <- ggplot(da)+
  geom_line(aes(x=x, y=y), color=wes_colors[2])+
  #geom_hline(yintercept = mean(da$y))+
  ylim(limit_y)
#  theme(text=element_text(size=16,  family="serif")))

p_b <- ggplot(db)+
  geom_line(aes(x=x, y=y), color=wes_colors[3])+
  #geom_hline(yintercept = mean(db$y))+
  ylim(limit_y)

p_c <- ggplot(dc)+
  geom_line(aes(x=x, y=y), color=wes_colors[4])+
  #geom_hline(yintercept = mean(dc$y))+
  ylim(limit_y)

p_da <- ggplot(da)+
  geom_line(aes(x=x, y=pct_change), color=wes_colors[2])

p_db <- ggplot(db)+
  geom_line(aes(x=x, y=pct_change), color=wes_colors[3])

p_dc <- ggplot(dc)+
  geom_line(aes(x=x, y=pct_change), color=wes_colors[4])

grid.arrange(p_a, p_b, p_c, 
             p_da, p_db, p_dc, 
             nrow=2, ncol=3, top="Three Datasets Each with Mean=0")

