install.packages("wesanderson")
install.packages('zoo')
library(wesanderson)
library(gridExtra)
library(dplyr)
library(statsr)
library(ggplot2)
library(purrr)
library(zoo)
library(scales)
install.packages('svglite')
library(svglite)

# Shared functions
randomNumbers <- function(len, mean=0, sd=1) {
  len <- round(len/2)*2
  v <- rnorm(len/2, mean = mean, sd = sd)
  neg <- v %>% map(function(x) -x)
  randomNumbers <- sample(c(v, unlist(neg)))
}

pctChange <- function(x) {
  return (lead(x)/x - 1) * 100
}

# Set up aesthetics
wes_colors <- wes_palette(name="BottleRocket1")
color_primary = wes_colors[7]
color_secondary = wes_colors[1]
#font_family <- X11Font("-cronyx-helvetica-%s-%s-*-*-%d-*-*-*-*-*-*-*")
font_family <- X11Font("-monotype-arial-%s-%s-*-*-%d-*-*-*-*-*-*-*")

time = c(1:298)
len = length(time)

da <- rnorm(len, mean=40,sd=2)
# Get an intuition of what percentage change looks like
demo <- ts(da, start=c(2019,1), deltat = 1/12)
demo <- data.frame(y=as.matrix(demo), date=as.Date(as.yearmon(time(demo))))
demo <- demo %>% mutate(pct_change = pctChange(y))
demo <- demo %>% mutate(pct_change = lag(pct_change))
demo <- demo %>% mutate(pct_change_label = percent(pct_change))

p_demo <- ggplot(data=demo, aes(x=date, y=y))+
  geom_point(color=color_secondary)+
  geom_line(aes(x=date, y=y), color=color_secondary)+
  geom_smooth(method = lm)+
  theme_void()
  # geom_point(aes(y=pct_change), color=color_primary)+
  # geom_line(aes(y=pct_change), color=color_primary)+
  # geom_hline(yintercept = 0, linetype="dashed", color=color_secondary)+
  # theme(legend.position = "none")+
  # ggtitle("Percentage Change Over Time")+
  # annotate(geom="point", x=as.Date("2019-08-01"), y=Inf, size=6, shape=21, fill="transparent", color=color_primary)+
  # geom_text(label=demo$y, vjust=2, color=color_secondary)+
  # geom_text(data=subset(demo, select=c(date, pct_change, pct_change_label)), aes(x=date, y=pct_change, label=pct_change_label, vjust=2), color=color_primary)

p_demo

