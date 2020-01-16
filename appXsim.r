library(statsr)
library(dplyr)
library(ggplot2)
## source: https://stackoverflow.com/questions/25775375/create-a-24-hour-vector-with-5-minutes-time-interval-in-r

## score <- replace(sample(8:10, 289, replace=T), 278:289, sample(1:3, 12, replace=T))
x <- 1:289
score <- c(rep(9, 278), rep(2, 11)) 
healthscores <- data.frame(time = format( seq.POSIXt(as.POSIXct(Sys.Date()), as.POSIXct(Sys.Date()+1), 
                                                     by = "5 min"),"%H%M", tz="GMT"),
                           x = x,
                           score = score)

ggplot(data=healthscores, aes(y=score, x=x))+
    geom_line()

packet_loss <- data.frame(x = x, percentage = c(runif(278, min=0.0, max=5.8), runif(11, min=22.0, max=32.1)))

plot_packet_loss <- ggplot(data=packet_loss, aes(y=percentage, x=x))+
  geom_line()
  
plot_packet_loss + 
  stat_smooth(aes(x = x, y = percentage), method = "lm",
            formula = y ~ poly(x, 21), se = F)

# source: https://stackoverflow.com/questions/35205795/plotting-smooth-line-through-all-data-points
# spline_int <- as.data.frame(spline(packet_loss$x, packet_loss$percentage))

# plot_packet_loss +
#  geom_point(aes(x = x, y = percentage), size = 1) +
#  geom_line(data = spline_int, aes(x = x, y = percentage))
ggsave('packet_loss.pdf')
##plot(healthscores)
