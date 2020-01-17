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

throughput <- data.frame(x = x, kpbs = c(runif(268, min=386.2, max=390.9), runif(21, min=370, max=380.2)))

plot_throughput <- ggplot(data=throughput, aes(y=kpbs, x=x))+
  geom_area(fill = "#CFD5EA", alpha=.3)+
  geom_path(color= "#4059AA", size=2, linejoin="bevel")+
  geom_hline(aes(yintercept = 360), linetype="dashed", color="#4059AA", size=1)+
  xlim(0, 289)+
  #ylim(350, 400)+
  #theme_void()+
  theme_bw()+
  #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.ticks.x=element_blank())+
  annotate("rect", xmin = 0, xmax = 289, ymin = 350, ymax = 400, fill="transparent", colour="#744ec6", alpha=0.1)

plot_throughput

ggsave('throug.pdf')

packet_loss <- data.frame(x = x, percentage = c(runif(278, min=0.0, max=5.8), runif(11, min=22.0, max=32.1)))

plot_packet_loss <- ggplot(data=packet_loss, aes(y=percentage, x=x))+
  geom_area(fill = "#CFD5EA", alpha=.3)+
  geom_path(color="#4059AA", size=2, linejoin="bevel")+
  geom_hline(aes(yintercept = 20), linetype="dashed", color="#4059AA", size=1)+
  xlim(0, 289)+
  theme_void()+
  #theme_bw()+
  #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.ticks.x=element_blank())+
  annotate("rect", xmin = 0, xmax = 289, ymin = 0, ymax = 35, fill="transparent", colour="#744ec6", alpha=0.1)

plot_packet_loss

latency <- data.frame(x = x, ms = runif(length(x), min=38, max=42))

plot_latency <- ggplot(data=latency, aes(y=ms, x=x))+
  geom_area(fill = "#CFD5EA", alpha=.3)+
  geom_path(color="#4059AA", size=2, linejoin="bevel")+
  geom_hline(aes(yintercept = 60), linetype="dashed", color="#4059AA", size=1)+
  xlim(0, 289)+
  ylim(0,80)+
  theme_void()+
  #theme_bw()+
  #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.ticks.x=element_blank())+
  annotate("rect", xmin = 0, xmax = length(x), ymin = 0, ymax = 80, fill="transparent", colour="#744ec6", alpha=0.1)

plot_latency

ggsave('latency.pdf')

#plot_packet_loss + 
 # stat_smooth(aes(x = x, y = percentage), method = "lm",
  #          formula = y ~ poly(x, 21), se = F)

# source: https://stackoverflow.com/questions/35205795/plotting-smooth-line-through-all-data-points
# spline_int <- as.data.frame(spline(packet_loss$x, packet_loss$percentage))

# plot_packet_loss +
#  geom_point(aes(x = x, y = percentage), size = 1) +
#  geom_line(data = spline_int, aes(x = x, y = percentage))
ggsave('packet_loss.pdf')
##plot(healthscores)
