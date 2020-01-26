library(gridExtra)

tmp_1 <- pnorm(100. mean=0, sd=1)
tmp_2 <- pnorm(100. mean=0, sd=2)

p1 <- ggplot(tmp_1, aes(x=y))+
  coord_cartesian(xlim=c(-7,7), ylim=c(0,30))+
  geom_histogram(alpha=0.2, fill="white", color="red", binwidth=0.3)+
  theme(legend.position = "none")

p2 <- ggplot(tmp_2, aes(x=y))+
  coord_cartesian(xlim=c(-7,7), ylim=c(0,30))+
  geom_histogram(alpha=0.2, fill="white", color="blue", binwidth=0.3)+
  theme(legend.position = "none")


grid.arrange(p1 + geom_vline(xintercept = mean(tmp_1$y), linetype="dashed"), p2, nrow=2)


p_line_1 <- ggplot(d1)+
       geom_line(aes(x=x, y=y), color="red")+
       ylim(-6,6)+
       geom_smooth(aes(y=y,x=x),color="red")

p_line_2 <- ggplot(d2)+
  geom_line(aes(x=x, y=y), color="blue")+
  ylim(-6,6)+
  geom_smooth(aes(y=y,x=x))

grid.arrange(p_line_1, p_line_2, nrow=2)

