install.packages('fmsb')
library(fmsb)
library(ggplot2)

# quartiles <- c(1,1,1,4,2,1)
quartiles <- c(0.19*4, 0.18*4, 0.20*4, 0.99*4, 0.49*4, 0.24*4)
data <- as.data.frame(matrix( quartiles , ncol=6))
colnames(data) <- c( "Throughput", 
                     "Client Count", 
                     "Packet Loss", 
                     "Jitter", 
                     "Network Latency", 
                     "App Server Latency")
# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
data <- rbind(rep(5,6) , rep(0,6) , data)
# Check your data, it has to look like this!
head(data)
# The default radar chart 
radarchart(data,
           axistype=1,
           #custom polygon
           pcol=rgb(0/255, 128/255, 148/255, 1.0) , pfcol=rgb(6/255, 96/255, 112/255, 0.3) , plwd=4 , 
           # custom the grid
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=c("","Q1","Q2","Q3","Q4"), cglwd=0.6,
           #custom labels
           vlcex=1,
           # title
           title="Quartiles by Application Performance Indicator",
           cex.main = 1
           )
mtext("DNAC observes higher than usual jitter and network latency.", side = 1)
# mtext("Quartiles by Application Performance Indicator", side = 2)
