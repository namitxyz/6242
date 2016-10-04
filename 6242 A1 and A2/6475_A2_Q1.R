library(ggplot2)
library(reshape)
library(plyr)
Plot<-function(n)
{
   g_1<-ggplot(mpg, aes(manufacturer, hwy)) + geom_boxplot() + ggtitle("Box plot of highway mileage and manufacturer")
   print(g_1)
   
   g_1<-ggplot(mpg, aes(hwy, manufacturer)) + geom_point(aes(color=(hwy), size=hwy)) +  ggtitle("Absolute Highway Mileage and Manufacturer")
   print(g_1)
   
   df<-(ddply(mpg, .(manufacturer), summarize,  Average_Mileage=mean(hwy)))
   g_2<-ggplot(df, aes(Average_Mileage,manufacturer)) + geom_point(aes(size = Average_Mileage)) +  ggtitle("Average Highway Mileage and Manufacturer")
   print(g_2)
}

Plot()
