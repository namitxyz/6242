library(ggplot2)
library(reshape)
library(plyr)
Plot<-function(n)
{
  df <- data.frame(Class = mpg$class,
                   City_Mileage = mpg$cty,
                   Highway_Milage = mpg$hwy)
  
  df <- melt(df ,  id.vars = 'Class')
  
  g_1<-ggplot(df, aes(Class,value)) + geom_boxplot(aes(colour=variable)) +  ggtitle("BoxPlot: Highway mpg, City mpg and Class")
  print(g_1)
}

Plot()
