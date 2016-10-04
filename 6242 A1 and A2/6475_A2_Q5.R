library(ggplot2)
library(reshape)
library(plyr)
Plot<-function()
{
  g_1<-ggplot(diamonds, aes(color)) + geom_bar(col="red", 
                                               fill="green", 
                                               alpha = .05)  + ggtitle("Histogram of color for diamonds dataset")
  print(g_1)
  
  g_1<-ggplot(diamonds, aes(carat)) + geom_histogram(col="red", 
                                                     fill="green", 
                                                     alpha = .5,
                                                     binwidth =.2)  + ggtitle("Histogram of carat for diamonds dataset")
  print(g_1)
  
   g_1<-ggplot(diamonds, aes(price)) + geom_histogram(col="red", 
                                                      fill="green", 
                                                      alpha = .5,
                                                      binwidth =400) + ggtitle("Histogram of price for diamonds dataset")
   print(g_1)
   
   dsmall <- diamonds[sample(nrow(diamonds), 100), ]
   g_1<-ggplot(dsmall, aes(carat, price)) + geom_point(aes(shape=cut, colour = cut)) + geom_smooth(span=0.2) + ggtitle("Relationship btwn price/carat/cut, 100 sample")
   print(g_1)
   
   g_1<-ggplot(diamonds, aes(carat, price)) + geom_point(aes(shape=cut)) + geom_smooth(span=0.2) + ggtitle("Three way relationship: price/carat/cut")
   print(g_1)
}

Plot()
