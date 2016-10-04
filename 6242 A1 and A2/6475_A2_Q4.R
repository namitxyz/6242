library(ggplot2)
library(reshape)
library(plyr)
Plot<-function(n)
{
  n = 2000
  df <- data.frame(N = 1:n,
                   Random_Variable_1 = runif(n, 1, 1000),
                   Random_Variable_2 = runif(n, 1300, 2300))

  df <- melt(df ,  id.vars = 'N')

  g_1<-ggplot(df, aes(N,value)) + geom_point(aes(colour=variable, shape = factor(variable))) +  ggtitle("Scatter plot on two random variables")
  print(g_1)
}

Plot()
