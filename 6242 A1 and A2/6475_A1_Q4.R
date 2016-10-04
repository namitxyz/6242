library(ggplot2)
library(reshape)
ComputeFactorialbyRecursion <- function(n)
{
  result = 0
  
  if(n == 1)
    return (log(n))
  else
    return(log(n) + ComputeFactorialbyRecursion(n-1))
}

ComputeFactorialbyIteration <- function(n)
{
  result = 0
  
  for (i in  seq(1,n, by = 1))
    result = result + log(i)
  
  return (result)
}

ComputeSumOfLogFactorial <- function(n, iFunctionType)
{
  result = 0
  
  for(i in seq(1,n, by = 1))
  {
    if(iFunctionType == 1)
      result = result + ComputeFactorialbyRecursion(i)
    else if(iFunctionType == 2)
      result = result + ComputeFactorialbyIteration(i)
    else if(iFunctionType == 3)
      result = result + lfactorial(i)
  }
  
  return (result)
}

PlotLatency<-function(n, bComplex)
{
  iter_vect <- vector(mode="numeric", length=n)
  rcsn_vect <- vector(mode="numeric", length=n)
  lfac_vect <- vector(mode="numeric", length=n)
  
  for(i in seq(1,n, by = 1))
  {
    if(bComplex)
    {
      rcsn_vect[i] = (system.time(ComputeSumOfLogFactorial(i, iFunctionType = 1))[3])
      iter_vect[i] = (system.time(ComputeSumOfLogFactorial(i, iFunctionType = 2))[3])
      lfac_vect[i] = (system.time(ComputeSumOfLogFactorial(i, iFunctionType = 3))[3])
    }
    else
    {
      rcsn_vect[i] = (system.time(ComputeFactorialbyRecursion(i))[3])
      iter_vect[i] = (system.time(ComputeFactorialbyIteration(i))[3])
      lfac_vect[i] = (system.time(lfactorial(i))[3])
    }
  }
  
  df <- data.frame(N = 1:n,
                   recursion = rcsn_vect,
                   iteration = iter_vect,
                   lfactorial = lfac_vect)

  df <- melt(df ,  id.vars = 'N', variable.name = 'series')

  g_1<-ggplot(df, aes(N,value)) + geom_line(aes(colour=variable))
  
  print(g_1)
}

PlotLatency(5000, 1)