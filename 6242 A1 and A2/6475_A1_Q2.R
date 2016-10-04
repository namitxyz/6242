ComputeFactorialbyIteration <- function(n)
{
  result = 0
  
  for (i in  seq(1,n, by = 1))
    result = result + log(i)
  
  return (result)
}


print(ComputeFactorialbyIteration(40))