ComputeFactorialbyRecursion <- function(n)
{
  result = 0
  
  if(n == 1)
    return (log(n))
  else
    return(log(n) + ComputeFactorialbyRecursion(n-1))
}


print(ComputeFactorialbyRecursion(40))