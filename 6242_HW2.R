HypothesisFunction<-function(x, theta)
{
  H_x = 1/(1 + exp(-1*(x%*%theta)))
  return(H_x)
}

gradDescent<-function(x, y, theta, alpha, num_iterations)
{
  m <- length(x)
  J <- rep(0, num_iterations)
  
  for(i in 1:num_iterations)
  {
    theta <- theta - alpha*(1/m)*(t(x)%*%(HypothesisFunction(x, theta) - y))
    J[i]  <- sum(y*log(HypothesisFunction(x, theta)) + (1-y)*log(1-HypothesisFunction(x, theta)))/(-1*m)
  }
  
  return(list(theta, J))
}

LogisticRegression <-function(x,y, NUMBER_ITERATIONS = 12000, Alpha = .01)
{
  x<-scale(x)
  x<-cbind(rep(1, nrow(x)), x)
  theta<-rep(0,ncol(x))
  #print(colMeans(x)) 
  
  results <- gradDescent(x, y, theta, ALPHA, NUMBER_ITERATIONS)
  theta <- results[[1]]
  J <- results[[2]]
  
  plot(1:NUMBER_ITERATIONS, J, type = 'l')
  return(theta)
}

set.seed(11)
NUMBER_OF_FEATURES = 4
NUMBER_OF_ROWS = 100

x <- matrix(rnorm(400), ncol = NUMBER_OF_FEATURES)
y <- rnorm(NUMBER_OF_ROWS)

theta = LogisticRegression(x,y)
print(theta)
z = (HypothesisFunction(cbind(rep(1, nrow(x)), x), theta))
View(y)
