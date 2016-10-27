require(mlbench)
library(ggplot2)
library(reshape)
library(glmnet)

data(BreastCancer)
BreastCancer$Id <- NULL 
BreastCancer <- na.omit(BreastCancer) 
BreastCancer$Class<-as.character(BreastCancer$Class)
BreastCancer$Class[BreastCancer$Class == "benign"] <- 0
BreastCancer$Class[BreastCancer$Class == "malignant"] <- 1
BreastCancer$Class<-as.numeric(BreastCancer$Class)

SplitDataFrame<-function(df, train_ratio)
{
  smp_size <- floor(train_ratio * nrow(df))
  
  train_ind <- sample(seq_len(nrow(df)), size = smp_size)
  
  train <- df[train_ind, ]
  test <- df[-train_ind, ]
  
  return(list(train, test))
}

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

LogisticRegression <-function(x,y, NUMBER_ITERATIONS, ALPHA)
{
  x<-cbind(rep(1, nrow(x)), x)
  theta<-rep(0,ncol(x))
  
  results <- gradDescent(x, y, theta, ALPHA, NUMBER_ITERATIONS)
  theta <- results[[1]]
  J <- results[[2]]
  
  plot(1:NUMBER_ITERATIONS, J, type = 'l')
  return(theta)
}

FindErrorsForLogisticRegression<-function(BreastCancer, split_ratio = 0.7, NUMBER_ITERATIONS = 15000, ALPHA = .1)
{
  epsilon = 0.5
  result = SplitDataFrame(BreastCancer, split_ratio)
  train_set = result[[1]]
  test_set = result[[2]]
  train_x = data.matrix(train_set[, !colnames(train_set) %in% c("Class")])
  train_y = data.matrix(train_set$Class)
  test_x = data.matrix(test_set[, !colnames(test_set) %in% c("Class")])
  test_y = data.matrix(test_set$Class)
  
  theta = LogisticRegression(train_x, train_y, NUMBER_ITERATIONS, ALPHA)
  pred_train_y = HypothesisFunction(cbind(rep(1, nrow(train_x)), train_x), theta)
  pred_test_y = HypothesisFunction(cbind(rep(1, nrow(test_x)), test_x), theta)
  
  train_prediction_df<-data.frame(train_y=train_y,pred_train_y=pred_train_y)
  train_prediction_df$pred_train_y[train_prediction_df$pred_train_y + epsilon >= 1.0] <-1
  train_prediction_df$pred_train_y[train_prediction_df$pred_train_y + epsilon <  1.0] <-0
  train_prediction_df$error = abs(train_prediction_df$pred_train_y - train_prediction_df$train_y)
  #cat("Percentage of correct predictions in training set :", 1 - sum(train_prediction_df$error)/nrow(train_x))
  
  test_prediction_df<-data.frame(test_y=test_y,pred_test_y=pred_test_y)
  test_prediction_df$pred_test_y[test_prediction_df$pred_test_y + epsilon >= 1.0] <-1
  test_prediction_df$pred_test_y[test_prediction_df$pred_test_y + epsilon <  1.0] <-0
  test_prediction_df$error = abs(test_prediction_df$pred_test_y - test_prediction_df$test_y)
  #cat("\nPercentage of correct predictions in test set :", 1 - sum(test_prediction_df$error)/nrow(test_x))
  
  error_training = sum(train_prediction_df$error)/nrow(train_x)
  error_test     = sum(test_prediction_df$error)/nrow(test_x)
  
  return(list(error_training, error_test))
}

GLMErrorsForLogisticRegression<-function(BreastCancer, split_ratio = 0.7, NUMBER_ITERATIONS = 15000, ALPHA = .1)
{
  epsilon = 0.5
  result = SplitDataFrame(BreastCancer, split_ratio)
  train_set = result[[1]]
  test_set = result[[2]]
  train_x = data.matrix(train_set[, !colnames(train_set) %in% c("Class")])
  train_y = data.matrix(train_set$Class)
  test_x = data.matrix(test_set[, !colnames(test_set) %in% c("Class")])
  test_y = data.matrix(test_set$Class)
  
  model <- glmnet (train_x, train_y, family = "binomial", alpha = ALPHA, maxit = NUMBER_ITERATIONS)
  pred_train_y <- predict(model, train_x, s=0.01, type = 'response')
  pred_test_y <-  predict(model, test_x, s=0.01, type = 'response')
  
  train_prediction_df<-data.frame(train_y=train_y,pred_train_y=pred_train_y)
  colnames(train_prediction_df)[2] <- "pred_train_y"
  train_prediction_df$pred_train_y[train_prediction_df$pred_train_y + epsilon >= 1.0] <-1
  train_prediction_df$pred_train_y[train_prediction_df$pred_train_y + epsilon <  1.0] <-0
  train_prediction_df$error = abs(train_prediction_df$pred_train_y - train_prediction_df$train_y)
  #cat("Percentage of correct predictions in training set :", 1 - sum(train_prediction_df$error)/nrow(train_x))
  
  test_prediction_df<-data.frame(test_y=test_y,pred_test_y=pred_test_y)
  colnames(test_prediction_df)[2] <- "pred_test_y"
  test_prediction_df$pred_test_y[test_prediction_df$pred_test_y + epsilon >= 1.0] <-1
  test_prediction_df$pred_test_y[test_prediction_df$pred_test_y + epsilon <  1.0] <-0
  test_prediction_df$error = abs(test_prediction_df$pred_test_y - test_prediction_df$test_y)
  #cat("\nPercentage of correct predictions in test set :", 1 - sum(test_prediction_df$error)/nrow(test_x))
  
  error_training = sum(train_prediction_df$error)/nrow(train_x)
  error_test     = sum(test_prediction_df$error)/nrow(test_x)
  
  test_set_1 <- test_set[test_set$Class == 1,]
  test_set_0 <- test_set[test_set$Class == 0,]
  test_x_1   <- data.matrix(test_set_1[, !colnames(test_set_1) %in% c("Class")])
  test_x_0   <- data.matrix(test_set_0[, !colnames(test_set_0) %in% c("Class")])
  
  train_set_1 <- train_set[train_set$Class == 1,]
  train_set_0 <- train_set[train_set$Class == 0,]
  train_x_1   <- data.matrix(train_set_1[, !colnames(train_set_1) %in% c("Class")])
  train_x_0   <- data.matrix(train_set_0[, !colnames(train_set_0) %in% c("Class")])
  
  LL_test  = sum(log(    predict(model, test_x_1, s=0.01, type = 'response'))) + 
             sum(log(1 - predict(model, test_x_0, s=0.01, type = 'response')))
  
  LL_train = sum(log(    predict(model, train_x_1, s=0.01, type = 'response'))) + 
             sum(log(1 - predict(model, train_x_0, s=0.01, type = 'response')))
  
  return(list(error_training, error_test, LL_train, LL_test))
}

Question_3<-function()
{
  error_training <- rep(0, 10)
  error_test     <- rep(0, 10)
  
  for(i in 1:10)
  {
    set.seed(i)
    errors = FindErrorsForLogisticRegression(BreastCancer, split_ratio = 0.7, NUMBER_ITERATIONS = 15000, ALPHA = .1)
    error_training[i] = errors[[1]]
    error_test[i] = errors[[2]]
  }
  
  cat("Average error on training set with split ratio of 70/30, 15000 iterations and alpha 0.1 :", mean(error_training))
  cat("\nAverage error on test set with split ratio of 70/30, 15000 iterations and alpha 0.1 :", mean(error_test))
  
  error_training <- rep(0, 100)
  error_test     <- rep(0, 100)
  iterations     <-rep(0, 100)
  set.seed(10)
  
  for(i in 1:100)
  {
    error_training_v2 <- rep(0, 10)
    error_test_v2     <- rep(0, 10)
    
    for(j in 1:10)
    {
      set.seed(j)
      errors = FindErrorsForLogisticRegression(BreastCancer, split_ratio = 0.7, NUMBER_ITERATIONS = i*200, ALPHA = .1)
      error_training_v2[j] = errors[[1]] * errors[[1]]
      error_test_v2[j] = errors[[2]] * errors[[2]]
    }
    
    error_training[i] = mean(error_training_v2)
    error_test[i] = mean(error_test_v2)
    iterations[i] = i * 200
  }
  
  df <- data.frame(N = iterations,
                   training_error = error_training,
                   test_error = error_test)
  
  df <- melt(df ,  id.vars = 'N', variable.name = 'series')
  
  g_1<-ggplot(df, aes(N,value)) + geom_line(aes(colour=variable)) + ggtitle("mean-training error and mean-test error as a function of number of iterations")
  
  print(g_1)
  
  error_training <- rep(0, 100)
  error_test     <- rep(0, 100)
  iterations     <-rep(0, 100)
  set.seed(10)
  
  for(i in 1:100)
  {
    error_training_v2 <- rep(0, 10)
    error_test_v2     <- rep(0, 10)
    
    for(j in 1:10)
    {
      set.seed(j)
      errors = FindErrorsForLogisticRegression(BreastCancer, split_ratio = 0.7, NUMBER_ITERATIONS = 15000, ALPHA = i *.001)
      error_training_v2[j] = errors[[1]] * errors[[1]]
      error_test_v2[j] = errors[[2]] * errors[[2]]
    }
    
    error_training[i] = mean(error_training_v2)
    error_test[i] = mean(error_test_v2)
    iterations[i] = i * 0.001
  }
  
  df <- data.frame(N = iterations,
                   training_error = error_training,
                   test_error = error_test)
  
  df <- melt(df ,  id.vars = 'N', variable.name = 'series')
  
  g_1<-ggplot(df, aes(N,value)) + geom_line(aes(colour=variable)) + ggtitle("mean-training error and mean-test error as a function of alpha")
  
  print(g_1)
}

Question_4<-function()
{
  error_training <- rep(0, 10)
  error_test     <- rep(0, 10)
  
  for(i in 1:10)
  {
    set.seed(i)
    errors = GLMErrorsForLogisticRegression(BreastCancer, split_ratio = .7, NUMBER_ITERATIONS = 15000, ALPHA = .1)
    error_training[i] = errors[[1]]
    error_test[i] = errors[[2]]
  }
  
  cat("Average error on training set with split ratio of 70/30, 15000 iterations and alpha 0.1 :", mean(error_training))
  cat("\nAverage error on test set with split ratio of 70/30, 15000 iterations and alpha 0.1 :", mean(error_test))
  
  error_training <- rep(0, 100)
  error_test     <- rep(0, 100)
  iterations     <-rep(0, 100)
  set.seed(10)
  
  for(i in 1:100)
  {
    error_training_v2 <- rep(0, 10)
    error_test_v2     <- rep(0, 10)
    
    for(j in 1:10)
    {
      set.seed(j)
      errors = GLMErrorsForLogisticRegression(BreastCancer, split_ratio = 0.7, NUMBER_ITERATIONS = i*3, ALPHA = .1)
      error_training_v2[j] = errors[[1]] * errors[[1]]
      error_test_v2[j] = errors[[2]] * errors[[2]]
    }
    
    error_training[i] = mean(error_training_v2)
    error_test[i] = mean(error_test_v2)
    iterations[i] = i * 3
  }
  
  df <- data.frame(N = iterations,
                   training_error = error_training,
                   test_error = error_test)
  
  df <- melt(df ,  id.vars = 'N', variable.name = 'series')
  
  g_1<-ggplot(df, aes(N,value)) + geom_line(aes(colour=variable)) + ggtitle("mean training error and mean test error as a function of number of iterations")
  
  print(g_1)
  
  error_training <- rep(0, 100)
  error_test     <- rep(0, 100)
  iterations     <-rep(0, 100)
  set.seed(10)
  
  for(i in 1:100)
  {
    error_training_v2 <- rep(0, 10)
    error_test_v2     <- rep(0, 10)
    
    for(j in 1:10)
    {
      set.seed(j)
      errors = GLMErrorsForLogisticRegression(BreastCancer, split_ratio = 0.7, NUMBER_ITERATIONS = 15000, ALPHA = i *.001)
      error_training_v2[j] = errors[[1]] * errors[[1]]
      error_test_v2[j] = errors[[2]] * errors[[2]]
    }
    
    error_training[i] = mean(error_training_v2)
    error_test[i] = mean(error_test_v2)
    iterations[i] = i * 0.001
  }
  
  df <- data.frame(N = iterations,
                   training_error = error_training,
                   test_error = error_test)
  
  df <- melt(df ,  id.vars = 'N', variable.name = 'series')
  
  g_1<-ggplot(df, aes(N,value)) + geom_line(aes(colour=variable)) + ggtitle("mean training error and mean test error as a function of alpha")
  
  print(g_1)
}

Question_5<-function()
{
  error_training <- rep(0, 19)
  error_test     <- rep(0, 19)
  iterations     <-rep(0, 19)
  
  for(i in 1:19)
  {
    error_training_v2 <- rep(0, 10)
    error_test_v2     <- rep(0, 10)
    
    for(j in 1:10)
    {
      set.seed(j)
      errors = GLMErrorsForLogisticRegression(BreastCancer, split_ratio = i*0.05, NUMBER_ITERATIONS = 15000, ALPHA = .1)
      error_training_v2[j] = errors[[1]] * errors[[1]]
      error_test_v2[j] = errors[[2]] * errors[[2]]
    }
    
    error_training[i] = mean(error_training_v2)
    error_test[i] = mean(error_test_v2)
    iterations[i] = i * 0.05 * nrow(BreastCancer)
  }
  
  df <- data.frame(N = iterations,
                   training_error = error_training,
                   test_error = error_test)
  
  df <- melt(df ,  id.vars = 'N', variable.name = 'series')
  
  g_1<-ggplot(df, aes(N,value)) + geom_line(aes(colour=variable)) + ggtitle("mean-training error and mean-test error as a function of number of samples in training set")
  
  print(g_1)
}

Question_6<-function()
{
  LL_training <- rep(0, 19)
  LL_test     <- rep(0, 19)
  iterations     <-rep(0, 19)
  
  for(i in 1:19)
  {
    LL_training_v2 <- rep(0, 10)
    LL_test_v2     <- rep(0, 10)
    
    for(j in 1:10)
    {
      set.seed(j)
      LL = GLMErrorsForLogisticRegression(BreastCancer, split_ratio = i*0.05, NUMBER_ITERATIONS = 15000, ALPHA = .1)
      LL_training_v2[j] = LL[[3]] 
      LL_test_v2[j] = LL[[4]] 
    }
    
    LL_training[i] = mean(LL_training_v2)
    LL_test[i] = mean(LL_test_v2)
    iterations[i] = i * 0.05 * nrow(BreastCancer)
  }
  
  df <- data.frame(N = iterations,
                   training_log_likelihood = LL_training,
                   test_log_likelihood = LL_test)
  
  df <- melt(df ,  id.vars = 'N', variable.name = 'series')
  
  g_1<-ggplot(df, aes(N,value)) + geom_line(aes(colour=variable)) + ggtitle("negative log likelihood of training set and test set as a function of number of samples in training set")
  
  print(g_1)
}

Question_3()
Question_4()
Question_5()
Question_6()
