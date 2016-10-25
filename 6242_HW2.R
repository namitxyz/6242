require(mlbench)
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

LogisticRegression <-function(x,y, NUMBER_ITERATIONS = 15000, ALPHA = .01)
{
  x<-cbind(rep(1, nrow(x)), x)
  theta<-rep(0,ncol(x))
  
  results <- gradDescent(x, y, theta, ALPHA, NUMBER_ITERATIONS)
  theta <- results[[1]]
  J <- results[[2]]
  
  plot(1:NUMBER_ITERATIONS, J, type = 'l')
  return(theta)
}

Question_1<-function(BreastCancer, split_ratio = 0.7)
{
  epsilon = 0.5
  result = SplitDataFrame(BreastCancer, split_ratio)
  train_set = result[[1]]
  test_set = result[[2]]
  train_x = data.matrix(train_set[, !colnames(train_set) %in% c("Class")])
  train_y = data.matrix(train_set$Class)
  test_x = data.matrix(test_set[, !colnames(test_set) %in% c("Class")])
  test_y = data.matrix(test_set$Class)
  
  theta = LogisticRegression(train_x, train_y)
  pred_train_y = HypothesisFunction(cbind(rep(1, nrow(train_x)), train_x), theta)
  pred_test_y = HypothesisFunction(cbind(rep(1, nrow(test_x)), test_x), theta)
  
  train_prediction_df<-data.frame(train_y=train_y,pred_train_y=pred_train_y)
  train_prediction_df$pred_train_y[train_prediction_df$pred_train_y + epsilon >= 1.0] <-1
  train_prediction_df$pred_train_y[train_prediction_df$pred_train_y + epsilon <  1.0] <-0
  train_prediction_df$error = abs(train_prediction_df$pred_train_y - train_prediction_df$train_y)
  cat("Percentage of correct predictions in training set :", 1 - sum(train_prediction_df$error)/nrow(train_x))
  
  test_prediction_df<-data.frame(test_y=test_y,pred_test_y=pred_test_y)
  test_prediction_df$pred_test_y[test_prediction_df$pred_test_y + epsilon >= 1.0] <-1
  test_prediction_df$pred_test_y[test_prediction_df$pred_test_y + epsilon <  1.0] <-0
  test_prediction_df$error = abs(test_prediction_df$pred_test_y - test_prediction_df$test_y)
  print("")
  cat("Percentage of correct predictions in test set :", 1 - sum(test_prediction_df$error)/nrow(test_x))
  
  error_training = 1 - sum(train_prediction_df$error)/nrow(train_x)
  error_test     = 1 - sum(test_prediction_df$error)/nrow(test_x)
  
  return(list(error_training, error_test))
}

set.seed(13)
errors = Question_1(BreastCancer, 0.5)
