setwd("/media/server2/Seagate Backup Plus Drive/Epitode_project")
library(readxl)
df <- read_excel("data.xlsx")
df <- na.omit(df)
Class <- df$Class
Class <- as.factor(Class)
descriptors <- df[, 2:ncol(df)]
library(caret)
descriptor_remove_zero_var <- descriptors[, -nearZeroVar(descriptors)]
df <- cbind(Class, descriptor_remove_zero_var)
B_negative <- subset(df, Class == "B-negative")
B_positive <- subset(df, Class == "B-positive")
Dual <- subset(df, Class == "Dual")
T_negative <- subset(df, Class == "T-negative")
T_positive <- subset(df, Class == "T-positive")
library(dplyr)
B_negative <- sample_n(B_negative, size = 681, replace = TRUE)
B_positive <- sample_n(B_positive, size = 681, replace = TRUE)
Dual <- sample_n(Dual, size = 681, replace = TRUE)
T_negative <- sample_n(T_negative, size = 681, replace = TRUE)
T_positive <- sample_n(T_positive, size = 681, replace = TRUE)
df <- rbind(B_negative, B_positive, Dual, T_negative, T_positive)

library(ranger)
library(parallel)
library(doSNOW)
cl <- makeCluster(24)
registerDoSNOW(cl)
results_train <- list(10)
results_train <- foreach(i = 1:10) %dopar% {
  in_train <- caret::createDataPartition(df$Class, p = 0.80, list = FALSE)
  train <- df[in_train, ]
  test <- df[-in_train, ]
  rm(in_train)
  rm(test)
  fit <- ranger::ranger(Class ~., data = train, write.forest  = TRUE,
                        save.memory = TRUE, importance = "impurity")
  actual <- train$Class
  prediction <- ranger::predictions(fit, train)
  rm(fit)
  rm(train)
  results <- caret::confusionMatrix(prediction, actual)
  #results <- results$table
  rm(actual)
  rm(prediction)
  results_train[[i]] <- results$byClass
  
}
#######
setwd("/media/server2/Seagate Backup Plus Drive/Epitode_project/train")
for (i in 1:10){
  data <- results_train[[i]]
   write.csv(data, file = paste0("Model_Epitode_Train_Result_", i, ".csv"))
}

##################################
#results 10-CV
results_CV <- list(10)
results_CV <- foreach(i = 1:10) %dopar% {
  in_train <- caret::createDataPartition(df$Class, p = 0.80, list = FALSE)
  myData <- df[in_train, ]
  test <- df[-in_train, ]
  rm(in_train)
  rm(test)
  k = 10
  index <- sample(1:k, nrow(myData), replace = TRUE)
  folds <- 1:k
  myRes <- data.frame()
  for (j in 1:k) {
    training <- subset(myData, index %in% folds[-j])
    testing <- subset(myData, index %in% c(j))
    model <- ranger::ranger(Class ~., data = training, write.forest = TRUE,
                          save.memory = TRUE, importance = "impurity")
    
    prediction <- predict(model, testing)
    prediction <- prediction$predictions
    actual <- testing$Label
    rm(training)
    rm(testing)
    rm(model)
    data <- cbind(prediction, actual)
    rm(prediction)
    rm(actual)
    myRes <- rbind(myRes, data)
  }
  prediction <- myRes$prediction
  actual <- myRes$actual
  results <- caret::confusionMatrix(prediction, actual)
  #results <- results$table
  rm(MyRes)
  rm(myData)
  rm(fit)
  rm(actual)
  rm(prediction)
  results_CV[[i]] <- results$byClass
  
}
##############################






library(ranger)
library(parallel)
library(doSNOW)
cl <- makeCluster(24)
registerDoSNOW(cl)
results_test <- list(10)
results_test <- foreach(i = 1:10) %dopar% {
  in_train <- caret::createDataPartition(df$Class, p = 0.80, list = FALSE)
  train <- df[in_train, ]
  test <- df[-in_train, ]
  rm(in_train)
  fit <- ranger::ranger(Class ~., data = train, write.forest  = TRUE,
                        save.memory = TRUE, importance = "impurity")
  actual <- test$Class
  prediction <- predict(fit, test)
  prediction <- prediction$predictions
  rm(fit)
  rm(train)
  rm(test)
  results <- caret::confusionMatrix(prediction, actual)
  #results <- results$table
  rm(actual)
  rm(prediction)
  results_test[[i]] <- results$byClass
  
}

setwd("/media/server2/Seagate Backup Plus Drive/Epitode_project/test")
for (i in 1:10){
  data <- results_test[[i]]
  write.csv(data, file = paste0("Model_Epitode_Test_Result_", i, ".csv"))
}


























































in_train <- caret::createDataPartition(df, p = 0.80, list = FALSE)
train <- df[in_train, ]
test <- df[-in_train, ]
rm(in_train)
rm(test)
fit <- ranger::ranger(Class ~., data = train, write.forst = TRUE,
                      save.memory = TRUE, importance = "impurity")
actual <- train$Class
prediction <- predictions(fit, train)
results <- caret::confusionMatrix(prediction, actual)






























statistical_result <- function(x) {
  #ok <- J48_testing(x)
  results <- data.frame(x)
  data <- data.frame(results)
  m = ncol(data)
  ACC  <- matrix(nrow = m, ncol = 1)
  SENS  <- matrix(nrow = m, ncol = 1)
  SPEC  <-matrix(nrow = m, ncol = 1)
  MCC <- matrix(nrow = m, ncol = 1)
  
  for(i in 1:m){ 
    ACC[i,1]  = (data[1,i]+data[4,i])/(data[1,i]+data[2,i]+data[3,i]+data[4,i])*100
    SENS[i,1]  =  (data[4,i])/(data[3,i]+data[4,i])*100
    SPEC[i,1]  = (data[1,i]/(data[1,i]+data[2,i]))*100
    MCC1      = (data[1,i]*data[4,i]) - (data[2,i]*data[3,i])
    MCC2      =  (data[4,i]+data[2,i])*(data[4,i]+data[3,i])
    MCC3      =  (data[1,i]+data[2,i])*(data[1,i]+data[3,i])
    MCC4  =  sqrt(MCC2)*sqrt(MCC3)
    
    
    MCC[i,1]  = MCC1/MCC4
  }
  results_ACC <- mean_and_sd(ACC)
  results_SENS <- mean_and_sd(SENS)
  results_SPEC <- mean_and_sd(SPEC)
  results_MCC <- mean_and_sd(MCC)
  results_all <- (data.frame(c(results_ACC, results_SENS, results_SPEC, results_MCC)))
  rownames(results_all) <- c("ACC_Mean", "ACC_SD", "Sens_Mean", "Sens_SD", "Spec_Mean", "Spec_SD",
                             "MCC_Mean", "MCC_SD")
  return(results_all)
}


###3 B-negative
### B-positive
### Dual
### T-negative
### T-positive

####3
library(caret)
library(doMC)
registerDoMC(cores = 24)
ga_ctrl <- gafsControl(functions = rfGA,
                       method = "repeatedcv", number = 10, repeats = 5)
rf_ga <- gafs(x = descriptor_remove_zero_var, y = Class, iters = 200, 
              gafsControl = ga_ctrl)
rf_ga

# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
library(doMC)
registerDoMC(cores = 20)
results <- rfe(descriptors, Class, sizes=c(1:8), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))