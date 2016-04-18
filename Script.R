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
#### feature selection with Genetic Algorithm
library(ranger)
library(parallel)
library(doSNOW)
cl <- makeCluster(24)
registerDoSNOW(cl)
results <- foreach(i = 1:100) %dopar% {
  in_train <- caret::createDataPartition(df, p = 0.80, list = FALSE)
  train <- df[in_train, ]
  test <- df[-in_train, ]
  rm(in_train)
  rm(test)
  fit <- ranger::ranger(Class ~., data = train, write.forst = TRUE,
                        save.memory = TRUE, importance = "impurity")
  actual <- train$Class
  prediction <- predict(fit, train)
  prediction <- prediction$predictions
  results <- table(prediction, actual)
  rm(fit)
  rm(prediction)
  rm(actual)
  results[[i]] <- as.numeric(results)
  
}

fit <- ranger(Class ~., data = data_new, write.forest = TRUE,
              importance = "impurity")


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