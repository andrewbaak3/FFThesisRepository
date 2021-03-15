library(Metrics)
library(caret)
library(data.table)
library(glmnet)

#Read in  data 
data<-fread("./project/volume/data/teamconstruction/packingdata.csv")

modeldata<-data[,.(PredPoints,`DK salary`,`DK points`)]

setnames(modeldata, c("DK points","DK salary"), c("DKpoints","DKsalary"))


modeldata[is.na(modeldata$PredPoints)]$PredPoints<-0
modeldata[is.na(modeldata$DKsalary)]$DKsalary<-0
modeldata[is.na(modeldata$DKpoints)]$DKpoints<-0


#Set seed for randomization
set.seed(123) 

#creating index so data can be split into train and test
trainIndex <- createDataPartition(modeldata$DKpoints,p=0.75,list=FALSE)

#splitting data into training/testing data using the trainIndex object
train <- modeldata[trainIndex,] #training data (75% of data)
test <- modeldata[-trainIndex,] #testing data (25% of data)



#Preprocess numeric columns
pre_proc_val <- preProcess(train, method = c("center", "scale"))
train = predict(pre_proc_val, train)
test = predict(pre_proc_val, test)

#Make dummy variables
dummies <- dummyVars(DKpoints ~ ., data = modeldata[,.(PredPoints,DKsalary,DKpoints)])
train_dummies = predict(dummies, newdata = train[,.(PredPoints,DKsalary,DKpoints)])
test_dummies = predict(dummies, newdata = test[,.(PredPoints,DKsalary,DKpoints)])

x = as.matrix(train_dummies)
y_train = train$DKpoints

x_test = as.matrix(test_dummies)
y_test = test$DKpoints


#Model

#Cross validation
lambdas <- 10^seq(2, -3, by = -.1)
cv_ridge <- cv.glmnet(x, y_train, alpha = 1, lambda = lambdas, na.action = na.omit)
optimal_lambda <- cv_ridge$lambda.min
optimal_lambda

#Training
ridge_reg = glmnet(x, y_train, nlambda = 25, alpha = 1, family = 'gaussian', lambda = optimal_lambda)

summary(ridge_reg)


predictions = predict(ridge_reg, s = optimal_lambda, newx = x_test)

# Compute R^2 from true and predicted values
eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))
  
  
  # Model performance metrics
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square
  )
}
# Prediction and evaluation on test data
eval_results(y_test, predictions, test)

plot(ridge_reg)

