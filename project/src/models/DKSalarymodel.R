library(Metrics)
library(caret)
library(data.table)
library(glmnet)

#Read in  data 
data<-fread("./project/volume/data/teamconstruction/packingdata.csv")


modeldata<-data[,.(PredPoints,`DK salary`,`DK points`)]

setnames(modeldata, c("DK points","DK salary"), c("DKpoints","DKsalary"))

#Set seed for randomization
set.seed(123) 

#creating index so data can be split into train and test
trainIndex <- createDataPartition(modeldata$DKpoints,p=0.75,list=FALSE, na.rm = T)

#splitting data into training/testing data using the trainIndex object
train <- data[trainIndex,] #training data (75% of data)
test <- data[-trainIndex,] #testing data (25% of data)

x<-modeldata[,.(DKpoints,PredPoints)]
y_train<-modeldata$DKsalary

x = as.matrix(x)

#Model

#Cross validation
lambdas <- 10^seq(2, -3, by = -.1)
cv_ridge <- cv.glmnet(x, y_train, alpha = 0, lambda = lambdas, na.action = na.omit)
optimal_lambda <- cv_ridge$lambda.min
optimal_lambda

#Training
ridge_reg = glmnet(x, y_train, nlambda = 25, alpha = 0, family = 'gaussian', lambda = optimal_lambda)

summary(ridge_reg)


predictions = predict(glmnet, newdata = test)
eval_metrics(glmnet, test, predictions, target = 'DK Salary')






