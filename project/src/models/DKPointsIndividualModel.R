library(Metrics)
library(caret)
library(data.table)


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

y_train = train$DKpoints
y_test = test$DKpoints

#Function to calculate rmse and Rsquared
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

##########################################
#Model to examine PredPoints and DKPoints
##########################################

linearmodel1<-lm(DKpoints~PredPoints, data = train)
summary(linearmodel1)
plot(linearmodel1)

predictions1<-predict(linearmodel1, test)

results1<-eval_results(y_test, predictions1, test)

##########################################
#Model to examine Dksalary and DKPoints
##########################################

linearmodel2<-lm(DKpoints~DKsalary, data = train)
summary(linearmodel2)
plot(linearmodel2)

predictions2<-predict(linearmodel2, test)
results2<-eval_results(y_test, predictions2, test)

