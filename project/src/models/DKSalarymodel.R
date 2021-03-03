library(Metrics)
library(caret)
library(data.table)
library(glmnet)

#Read in player data 
qb<-fread("./project/volume/data/teamconstruction/")
te<-fread("./project/volume/data/teamconstruction/")
wr<-fread("./project/volume/data/teamconstruction/")
rb<-fread("./project/volume/data/teamconstruction/")

pos<-list(qb,te,wr,rb)

final<-data.table()

#Merge all player data into one final dataframe
final<-rbind(final,list)

#Read in draftkings data
DKdata<-fread("./project/volume/data/interim/DKsalaries.csv")

#Drop unimportant columns
DKdrops<-c("GID","Team","h/a","Oppt")
DKdata<-DKdata[, !DKdrops, with = FALSE]


#Rename columns in draftkings data to ease merging process
setnames(DKdata, c("Week","Year","Name"), c("week","season","Player"))

#Set keys, merge the two datasets together
setkey(final,season,week,Player,Pos)
setkey(DKdata,season,week,Player,Pos)

modelready<-merge(final,DKdata, all.x = TRUE)


modeldata<-modelready[,.]

x<-modeldata[,.]
y_train<-modeldata$DKSalary

x = as.matrix(x)

#Model

#Cross validation
lambdas <- 10^seq(2, -3, by = -.1)
cv_ridge <- cv.glmnet(x, y_train, alpha = 0, lambda = lambdas)
optimal_lambda <- cv_ridge$lambda.min
optimal_lambda

#Training
ridge_reg = glmnet(x, y_train, nlambda = 25, alpha = 0, family = 'gaussian', lambda = optimal_lambda)

summary(ridge_reg)


predictions = predict(glmnet, newdata = test)
eval_metrics(glmnet, test, predictions, target = 'DK Salary')






