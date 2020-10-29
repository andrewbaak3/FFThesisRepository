library(data.table)
library(Metrics)
library(caret)
library(xgboost)
library(ggplot2)

#read in data
train<-fread("./project/volume/data/processed/train(roll5).csv")

sample_pos<-"WR"
train<-train[Pos== sample_pos]


#Create Multiple Training and Testing Sets
train1<-train[season<2019]
test1<-train[season==2019]

#Prep Data for Modeling
train1_y<-train1$PPRFantasyPoints
test_y<-test1$PPRFantasyPoints
test1$PPRFantasyPoints<-0


#Get rid of columns not needed for modeling
drops<-c("season","Tm","game_id","Opponent","Player","Pos","cumulativeweek")

train1<-train1[, !drops, with = FALSE]
test1<-test1[, !drops, with = FALSE]


#Need to keep position in as a variable and create dummies for this purpose
dummies<-dummyVars(PPRFantasyPoints~., data=train1)
x_train<-predict(dummies, newdata = train1)
x_test<-predict(dummies, newdata = test1)


#Create proper representation of data for modeling
dtrain <- xgb.DMatrix(x_train,label=train1_y,missing=NA)
dtest <- xgb.DMatrix(x_test,label=test_y,missing=NA)



#Define XGboost model and parameters 
param <- list(  objective           = "reg:linear",
                gamma               =0.2,
                booster             = "gbtree",
                eval_metric         = "rmse",
                eta                 = 0.2,
                max_depth           = 3,
                min_child_weight    = 30,
                subsample           = .8,
                colsample_bytree    = .8,
                tree_method = 'hist'
)


#Add the watchlist
watchlist <- list(train = dtrain, test = dtest)


null_y<-mean(train1_y)
rmse(test_y,null_y)
rollingfantasypoint_y<-test1$Player_roll_4_PPRFantasyPoints/4
rollingfantasypoint_y[is.na(rollingfantasypoint_y)]<-null_y
rmse(test_y, rollingfantasypoint_y)

#Create Predictions
XGB_model<-xgb.train(params=param,nrounds=10000,missing=NA,data=dtrain,watchlist=watchlist,print_every_n=1,nthread=4, 
                     early_stopping_rounds=25)

#Write out hyperparameters so they can be compared
best_ntrees<-unclass(XGB_model)$best_iteration
train_param<-data.table(t(param))
train_param$best_ntrees<-best_ntrees
test_error<-unclass(XGB_model)$evaluation_log[as.numeric(best_ntrees),]$test_rmse
train_param$test_error<-test_error

#fwrite(train_param, "./project/src/models/trainingHyperparametersWR(roll3).csv", append = T)
#fwrite(train_param, "./project/src/models/trainingHyperparametersWR(roll4).csv", append = T)
fwrite(train_param, "./project/src/models/trainingHyperparametersWR(roll5).csv", append = T)


importance_table<-xgb.importance(model=XGB_model)
xgb.plot.importance(importance_table)

test1$PPRFantasyPoints<-predict(XGB_model, newdata=dtest)