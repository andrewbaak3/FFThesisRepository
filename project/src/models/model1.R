library(data.table)
library(Metrics)
library(caret)
library(xgboost)

#read in data
train<-fread("./project/volume/data/processed/train.csv")

#Create Multiple Training and Testing Sets
train1<-train[season<2019]
test1<-train[season==2019]

#Prep Data for Modeling
train1_y<-train$PPRFantasyPoints
test1$PPRFantasyPoints<-0


#Get rid of columns not needed for modeling
drops<-c("season","week","Tm","game_id","Opponent","Player","Position","cumulativeweek")




#Create proper representation of data for modeling
dtrain <- xgb.DMatrix(train1,label=train1_y,missing=NA)
dtest <- xgb.DMatrix(test1,missing=NA)



#Define XGboost model and parameters 
param <- list(  objective           = "reg:linear",
                gamma               =0.2,
                booster             = "gbtree",
                eval_metric         = "rmse",
                eta                 = 0.005,
                max_depth           = 20,
                min_child_weight    = 1,
                subsample           = 0.9,
                colsample_bytree    = 0.9,
                tree_method = 'exact'
)

XGB_model<-xgb.cv(params=param,nfold=5,nrounds=5000,missing=NA,data=dtrain,print_every_n=1)

test1$PPRFantasyPoints<-predict(XGB_model, newdata=dtest)
