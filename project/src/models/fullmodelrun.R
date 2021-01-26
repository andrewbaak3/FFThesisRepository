library(Metrics)
library(caret)
library(xgboost)
library(ggplot2)
library(optparse)
library(data.table)


#Create Options List
option_list = list(
  make_option(c("-r", "--roll"), default=NA, type="character",
              help="what rolling merge set of data you want to use", metavar = "character"),
  make_option(c("-p", "--position"),  default=NA, type='character',
              help="position that you want to train model on")
);

opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);


#Extract arguments passed in to options parser
rolltime<-opt$roll
pos<-opt$position


runModel<- function(rolltime,pos) {
  
  path1<-"./project/volume/data/processed/train"
  path2<-paste0(path1,rolltime,".csv")
  train<-fread(path2)
  
  #read in hyperparameter data
  param_table<-fread("./project/src/models/HyperParams/hyperparametertuning.csv")
  
  train[train$Pos=="HB"]$Pos<-"RB"
  
  sample_pos<-pos
  train<-train[Pos==sample_pos]
  
  train1<-train[season<2017]
  test1<-train[season==2017]
  
  #Prep Data for Modeling
  train1_y<-train1$PPRFantasyPoints
  test_y<-test1$PPRFantasyPoints
  test1$PPRFantasyPoints<-0
  
  
  #Get rid of columns not needed for modeling
  drops<-c("season","Tm","game_id","Opponent","Player","Pos","cumulativeweek")
  
  train1<-train1[, !drops, with = FALSE]
  test1<-test1[, !drops, with = FALSE]
  
  #Fix game status columns to remove null values
  train1[train1==""]<-"None"
  train1$Wed[is.na(train1$Wed)]<-"None"
  train1$Thu[is.na(train1$Thu)]<-"None"
  train1$Fri[is.na(train1$Fri)]<-"None"
  train1$GameStatus[is.na(train1$GameStatus)]<-"None"
  test1$Wed[is.na(test1$Wed)]<-"None"
  test1$Thu[is.na(test1$Thu)]<-"None"
  test1$Fri[is.na(test1$Fri)]<-"None"
  test1$GameStatus[is.na(test1$GameStatus)]<-"None"
  test1[test1==""]<-"None"
  
  #Find zero variance columns and remove them
  summaryvar<-nearZeroVar(train1,saveMetrics = T)
  injurycols<-rownames(summaryvar)
  summaryvar<-data.table(summaryvar)
  summaryvar$columnames<-injurycols
  zerovarcols<-summaryvar[zeroVar==T]
  
  novardrops<-zerovarcols$columnames
  
  train1<-train1[, !novardrops, with = FALSE]
  test1<-test1[, !novardrops, with = FALSE]
  
  #train1$train<-1
  #test1$train<-0
  
  master<-rbind(train1,test1)
  
  #master$train<-NULL
  #train1$train<-NULL
  #test1$train<-NULL
  
  #Need to keep position in as a variable and create dummies for this purpose
  dummies<-dummyVars(PPRFantasyPoints~., data=master)
  x_train<-predict(dummies, newdata = train1)
  x_test<-predict(dummies, newdata = test1)
  
  #Create proper representation of data for modeling
  dtrain <- xgb.DMatrix(x_train,label=train1_y,missing=NA)
  dtest <- xgb.DMatrix(x_test,label=test_y,missing=NA)
  
  
  for (i in 2:nrow(param_table)) { 
    
    #Define XGboost model and parameters 
    param <- list(  objective           = "reg:linear",
                    gamma               =param_table$gamma[i],
                    booster             = "gbtree",
                    eval_metric         = "rmse",
                    eta                 = param_table$eta[i],
                    max_depth           = param_table$max_depth[i],
                    min_child_weight    = param_table$min_child_weight[i],
                    subsample           = param_table$subsample[i],
                    colsample_bytree    =param_table$colsample_bytree[i],
                    tree_method = 'hist'
    )
    
    #Add the watchlist
    watchlist <- list(train = dtrain, test = dtest)
    
    
    #null_y<-mean(train1_y)
    #rmse(test_y,null_y)
    #rollingfantasypoint_y<-test1$Player_roll_4_PPRFantasyPoints/4
    #rollingfantasypoint_y[is.na(rollingfantasypoint_y)]<-null_y
    #rmse(test_y, rollingfantasypoint_y)
    
    #Create Predictions
    XGB_model<-xgb.train(params=param,nrounds=10000,missing=NA,data=dtrain,watchlist=watchlist,print_every_n=1,nthread=4, 
                         early_stopping_rounds=25)
    
    #Write out hyperparameters so they can be compared
    best_ntrees<-unclass(XGB_model)$best_iteration
    train_param<-data.table(t(param))
    train_param$best_ntrees<-best_ntrees
    train_error<-unclass(XGB_model)$evaluation_log[as.numeric(best_ntrees),]$train_rmse
    train_param$train_error<-train_error
    test_error<-unclass(XGB_model)$evaluation_log[as.numeric(best_ntrees),]$test_rmse
    train_param$test_error<-test_error
    
    path3<-"./project/src/models/HyperParams/trainingHyperparameters"
    path4<-paste0(path3,pos)
    path5<-paste0(path4,rolltime,".csv")
    fwrite(train_param,path5, append = T)
  }
  
  cols<-XGB_model[["feature_names"]]
  importance <- xgb.importance(feature_names = cols, model = XGB_model)
  print(xgb.plot.importance(importance_matrix = importance, top_n = 50))
  test1$PPRFantasyPoints<-predict(XGB_model, newdata=dtest)
}
  
###############
#Main Function
###############
model1<-function(rolltime, pos){
  runModel(rolltime,pos)
}

model1(rolltime,pos)

  
  