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
  train<-train[order(season,week)]
  
  sample_pos<-pos
  train<-train[Pos==sample_pos]
  
  train1<-train[season==2016]
  test1<-train[season==2017]
  
  #Create output data table which is a subset of the columns of test1
  output<-test1[,.(season,week,cumulativeweek,Tm,game_id,Opponent,Player,Pos,PPRFantasyPoints)]
  output<-output[order(season,week)]
  
  #Prep Data for Modeling
  train1_y<-train1$PPRFantasyPoints
  test_y<-test1$PPRFantasyPoints
  test1$PPRFantasyPoints<-0
  
  #Get rid of columns not needed for modeling
  drops<-c("season","Tm","game_id","Opponent","Player","Pos","cumulativeweek")
  
  train1<-train1[, !drops, with = FALSE]
  test1<-test1[, !drops, with = FALSE]
  
  #Fix game status columns in train1 to remove null values
  train1[train1==""]<-"None"
  train1$Wed[is.na(train1$Wed)]<-"None"
  train1$Thu[is.na(train1$Thu)]<-"None"
  train1$Fri[is.na(train1$Fri)]<-"None"
  train1$GameStatus[is.na(train1$GameStatus)]<-"None"
  
  #Fix game status columns in test1 to remove null values
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
  
  #Create column to discern between train and test data for after they are split
  train1$train<-1
  test1$train<-0
  
  #Create master set of train and test
  master<-rbind(train1,test1)
  
  #Create categorical levels for variables before dummy creation
  col_type<-sapply(master, class)
  char_cols<-names(col_type[col_type=="character"])
  master[,(char_cols):=lapply(.SD, as.factor),.SDcols=char_cols]
  
  #Split factorized data back into train and test for dummy creation
  m_test<-master[train==0]
  m_train<-master[train==1]
  
  #Remove train column as it is no longer needed
  m_test$train<-NULL
  m_train$train<-NULL
  master$train<-NULL
  
  
  #Need to keep position in as a variable and create dummies for this purpose
  dummies<-dummyVars(PPRFantasyPoints~., data=master)
  x_train<-predict(dummies, newdata = m_train)
  x_test<-predict(dummies, newdata = m_test)
  
  
  #Create proper representation of data for modeling
  dtrain <- xgb.DMatrix(x_train,label=train1_y,missing=NA)
  dtest <- xgb.DMatrix(x_test,missing=NA)
  
  

    
  #Define XGboost model and parameters 
  param <- list(  objective           = "reg:linear",
                  gamma               =.1,
                  booster             = "gbtree",
                  eval_metric         = "rmse",
                  eta                 = .1,
                  max_depth           = 2,
                  min_child_weight    = 27,
                  subsample           = .9,
                  colsample_bytree    = .9,
                  tree_method = 'hist'
  )
    
    
    
  #Cross Validation
  XGB_cv<-xgb.cv(params=param,nrounds=10000,nfold=5,missing=NA,data=dtrain,print_every_n=1,nthread=4, 
                   early_stopping_rounds=25)
    
  #removed test = dtest and put after the cross validation (potential bug fix)
  watchlist <- list(train = dtrain)
    
  #Create Predictions
  XGB_model<-xgb.train(params=param,nrounds= XGB_cv$best_iteration,missing=NA,data=dtrain,watchlist=watchlist,
                         print_every_n=1,nthread=4)
    
  #Write out hyperparameters so they can be compared
  best_ntrees<-XGB_cv$best_iteration
  train_param<-data.table(t(param))
  train_param$best_ntrees<-best_ntrees
  train_error<-unclass(XGB_model)$evaluation_log[as.numeric(best_ntrees),]$train_rmse
  train_param$train_error<-train_error
  pred<-predict(XGB_model, newdata=dtest)
  train_param$test_error_train<-rmse(test_y, pred)
  train_param$cv_error<-XGB_cv$evaluation_log[XGB_cv$best_iteration]$test_rmse_mean
    
  path3<-"./project/src/models/HyperParams/Hyperparams"
  path4<-paste0(path3,pos)
  path5<-paste0(path4,rolltime,"final",".csv")
  fwrite(train_param,path5)
  
  #Save model
  modelpath<-paste0('./project/volume/models/',rolltime,'_',pos,"_hyperparam_2016.model")
  saveRDS(XGB_model, modelpath)
    

  
  #best_hyper_index<-which(train_param$cv_error==min(train_param$cv_error))
  #modelpath<-paste0('./project/volume/models/',rolltime,'_',pos,"final",best_hyper_index,".model")
  #mXGB_model<-readRDS(modelpath)
  
  plotfile<-paste0("./project/volume/data/importanceplots/",rolltime,'final',pos,".pdf")
  pdf(plotfile)
  
  cols<-XGB_model[["feature_names"]]
  importance <- xgb.importance(feature_names = cols, model = XGB_model)
  xgb.plot.importance(importance_matrix = importance, top_n = 50)
  dev.off()
  
  #Create Predictions
  pred<-predict(XGB_model, newdata=dtest)
  
  #Add predictions to output data table as new column, PredPoints
  output$PredPoints<-pred

  #Write out output table to teamconstruction folder, where it can be used to aid in team construction
  teampath<-paste0("./project/volume/data/teamconstruction/",rolltime,"_final_",pos,".csv")
  fwrite(output,teampath)
  
}

###############
#Main Function
###############
model1<-function(rolltime, pos){
  runModel(rolltime,pos)
}

model1(rolltime,pos)


