library(data.table)
library(caret)

#remove file if it already exists to not override the data
if (file.exists("./project/volume/data/processed/train(roll7).csv")) {
  file.remove("./project/volume/data/processed/train(roll7).csv")}

playerstats<-fread("./project/volume/data/processed/playerstats(roll7).csv")
teamstats<-fread("./project/volume/data/processed/teamstats(roll7).csv")

playerstats<-playerstats[,.(season,week,Tm,Player,Pos,PPRFantasyPoints,game_id,Opponent,roll_7_PassingYds,roll_7_PassingTD,
                            roll_7_Int,roll_7_PassingAtt,roll_7_Cmp,roll_7_RushingAtt,roll_7_RushingYds,
                            roll_7_RushingTD,roll_7_Rec,roll_7_Tgt,roll_7_ReceivingYds,roll_7_ReceivingTD,
                            roll_7_FL,roll_7_PPRFantasyPoints,roll_7_Tgt_share,roll_7_Rushing_share,roll_7_Passing_share,
                            roll_7_Teamtotalpassingattempts,roll_7_Teamtotalrushingattempts,roll_7_Teamvariancepassingshare,
                            roll_7_Teamvariancerushingshare,roll_7_Teamvariancetargetshare)]

teamstats<-teamstats[,.(season,week,Tm,cumulativeweek,game_id,Opponent,roll_7_PassingYds,roll_7_PassingTD,roll_7_Int,
                        roll_7_PassingAtt, roll_7_Cmp,roll_7_RushingAtt,roll_7_RushingYds,roll_7_RushingTD,
                        roll_7_Rec,roll_7_Tgt,roll_7_ReceivingYds,roll_7_ReceivingTD,roll_7_FL,roll_7_Tgt_share,roll_7_Rushing_share,
                        roll_7_Passing_share,roll_7_Teamtotalpassingattempts,roll_7_Teamtotalrushingattempts,
                        roll_7_Teamvariancepassingshare,roll_7_Teamvariancerushingshare,roll_7_Teamvariancetargetshare)]

setnames(playerstats, c("roll_7_PassingYds","roll_7_PassingTD","roll_7_Int","roll_7_PassingAtt","roll_7_Cmp","roll_7_RushingAtt",
                        "roll_7_RushingYds","roll_7_RushingTD","roll_7_Rec","roll_7_Tgt","roll_7_ReceivingYds",
                        "roll_7_ReceivingTD","roll_7_FL","roll_7_PPRFantasyPoints","roll_7_Tgt_share","roll_7_Rushing_share",
                        "roll_7_Passing_share","roll_7_Teamtotalpassingattempts","roll_7_Teamtotalrushingattempts",
                        "roll_7_Teamvariancepassingshare","roll_7_Teamvariancerushingshare","roll_7_Teamvariancetargetshare"), 
         c("Player_roll_7_PassingYds", "Player_roll_7_PassingTD","Player_roll_7_Int","Player_roll_7_PassingAtt",
           "Player_roll_7_Cmp","Player_roll_7_RushingAtt","Player_roll_7_RushingYds",
           "Player_roll_7_RushingTD","Player_roll_7_Rec","Player_roll_7_Tgt",
           "Player_roll_7_ReceivingYds","Player_roll_7_ReceivingTD","Player_roll_7_FL",
           "Player_roll_7_PPRFantasyPoints","Player_roll_7_Tgt_share","Player_roll_7_Rushing_share",
           "Player_roll_7_Passing_share","Player_roll_7_Teamtotalpassingattempts","Player_roll_7_Teamtotalrushingattempts",
           "Player_roll_7_Teamvariancepassingshare","Player_roll_7_Teamvariancerushingshare",
           "Player_roll_7_Teamvariancetargetshare"))


setnames(teamstats, c("roll_7_PassingYds","roll_7_PassingTD","roll_7_Int","roll_7_PassingAtt","roll_7_Cmp","roll_7_RushingAtt",
                      "roll_7_RushingYds","roll_7_RushingTD","roll_7_Rec","roll_7_Tgt","roll_7_ReceivingYds",
                      "roll_7_ReceivingTD","roll_7_FL","roll_7_Tgt_share","roll_7_Rushing_share",
                      "roll_7_Passing_share","roll_7_Teamtotalpassingattempts","roll_7_Teamtotalrushingattempts",
                      "roll_7_Teamvariancepassingshare","roll_7_Teamvariancerushingshare","roll_7_Teamvariancetargetshare"), 
         c("TeamAllowed_roll_7_PassingYds", "TeamAllowed_roll_7_PassingTD","TeamAllowed_roll_7_Int","TeamAllowed_roll_7_PassingAtt",
           "TeamAllowed_roll_7_Cmp","TeamAllowed_roll_7_RushingAtt","TeamAllowed_roll_7_RushingYds",
           "TeamAllowed_roll_7_RushingTD","TeamAllowed_roll_7_Rec","TeamAllowed_roll_7_Tgt",
           "TeamAllowed_roll_7_ReceivingYds","TeamAllowed_roll_7_ReceivingTD","TeamAllowed_roll_7_FL",
           "TeamAllowed_roll_7_Tgt_share","TeamAllowed_roll_7_Rushing_share","TeamAllowed_roll_7_Passing_share",
           "TeamAllowed_roll_7_Teamtotalpassingattempts","TeamAllowed_roll_7_Teamtotalrushingattempts",
           "TeamAllowed_roll_7_Teamvariancepassingshare","TeamAllowed_roll_7_Teamvariancerushingshare",
           "TeamAllowed_roll_7_Teamvariancetargetshare"))

#Set Keys and Merge Together PlayerStats and Teamstats to create train set
setkey(playerstats,season,week,Tm,game_id,Opponent)
setkey(teamstats,season,week,Tm,game_id,Opponent)
train<-merge(playerstats,teamstats)

#Calculate difference values for applicable rows

#ReceivingYds
scale_transform<-preProcess(train[,.(Player_roll_7_ReceivingYds,TeamAllowed_roll_7_ReceivingYds)],
                            method=c("center","scale"))
scaled_receiving_yds<-predict(scale_transform,train[,.(Player_roll_7_ReceivingYds,TeamAllowed_roll_7_ReceivingYds)])
train$ReceivingYds_Dif<-scaled_receiving_yds$Player_roll_7_ReceivingYds-scaled_receiving_yds$TeamAllowed_roll_7_ReceivingYds
#RushingYds
scale_transform2<-preProcess(train[,.(Player_roll_7_RushingYds,TeamAllowed_roll_7_RushingYds)],
                             method=c("center","scale"))
scaled_rushing_yds<-predict(scale_transform2,train[,.(Player_roll_7_RushingYds,TeamAllowed_roll_7_RushingYds)])
train$RushingYds_Dif<-scaled_rushing_yds$Player_roll_7_RushingYds-scaled_rushing_yds$TeamAllowed_roll_7_RushingYds
#PassingYds
scale_transform3<-preProcess(train[,.(Player_roll_7_PassingYds,TeamAllowed_roll_7_PassingYds)],
                             method=c("center","scale"))
scaled_passing_yds<-predict(scale_transform3,train[,.(Player_roll_7_PassingYds,TeamAllowed_roll_7_PassingYds)])
train$PassingYds_Dif<-scaled_passing_yds$Player_roll_7_PassingYds-scaled_passing_yds$TeamAllowed_roll_7_PassingYds
#Targets
scale_transform4<-preProcess(train[,.(Player_roll_7_Tgt,TeamAllowed_roll_7_Tgt)],
                             method=c("center","scale"))
scaled_targets<-predict(scale_transform4,train[,.(Player_roll_7_Tgt,TeamAllowed_roll_7_Tgt)])
train$Targets_Dif<-scaled_targets$Player_roll_7_Tgt-scaled_targets$TeamAllowed_roll_7_Tgt
#PassingAttempts
scale_transform5<-preProcess(train[,.(Player_roll_7_PassingAtt,TeamAllowed_roll_7_PassingAtt)],
                             method=c("center","scale"))
scaled_passingattempts<-predict(scale_transform5,train[,.(Player_roll_7_PassingAtt,TeamAllowed_roll_7_PassingAtt)])
train$Passing_Attempts_Dif<-scaled_passingattempts$Player_roll_7_PassingAtt-scaled_passingattempts$TeamAllowed_roll_7_PassingAtt
#RushingAttempts
scale_transform6<-preProcess(train[,.(Player_roll_7_RushingAtt,TeamAllowed_roll_7_RushingAtt)],
                             method=c("center","scale"))
scaled_rushingattempts<-predict(scale_transform6,train[,.(Player_roll_7_RushingAtt,TeamAllowed_roll_7_RushingAtt)])
train$Rushing_Attempts_Dif<-scaled_rushingattempts$Player_roll_7_RushingAtt-scaled_rushingattempts$TeamAllowed_roll_7_RushingAtt
#ReceivingTDs
scale_transform7<-preProcess(train[,.(Player_roll_7_ReceivingTD,TeamAllowed_roll_7_ReceivingTD)],
                             method=c("center","scale"))
scaled_receivingtds<-predict(scale_transform7,train[,.(Player_roll_7_ReceivingTD,TeamAllowed_roll_7_ReceivingTD)])
train$ReceivingTDs_Dif<-scaled_receivingtds$Player_roll_7_ReceivingTD-scaled_receivingtds$TeamAllowed_roll_7_ReceivingTD
#RushingTDs
scale_transform8<-preProcess(train[,.(Player_roll_7_RushingTD,TeamAllowed_roll_7_RushingTD)],
                             method=c("center","scale"))
scaled_rushingtds<-predict(scale_transform8,train[,.(Player_roll_7_RushingTD,TeamAllowed_roll_7_RushingTD)])
train$RushingTDs_Dif<-scaled_rushingtds$Player_roll_7_RushingTD-scaled_rushingtds$TeamAllowed_roll_7_RushingTD
#PassingTDs
scale_transform9<-preProcess(train[,.(Player_roll_7_PassingTD,TeamAllowed_roll_7_PassingTD)],
                             method=c("center","scale"))
scaled_passingtds<-predict(scale_transform9,train[,.(Player_roll_7_PassingTD,TeamAllowed_roll_7_PassingTD)])
train$PassingTDs_Dif<-scaled_passingtds$Player_roll_7_PassingTD-scaled_passingtds$TeamAllowed_roll_7_PassingTD
#RushingVar
scale_transform10<-preProcess(train[,.(Player_roll_7_Teamvariancerushingshare,TeamAllowed_roll_7_Teamvariancerushingshare)],
                              method=c("center","scale"))
scaled_rushingshare<-predict(scale_transform10,train[,.(Player_roll_7_Teamvariancerushingshare,TeamAllowed_roll_7_Teamvariancerushingshare)])
train$Rushing_Var_Dif<-scaled_rushingshare$Player_roll_7_Teamvariancerushingshare-scaled_rushingshare$TeamAllowed_roll_7_Teamvariancerushingshare
#PassingVar
scale_transform11<-preProcess(train[,.(Player_roll_7_Teamvariancepassingshare,TeamAllowed_roll_7_Teamvariancepassingshare)],
                              method=c("center","scale"))
scaled_passingshare<-predict(scale_transform11,train[,.(Player_roll_7_Teamvariancepassingshare,TeamAllowed_roll_7_Teamvariancepassingshare)])
train$Passing_Var_Dif<-scaled_passingshare$Player_roll_7_Teamvariancepassingshare-scaled_passingshare$TeamAllowed_roll_7_Teamvariancepassingshare
#TargetVar
scale_transform12<-preProcess(train[,.(Player_roll_7_Teamvariancetargetshare,TeamAllowed_roll_7_Teamvariancetargetshare)],
                              method=c("center","scale"))
scaled_tgtshare<-predict(scale_transform12,train[,.(Player_roll_7_Teamvariancetargetshare,TeamAllowed_roll_7_Teamvariancetargetshare)])
train$Tgt_Var_Dif<-scaled_tgtshare$Player_roll_7_Teamvariancetargetshare-scaled_tgtshare$TeamAllowed_roll_7_Teamvariancetargetshare
#RushingShare
scale_transform13<-preProcess(train[,.(Player_roll_7_Rushing_share,TeamAllowed_roll_7_Rushing_share)],
                              method=c("center","scale"))
scaled_rushing<-predict(scale_transform13,train[,.(Player_roll_7_Rushing_share,TeamAllowed_roll_7_Rushing_share)])
train$Rushing_Share_Dif<-scaled_rushing$Player_roll_7_Rushing_share-scaled_rushing$TeamAllowed_roll_7_Rushing_share
#PassingShare
scale_transform14<-preProcess(train[,.(Player_roll_7_Passing_share,TeamAllowed_roll_7_Passing_share)],
                              method=c("center","scale"))
scaled_passing<-predict(scale_transform14,train[,.(Player_roll_7_Passing_share,TeamAllowed_roll_7_Passing_share)])
train$Passing_Share_Dif<-scaled_passing$Player_roll_7_Passing_share-scaled_passing$TeamAllowed_roll_7_Passing_share
#TargetShare
scale_transform15<-preProcess(train[,.(Player_roll_7_Tgt_share,TeamAllowed_roll_7_Tgt_share)],
                              method=c("center","scale"))
scaled_tgt<-predict(scale_transform15,train[,.(Player_roll_7_Tgt_share,TeamAllowed_roll_7_Tgt_share)])
train$Tgt_Share_Dif<-scaled_tgt$Player_roll_7_Tgt_share-scaled_tgt$TeamAllowed_roll_7_Tgt_share



#Write out train set to processed folder
fwrite(train, "./project/volume/data/processed/train(roll7).csv")


