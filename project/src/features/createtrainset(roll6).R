library(data.table)
library(caret)

#remove file if it already exists to not override the data
if (file.exists("./project/volume/data/processed/train(roll6).csv")) {
  file.remove("./project/volume/data/processed/train(roll6).csv")}

playerstats<-fread("./project/volume/data/processed/playerstats(roll6).csv")
teamstats<-fread("./project/volume/data/processed/teamstats(roll6).csv")

playerstats<-playerstats[,.(season,week,Tm,Player,Pos,PPRFantasyPoints,game_id,Opponent,roll_6_PassingYds,roll_6_PassingTD,
                            roll_6_Int,roll_6_PassingAtt,roll_6_Cmp,roll_6_RushingAtt,roll_6_RushingYds,
                            roll_6_RushingTD,roll_6_Rec,roll_6_Tgt,roll_6_ReceivingYds,roll_6_ReceivingTD,
                            roll_6_FL,roll_6_PPRFantasyPoints,roll_6_Tgt_share,roll_6_Rushing_share,roll_6_Passing_share,
                            roll_6_Teamtotalpassingattempts,roll_6_Teamtotalrushingattempts,roll_6_Teamvariancepassingshare,
                            roll_6_Teamvariancerushingshare,roll_6_Teamvariancetargetshare)]

teamstats<-teamstats[,.(season,week,Tm,cumulativeweek,game_id,Opponent,roll_6_PassingYds,roll_6_PassingTD,roll_6_Int,
                        roll_6_PassingAtt, roll_6_Cmp,roll_6_RushingAtt,roll_6_RushingYds,roll_6_RushingTD,
                        roll_6_Rec,roll_6_Tgt,roll_6_ReceivingYds,roll_6_ReceivingTD,roll_6_FL,roll_6_Tgt_share,roll_6_Rushing_share,
                        roll_6_Passing_share,roll_6_Teamtotalpassingattempts,roll_6_Teamtotalrushingattempts,
                        roll_6_Teamvariancepassingshare,roll_6_Teamvariancerushingshare,roll_6_Teamvariancetargetshare)]

setnames(playerstats, c("roll_6_PassingYds","roll_6_PassingTD","roll_6_Int","roll_6_PassingAtt","roll_6_Cmp","roll_6_RushingAtt",
                        "roll_6_RushingYds","roll_6_RushingTD","roll_6_Rec","roll_6_Tgt","roll_6_ReceivingYds",
                        "roll_6_ReceivingTD","roll_6_FL","roll_6_PPRFantasyPoints","roll_6_Tgt_share","roll_6_Rushing_share",
                        "roll_6_Passing_share","roll_6_Teamtotalpassingattempts","roll_6_Teamtotalrushingattempts",
                        "roll_6_Teamvariancepassingshare","roll_6_Teamvariancerushingshare","roll_6_Teamvariancetargetshare"), 
         c("Player_roll_6_PassingYds", "Player_roll_6_PassingTD","Player_roll_6_Int","Player_roll_6_PassingAtt",
           "Player_roll_6_Cmp","Player_roll_6_RushingAtt","Player_roll_6_RushingYds",
           "Player_roll_6_RushingTD","Player_roll_6_Rec","Player_roll_6_Tgt",
           "Player_roll_6_ReceivingYds","Player_roll_6_ReceivingTD","Player_roll_6_FL",
           "Player_roll_6_PPRFantasyPoints","Player_roll_6_Tgt_share","Player_roll_6_Rushing_share",
           "Player_roll_6_Passing_share","Player_roll_6_Teamtotalpassingattempts","Player_roll_6_Teamtotalrushingattempts",
           "Player_roll_6_Teamvariancepassingshare","Player_roll_6_Teamvariancerushingshare",
           "Player_roll_6_Teamvariancetargetshare"))


setnames(teamstats, c("roll_6_PassingYds","roll_6_PassingTD","roll_6_Int","roll_6_PassingAtt","roll_6_Cmp","roll_6_RushingAtt",
                      "roll_6_RushingYds","roll_6_RushingTD","roll_6_Rec","roll_6_Tgt","roll_6_ReceivingYds",
                      "roll_6_ReceivingTD","roll_6_FL","roll_6_Tgt_share","roll_6_Rushing_share",
                      "roll_6_Passing_share","roll_6_Teamtotalpassingattempts","roll_6_Teamtotalrushingattempts",
                      "roll_6_Teamvariancepassingshare","roll_6_Teamvariancerushingshare","roll_6_Teamvariancetargetshare"), 
         c("TeamAllowed_roll_6_PassingYds", "TeamAllowed_roll_6_PassingTD","TeamAllowed_roll_6_Int","TeamAllowed_roll_6_PassingAtt",
           "TeamAllowed_roll_6_Cmp","TeamAllowed_roll_6_RushingAtt","TeamAllowed_roll_6_RushingYds",
           "TeamAllowed_roll_6_RushingTD","TeamAllowed_roll_6_Rec","TeamAllowed_roll_6_Tgt",
           "TeamAllowed_roll_6_ReceivingYds","TeamAllowed_roll_6_ReceivingTD","TeamAllowed_roll_6_FL",
           "TeamAllowed_roll_6_Tgt_share","TeamAllowed_roll_6_Rushing_share","TeamAllowed_roll_6_Passing_share",
           "TeamAllowed_roll_6_Teamtotalpassingattempts","TeamAllowed_roll_6_Teamtotalrushingattempts",
           "TeamAllowed_roll_6_Teamvariancepassingshare","TeamAllowed_roll_6_Teamvariancerushingshare",
           "TeamAllowed_roll_6_Teamvariancetargetshare"))

#Set Keys and Merge Together PlayerStats and Teamstats to create train set
setkey(playerstats,season,week,Tm,game_id,Opponent)
setkey(teamstats,season,week,Tm,game_id,Opponent)
train<-merge(playerstats,teamstats)

#Calculate difference values for applicable rows

#ReceivingYds
scale_transform<-preProcess(train[,.(Player_roll_6_ReceivingYds,TeamAllowed_roll_6_ReceivingYds)],
                            method=c("center","scale"))
scaled_receiving_yds<-predict(scale_transform,train[,.(Player_roll_6_ReceivingYds,TeamAllowed_roll_6_ReceivingYds)])
train$ReceivingYds_Dif<-scaled_receiving_yds$Player_roll_6_ReceivingYds-scaled_receiving_yds$TeamAllowed_roll_6_ReceivingYds
#RushingYds
scale_transform2<-preProcess(train[,.(Player_roll_6_RushingYds,TeamAllowed_roll_6_RushingYds)],
                             method=c("center","scale"))
scaled_rushing_yds<-predict(scale_transform2,train[,.(Player_roll_6_RushingYds,TeamAllowed_roll_6_RushingYds)])
train$RushingYds_Dif<-scaled_rushing_yds$Player_roll_6_RushingYds-scaled_rushing_yds$TeamAllowed_roll_6_RushingYds
#PassingYds
scale_transform3<-preProcess(train[,.(Player_roll_6_PassingYds,TeamAllowed_roll_6_PassingYds)],
                             method=c("center","scale"))
scaled_passing_yds<-predict(scale_transform3,train[,.(Player_roll_6_PassingYds,TeamAllowed_roll_6_PassingYds)])
train$PassingYds_Dif<-scaled_passing_yds$Player_roll_6_PassingYds-scaled_passing_yds$TeamAllowed_roll_6_PassingYds
#Targets
scale_transform4<-preProcess(train[,.(Player_roll_6_Tgt,TeamAllowed_roll_6_Tgt)],
                             method=c("center","scale"))
scaled_targets<-predict(scale_transform4,train[,.(Player_roll_6_Tgt,TeamAllowed_roll_6_Tgt)])
train$Targets_Dif<-scaled_targets$Player_roll_6_Tgt-scaled_targets$TeamAllowed_roll_6_Tgt
#PassingAttempts
scale_transform5<-preProcess(train[,.(Player_roll_6_PassingAtt,TeamAllowed_roll_6_PassingAtt)],
                             method=c("center","scale"))
scaled_passingattempts<-predict(scale_transform5,train[,.(Player_roll_6_PassingAtt,TeamAllowed_roll_6_PassingAtt)])
train$Passing_Attempts_Dif<-scaled_passingattempts$Player_roll_6_PassingAtt-scaled_passingattempts$TeamAllowed_roll_6_PassingAtt
#RushingAttempts
scale_transform6<-preProcess(train[,.(Player_roll_6_RushingAtt,TeamAllowed_roll_6_RushingAtt)],
                             method=c("center","scale"))
scaled_rushingattempts<-predict(scale_transform6,train[,.(Player_roll_6_RushingAtt,TeamAllowed_roll_6_RushingAtt)])
train$Rushing_Attempts_Dif<-scaled_rushingattempts$Player_roll_6_RushingAtt-scaled_rushingattempts$TeamAllowed_roll_6_RushingAtt
#ReceivingTDs
scale_transform7<-preProcess(train[,.(Player_roll_6_ReceivingTD,TeamAllowed_roll_6_ReceivingTD)],
                             method=c("center","scale"))
scaled_receivingtds<-predict(scale_transform7,train[,.(Player_roll_6_ReceivingTD,TeamAllowed_roll_6_ReceivingTD)])
train$ReceivingTDs_Dif<-scaled_receivingtds$Player_roll_6_ReceivingTD-scaled_receivingtds$TeamAllowed_roll_6_ReceivingTD
#RushingTDs
scale_transform8<-preProcess(train[,.(Player_roll_6_RushingTD,TeamAllowed_roll_6_RushingTD)],
                             method=c("center","scale"))
scaled_rushingtds<-predict(scale_transform8,train[,.(Player_roll_6_RushingTD,TeamAllowed_roll_6_RushingTD)])
train$RushingTDs_Dif<-scaled_rushingtds$Player_roll_6_RushingTD-scaled_rushingtds$TeamAllowed_roll_6_RushingTD
#PassingTDs
scale_transform9<-preProcess(train[,.(Player_roll_6_PassingTD,TeamAllowed_roll_6_PassingTD)],
                             method=c("center","scale"))
scaled_passingtds<-predict(scale_transform9,train[,.(Player_roll_6_PassingTD,TeamAllowed_roll_6_PassingTD)])
train$PassingTDs_Dif<-scaled_passingtds$Player_roll_6_PassingTD-scaled_passingtds$TeamAllowed_roll_6_PassingTD
#RushingVar
scale_transform10<-preProcess(train[,.(Player_roll_6_Teamvariancerushingshare,TeamAllowed_roll_6_Teamvariancerushingshare)],
                              method=c("center","scale"))
scaled_rushingshare<-predict(scale_transform10,train[,.(Player_roll_6_Teamvariancerushingshare,TeamAllowed_roll_6_Teamvariancerushingshare)])
train$Rushing_Var_Dif<-scaled_rushingshare$Player_roll_6_Teamvariancerushingshare-scaled_rushingshare$TeamAllowed_roll_6_Teamvariancerushingshare
#PassingVar
scale_transform11<-preProcess(train[,.(Player_roll_6_Teamvariancepassingshare,TeamAllowed_roll_6_Teamvariancepassingshare)],
                              method=c("center","scale"))
scaled_passingshare<-predict(scale_transform11,train[,.(Player_roll_6_Teamvariancepassingshare,TeamAllowed_roll_6_Teamvariancepassingshare)])
train$Passing_Var_Dif<-scaled_passingshare$Player_roll_6_Teamvariancepassingshare-scaled_passingshare$TeamAllowed_roll_6_Teamvariancepassingshare
#TargetVar
scale_transform12<-preProcess(train[,.(Player_roll_6_Teamvariancetargetshare,TeamAllowed_roll_6_Teamvariancetargetshare)],
                              method=c("center","scale"))
scaled_tgtshare<-predict(scale_transform12,train[,.(Player_roll_6_Teamvariancetargetshare,TeamAllowed_roll_6_Teamvariancetargetshare)])
train$Tgt_Var_Dif<-scaled_tgtshare$Player_roll_6_Teamvariancetargetshare-scaled_tgtshare$TeamAllowed_roll_6_Teamvariancetargetshare
#RushingShare
scale_transform13<-preProcess(train[,.(Player_roll_6_Rushing_share,TeamAllowed_roll_6_Rushing_share)],
                              method=c("center","scale"))
scaled_rushing<-predict(scale_transform13,train[,.(Player_roll_6_Rushing_share,TeamAllowed_roll_6_Rushing_share)])
train$Rushing_Share_Dif<-scaled_rushing$Player_roll_6_Rushing_share-scaled_rushing$TeamAllowed_roll_6_Rushing_share
#PassingShare
scale_transform14<-preProcess(train[,.(Player_roll_6_Passing_share,TeamAllowed_roll_6_Passing_share)],
                              method=c("center","scale"))
scaled_passing<-predict(scale_transform14,train[,.(Player_roll_6_Passing_share,TeamAllowed_roll_6_Passing_share)])
train$Passing_Share_Dif<-scaled_passing$Player_roll_6_Passing_share-scaled_passing$TeamAllowed_roll_6_Passing_share
#TargetShare
scale_transform15<-preProcess(train[,.(Player_roll_6_Tgt_share,TeamAllowed_roll_6_Tgt_share)],
                              method=c("center","scale"))
scaled_tgt<-predict(scale_transform15,train[,.(Player_roll_6_Tgt_share,TeamAllowed_roll_6_Tgt_share)])
train$Tgt_Share_Dif<-scaled_tgt$Player_roll_6_Tgt_share-scaled_tgt$TeamAllowed_roll_6_Tgt_share



#Write out train set to processed folder
fwrite(train, "./project/volume/data/processed/train(roll6).csv")


