library(data.table)
library(caret)

#remove file if it already exists to not override the data
if (file.exists("./project/volume/data/processed/train(roll4).csv")) {
        file.remove("./project/volume/data/processed/train(roll4).csv")}

playerstats<-fread("./project/volume/data/processed/playerstats(roll4).csv")
teamstats<-fread("./project/volume/data/processed/teamstats(roll4).csv")

playerstats<-playerstats[,.(season,week,Tm,Player,Pos,PPRFantasyPoints,game_id,Opponent,roll_4_PassingYds,roll_4_PassingTD,
                            roll_4_Int,roll_4_PassingAtt,roll_4_Cmp,roll_4_RushingAtt,roll_4_RushingYds,
                            roll_4_RushingTD,roll_4_Rec,roll_4_Tgt,roll_4_ReceivingYds,roll_4_ReceivingTD,
                            roll_4_FL,roll_4_PPRFantasyPoints)]

teamstats<-teamstats[,.(season,week,Tm,cumulativeweek,game_id,Opponent,roll_4_PassingYds,roll_4_PassingTD,roll_4_Int,
                        roll_4_PassingAtt, roll_4_Cmp,roll_4_RushingAtt,roll_4_RushingYds,roll_4_RushingTD,
                        roll_4_Rec,roll_4_Tgt,roll_4_ReceivingYds,roll_4_ReceivingTD,roll_4_FL)]

setnames(playerstats, c("roll_4_PassingYds","roll_4_PassingTD","roll_4_Int","roll_4_PassingAtt","roll_4_Cmp","roll_4_RushingAtt",
                        "roll_4_RushingYds","roll_4_RushingTD","roll_4_Rec","roll_4_Tgt","roll_4_ReceivingYds",
                        "roll_4_ReceivingTD","roll_4_FL","roll_4_PPRFantasyPoints"), 
                        c("Player_roll_4_PassingYds", "Player_roll_4_PassingTD","Player_roll_4_Int","Player_roll_4_PassingAtt",
                        "Player_roll_4_Cmp","Player_roll_4_RushingAtt","Player_roll_4_RushingYds",
                        "Player_roll_4_RushingTD","Player_roll_4_Rec","Player_roll_4_Tgt",
                        "Player_roll_4_ReceivingYds","Player_roll_4_ReceivingTD","Player_roll_4_FL",
                        "Player_roll_4_PPRFantasyPoints"))


setnames(teamstats, c("roll_4_PassingYds","roll_4_PassingTD","roll_4_Int","roll_4_PassingAtt","roll_4_Cmp","roll_4_RushingAtt",
                        "roll_4_RushingYds","roll_4_RushingTD","roll_4_Rec","roll_4_Tgt","roll_4_ReceivingYds",
                        "roll_4_ReceivingTD","roll_4_FL"), 
                         c("TeamAllowed_roll_4_PassingYds", "TeamAllowed_roll_4_PassingTD","TeamAllowed_roll_4_Int","TeamAllowed_roll_4_PassingAtt",
                         "TeamAllowed_roll_4_Cmp","TeamAllowed_roll_4_RushingAtt","TeamAllowed_roll_4_RushingYds",
                         "TeamAllowed_roll_4_RushingTD","TeamAllowed_roll_4_Rec","TeamAllowed_roll_4_Tgt",
                         "TeamAllowed_roll_4_ReceivingYds","TeamAllowed_roll_4_ReceivingTD","TeamAllowed_roll_4_FL"))

#Set Keys and Merge Together PlayerStats and Teamstats to create train set
setkey(playerstats,season,week,Tm,game_id,Opponent)
setkey(teamstats,season,week,Tm,game_id,Opponent)
train<-merge(playerstats,teamstats)

#Calculate difference values for applicable rows

#ReceivingYds
scale_transform<-preProcess(train[,.(Player_roll_4_ReceivingYds,TeamAllowed_roll_4_ReceivingYds)],
                            method=c("center","scale"))
scaled_receiving_yds<-predict(scale_transform,train[,.(Player_roll_4_ReceivingYds,TeamAllowed_roll_4_ReceivingYds)])
train$ReceivingYds_Dif<-scaled_receiving_yds$Player_roll_4_ReceivingYds-scaled_receiving_yds$TeamAllowed_roll_4_ReceivingYds
#RushingYds
scale_transform2<-preProcess(train[,.(Player_roll_4_RushingYds,TeamAllowed_roll_4_RushingYds)],
                             method=c("center","scale"))
scaled_rushing_yds<-predict(scale_transform2,train[,.(Player_roll_4_RushingYds,TeamAllowed_roll_4_RushingYds)])
train$RushingYds_Dif<-scaled_rushing_yds$Player_roll_4_RushingYds-scaled_rushing_yds$TeamAllowed_roll_4_RushingYds
#PassingYds
scale_transform3<-preProcess(train[,.(Player_roll_4_PassingYds,TeamAllowed_roll_4_PassingYds)],
                             method=c("center","scale"))
scaled_passing_yds<-predict(scale_transform3,train[,.(Player_roll_4_PassingYds,TeamAllowed_roll_4_PassingYds)])
train$PassingYds_Dif<-scaled_passing_yds$Player_roll_4_PassingYds-scaled_passing_yds$TeamAllowed_roll_4_PassingYds
#Targets
scale_transform4<-preProcess(train[,.(Player_roll_4_Tgt,TeamAllowed_roll_4_Tgt)],
                             method=c("center","scale"))
scaled_targets<-predict(scale_transform4,train[,.(Player_roll_4_Tgt,TeamAllowed_roll_4_Tgt)])
train$Targets_Dif<-scaled_targets$Player_roll_4_Tgt-scaled_targets$TeamAllowed_roll_4_Tgt
#PassingAttempts
scale_transform5<-preProcess(train[,.(Player_roll_4_PassingAtt,TeamAllowed_roll_4_PassingAtt)],
                             method=c("center","scale"))
scaled_passingattempts<-predict(scale_transform5,train[,.(Player_roll_4_PassingAtt,TeamAllowed_roll_4_PassingAtt)])
train$Passing_Attempts_Dif<-scaled_passingattempts$Player_roll_4_PassingAtt-scaled_passingattempts$TeamAllowed_roll_4_PassingAtt
#RushingAttempts
scale_transform6<-preProcess(train[,.(Player_roll_4_RushingAtt,TeamAllowed_roll_4_RushingAtt)],
                             method=c("center","scale"))
scaled_rushingattempts<-predict(scale_transform6,train[,.(Player_roll_4_RushingAtt,TeamAllowed_roll_4_RushingAtt)])
train$Rushing_Attempts_Dif<-scaled_rushingattempts$Player_roll_4_RushingAtt-scaled_rushingattempts$TeamAllowed_roll_4_RushingAtt
#ReceivingTDs
scale_transform7<-preProcess(train[,.(Player_roll_4_ReceivingTD,TeamAllowed_roll_4_ReceivingTD)],
                             method=c("center","scale"))
scaled_receivingtds<-predict(scale_transform7,train[,.(Player_roll_4_ReceivingTD,TeamAllowed_roll_4_ReceivingTD)])
train$ReceivingTDs_Dif<-scaled_receivingtds$Player_roll_4_ReceivingTD-scaled_receivingtds$TeamAllowed_roll_4_ReceivingTD
#RushingTDs
scale_transform8<-preProcess(train[,.(Player_roll_4_RushingTD,TeamAllowed_roll_4_RushingTD)],
                             method=c("center","scale"))
scaled_rushingtds<-predict(scale_transform8,train[,.(Player_roll_4_RushingTD,TeamAllowed_roll_4_RushingTD)])
train$RushingTDs_Dif<-scaled_rushingtds$Player_roll_4_RushingTD-scaled_rushingtds$TeamAllowed_roll_4_RushingTD
#PassingTDs
scale_transform9<-preProcess(train[,.(Player_roll_4_PassingTD,TeamAllowed_roll_4_PassingTD)],
                             method=c("center","scale"))
scaled_passingtds<-predict(scale_transform9,train[,.(Player_roll_4_PassingTD,TeamAllowed_roll_4_PassingTD)])
train$PassingTDs_Dif<-scaled_passingtds$Player_roll_4_PassingTD-scaled_passingtds$TeamAllowed_roll_4_PassingTD


#Write out train set to processed folder
fwrite(train, "./project/volume/data/processed/train(roll4).csv")


