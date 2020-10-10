library(data.table)

#Load in player and team data
playerstats<-fread("./project/volume/data/processed/playerstats.csv")
teamstats<-fread("./project/volume/data/processed/teamstats.csv")

#Subset out columns of interest in playerstats table
playerstats<-playerstats[,.(season,week,Tm,Player,Pos,PPRFantasyPoints,game_id,Opponent,roll_4_PassingYds,
                            roll_4_Int,roll_4_PassingAtt,roll_4_Cmp,roll_4_RushingAtt,roll_4_RushingYds,
                            roll_4_RushingTD,roll_4_Rec,roll_4_Tgt,roll_4_ReceivingYds,roll_4_ReceivingTD,
                            roll_4_FL,roll_4_PPRFantasyPoints)]

#Subset out columns of interest in teamstats table
teamstats<-teamstats[,.(season,week,Tm,cumulativeweek,game_id,Opponent,roll_4_PassingYds,roll_4_Int,
                        roll_4_PassingAtt, roll_4_Cmp,roll_4_RushingAtt,roll_4_RushingYds,roll_4_RushingTD,
                        roll_4_Rec,roll_4_Tgt,roll_4_ReceivingYds,roll_4_ReceivingTD,roll_4_FL)]


#Change column names in playerstats table
setnames(playerstats, c("roll_4_PassingYds","roll_4_Int","roll_4_PassingAtt","roll_4_Cmp","roll_4_RushingAtt",
                        "roll_4_RushingYds","roll_4_RushingTD","roll_4_Rec","roll_4_Tgt","roll_4_ReceivingYds",
                        "roll_4_ReceivingTD","roll_4_FL"),c("Player_roll_4_PassingYds","Player_roll_4_Int",
                        "Player_roll_4_PassingAtt", "Player_roll_4_Cmp","Player_roll_4_RushingAtt",
                        "Player_roll_4_RushingYds","Player_roll_4_RushingTD","Player_roll_4_Rec",
                        "Player_roll_4_Tgt","Player_roll_4_ReceivingYds","Player_roll_4_ReceivingTD",
                        "Player_roll_4_FL"))

#Change column names in teamstats table
setnames(teamstats, c("roll_4_PassingYds","roll_4_Int","roll_4_PassingAtt","roll_4_Cmp","roll_4_RushingAtt",
                        "roll_4_RushingYds","roll_4_RushingTD","roll_4_Rec","roll_4_Tgt","roll_4_ReceivingYds",
                        "roll_4_ReceivingTD","roll_4_FL"),c("Team_roll_4_PassingYds","Team_roll_4_Int",
                        "Team_roll_4_PassingAtt", "Team_roll_4_Cmp","Team_roll_4_RushingAtt",
                        "Team_roll_4_RushingYds","Team_roll_4_RushingTD","Team_roll_4_Rec",
                        "Team_roll_4_Tgt","Team_roll_4_ReceivingYds","Team_roll_4_ReceivingTD",
                        "Team_roll_4_FL"))

#Set keys
setkey(playerstats,season,week,Tm,game_id,Opponent)
setkey(teamstats,season,week,Tm,game_id,Opponent)

#Create train set by merging tables on shared columns
train<-merge(playerstats,teamstats)


fwrite(train,"./project/volume/data/processed/train.csv")

