library(data.table)


#remove file if it already exists to not override the data
if (file.exists("./project/volume/data/processed/train.csv")) {
        file.remove("./project/volume/data/processed/train.csv")}

playerstats<-fread("./project/volume/data/processed/playerstats.csv")
teamstats<-fread("./project/volume/data/processed/teamstats.csv")

playerstats<-playerstats[,.(season,week,Tm,Player,Pos,PPRFantasyPoints,game_id,Opponent,roll_4_PassingYds,
                            roll_4_Int,roll_4_PassingAtt,roll_4_Cmp,roll_4_RushingAtt,roll_4_RushingYds,
                            roll_4_RushingTD,roll_4_Rec,roll_4_Tgt,roll_4_ReceivingYds,roll_4_ReceivingTD,
                            roll_4_FL,roll_4_PPRFantasyPoints)]

teamstats<-teamstats[,.(season,week,Tm,cumulativeweek,game_id,Opponent,roll_4_PassingYds,roll_4_Int,
                        roll_4_PassingAtt, roll_4_Cmp,roll_4_RushingAtt,roll_4_RushingYds,roll_4_RushingTD,
                        roll_4_Rec,roll_4_Tgt,roll_4_ReceivingYds,roll_4_ReceivingTD,roll_4_FL)]

setnames(playerstats, c("roll_4_PassingYds","roll_4_Int","roll_4_PassingAtt","roll_4_Cmp","roll_4_RushingAtt",
                        "roll_4_RushingYds","roll_4_RushingTD","roll_4_Rec","roll_4_Tgt","roll_4_ReceivingYds",
                        "roll_4_ReceivingTD","roll_4_FL","roll_4_PPRFantasyPoints"), 
                        c("Player_roll_4_PassingYds","Player_roll_4_Int","Player_roll_4_PassingAtt",
                        "Player_roll_4_Cmp","Player_roll_4_RushingAtt","Player_roll_4_RushingYds",
                        "Player_roll_4_RushingTD","Player_roll_4_Rec","Player_roll_4_Tgt",
                        "Player_roll_4_ReceivingYds","Player_roll_4_ReceivingTD","Player_roll_4_FL",
                        "Player_roll_4_PPRFantasyPoints"))


setnames(teamstats, c("roll_4_PassingYds","roll_4_Int","roll_4_PassingAtt","roll_4_Cmp","roll_4_RushingAtt",
                        "roll_4_RushingYds","roll_4_RushingTD","roll_4_Rec","roll_4_Tgt","roll_4_ReceivingYds",
                        "roll_4_ReceivingTD","roll_4_FL"), 
                         c("TeamAllowed_roll_4_PassingYds","TeamAllowed_roll_4_Int","TeamAllowed_roll_4_PassingAtt",
                         "TeamAllowed_roll_4_Cmp","TeamAllowed_roll_4_RushingAtt","TeamAllowed_roll_4_RushingYds",
                         "TeamAllowed_roll_4_RushingTD","TeamAllowed_roll_4_Rec","TeamAllowed_roll_4_Tgt",
                         "TeamAllowed_roll_4_ReceivingYds","TeamAllowed_roll_4_ReceivingTD","TeamAllowed_roll_4_FL"))


setkey(playerstats,season,week,Tm,game_id,Opponent)

setkey(teamstats,season,week,Tm,game_id,Opponent)

train<-merge(playerstats,teamstats)


fwrite(train, "./project/volume/data/processed/train.csv")


