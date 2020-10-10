library(data.table)

playerstats<-fread("./project/volume/data/processed/playerstats.csv")
teamstats<-fread("./project/volume/data/processed/teamstats.csv")

playerstats<-playerstats[,.(season,week,Tm,Player,Pos,PPRFantasyPoints,game_id,Opponent,roll_4_PassingYds,
                            roll_4_Int,roll_4_PassingAtt,roll_4_Cmp,roll_4_RushingAtt,roll_4_RushingYds,
                            roll_4_RushingTD,roll_4_Rec,roll_4_Tgt,roll_4_ReceivingYds,roll_4_ReceivingTD,
                            roll_4_FL,roll_4_PPRFantasyPoints)]

teamstats<-teamstats[,.(season,week,Tm,cumulativeweek,game_id,Opponent,roll_4_PassingYds,roll_4_Int,
                        roll_4_PassingAtt, roll_4_Cmp,roll_4_RushingAtt,roll_4_RushingYds,roll_4_RushingTD,
                        roll_4_Rec,roll_4_Tgt,roll_4_ReceivingYds,roll_4_ReceivingTD,roll_4_FL)]


setkey(playerstats,season,week,Tm,game_id,Opponent,roll_4_PassingYds,
       roll_4_Int,roll_4_PassingAtt,roll_4_Cmp,roll_4_RushingAtt,roll_4_RushingYds,
       roll_4_RushingTD,roll_4_Rec,roll_4_Tgt,roll_4_ReceivingYds,roll_4_ReceivingTD,
       roll_4_FL)

setkey(teamstats,season,week,Tm,game_id,Opponent,roll_4_PassingYds,
       roll_4_Int,roll_4_PassingAtt,roll_4_Cmp,roll_4_RushingAtt,roll_4_RushingYds,
       roll_4_RushingTD,roll_4_Rec,roll_4_Tgt,roll_4_ReceivingYds,roll_4_ReceivingTD,
       roll_4_FL)

train<-merge(playerstats,teamstats, all.x = T)
