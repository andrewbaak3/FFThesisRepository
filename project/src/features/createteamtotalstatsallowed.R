library(data.table)


statsandscores<-fread("./project/volume/data/processed/StatsandBoxscores.csv")

teamAllowed<-dcast(statsandscores,Opponent+game_id+season+week+cumulativeweek~.,sum,na.rm=T,value.var = c("PassingYds","Int","PassingAtt"))
teamAllowed<-statsandscores[,.(PassingYds=sum(PassingYds)),by=c("Opponent","game_id","season",
                                                                "week", "cumulativeweek")]
setkey(teamAllowed,Opponent,cumulativeweek)

teamAllowed[, roll_3_passing_yards := Reduce(`+`, shift(PassingYds, 0:2))]
