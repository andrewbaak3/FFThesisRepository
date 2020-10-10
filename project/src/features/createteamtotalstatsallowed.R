library(data.table)


statsandscores<-fread("./project/volume/data/processed/StatsandBoxscores.csv")

NFLvalues<-c("PassingYds","Int","PassingAtt","Cmp","RushingAtt",
             "RushingYds","RushingTD","Rec","Tgt","ReceivingYds",
             "ReceivingTD","FL","PPRFantasyPoints","StandardFantasyPoints",
             "HalfPPRFantasyPoints")


teamAllowed<-dcast(statsandscores,Opponent+Tm+game_id+season+week+cumulativeweek~.,
                   sum,na.rm=T,value.var = NFLvalues)

setkey(teamAllowed,Opponent,cumulativeweek)

for (i in 1:length(NFLvalues)) {
  teamAllowed[, new_column := Reduce(`+`, shift(get(NFLvalues[i]), 1:4))]
  setnames(teamAllowed,"new_column",paste0("roll_4_",NFLvalues[i]))
}

fwrite(teamAllowed, "./project/volume/data/processed/teamstats.csv")