library(data.table)


statsandscores<-fread("./project/volume/data/processed/StatsandBoxscores.csv")


#remove file if it already exists to not override the data
if (file.exists("./project/volume/data/processed/teamstats(roll6).csv")) {
  file.remove("./project/volume/data/processed/teamstats(roll6).csv")}

NFLvalues<-c("PassingYds","PassingTD","Int","PassingAtt","Cmp","RushingAtt",
             "RushingYds","RushingTD","Rec","Tgt","ReceivingYds",
             "ReceivingTD","FL", "Tgt_share","Rushing_share","Passing_share", "Teamtotalpassingattempts", 
             "Teamtotalrushingattempts","Teamvariancetargetshare","Teamvariancerushingshare","Teamvariancepassingshare",      
             "PPRFantasyPoints","StandardFantasyPoints","HalfPPRFantasyPoints")


teamAllowed<-dcast(statsandscores,Opponent+Tm+game_id+season+week+cumulativeweek~.,
                   sum,na.rm=T,value.var = NFLvalues)


setkey(teamAllowed,Opponent,cumulativeweek)

for (i in 1:length(NFLvalues)) {
  teamAllowed[, new_column := Reduce(`+`, shift(get(NFLvalues[i]), 1:6))]
  setnames(teamAllowed,"new_column",paste0("roll_6_",NFLvalues[i]))
}




fwrite(teamAllowed, "./project/volume/data/processed/teamstats(roll6).csv")