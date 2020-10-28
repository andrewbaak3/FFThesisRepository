library(data.table)


statsandscores<-fread("./project/volume/data/processed/StatsandBoxscores.csv")

player_achieved<-statsandscores

#remove file if it already exists to not override the data
if (file.exists("./project/volume/data/processed/playerstats(roll3).csv")) {
  file.remove("./project/volume/data/processed/playerstats(roll3).csv")}

NFLvalues<-c("PassingYds","PassingTD","Int","PassingAtt","Cmp","RushingAtt",
             "RushingYds","RushingTD","Rec","Tgt","ReceivingYds",
             "ReceivingTD","FL","PPRFantasyPoints","StandardFantasyPoints",
             "HalfPPRFantasyPoints")


setkey(player_achieved,Player,cumulativeweek)

for (i in 1:length(NFLvalues)) {
  player_achieved[, new_column := Reduce(`+`, shift(get(NFLvalues[i]), 1:3))]
  setnames(player_achieved,"new_column",paste0("roll_3_",NFLvalues[i]))
}


fwrite(player_achieved, "project/volume/data/processed/playerstats(roll3).csv")
