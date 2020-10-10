library(data.table)


statsandscores<-fread("./project/volume/data/processed/StatsandBoxscores.csv")

player_achieved<-statsandscores

NFLvalues<-c("PassingYds","Int","PassingAtt","Cmp","RushingAtt",
             "RushingYds","RushingTD","Rec","Tgt","ReceivingYds",
             "ReceivingTD","FL","PPRFantasyPoints","StandardFantasyPoints",
             "HalfPPRFantasyPoints")


setkey(player_achieved,Player,cumulativeweek)

for (i in 1:length(NFLvalues)) {
  player_achieved[, new_column := Reduce(`+`, shift(get(NFLvalues[i]), 1:4))]
  setnames(player_achieved,"new_column",paste0("roll_4_",NFLvalues[i]))
}


fwrite(player_achieved, "project/volume/data/processed/playerstats.csv")

#Next steps: merge two tables to get our model ready train set together
#subset out keys using hard bracket notation [,.] game_id, player, Tm, position, week, season
#x values = all rolling stats
#y value = PPR fantasy

#merge over rolling stats from team allowed what opponent allowed (use game_id,opponent,tm, week, season as keys)
#always set shared columns as key 