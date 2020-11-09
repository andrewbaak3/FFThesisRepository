#Declare Libraries
library(data.table)
library(caret)
library(optparse)

#Create Options List
option_list = list(
  make_option(c("-r", "--roll"), default=NA, type="character",
              help="how many weeks to perform a rolling merge on stats", metavar = "character"),
  make_option(c("-w", "--week"),  default=NA, type='integer',
              help="week number for how many weeks to perform rolling merge on")
);

opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);

#Extract arguments passed in to options parser
roll<-opt$roll
week<-opt$week

###############
#Function 1
###############
#Function to properly format all box scores and output them into one file
cleanboxscores <- function(){
  #read in path and list files in path
  path<-"./project/volume/data/external/SeasonBoxScores/"
  year_files<-list.files(path)
  
  #remove file if it already exists to prevent overwriting
  if (file.exists("./project/volume/data/interim/allBoxscores.csv")) {
    file.remove("./project/volume/data/interim/allBoxscores.csv")}
  
  #for loop to search through all files in the path
  for (i in 1:length(year_files)) {
    
    #set year equal to the year of the file being looked at
    year <- year_files[i]
    data_table<-fread(paste0(path,"/",year_files[i]))
    #create data_table with only relevant columns
    data_table<-data_table[,.(game_id,home_team,away_team,week,season,home_score,away_score)]
    #write out result for each year to one big file
    fwrite(data_table,"./project/volume/data/interim/allBoxscores.csv", append=T)
  }
}


###############
#Function 2
###############
#Function to properly format all player statistics from each game for which a record exists
cleanstatistics<- function() {
  #read in path of where data is located and list files in that path
  path1<-"./project/volume/data/external/external_data/weekly"
  year_files<- list.files(path1)
  
  #remove file if it already exists to not override the data
  if (file.exists("./project/volume/data/interim/gamePlayerstats.csv")) {
    file.remove("./project/volume/data/interim/gamePlayerstats.csv")}
  
  #for loop to examine all the files containing data for different years
  for (i in 1:length(year_files)){
    
    #set year equal to what year file is currently being looked at, and create the subpath
    year <- year_files[i]
    path2<-paste0(path1,"/",year)
    
    year_week_files<-list.files(path2)
    
    master<-NULL
    
    #j for loop to get data from every week in a given year
    for (j in 1:length(year_week_files)) {
      week <- fread(paste0(path2, "/",year_week_files[j]))
      week_num<-gsub("week","",year_week_files[j])
      week_num<-gsub(".csv","",week_num)
      week_num<-as.numeric(week_num)
      week$week<-week_num
      week$c_week<-1
      week$season<-year
      master<-rbind(master,week)
    }
    #write out rbinded master files for each year to one big file  
    fwrite(master,"./project/volume/data/interim/gamePlayerstats.csv",append = T)
  }
}


###############
#Function 3
###############
#Function to properly merge together box scores and player stats
createmasterfile <- function() {
  #remove file if it already exists
  if (file.exists("./project/volume/data/processed/StatsandBoxscores.csv")) {
    file.remove("./project/volume/data/processed/StatsandBoxscores.csv")}
  
  #read in player statistics table
  playerstats<-fread("./project/volume/data/interim/gamePlayerstats.csv")
  
  #read in box scores table
  boxscores<-fread("./project/volume/data/interim/allBoxscores.csv")
  
  #only concerned with stats from 2009 on as the box scores data starts at 2009
  playerstats<-playerstats[season>2008]
  
  #Create home and away team tables
  home<-boxscores[,.(game_id,home_team,week,season,home_score)]
  setnames(home,c("home_team","home_score"),c("Tm","score"))
  
  away<-boxscores[,.(game_id,away_team,week,season,away_score)]
  setnames(away,c("away_team","away_score"),c("Tm","score"))
  
  #Rbind home and away to create allboxscores
  allboxscores<-rbind(home,away)
  allboxscores<-merge(allboxscores,allboxscores,by=c("game_id","week","season"),allow.cartesian = T)
  setnames(allboxscores,c("Tm.x","score.x","Tm.y","score.y"), c("Tm","Tmscore","Opponent","Opponentscore"))
  allboxscores<-allboxscores[!Tm==Opponent]
  
  #Fix situations where Team name (Tm) is different from one table to the next
  playerstats[playerstats$Tm == "NOR"]$Tm<-"NO"
  playerstats[playerstats$Tm == "KAN"]$Tm<-"KC"
  playerstats[playerstats$Tm == "NWE"]$Tm<-"NE"
  playerstats[playerstats$Tm == "GNB"]$Tm<-"GB"
  playerstats[playerstats$Tm == "SFO"]$Tm<-"SF"
  playerstats[playerstats$Tm == "TAM"]$Tm<-"TB"
  playerstats[playerstats$Tm == "SDG"]$Tm<-"SD"
  playerstats[playerstats$Tm == "LAR"]$Tm<-"LA"
  allboxscores[allboxscores$Tm=="JAC"]$Tm<-"JAX"
  
  
  #Set keys for each table so merge can occur
  setkey(playerstats,week,season,Tm)
  setkey(allboxscores,week,season,Tm)
  scoresandstats<-merge(playerstats,allboxscores, all.x = T)
  scoresandstats<-scoresandstats[order(season,week)]
  
  #Create separate table with just team, week, and c_week to calculate the correct cumulative week
  DT<- scoresandstats[,.(season,week,c_week)]
  DT<-DT[!duplicated(DT)]
  DT<-DT[,cumulativeweek := cumsum(c_week)]
  
  DT$c_week<-NULL
  scoresandstats$c_week<-NULL
  
  setkey(scoresandstats, season, week)
  setkey(DT,season,week)
  scoresandstats<-merge(scoresandstats,DT)
  
  #Add in variable to look at target %
  passingstats<-dcast(scoresandstats, game_id+Tm~.,sum,na.rm=T,value.var= "PassingAtt")
  setnames(passingstats, ".","Teamtotalpassingattempts")
  setkey(passingstats, game_id, Tm)
  setkey(scoresandstats,game_id,Tm)
  scoresandstats<-merge(scoresandstats,passingstats, all.x=T)
  scoresandstats$Tgt_share<-scoresandstats$Tgt/scoresandstats$Teamtotalpassingattempts
  
  #Add variable to get passing variance
  passingvariancestats<-dcast(scoresandstats[Tgt>0], game_id+Tm~.,var,na.rm=T,value.var= "Tgt_share")
  setnames(passingvariancestats, ".", "Teamvariancetargetshare")
  setkey(passingvariancestats, game_id, Tm)
  setkey(scoresandstats,game_id,Tm)
  scoresandstats<-merge(scoresandstats,passingvariancestats, all.x=T)
  
  #Add in variable for rushing % 
  rushingstats<-dcast(scoresandstats, game_id+Tm~.,sum,na.rm=T,value.var= "RushingAtt")
  setnames(rushingstats, ".","Teamtotalrushingattempts")
  setkey(rushingstats, game_id, Tm)
  setkey(scoresandstats,game_id,Tm)
  scoresandstats<-merge(scoresandstats,rushingstats, all.x=T)
  scoresandstats$Rushing_share<-scoresandstats$RushingAtt/scoresandstats$Teamtotalrushingattempts
  
  #Add variable to get rushing variance
  rushingvariancestats<-dcast(scoresandstats[RushingAtt>0], game_id+Tm~.,var,na.rm=T,value.var= "Rushing_share")
  setnames(rushingvariancestats, ".", "Teamvariancerushingshare")
  setkey(rushingvariancestats, game_id, Tm)
  setkey(scoresandstats,game_id,Tm)
  scoresandstats<-merge(scoresandstats,rushingvariancestats, all.x=T)
  
  #Add in variable for passing % 
  scoresandstats$Passing_share<-scoresandstats$PassingAtt/scoresandstats$Teamtotalpassingattempts
  
  #Add variable to get passing variance
  passingvariancestats<-dcast(scoresandstats[PassingAtt>0], game_id+Tm~.,var,na.rm=T,value.var= "Passing_share")
  setnames(passingvariancestats, ".", "Teamvariancepassingshare")
  setkey(passingvariancestats, game_id, Tm)
  setkey(scoresandstats,game_id,Tm)
  scoresandstats<-merge(scoresandstats,passingvariancestats, all.x=T)
  
  #Write out data
  fwrite(scoresandstats,"project/volume/data/processed/StatsandBoxscores.csv") 
}


###############
#Function 4
###############
#Function to create stats what a team allowed
createteamstatsallowed<- function(roll,week) {
  
  statsandscores<-fread("./project/volume/data/processed/StatsandBoxscores.csv")
  
  #remove file if it already exists to not override the data
  path1<-"./project/volume/data/processed/teamstats"
  path2<-paste0(path1,roll,".csv")
  if (file.exists(path2)) {
    file.remove(path2)}
  
  NFLvalues<-c("PassingYds","PassingTD","Int","PassingAtt","Cmp","RushingAtt",
               "RushingYds","RushingTD","Rec","Tgt","ReceivingYds",
               "ReceivingTD","FL", "Tgt_share","Rushing_share","Passing_share", "Teamtotalpassingattempts", 
               "Teamtotalrushingattempts","Teamvariancetargetshare","Teamvariancerushingshare","Teamvariancepassingshare",      
               "PPRFantasyPoints","StandardFantasyPoints","HalfPPRFantasyPoints")
  
  teamAllowed<-dcast(statsandscores,Opponent+Tm+game_id+season+week+cumulativeweek~.,
                     sum,na.rm=T,value.var = NFLvalues)
  
  setkey(teamAllowed,Opponent,cumulativeweek)
  
  for (i in 1:length(NFLvalues)) {
    teamAllowed[, new_column := Reduce(`+`, shift(get(NFLvalues[i]), 1:week))]
    setnames(teamAllowed,"new_column",paste0(roll,NFLvalues[i]))
  }
  fwrite(teamAllowed, path2)
}
###############
#Function 5
###############
#Function to create stats what a player achieved
createplayerstats<- function(roll, week) {
  
  statsandscores<-fread("./project/volume/data/processed/StatsandBoxscores.csv")
  player_achieved<-statsandscores

  #remove file if it already exists to not override the data
  path1<-"./project/volume/data/processed/playerstats"
  path2<-paste0(path1,roll,".csv")
  if (file.exists(path2)) {
    file.remove(path2)}
  
  NFLvalues<-c("PassingYds","PassingTD","Int","PassingAtt","Cmp","RushingAtt",
               "RushingYds","RushingTD","Rec","Tgt","ReceivingYds",
               "ReceivingTD","FL", "Tgt_share","Rushing_share","Passing_share", "Teamtotalpassingattempts", 
               "Teamtotalrushingattempts","Teamvariancetargetshare","Teamvariancerushingshare","Teamvariancepassingshare",      
               "PPRFantasyPoints","StandardFantasyPoints","HalfPPRFantasyPoints")
  
  setkey(player_achieved,Player,cumulativeweek)
  
  for (i in 1:length(NFLvalues)) {
    player_achieved[, new_column := Reduce(`+`, shift(get(NFLvalues[i]), 1:week))]
    setnames(player_achieved,"new_column",paste0(roll,NFLvalues[i]))
  }
  fwrite(player_achieved, path2)
}

###############
#Function 6
###############
#Function to create a set of training data
createtrainset <- function(roll, week) {
  
  #remove file if it already exists to not override the data
  path1<-"./project/volume/data/processed/train"
  path2<-paste0(path1,roll,".csv")
  if (file.exists(path2)) {
    file.remove(path2)}
  
  path3 <- "./project/volume/data/processed/playerstats"
  path4 <- "./project/volume/data/processed/teamstats"
  path5 <- paste0(path3,roll,".csv")
  path6 <- paste0(path4,roll,".csv")
  
  playerstats<-fread(path5)
  teamstats<-fread(path6)
  
  playerstats<-playerstats[,.(season,week,Tm,Player,Pos,PPRFantasyPoints,game_id,Opponent,paste0(roll,PassingYds),
                              paste0(roll,PassingTD), paste0(roll,Int),paste0(roll,PassingAtt),paste0(roll,Cmp),
                              paste0(roll,RushingAtt),paste0(roll,RushingYds),paste0(roll,RushingTD),paste0(roll,Rec),
                              paste0(roll,Tgt),paste0(roll,ReceivingYds),paste0(roll,ReceivingTD), paste0(roll,FL),
                              paste0(roll,PPRFantasyPoints),paste0(roll,Tgt_share),paste0(roll,Rushing_share),
                              paste0(roll,Passing_share),paste0(roll,Teamtotalpassingattempts),
                              paste0(roll,Teamtotalrushingattempts),paste0(roll,Teamvariancepassingshare),
                              paste0(roll,Teamvariancerushingshare),paste0(roll,Teamvariancetargetshare))]
  
  teamstats<-teamstats[,.(season,week,Tm,cumulativeweek,game_id,Opponent,paste0(roll,PassingYds),
                          paste0(roll,PassingTD), paste0(roll,Int),paste0(roll,PassingAtt),paste0(roll,Cmp),
                          paste0(roll,RushingAtt),paste0(roll,RushingYds),paste0(roll,RushingTD),paste0(roll,Rec),
                          paste0(roll,Tgt),paste0(roll,ReceivingYds),paste0(roll,ReceivingTD), paste0(roll,FL),
                          paste0(roll,PPRFantasyPoints),paste0(roll,Tgt_share),paste0(roll,Rushing_share),
                          paste0(roll,Passing_share),paste0(roll,Teamtotalpassingattempts),
                          paste0(roll,Teamtotalrushingattempts),paste0(roll,Teamvariancepassingshare),
                          paste0(roll,Teamvariancerushingshare),paste0(roll,Teamvariancetargetshare))]
  
  setnames(playerstats, c(paste0("roll",week,"PassingYds"),paste0("roll",week,"PassingTD"),paste0("roll",week,"Int"),
                          paste0("roll",week,"PassingAtt"),paste0("roll",week,"Cmp"),paste0("roll",week,"RushingAtt"),
                          paste0("roll",week,"RushingYds"),paste0("roll",week,"RushingTD"),paste0("roll",week,"Rec"),
                          paste0("roll",week,"Tgt"),paste0("roll",week,"ReceivingYds"),paste0("roll",week,"ReceivingTD"),
                          paste0("roll",week,"FL"),paste0("roll",week,"PPRFantasyPoints"),paste0("roll",week,"Tgt_share"),
                          paste0("roll",week,"Rushing_share"),paste0("roll",week,"Passing_share"),
                          paste0("roll",week,"Teamtotalpassingattempts"),paste0("roll",week,"Teamtotalrushingattempts"),
                          paste0("roll",week,"Teamvariancepassingshare"),paste0("roll",week,"Teamvariancerushingshare"),
                          paste0("roll",week,"Teamvariancetargetshare")), 
           c(paste0("Player","roll",week,"PassingYds"),paste0("Player","roll",week,"PassingTD"),paste0("Player","roll",week,"Int"),
             paste0("Player","roll",week,"PassingAtt"),paste0("Player","roll",week,"Cmp"),paste0("Player","roll",week,"RushingAtt"),
             paste0("Player","roll",week,"RushingYds"),paste0("Player","roll",week,"RushingTD"),paste0("Player","roll",week,"Rec"),
             paste0("Player","roll",week,"Tgt"),paste0("Player","roll",week,"ReceivingYds"),paste0("Player","roll",week,"ReceivingTD"),
             paste0("Player","roll",week,"FL"),paste0("Player","roll",week,"PPRFantasyPoints"),paste0("Player","roll",week,"Tgt_share"),
             paste0("Player","roll",week,"Rushing_share"),paste0("Player","roll",week,"Passing_share"),
             paste0("Player","roll",week,"Teamtotalpassingattempts"),paste0("Player","roll",week,"Teamtotalrushingattempts"),
             paste0("Player","roll",week,"Teamvariancepassingshare"),paste0("Player","roll",week,"Teamvariancerushingshare"),
             paste0("Player","roll",week,"Teamvariancetargetshare")))
  
  
  setnames(teamstats, c(paste0("roll",week,"PassingYds"),paste0("roll",week,"PassingTD"),paste0("roll",week,"Int"),
                        paste0("roll",week,"PassingAtt"),paste0("roll",week,"Cmp"),paste0("roll",week,"RushingAtt"),
                        paste0("roll",week,"RushingYds"),paste0("roll",week,"RushingTD"),paste0("roll",week,"Rec"),
                        paste0("roll",week,"Tgt"),paste0("roll",week,"ReceivingYds"),paste0("roll",week,"ReceivingTD"),
                        paste0("roll",week,"FL"),paste0("roll",week,"PPRFantasyPoints"),paste0("roll",week,"Tgt_share"),
                        paste0("roll",week,"Rushing_share"),paste0("roll",week,"Passing_share"),
                        paste0("roll",week,"Teamtotalpassingattempts"),paste0("roll",week,"Teamtotalrushingattempts"),
                        paste0("roll",week,"Teamvariancepassingshare"),paste0("roll",week,"Teamvariancerushingshare"),
                        paste0("roll",week,"Teamvariancetargetshare")), 
           c(paste0("TeamAllowed","roll",week,"PassingYds"),paste0("TeamAllowed","roll",week,"PassingTD"),paste0("TeamAllowed","roll",week,"Int"),
             paste0("TeamAllowed","roll",week,"PassingAtt"),paste0("TeamAllowed","roll",week,"Cmp"),paste0("TeamAllowed","roll",week,"RushingAtt"),
             paste0("TeamAllowed","roll",week,"RushingYds"),paste0("TeamAllowed","roll",week,"RushingTD"),paste0("TeamAllowed","roll",week,"Rec"),
             paste0("TeamAllowed","roll",week,"Tgt"),paste0("TeamAllowed","roll",week,"ReceivingYds"),paste0("TeamAllowed","roll",week,"ReceivingTD"),
             paste0("TeamAllowed","roll",week,"FL"),paste0("TeamAllowed","roll",week,"PPRFantasyPoints"),paste0("TeamAllowed","roll",week,"Tgt_share"),
             paste0("TeamAllowed","roll",week,"Rushing_share"),paste0("TeamAllowed","roll",week,"Passing_share"),
             paste0("TeamAllowed","roll",week,"Teamtotalpassingattempts"),paste0("TeamAllowed","roll",week,"Teamtotalrushingattempts"),
             paste0("TeamAllowed","roll",week,"Teamvariancepassingshare"),paste0("TeamAllowed","roll",week,"Teamvariancerushingshare"),
             paste0("TeamAllowed","roll",week,"Teamvariancetargetshare")))
  
  

  
  #Set Keys and Merge Together PlayerStats and Teamstats to create train set
  setkey(playerstats,season,week,Tm,game_id,Opponent)
  setkey(teamstats,season,week,Tm,game_id,Opponent)
  train<-merge(playerstats,teamstats)
  
  #Calculate difference values for applicable rows
  
  #ReceivingYds
  scale_transform<-preProcess(train[,.(paste0(Playerroll,week,ReceivingYds),paste0(TeamAllowedroll,week,ReceivingYds))],
                              method=c("center","scale"))
  scaled_receiving_yds<-predict(scale_transform,train[,.(paste0(Playerroll,week,ReceivingYds),paste0(TeamAllowedroll,week,ReceivingYds))])
  train$ReceivingYds_Dif<-scaled_receiving_yds$paste0(Playerroll,week,ReceivingYds)-scaled_receiving_yds$paste0(TeamAllowedroll,week,ReceivingYds)
  #RushingYds
  scale_transform2<-preProcess(train[,.(paste0(Playerroll,week,RushingYds),paste0(TeamAllowedroll,week,RushingYds))],
                               method=c("center","scale"))
  scaled_rushing_yds<-predict(scale_transform2,train[,.(paste0(Playerroll,week,RushingYds),paste0(TeamAllowedroll,week,RushingYds))])
  train$RushingYds_Dif<-scaled_rushing_yds$paste0(Playerroll,week,RushingYds)-scaled_rushing_yds$paste0(TeamAllowedroll,week,RushingYds)
  #PassingYds
  scale_transform3<-preProcess(train[,.(paste0(Playerroll,week,PassingYds),paste0(TeamAllowedroll,week,PassingYds))],
                               method=c("center","scale"))
  scaled_passing_yds<-predict(scale_transform3,train[,.(paste0(Playerroll,week,PassingYds),paste0(TeamAllowedroll,week,PassingYds))])
  train$PassingYds_Dif<-scaled_passing_yds$paste0(Playerroll,week,PassingYds)-scaled_passing_yds$paste0(TeamAllowedroll,week,PassingYds)
  #Targets
  scale_transform4<-preProcess(train[,.(paste0(Playerroll,week,Tgt),paste0(TeamAllowedroll,week,Tgt))],
                               method=c("center","scale"))
  scaled_targets<-predict(scale_transform4,train[,.(paste0(Playerroll,week,Tgt),paste0(TeamAllowedroll,week,Tgt))])
  train$Targets_Dif<-scaled_targets$paste0(Playerroll,week,Tgt)-scaled_targets$paste0(TeamAllowedroll,week,Tgt)
  #PassingAttempts
  scale_transform5<-preProcess(train[,.(paste0(Playerroll,week,PassingAtt),paste0(TeamAllowedroll,week,PassingAtt))],
                               method=c("center","scale"))
  scaled_passingattempts<-predict(scale_transform5,train[,.(paste0(Playerroll,week,PassingAtt),paste0(TeamAllowedroll,week,PassingAtt))])
  train$Passing_Attempts_Dif<-scaled_passingattempts$paste0(Playerroll,week,PassingAtt)-scaled_passingattempts$paste0(TeamAllowedroll,week,PassingAtt)
  #RushingAttempts
  scale_transform6<-preProcess(train[,.(paste0(Playerroll,week,RushingAtt),paste0(TeamAllowedroll,week,RushingAtt))],
                               method=c("center","scale"))
  scaled_rushingattempts<-predict(scale_transform6,train[,.(paste0(Playerroll,week,RushingAtt),paste0(TeamAllowedroll,week,RushingAtt))])
  train$Rushing_Attempts_Dif<-scaled_rushingattempts$paste0(Playerroll,week,RushingAtt)-scaled_rushingattempts$paste0(TeamAllowedroll,week,RushingAtt)
  #ReceivingTDs
  scale_transform7<-preProcess(train[,.(paste0(Playerroll,week,ReceivingTD),paste0(TeamAllowedroll,week,ReceivingTD))],
                               method=c("center","scale"))
  scaled_receivingtds<-predict(scale_transform7,train[,.(paste0(Playerroll,week,ReceivingTD),paste0(TeamAllowedroll,week,ReceivingTD))])
  train$ReceivingTDs_Dif<-scaled_receivingtds$paste0(Playerroll,week,ReceivingTD)-scaled_receivingtds$paste0(TeamAllowedroll,week,ReceivingTD)
  #RushingTDs
  scale_transform8<-preProcess(train[,.(paste0(Playerroll,week,RushingTD),paste0(TeamAllowedroll,week,RushingTD))],
                               method=c("center","scale"))
  scaled_rushingtds<-predict(scale_transform8,train[,.(paste0(Playerroll,week,RushingTD),paste0(TeamAllowedroll,week,RushingTD))])
  train$RushingTDs_Dif<-scaled_rushingtds$paste0(Playerroll,week,RushingTD)-scaled_rushingtds$paste0(TeamAllowedroll,week,RushingTD)
  #PassingTDs
  scale_transform9<-preProcess(train[,.(paste0(Playerroll,week,PassingTD),paste0(TeamAllowedroll,week,PassingTD))],
                               method=c("center","scale"))
  scaled_passingtds<-predict(scale_transform9,train[,.(paste0(Playerroll,week,PassingTD),paste0(TeamAllowedroll,week,PassingTD))])
  train$PassingTDs_Dif<-scaled_passingtds$paste0(Playerroll,week,PassingTD)-scaled_passingtds$paste0(TeamAllowedroll,week,PassingTD)
  #RushingVar
  scale_transform10<-preProcess(train[,.(paste0(Playerroll,week,Teamvariancerushingshare),paste0(TeamAllowedroll,week,Teamvariancerushingshare))],
                                method=c("center","scale"))
  scaled_rushingshare<-predict(scale_transform10,train[,.(paste0(Playerroll,week,Teamvariancerushingshare),paste0(TeamAllowedroll,week,Teamvariancerushingshare))])
  train$Rushing_Var_Dif<-scaled_rushingshare$paste0(Playerroll,week,Teamvariancerushingshare)-scaled_rushingshare$paste0(TeamAllowedroll,week,Teamvariancerushingshare)
  #PassingVar
  scale_transform11<-preProcess(train[,.(paste0(Playerroll,week,Teamvariancepassingshare),paste0(TeamAllowedroll,week,Teamvariancepassingshare))],
                                method=c("center","scale"))
  scaled_passingshare<-predict(scale_transform11,train[,.(paste0(Playerroll,week,Teamvariancepassingshare),paste0(TeamAllowedroll,week,Teamvariancepassingshare))])
  train$Passing_Var_Dif<-scaled_passingshare$paste0(Playerroll,week,Teamvariancepassingshare)-scaled_passingshare$paste0(TeamAllowedroll,week,Teamvariancepassingshare)
  #TargetVar
  scale_transform12<-preProcess(train[,.(paste0(Playerroll,week,Teamvariancetargetshare),paste0(TeamAllowedroll,week,Teamvariancetargetshare))],
                                method=c("center","scale"))
  scaled_tgtshare<-predict(scale_transform12,train[,.(paste0(Playerroll,week,Teamvariancetargetshare),paste0(TeamAllowedroll,week,Teamvariancetargetshare))])
  train$Tgt_Var_Dif<-scaled_tgtshare$paste0(Playerroll,week,Teamvariancetargetshare)-scaled_tgtshare$paste0(TeamAllowedroll,week,Teamvariancetargetshare)
  #RushingShare
  scale_transform13<-preProcess(train[,.(paste0(Playerroll,week,Rushing_share),paste0(TeamAllowedroll,week,Rushing_share))],
                                method=c("center","scale"))
  scaled_rushing<-predict(scale_transform13,train[,.(paste0(Playerroll,week,Rushing_share),paste0(TeamAllowedroll,week,Rushing_share))])
  train$Rushing_Share_Dif<-scaled_rushing$paste0(Playerroll,week,Rushing_share)-scaled_rushing$paste0(TeamAllowedroll,week,Rushing_share)
  #PassingShare
  scale_transform14<-preProcess(train[,.(paste0(Playerroll,week,Passing_share),paste0(TeamAllowedroll,week,Passing_share))],
                                method=c("center","scale"))
  scaled_passing<-predict(scale_transform14,train[,.(paste0(Playerroll,week,Passing_share),paste0(TeamAllowedroll,week,Passing_share))])
  train$Passing_Share_Dif<-scaled_passing$paste0(Playerroll,week,Passing_share)-scaled_passing$paste0(TeamAllowedroll,week,Passing_share)
  #TargetShare
  scale_transform15<-preProcess(train[,.(paste0(Playerroll,week,Tgt_share),paste0(TeamAllowedroll,week,Tgt_share))],
                                method=c("center","scale"))
  scaled_tgt<-predict(scale_transform15,train[,.(paste0(Playerroll,week,Tgt_share),paste0(TeamAllowedroll,week,Tgt_share))])
  train$Tgt_Share_Dif<-scaled_tgt$paste0(Playerroll,week,Tgt_share)-scaled_tgt$paste0(TeamAllowedroll,week,Tgt_share)

  
  #Write out train set to processed folder
  fwrite(train, path2)
  
}



###############
#Main Function
###############
datawrangle<-function(roll, week){
  cleanboxscores()
  cleanstatistics()
  createmasterfile()
  createplayerstats(roll,week)
  createteamstatsallowed(roll,week)
  createtrainset(roll,week)
}

datawrangle(roll,week)
