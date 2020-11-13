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
rolltime<-opt$roll
numweeks<-opt$week

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
createteamstatsallowed<- function(rolltime,numweeks) {
  
  statsandscores<-fread("./project/volume/data/processed/StatsandBoxscores.csv")
  

  #remove file if it already exists to not override the data
  path1<-"./project/volume/data/processed/teamstats"
  path2<-paste0(path1,rolltime,".csv")
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
    teamAllowed[, new_column := Reduce(`+`, shift(get(NFLvalues[i]), 1:numweeks))]
    setnames(teamAllowed,"new_column",paste0(rolltime,NFLvalues[i]))
  }
  fwrite(teamAllowed, path2)
}
###############
#Function 5
###############
#Function to create stats what a player achieved
createplayerstats<- function(rolltime, numweeks) {
  
  statsandscores<-fread("./project/volume/data/processed/StatsandBoxscores.csv")
  player_achieved<-statsandscores

  #remove file if it already exists to not override the data
  path1<-"./project/volume/data/processed/playerstats"
  path2<-paste0(path1,rolltime,".csv")
  if (file.exists(path2)) {
    file.remove(path2)}
  
  NFLvalues<-c("PassingYds","PassingTD","Int","PassingAtt","Cmp","RushingAtt",
               "RushingYds","RushingTD","Rec","Tgt","ReceivingYds",
               "ReceivingTD","FL", "Tgt_share","Rushing_share","Passing_share", "Teamtotalpassingattempts", 
               "Teamtotalrushingattempts","Teamvariancetargetshare","Teamvariancerushingshare","Teamvariancepassingshare",      
               "PPRFantasyPoints","StandardFantasyPoints","HalfPPRFantasyPoints")
  
  setkey(player_achieved,Player,cumulativeweek)
  
  for (i in 1:length(NFLvalues)) {
    player_achieved[, new_column := Reduce(`+`, shift(get(NFLvalues[i]), 1:numweeks))]
    setnames(player_achieved,"new_column",paste0(rolltime,NFLvalues[i]))
  }
  fwrite(player_achieved, path2)
}

###############
#Function 6
###############
#Function to create a set of training data
createtrainset <- function(rolltime, numweeks) {
  
  #remove file if it already exists to not override the data
  path1<-"./project/volume/data/processed/train"
  path2<-paste0(path1,rolltime,".csv")
  if (file.exists(path2)) {
    file.remove(path2)}
  
  path3 <- "./project/volume/data/processed/playerstats"
  path4 <- "./project/volume/data/processed/teamstats"
  path5 <- paste0(path3,rolltime,".csv")
  path6 <- paste0(path4,rolltime,".csv")
  
  playerstats<-fread(path5)
  teamstats<-fread(path6)

  
  playerdrops<-c("StandardFantasyPoints","HalfPPRFantasyPoints","Tmscore","Opponentscore","cumulativeweek", "PassingYds",
           "PassingTD","Int","PassingAtt","Cmp","RushingAtt","RushingYds","RushingTD","Rec","Tgt","ReceivingYds",
           "ReceivingTD","FL","Tgt_share","Rushing_share","Passing_share","Teamtotalpassingattempts","Teamtotalrushingattempts",
           "Teamvariancepassingshare","Teamvariancerushingshare","Teamvariancetargetshare",
           paste0("roll",numweeks,"StandardFantasyPoints"), paste0("roll",numweeks,"HalfPPRFantasyPoints"))
  
  teamdrops<-c("PassingYds","PassingTD","Int","PassingAtt","Cmp","RushingAtt","RushingYds","RushingTD","Rec","Tgt","ReceivingYds",
               "ReceivingTD","FL","Tgt_share","Rushing_share","Passing_share","Teamtotalpassingattempts","Teamtotalrushingattempts",
               "Teamvariancepassingshare","Teamvariancerushingshare","Teamvariancetargetshare", "PPRFantasyPoints", 
               "StandardFantasyPoints", "HalfPPRFantasyPoints",paste0("roll",numweeks,"StandardFantasyPoints"), 
               paste0("roll",numweeks,"HalfPPRFantasyPoints"),paste0("roll",numweeks,"PPRFantasyPoints"))
  
  playerstats<-playerstats[,!playerdrops,with = FALSE]
  teamstats<-teamstats[,!teamdrops,with=FALSE]
  
  #Set Names
  player_roll_names<-names(playerstats)[grep("roll",names(playerstats))]
  new_names<-paste0("Player",player_roll_names)
  setnames(playerstats, player_roll_names, new_names)
  
  team_roll_names<-names(teamstats)[grep("roll",names(teamstats))]
  new_team_names<-paste0("TeamAllowed",team_roll_names)
  setnames(teamstats, team_roll_names, new_team_names)

  
  #Set Keys and Merge Together PlayerStats and Teamstats to create train set
  setkey(playerstats,season,week,Tm,game_id,Opponent)
  setkey(teamstats,season,week,Tm,game_id,Opponent)
  train<-merge(playerstats,teamstats)
  
  #Calculate difference values for applicable rows
  
  #make a value list
  statslist<- c("ReceivingYds","RushingYds","PassingYds","Tgt","PassingAtt","RushingAtt","ReceivingTD","RushingTD",
                "PassingTD","Teamvariancerushingshare","Teamvariancepassingshare","Teamvariancetargetshare","Rushing_share",
                "Passing_share","Tgt_share")
  
   for (i in 1:length(statslist)) {
    keep<-c(paste0("Playerroll",numweeks,statslist[i]),paste0("TeamAllowedroll",numweeks,statslist[i]))
    sub_train<-train[,keep,with=FALSE]
    scale_transform<-preProcess(sub_train, method = c("center","scale"))
    scaled_values<-predict(scale_transform, sub_train)
    train$value_dif<-scaled_values[,1]-scaled_values[,2]
    setnames(train,"value_dif",paste0(statslist[i],"_Dif"))
  }

  
  #Write out train set to processed folder
  fwrite(train, path2)
  
}


###############
#Main Function
###############
datawrangle<-function(rolltime, numweeks){
  cleanboxscores()
  cleanstatistics()
  createmasterfile()
  createplayerstats(rolltime,numweeks)
  createteamstatsallowed(rolltime,numweeks)
  createtrainset(rolltime,numweeks)
}

datawrangle(rolltime,numweeks)
