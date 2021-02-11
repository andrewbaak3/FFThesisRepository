#Declare Libraries
library(data.table)
library(caret)
library(optparse)
library(purrr)
library(rvest)
library(stringr)
library(tidyr)

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
checkforboxscores <- function(){
  
  #remove file if it already exists to prevent overwriting
  if (file.exists("./project/volume/data/interim/allBoxscores.csv")) {
    print("The file already exists")}
  else {
    source("./project/src/features/cleanboxscores.R")
  }
}

###############
#Function 2
###############
#Function to properly format all player statistics from each game for which a record exists
checkforstats<- function() {
 
  if (file.exists("./project/volume/data/interim/gamePlayerstats.csv")) {
    print("The file already exists")}
  else {
    source("./project/src/features/createstatisticsmaster.R")
  }
}

###############
#Function 3
###############
#Function to properly merge together box scores and player stats
checkformasterfile <- function() {
  
  #remove file if it already exists
  if (file.exists("./project/volume/data/interim/StatsandBoxscores.csv")) {
    print("The file already exists")}
  else {
    source("./project/src/features/createmasterfile.R")
  }
}

###############
#Function 4
###############
#Function to create stats what a team allowed
createteamstatsallowed<- function(rolltime,numweeks) {
  
  print("Running Function 4")
  
  statsandscores<-fread("./project/volume/data/interim/StatsandBoxscores.csv")
  
  
  #remove file if it already exists to not overwrite the data
  path1<-"./project/volume/data/interim/teamstats"
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
  
  print("Function 4 Complete")
}
###############
#Function 5
###############
#Function to create stats what a player achieved
createplayerstats<- function(rolltime, numweeks) {
  
  print("Running Function 5")
  
  statsandscores<-fread("./project/volume/data/interim/StatsandBoxscores.csv")
  player_achieved<-statsandscores
  
  #remove file if it already exists to not override the data
  path1<-"./project/volume/data/interim/playerstats"
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
  
  print("Function 5 Complete")
}


##############
#Function 6
##############
#Function to scrape injury data
checkforinjuryfile <- function() {
  
  if (file.exists("./project/volume/data/interim/injuryreports.csv")) {
    print("The file already exists")}
  else {
    source("./project/src/features/injuryscraping.R")
  }
}

###############
#Function 7
###############
#Function to create a set of training data
createtrainset <- function(rolltime, numweeks) {
  
  print("Running Function 7")
  
  #remove file if it already exists to not override the data
  path1<-"./project/volume/data/processed/train"
  path2<-paste0(path1,rolltime,".csv")
  if (file.exists(path2)) {
    file.remove(path2)}
  
  path3 <- "./project/volume/data/interim/playerstats"
  path4 <- "./project/volume/data/interim/teamstats"
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
  
  #Add injury data in
  
  #Read in injurydata data table
  injurydata<-fread("./project/volume/data/interim/injuryreports.csv")
  
  #Create new name column to merge on and put everything to lowercase
  train$mergeName<-tolower(train$Player)
  injurydata$mergeName<-tolower(injurydata$Player)
  
  #Change column name Player in injury data
  names(injurydata)[names(injurydata) == 'Player'] <- 'DropName'
  
  #Get rid of all special characters
  train$mergeName<-gsub("[-]"," ",train$mergeName)
  injurydata$mergeName<-gsub("[-]"," ",injurydata$mergeName)
  train$mergeName<-gsub("[.]","",train$mergeName)
  injurydata$mergeName<-gsub("[.]","",injurydata$mergeName)
  train$mergeName<-gsub("[']","",train$mergeName)
  injurydata$mergeName<-gsub("[']","",injurydata$mergeName)
  
  
  #Set keys for both datasets
  setkey(train,season,week,mergeName)
  setkey(injurydata,season,week,mergeName)
  finaltrain<-merge(train,injurydata, all.x = TRUE)
  
  
  #Get rid of columns not of interest 
  drops<-c("DropName","mergeName")
  finaltrain<-finaltrain[, !drops, with = FALSE]
  
  #Write out train set to processed folder
  fwrite(finaltrain, path2)
  
  print("Function 7 Complete")
  
}

###############
#Main Function
###############
datawrangle<-function(rolltime, numweeks){
  checkforboxscores()
  checkforstats()
  checkformasterfile()
  createteamstatsallowed(rolltime,numweeks)
  createplayerstats(rolltime,numweeks)
  checkforinjuryfile()
  createtrainset(rolltime,numweeks)
}

datawrangle(rolltime,numweeks)
