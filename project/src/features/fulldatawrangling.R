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
cleanboxscores <- function(){
  
  print("Running Function 1")
  
  #read in path and list files in path
  path<-"./project/volume/data/external/SeasonBoxScores"
  year_files<-list.files(path)
  message(year_files)
  
  #remove file if it already exists to prevent overwriting
  if (file.exists("./project/volume/data/interim/allBoxscores.csv")) {
    file.remove("./project/volume/data/interim/allBoxscores.csv")}
  
  #for loop to search through all files in the path
  for (i in 1:length(year_files)) {
    message(year_files[i])
    #set year equal to the year of the file being looked at
    year <- year_files[i]
    data_table<-fread(paste0(path,"/",year_files[i]))
    #create data_table with only relevant columns
    data_table<-data_table[,.(game_id,home_team,away_team,week,season,home_score,away_score)]
    #write out result for each year to one big file
    fwrite(data_table,"./project/volume/data/interim/allBoxscores.csv", append=T)
  }
  
  print("Function 1 Complete")
}


###############
#Function 2
###############
#Function to properly format all player statistics from each game for which a record exists
cleanstatistics<- function() {
  
  print("Running Function 2")
  
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
  print("Function 2 Complete")
}


###############
#Function 3
###############
#Function to properly merge together box scores and player stats
createmasterfile <- function() {
  
  print("Running Function 3")
  
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
  
  print("Function 3 Complete")
}


###############
#Function 4
###############
#Function to create stats what a team allowed
createteamstatsallowed<- function(rolltime,numweeks) {
  
  print("Running Function 4")
  
  statsandscores<-fread("./project/volume/data/processed/StatsandBoxscores.csv")
  

  #remove file if it already exists to not overwrite the data
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
  
  print("Function 4 Complete")
}
###############
#Function 5
###############
#Function to create stats what a player achieved
createplayerstats<- function(rolltime, numweeks) {
  
  print("Running Function 5")
  
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
  
  print("Function 5 Complete")
}


##############
#Function 6
##############
#Function to scrape injury data
scrapeinjury <- function() {
  
  print("Running Function 6")
  
  #Remove file if file already exists
  if (file.exists("./project/volume/data/interim/injuryreports.csv")) {
    file.remove("./project/volume/data/interim/injuryreports.csv")}
  
  #Declare variables and empty data tables
  path1<-("https://www.footballdb.com/transactions/injuries.html?yr=")
  seasons<-c("2016", "2017", "2020")
  weeks<-1:17
  result<-data.table()
  temp<-NULL
  
  #Use nested for loops to get the url, season, and week for each webpage of interest, store in result data table
  for(s in 1:length(seasons)){
    for(w in 1:length(weeks)){
      temp$link<- paste0(path1, seasons[s],"&wk=", as.character(w), "&type=reg")
      temp$season<-as.numeric(seasons[s])
      temp$week<-weeks[w]
      result<-rbind(result,temp)
    }
  }
  
  #Get rid of any potential empty values from result
  result<-compact(result) 
  
  #Split result into a list with 3 pieces (link, season, week)
  result<-split(result, result$link)
  
  #Create final data table with all injury information
  DT <- map_df(result, function(x){ 
    page <- read_html(x[[1]])
    data.table(
      season = x[[2]],
      week = x[[3]],
      Player = page %>% html_nodes('.divtable .td:nth-child(1) b') %>% html_text(),
      Injury = page %>% html_nodes('.divtable .td:nth-child(2)') %>% html_text(),
      Wed = page %>% html_nodes('.divtable .td:nth-child(3)') %>% html_text(),
      Thu = page %>% html_nodes('.divtable .td:nth-child(4)') %>% html_text(),
      Fri = page %>% html_nodes('.divtable .td:nth-child(5)') %>% html_text(),
      GameStatus = page %>% html_nodes('.divtable .td:nth-child(6)') %>% html_text()
    )
  }
  )
  
  #Clean up DT
  
  #Order by season and week
  DT<-DT[order(season,week)]
  
  #Remove everything from DT$GameStatus except for status 
  DT$GameStatus<-gsub("[0-9]","",DT$GameStatus)
  DT$GameStatus<-gsub("[()]","",DT$GameStatus)
  DT$GameStatus<-gsub("vs","",DT$GameStatus)
  DT$GameStatus<-gsub("[@]","",DT$GameStatus)
  DT$GameStatus<-gsub("[/]","",DT$GameStatus)
  DT$GameStatus<-str_sub(DT$GameStatus, 1, str_length(DT$GameStatus)-3)
  DT$GameStatus<-gsub(" ", "", DT$GameStatus)
  
  
  #Remove -- values in each of the columns
  DT[DT$GameStatus == "--"]$GameStatus<-NA
  DT[DT$season == "--"]$season<-NA
  DT[DT$week == "--"]$week<-NA
  DT[DT$Player == "--"]$Player<-NA
  DT[DT$Wed == "--"]$Wed<-NA
  DT[DT$Thu == "--"]$Thu<-NA
  DT[DT$Fri == "--"]$Fri<-NA
  DT[DT$Injury == "--"]$Injury<-NA
  
  #Set all injuries to lowercase to help get rid of duplicates
  DT$Injury<-tolower(DT$Injury)
  
  #Create ID column for DT
  DT$id<-1:nrow(DT)
  
  #Split Injury Column into multiple injuries creating dummy variables
  types_tab<-as.data.table(tstrsplit(DT$Injury,","))
  types_tab$id<-DT$id
  m_types_tab<-melt(types_tab,id.vars = "id")
  m_types_tab$value<-trimws(m_types_tab$value, which = "both")
  m_types_tab$value<-gsub(" ", "_",m_types_tab$value)
  m_types_tab$value<-gsub("/", "_",m_types_tab$value)
  m_types_tab$value<-gsub("-", "", m_types_tab$value)
  m_types_tab<-m_types_tab[!is.na(m_types_tab$value)]
  m_types_tab$True<-1
  types_tab<-dcast(m_types_tab,id ~ value,length,value.var="True")
  
  #Merge tables together into one big table
  InjuryTable<-merge(DT,types_tab, by = "id")
  
  #Get rid of columns not of interest 
  drops<-c("id","Injury")
  InjuryTable<-InjuryTable[, !drops, with = FALSE]
  
  
  #Write out injury data table
  fwrite(InjuryTable,"./project/volume/data/interim/injuryreports.csv")
  
  print("Function 6 Complete")
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
  cleanboxscores()
  cleanstatistics()
  createmasterfile()
  createteamstatsallowed(rolltime,numweeks)
  createplayerstats(rolltime,numweeks)
  scrapeinjury()
  createtrainset(rolltime,numweeks)
}

datawrangle(rolltime,numweeks)
