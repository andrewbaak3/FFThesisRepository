library(purrr) 
library(rvest)
library(data.table)
library(stringr)
library(tidyr)

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
  
  #Create list of columns from injury table to be kept 
  keeps<-c("season","week","Player","Wed","Thu","Fri","GameStatus","ankle", "concussion","toe",
                 "left_shoulder","knee")
  
  
  
  #Create consolidate data table, to rowsum all injury columns not wanted in the final model
  consolidate<-InjuryTable[,.(abdomen,achilles,back,bicep,biceps,calf,calves,chest,clavicle,collarbone,
                              concussion_neck,core,core_muscle,core_muscle_injury,cramps,ear,elbow,
                              elbow_and_knee,eye,fibula,finger,finger_left_hand,
                              finger_right_hand,foot,forearm,forehead,glute,groin,hamstring,hamstrings,
                              hand,head,heat_cramps,heat_exhaustion,heel,hip,hips,illness,jaw,knees,
                              left_ankle,left_calf,left_foot,left_groin,left_hip,left_knee,left_thumb,left_wrist,
                              liver,load_management,low_back,lower_leg,lumbar,lung,mouth,neck,non_football_injury,
                              nonfootball_illness,nonfootball_injury,nose,not_injury_related,
                              not_injury_related__illness,oblique,other,pectoral,pelvis,quadricep,rest,
                              resting_vet,resting_veteran,rib,rib_cage,ribs,right_calf,right_collarbone,
                              right_elbow,right_finger,right_foot,right_groin,right_hamstring,right_hand,
                              right_hip,right_knee,right_quadricep,right_shoulder,right_thumb,right_wrist)]
  
  #rowSum all columns from consolidate into other
  other<-rowSums(consolidate)
  
  #Get rid of all columns but keeps in injury column, add Other 
  InjuryTable <- InjuryTable[, keeps, with = FALSE]
  InjuryTable$Other<-other
  
  
  #Write out injury data table
  fwrite(InjuryTable,"./project/volume/data/interim/injuryreports.csv")
  
  print("Function 6 Complete")
}
scrapeinjury()