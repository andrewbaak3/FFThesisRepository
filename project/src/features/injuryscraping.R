library(purrr) 
library(rvest)
library(data.table)
library(stringr)
library(tidyr)

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

DT <- map_df(result, function(x){ 
  page <- read_html(x[[1]])
  data.table(
    Season = x[[2]],
    Week = x[[3]],
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

DT<-DT[order(Season,Week)]
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
DT[DT$Season == "--"]$Season<-NA
DT[DT$Week == "--"]$Week<-NA
DT[DT$Player == "--"]$Player<-NA
DT[DT$Wed == "--"]$Wed<-NA
DT[DT$Thu == "--"]$Thu<-NA
DT[DT$Fri == "--"]$Fri<-NA
DT[DT$Injury == "--"]$Injury<-NA

#Create ID column for DT
DT$id<-1:nrow(DT)

#Split Injury Column into multiple injuries creating dummy variables
types_tab<-as.data.table(tstrsplit(DT$Injury,","))
types_tab$id<-DT$id
m_types_tab<-melt(types_tab,id.vars = "id")
m_types_tab<-m_types_tab[!is.na(m_types_tab$value)]
m_types_tab$True<-1
types_tab<-dcast(m_types_tab,id ~ value,length,value.var="True")



setkey(DT, id)
setkey(types_tab, id)
merge(DT,types_tab, )


#Write out injury data table
fwrite(DT,"./project/volume/data/interim/injuryreports.csv")