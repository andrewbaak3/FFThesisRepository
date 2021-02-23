library(purrr) 
library(rvest)
library(data.table)
library(stringr)
library(tidyr)


#Function to scrape Draft Kings salary related data

scrapeDK<- function() {

  print("Running function 8")
  #SCRAPING
  
  #Declare initial path, seasons and weeks to iterate through
  path1<-("http://rotoguru1.com/cgi-bin/fyday.pl?week=")
  seasons<-c("2014", "2015", "2016","2017","2018","2019","2020")
  weeks<-1:17
  result<-NULL
  temp<-NULL
  
  
  #Use nested for loops to generate all the urls to search through
  for(s in 1:length(seasons)){
    for(w in 1:length(weeks)){
      temp<- paste0(path1, as.character(w),"&year=",seasons[s],"&game=dk&scsv=1")
      result<-rbind(result,temp)
    }
  }
  
  
  #Loop over result, output to final data table 
  final <- data.table::rbindlist(
    lapply(result, function(x) {
      read_html(x) %>%
        html_nodes("pre") %>% 
        html_text() %>%
        data.table::fread( sep = ";" ) 
    } ),
    use.names = TRUE, fill = TRUE )
  
  #CLEANING
  
  #Change the order of name from Last, First to First Last
  final$Name<-sub("(^.*),\\s(.*$)","\\2 \\1", final$Name)
  
  #Change team names in Team column to the same abbreviation as they are in the training data
  final[final$Team == "atl"]$Team<-"ATL"
  final[final$Team == "det"]$Team<-"DET"
  final[final$Team == "ind"]$Team<-"IND"
  final[final$Team == "ari"]$Team<-"ARI"
  final[final$Team == "chi"]$Team<-"CHI"
  final[final$Team == "den"]$Team<-"DEN"
  final[final$Team == "pit"]$Team<-"PIT"
  final[final$Team == "cin"]$Team<-"CIN"
  final[final$Team == "phi"]$Team<-"PHI"
  final[final$Team == "bal"]$Team<-"BAL"
  final[final$Team == "ten"]$Team<-"TEN"
  final[final$Team == "nor"]$Team<-"NO"
  final[final$Team == "sea"]$Team<-"SEA"
  final[final$Team == "jac"]$Team<-"JAX"
  final[final$Team == "car"]$Team<-"CAR"
  final[final$Team == "buf"]$Team<-"BUF"
  final[final$Team == "sfo"]$Team<-"SF"
  final[final$Team == "tam"]$Team<-"TB"
  final[final$Team == "nyj"]$Team<-"NYJ"
  final[final$Team == "min"]$Team<-"MIN"
  final[final$Team == "oak"]$Team<-"OAK"
  final[final$Team == "mia"]$Team<-"MIA"
  final[final$Team == "sdg"]$Team<-"SD"
  final[final$Team == "hou"]$Team<-"HOU"
  final[final$Team == "cle"]$Team<-"CLE"
  final[final$Team == "kan"]$Team<-"KC"
  final[final$Team == "nwe"]$Team<-"NE"
  final[final$Team == "dal"]$Team<-"DAL"
  final[final$Team == "gnb"]$Team<-"GB"
  final[final$Team == "was"]$Team<-"WAS"
  final[final$Team == "nyg"]$Team<-"NYG"
  final[final$Team == "stl"]$Team<-"STL"
  final[final$Team == "lar"]$Team<-"LA"
  final[final$Team == "lac"]$Team<-"LAC"
  final[final$Team == "lvr"]$Team<-"OAK"
  
  #Change team names in Oppt column to the same abbreviation as they are in the training data
  final[final$Oppt == "atl"]$Oppt<-"ATL"
  final[final$Oppt == "det"]$Oppt<-"DET"
  final[final$Oppt == "ind"]$Oppt<-"IND"
  final[final$Oppt == "ari"]$Oppt<-"ARI"
  final[final$Oppt == "chi"]$Oppt<-"CHI"
  final[final$Oppt == "den"]$Oppt<-"DEN"
  final[final$Oppt == "pit"]$Oppt<-"PIT"
  final[final$Oppt == "cin"]$Oppt<-"CIN"
  final[final$Oppt == "phi"]$Oppt<-"PHI"
  final[final$Oppt == "bal"]$Oppt<-"BAL"
  final[final$Oppt == "ten"]$Oppt<-"TEN"
  final[final$Oppt == "nor"]$Oppt<-"NO"
  final[final$Oppt == "sea"]$Oppt<-"SEA"
  final[final$Oppt == "jac"]$Oppt<-"JAX"
  final[final$Oppt == "car"]$Oppt<-"CAR"
  final[final$Oppt == "buf"]$Oppt<-"BUF"
  final[final$Oppt == "sfo"]$Oppt<-"SF"
  final[final$Oppt == "tam"]$Oppt<-"TB"
  final[final$Oppt == "nyj"]$Oppt<-"NYJ"
  final[final$Oppt == "min"]$Oppt<-"MIN"
  final[final$Oppt == "oak"]$Oppt<-"OAK"
  final[final$Oppt == "mia"]$Oppt<-"MIA"
  final[final$Oppt == "sdg"]$Oppt<-"SD"
  final[final$Oppt == "hou"]$Oppt<-"HOU"
  final[final$Oppt == "cle"]$Oppt<-"CLE"
  final[final$Oppt == "kan"]$Oppt<-"KC"
  final[final$Oppt == "nwe"]$Oppt<-"NE"
  final[final$Oppt == "dal"]$Oppt<-"DAL"
  final[final$Oppt == "gnb"]$Oppt<-"GB"
  final[final$Oppt == "was"]$Oppt<-"WAS"
  final[final$Oppt == "nyg"]$Oppt<-"NYG"
  final[final$Oppt == "stl"]$Oppt<-"STL"
  final[final$Oppt == "lar"]$Oppt<-"LA"
  final[final$Oppt == "lac"]$Oppt<-"LAC"
  final[final$Oppt == "lvr"]$Oppt<-"OAK"
  
  
  #Add new column with points per dollar of salary (multiplying by 1000 to make the number easier to work with)
  final$PPD<-(final$`DK points`/final$`DK salary`)*1000
  
  
  #FINISHED
  
  #write out final as a csv to interim data folder 
  fwrite(final, "./project/volume/data/interim/DKsalaries.csv")
  
  print("Function 8 complete")
}

scrapeDK()
