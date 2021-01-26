library(data.table)


cleanboxscores <- function(){
  
  print("Running Function 1")
  
  #read in path and list files in path
  path<-"./project/volume/data/external/SeasonBoxScores"
  year_files<-list.files(path)
  message(year_files)
  
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

cleanboxscores()
