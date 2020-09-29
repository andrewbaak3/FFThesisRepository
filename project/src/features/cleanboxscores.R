library(data.table)


path<-"./project/volume/data/external/SeasonBoxScores/"
year_files<-list.files(path)

for (i in 1:length(year_files)) {
  year <- year_files[i]
  data_table<-fread(paste0(path,"/",year_files[i]))
  data_table<-data_table[,.(game_id,home_team,away_team,week,season,home_score,away_score)]
  fwrite(data_table, paste0("./project/volume/data/interim/", "season",year))
}