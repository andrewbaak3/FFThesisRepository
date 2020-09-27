library(data.table)

path1<-"./project/volume/data/external/external_data/weekly"
year_files<- list.files(path1)

for (i in 1:length(year_files)){

    year <- year_files[i]
    path2<-paste0(path1,"/",year)
    
    year_week_files<-list.files(path2)
    
    master<-NULL
    
    for (j in 1:length(year_week_files)) {
      week <- fread(paste0(path2, "/",year_week_files[j]))
      week$week<-j
      week$c_week<-1
      master<-rbind(master,week)
    }
  fwrite(master, paste0("./project/volume/data/interim/", year,"_", "season", ".csv"))
}
