library(data.table)

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
      week$week<-j
      week$c_week<-1
      week$season<-year
      master<-rbind(master,week)
    }
  #write out rbinded master files for each year to one big file  
  fwrite(master,"./project/volume/data/interim/gamePlayerstats.csv",append = T)
}
