library(data.table)


#Read in the Data
#To add all data would probably use a loop to iterate through, adding everything to a final data table
RBdata<-fread("./project/volume/data/teamconstruction/roll7_RB.csv")
DKdata<-fread("./project/volume/data/interim/DKsalaries.csv")

#Set columns to drop from draftkings data and remove them
DKdrops<-c("GID","Team","h/a","Oppt")
DKdata<-DKdata[, !DKdrops, with = FALSE]


#Rename columns in draftkings data to ease merging process
setnames(DKdata, c("Week","Year","Name"), c("week","season","Player"))


#Set keys, merge the two datasets together
setkey(RBdata,season,week,Player,Pos)
setkey(DKdata,season,week,Player,Pos)

final<-merge(RBdata,DKdata, all.x = TRUE)


