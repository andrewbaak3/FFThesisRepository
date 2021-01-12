library(data.table)

#Load Data
injurydata<-fread("./project/volume/data/interim/injuryreports.csv")
train<-fread("./project/volume/data/processed/trainroll4.csv")

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
finaltrain<-merge(train,injurydata)


#Get rid of columns not of interest 
drops<-c("DropName","mergeName")
finaltrain<-finaltrain[, !drops, with = FALSE]