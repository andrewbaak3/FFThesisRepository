library(data.table)

#Read in player data 
qb<-fread("./project/volume/data/teamconstruction/roll5_final_QB.csv")
te<-fread("./project/volume/data/teamconstruction/roll5_final_TE.csv")
wr<-fread("./project/volume/data/teamconstruction/roll5_final_WR.csv")
rb<-fread("./project/volume/data/teamconstruction/roll5_final_RB.csv")

final<-data.table()

#Merge all player data into one final dataframe
final<-rbind(final,qb,te,wr,rb)

#Read in draftkings data
DKdata<-fread("./project/volume/data/interim/DKsalaries.csv")

#Drop unimportant columns
DKdrops<-c("GID","Team","h/a","Oppt")
DKdata<-DKdata[, !DKdrops, with = FALSE]


#Rename columns in draftkings data to ease merging process
setnames(DKdata, c("Week","Year","Name"), c("week","season","Player"))

#Set keys, merge the two datasets together
setkey(final,season,week,Player,Pos)
setkey(DKdata,season,week,Player,Pos)

modelready<-merge(final,DKdata, all.x = TRUE)

#Add in defensive stats
def<-fread("./project/volume/data/interim/DKsalaries.csv") 
def<-def[Year==2017]
def<-def[Pos == "Def"]
setnames(def, c("Week","Year","Name"), c("week","season","Player"))


Defdrops<-c("GID","Team","h/a","Oppt")
def<-def[, !Defdrops, with = FALSE]

modelready<-rbind(modelready, def, fill = T)

#Compute points per dollar (PPD) using Predicted Points divided by Salary. Multiplying by 1000 to make it easier
#to interpret 
modelready$PPD<-(modelready$PredPoints/modelready$`DK salary`)*1000


#Write out data
fwrite(modelready, "./project/volume/data/teamconstruction/packingdata.csv")
