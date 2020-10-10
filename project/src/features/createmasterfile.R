library(data.table)

#read in player statistics table
playerstats<-fread("./project/volume/data/interim/gamePlayerstats.csv")

#read in box scores table
boxscores<-fread("./project/volume/data/interim/allBoxscores.csv")

#only concerned with stats from 2009 on as the box scores data starts at 2009
playerstats<-playerstats[season>2008]

#Create home and away team tables
home<-boxscores[,.(game_id,home_team,week,season,home_score)]
setnames(home,c("home_team","home_score"),c("Tm","score"))

away<-boxscores[,.(game_id,away_team,week,season,away_score)]
setnames(away,c("away_team","away_score"),c("Tm","score"))

#Rbind home and away to create allboxscores
allboxscores<-rbind(home,away)
allboxscores<-merge(allboxscores,allboxscores,by=c("game_id","week","season"),allow.cartesian = T)
setnames(allboxscores,c("Tm.x","score.x","Tm.y","score.y"), c("Tm","Tmscore","Opponent","Opponentscore"))
allboxscores<-allboxscores[!Tm==Opponent]

#Fix situations where Team name (Tm) is different from one table to the next
playerstats[playerstats$Tm == "NOR"]$Tm<-"NO"
playerstats[playerstats$Tm == "KAN"]$Tm<-"KC"
playerstats[playerstats$Tm == "NWE"]$Tm<-"NE"
playerstats[playerstats$Tm == "GNB"]$Tm<-"GB"
playerstats[playerstats$Tm == "SFO"]$Tm<-"SF"
playerstats[playerstats$Tm == "TAM"]$Tm<-"TB"
playerstats[playerstats$Tm == "SDG"]$Tm<-"SD"
playerstats[playerstats$Tm == "LAR"]$Tm<-"LA"
allboxscores[allboxscores$Tm=="JAC"]$Tm<-"JAX"


#Set keys for each table so merge can occur
setkey(playerstats,week,season,Tm)
setkey(allboxscores,week,season,Tm)
#something not happening properly on this merge
scoresandstats<-merge(playerstats,allboxscores, all.x = T)
scoresandstats<-scoresandstats[order(season,week)]




#Create separate table with just team, week, and c_week to calculate the correct cumulative week
DT<- scoresandstats[,.(season,week,c_week)]
DT<-DT[!duplicated(DT)]
DT<-DT[,cumulativeweek := cumsum(c_week)]

DT$c_week<-NULL
scoresandstats$c_week<-NULL

setkey(scoresandstats, season, week)
setkey(DT,season,week)
scoresandstats<-merge(scoresandstats,DT)



fwrite(scoresandstats,"project/volume/data/processed/StatsandBoxscores.csv") 
