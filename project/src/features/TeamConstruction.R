library(data.table)

DT<-fread("./project/volume/data/teamconstruction/packingdata.csv")

setnames(DT, c("DK points","DK salary"), c("DK_points","DK_salary"))


getPlayers<-function(week_DT,pos,n,salary_cap){
  week_DT_pos<-week_DT[Pos==pos]  
  week_DT_pos$DPP<-week_DT_pos$DK_salary/week_DT_pos$PredPoints
  n_players<-nrow(week_DT_pos)
  
  player_combos<-data.table(t(combn(1:n_players,n)))
  salary_combos<-data.table(t(combn(week_DT_pos$DK_salary,n)))
  point_combos<-data.table(t(combn(week_DT_pos$PredPoints,n)))
  
  player_combos$total_salary<-rowSums(salary_combos)
  player_combos$total_PredPoints<-rowSums(point_combos)
  
  player_combos<-player_combos[total_salary<=salary_cap][order(-total_PredPoints)]
  
  best_players<-data.table(t(player_combos[1,1:n]))
  
  player_DT<-week_DT_pos[best_players$V1]
  
  out<-list(players = player_DT )
  out$n<-nrow(player_DT)
  out$salary_tot<-sum(player_DT$DK_salary)
  out$predDPP<-sum(player_DT$DK_salary)/sum(player_DT$PredPoints)
  out$DK_points<-sum(player_DT$DK_points)
  out
}


teamConstruct<-function(week_DT,C){
  
  salary_cap<-50000
  salary_rm<-salary_cap
  pos_rm<-8
  avg_sal<-salary_rm/pos_rm
  
  pos_DT<-data.table(pos=c("RB","WR","TE","QB"),max=c(3,4,2,1),min=c(2,3,1,1))
  
  RB_out<- getPlayers(week_DT,"RB",pos_DT[pos=="RB"]$max,pos_DT[pos=="RB"]$max*avg_sal*C)
  WR_out<- getPlayers(week_DT,"WR",pos_DT[pos=="WR"]$max,pos_DT[pos=="WR"]$max*avg_sal*C)
  TE_out<- getPlayers(week_DT,"TE",pos_DT[pos=="TE"]$max,pos_DT[pos=="TE"]$max*avg_sal*C)
  QB_out<- getPlayers(week_DT,"QB",pos_DT[pos=="QB"]$max,pos_DT[pos=="QB"]$max*avg_sal*C)
  
  pos_order<-order(c(-RB_out$predDPP,-WR_out$predDPP,-TE_out$predDPP,-QB_out$predDPP))
  pos_DT<-pos_DT[pos_order,]
  
  #pos_DT<-rbind(pos_DT,data.table(pos="QB",max=1,min=1))
  
  team<-NULL
  flex_on_team<-FALSE
  
  for (i in 1:4){
    
    if (flex_on_team==FALSE){
      salary_pos<-(salary_rm/8)*pos_DT$max[i]*C
      new_players<-getPlayers(week_DT,pos_DT$pos[i],pos_DT$max[i],salary_pos)
      pos_rm<-pos_rm-pos_DT$max[i]
    }else{
      avg_sal<-salary_rm/pos_rm
      new_players<-getPlayers(week_DT,pos_DT$pos[i],pos_DT$min[i],avg_sal*pos_DT$min[i])
      pos_rm<-pos_rm-pos_DT$min[i]
    }
    
    if (new_players$n==pos_DT[i]$max){flex_on_team=TRUE}
    
    team<-list(players=rbind(team$players,new_players$players))
    team$n<-nrow(team$players)
    team$salary_tot<-sum(team$players$DK_salary)
    team$predPPD<-sum(team$players$PredPoints)/sum(team$players$DK_salary)
    team$predPoints<-sum(team$players$PredPoints)
    team$points<-sum(team$players$DK_points)
    salary_rm<-salary_cap-team$salary_tot
  }
  
  return(team)
}

#Create teams for every week of the season
weeks<-1:17
results<-data.table()
for (i in 1:length(weeks)) {
  week_dt<-DT[week==i]
  temp<-teamConstruct(week_dt, 1.2)
  new_row<-data.table(week=i,predpoints=temp$predPoints,salary = temp$salary_tot, points=temp$points)
  results<-rbind(results,new_row)
}



results$points<-results$points+6.26875
#Read in historical contest data 
historicalDK<-fread("./project/volume/data/teamconstruction/resultsdb.csv")
historicalDK<-historicalDK[1:624, ]

#Change name to make merging easier
setnames(historicalDK, "Week", "week")

#Set keys and merge the two datasets
setkey(results,week)
setkey(historicalDK,week)
analysis_table<-merge(historicalDK,results)

setnames(analysis_table, c("Cash Line","Top Prize","Buy In","Max Entries","Prize Pool"), 
         c("CashLine","TopPrize","BuyIn","MaxEntries","PrizePool"))


#Compute profits 
analysis_table$Profit<-((analysis_table$points>analysis_table$CashLine)*analysis_table$TopPrize)-analysis_table$BuyIn

profits<-sum(analysis_table$Profit, na.rm = T)

cost<-sum(analysis_table$BuyIn, na.rm =T)
