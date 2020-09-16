library("ffanalytics")
library(tidyverse)
library(data.table)

my_scrape <- scrape_data(src = c("CBS", "ESPN", "Yahoo"), 
                         pos = c("QB", "RB", "WR", "TE", "DST", "K"),
                         season = 2018, week = 0)

my_projections <- projections_table(my_scrape)

my_projections <- my_projections %>% add_ecr() %>% add_risk() %>%
  add_adp() %>% add_aav()

my_projections <- my_projections %>% add_player_info()

projections_table<-data.table(my_projections)

fwrite(projections_table, "./project/volume/data/raw/2018season.csv")
