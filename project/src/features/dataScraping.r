library("ffanalytics")
library(tidyverse)
library(data.table)

source("./project/src/features/tablescrapefunction.R")


getyeartables(2020, 2)


fwrite(projections_table, "./project/volume/data/raw/2018season.csv")
