getyeartables <- function(year, week) {
  
  my_scrape <- scrape_data(src = c("CBS", "ESPN", "Yahoo"), 
                           pos = c("QB", "RB", "WR", "TE", "DST", "K"),
                           season = year, week = week)
  
  for (i in 1:length(names(my_scrape))) {
    dt <- my_scrape[i]
    tablename <- names(my_scrape)[i]
    dt <- data.table(dt[[1]])
    fwrite(dt, paste0("./project/volume/data/raw/", year, "_", week, "_",tablename, ".csv"))
  }
  
}