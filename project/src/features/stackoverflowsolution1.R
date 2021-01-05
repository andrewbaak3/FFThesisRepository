library(purrr)
library(rvest)

path1 <- 'https://www.footballdb.com/transactions/injuries.html?yr='
seasons <- c("2016", "2017", "2020")
weeks <- 1:17
results <- list()
c <- 1

for(s in seq_along(seasons)){
  for(w in seq_along(weeks)){
    c <- c+1
    results[c] <- paste0(path1, seasons[s],"&wk=", as.character(w), "&type=reg")
  }
}

#use `results` for all rather than just `results[1:3]`

result <- map_df(compact(results), function(x){ 
  page <- read_html(x)
  data.frame(
    Player = page %>% html_nodes('.divtable .td:nth-child(1) b') %>% html_text(),
    Injury = page %>% html_nodes('.divtable .td:nth-child(2)') %>% html_text(),
    Wed = page %>% html_nodes('.divtable .td:nth-child(3)') %>% html_text(),
    Thu = page %>% html_nodes('.divtable .td:nth-child(4)') %>% html_text(),
    Fri = page %>% html_nodes('.divtable .td:nth-child(5)') %>% html_text(),
    GameStatus = page %>% html_nodes('.divtable .td:nth-child(6)') %>% html_text()
  )
}
)