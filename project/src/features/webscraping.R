library(rvest)
library(XML)
library(data.table)



#this function is useful for grabbing all links on a page
links <- function(URL) 
{
  getLinks <- function() {
    links <- character()
    list(a = function(node, ...) {
      links <<- c(links, xmlGetAttr(node, "href"))
      node
    },
    links = function() links)
  }
  h1 <- getLinks()
  htmlTreeParse(URL, handlers = h1)
  h1$links()
}

#function to return name of players
get_name<-function(url){
  page<-read_html(url)
  node<-html_nodes(page,".player-name")
  player_name<-html_text(node)
  player_name
}


####Approach 1#########

#Set the start web url
path1<-("https://www.footballdb.com/transactions/injuries.html?yr=")


seasons<-c("2016", "2017", "2020")
weeks<-1:17
data<-NULL
for (i in 1:length(seasons)) {
  path2<-paste0(path1,seasons[i])
  
  for (j in 1:length(weeks)) {
    path3<-paste0(path2,"&wk=",j,"&type=reg") 
    URL<-read_html(path3)
    divtables<-html_nodes(URL, ".divtable")
    
    for (k in 1:length(divtables)) {
      temp<-html_nodes(divtables[k], ".tr")
      
      temp<-data.table(temp)
      
    }
  }
}
################################

###Approach 2###################

start<-read_html("https://www.footballdb.com/transactions/injuries.html")
current<-links(start)
nextlink<-current[grep("?yr=",current)]
nextlink<-nextlink[-grep("/standings/",nextlink)]
nextlink<-nextlink[-grep("/draft/",nextlink)]
nextlink<-nextlink[-grep("/scores/",nextlink)]


page<-"https://www.footballdb.com"
data<-NULL
for (l in i:length(nextlink)) {
  goto<-paste0(page, nextlink[l])
  URL<-read_html(goto)
  divtables<-html_nodes(URL, ".divtable")
  
  for (m in 1:length(divtables)) {
    temp<-html_text(divtables[m])
    temp<-data.table(temp)
    
  }
}
########################33