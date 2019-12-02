library(RCurl)
library(rjson)

rawjson <- getURL('https://raw.githubusercontent.com/nhlscorebot/arenas/master/teams.json')
locations <- fromJSON(rawjson)

arenas <- data.frame('team' = names(locations))
uloc <- unlist(locations)
arenas$name<-uloc[seq(1, length(uloc), by=3)]
arenas$lat<-uloc[seq(2, length(uloc), by=3)]
arenas$lng<-uloc[seq(3, length(uloc), by=3)]
arenas$lat<-as.numeric(arenas$lat)
arenas$lng<-as.numeric(arenas$lng)
arenas$label<-paste0(arenas$team, ' - ', arenas$name)


write_rds(arenas, "nhl/arenas.rds")