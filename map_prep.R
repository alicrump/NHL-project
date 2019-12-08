library(RCurl)
library(rjson)

# I got all of this code from http://rstudio-pubs-static.s3.amazonaws.com/257443_6639015f2f144de7af35ce4615902dfd.html
# This code is downloading the locations of the stadiums from a github account
# then they convert from JSON to a data frame arenas which we'll use in app.R
# for our map


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

# Move arenas to my shiny folder so my shiny app can "see" it

write_rds(arenas, "nhl/arenas.rds")