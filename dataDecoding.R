#Data Decoding
#Turning ESPN's coding of player info into human readable info

#Set up the environment
setwd("~/Documents/GitHub/fantasy-football-viz")
source("espnFantasyApi.R")


getPlayerPos <- function(playersDf){
  #Returns a data frame with player name and position
  playerPos = playersDf[c("fullName","eligibleSlots")]
  playerPos$combPos = sapply(playerPos$eligibleSlots, function(x) paste((toString(unlist(x))),collapse=""))

  #Connect ESPN codes to readable player positions
  codePos = data.frame(
    combPos = c("16, 20, 21","17, 20, 21","0, 7, 20, 21","5, 6, 23, 7, 20, 21","3, 4, 5, 23, 7, 20, 21","2, 3, 23, 7, 20, 21","8, 11, 15, 20, 21, 2, 3, 23, 7","25, 0, 7, 20, 21","25, 2, 3, 23, 7, 20, 21","25, 5, 6, 23, 7, 20, 21","25, 17, 20, 21","25, 3, 4, 5, 23, 7, 20, 21"),
    pos = c("D/ST","K","QB","TE","WR","RB","DT/RB","QB","RB","TE","K","WR")
  )
  playerPos = merge(playerPos,codePos,by="combPos")
  playerPos = playerPos[,c("fullName","pos")]
  return (playerPos)
}


