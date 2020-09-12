#getters.R
#Basic getter functions for ESPN Fantasy API

setwd("~/Documents/GitHub/fantasy-football-viz")

getPlayerProjections <- function(playersDf,playerId){
  player = playersDf[playersDf$id == playerId,]
  playerStats = player$stats[[1]]
  
  #Next step: Connect ESPN codes to stat categories. Need to do some research/matching of categories.
  #External ID refers to season, statSourceId is 0 actual/1 projected
  #playerProjs = playerStats[playerStats$externalId == "2020" & playerStats$statSourceId == "1" ,]$stats
  #for...
  #names(playerProjs)[names(playerProjs) == "23"] <- "CAR"
  #return (playerProjs)
  #For now: return seasonal projection
  return (as.numeric(playerStats[playerStats$externalId == "2020" & playerStats$statSourceId == "1" ,]$appliedTotal))
}

getADP <- function(playersDf, playerId){
  #Get average draft position
  player = playersDf[playersDf$id == playerId,]
  return (as.numeric(player$ownership$averageDraftPosition))
}



