#dataDecoding
#Getter functions that require turning ESPN's coding of player info into human readable info

#Set up the environment
setwd("~/Documents/GitHub/fantasy-football-viz")


getPlayerPos <- function(playersDf){
  #Returns a data frame with id, player name and position
  playerPos = playersDf[c("id","fullName","eligibleSlots")]
  playerPos$combPos = sapply(playerPos$eligibleSlots, function(x) paste((toString(unlist(x))),collapse=""))

  #Connect ESPN codes to readable player positions
  codePos = data.frame(
    combPos = c("16, 20, 21","17, 20, 21","0, 7, 20, 21","5, 6, 23, 7, 20, 21","3, 4, 5, 23, 7, 20, 21","2, 3, 23, 7, 20, 21","8, 11, 15, 20, 21, 2, 3, 23, 7","25, 0, 7, 20, 21","25, 2, 3, 23, 7, 20, 21","25, 5, 6, 23, 7, 20, 21","25, 17, 20, 21","25, 3, 4, 5, 23, 7, 20, 21"),
    pos = c("D/ST","K","QB","TE","WR","RB","DT/RB","QB","RB","TE","K","WR")
  )
  playerPos = merge(playerPos,codePos,by="combPos")
  playerPos = playerPos[,c("id","fullName","pos")]
  return (playerPos)
}

# getPlayerTeam <- function(playersDf){
#   #Returns a data frame with id, player name and team
#   playerTeam = playersDf[c("id","fullName","proTeamId")]
#   
#   #TODO: Connect ESPN codes to teams
#   codeTeam = data.frame(
#     proTeamId = c("")
#     team = c("")
#   )
#   playerTeam = merge(playerTeam,codeTeam,by="proTeamId")
#   playerTeam = playerTeam[,c("id","fullName","team")]
#   return (playerTeam)
# }




