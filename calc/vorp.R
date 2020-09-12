#Cole Conte
#VORP Calculations

calculateReplacementValues <- function(playerProj){
  #How do we choose the replacement level player?
  teams = 12
  iterDf = data.frame(count = c(round(teams),round(teams*2.5),round(teams*2.5),round(teams),teams,teams),
                      Pos = c("QB","RB","WR","TE","DST","K"), stringsAsFactors = F
                      )
  iterDf$replacement = apply(iterDf,1,getNthValue,pdf=playerProj)
  return(iterDf)
}

getNthValue <- function(idf,pdf){
  #subset players dataframe by position
  positionToSubset = idf[2]
  df = pdf[pdf$Pos == positionToSubset,]
  df = df[order(-df$FPTS),]
  n = as.numeric(as.character(idf[1])) 
  nth = df[n-1,]$FPTS
  return(nth)
}

calculateVorp <- function(player,replacementValues){
  playerPos = player[2]
  posRepl = replacementValues[replacementValues$Pos == playerPos,]$replacement
  return(round(as.numeric(as.character(player[3])) - posRepl[1],digits=2))
}