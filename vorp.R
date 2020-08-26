#Cole Conte
#VORP Calculations

calculateReplacementValues <- function(pdf){
  #How do we choose the replacement level player?
  teams = 12
  iterDf = data.frame(count = c(round(teams),round(teams*3),round(teams*3),round(teams),teams,teams),
                      pos = c("QB","RB","WR","TE","D/ST","K"), stringsAsFactors = F
                      )
  iterDf$replacement = apply(iterDf,1,getNthValue,pdf=pdf)
  return(iterDf)
}

getNthValue <- function(idf,pdf){
  #subset players dataframe by position
  positionToSubset = idf[2]
  df = pdf[pdf$pos == positionToSubset,]
  df = df[order(-df$projection),]
  n = as.numeric(as.character(idf[1])) 
  print(n)
  nth = df[n-1,]$proj
  print(nth)
  return(nth)
}

calculateVorp <- function(player,replacementValues){
  playerPos = player[3]
  posRepl = replacementValues[replacementValues$pos == playerPos,]$replacement
  return(as.numeric(as.character(player[4])) - posRepl[1])
}
