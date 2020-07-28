#ESPN Fantasy API Connection
#Some API Info: https://github.com/mkreiser/ESPN-Fantasy-Football-API
#A Resource to Peruse: https://www.reddit.com/r/fantasyfootball/comments/d6uf1p/espn_v3_api_endpoints/

# Set up the environment
setwd("~/Documents/GitHub/fantasy-football-viz")

getLeagueInfo <- function(){
  #Get league data: not sure why the blob is not giving back JSON
  
  #Load league ID and build request url
  baseurl = "http://games.espn.com/ffl/api/v2/leagueSettings?leagueId="
  leagueID = readLines("leagueIds.txt", warn=FALSE)[3]
  url = paste0(baseurl, leagueID, "&seasonId=",format(Sys.Date(), "%Y"))
  
  #Get ESPN blob
  leagueGet = httr::GET(url)
  leagueRaw = rawToChar(ESPNGet$content)
  #league = jsonlite::fromJSON(ESPNRaw)
  #return(league)
}

getPlayersData <- function(){
  #Access Players API endpoint
  url = paste0("http://fantasy.espn.com/apis/v3/games/ffl/seasons/",format(Sys.Date(), "%Y"),"/players?scoringPeriodId=0&view=players_wl")
  playersGet = httr::GET(url)
  playersRaw = rawToChar(playersGet$content)
  players = jsonlite::fromJSON(playersRaw)
  return(players)
}



playersDf = getPlayersData()

