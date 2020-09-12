#espnFantasyApi.R
#ESPN Fantasy API Connection
#Info on other endpoints: https://www.reddit.com/r/fantasyfootball/comments/d6uf1p/espn_v3_api_endpoints/
#Correctly formatting cookies: https://dusty-turner.netlify.app/post/espn-fantasy-football-v3-api-for-private-leagues-an-r-solution-finally/

# Set up the environment
setwd("~/Documents/GitHub/fantasy-football-viz")

getPlayersData <- function(leagueID,isPublic){
  #Access Players API endpoint
  url  = paste0("http://fantasy.espn.com/apis/v3/games/ffl/seasons/",format(Sys.Date(), "%Y"),"/segments/0/leagues/",leagueID,"?view=kona_player_info")
  if(isPublic){
    playersGet = httr::GET(url)
  }
  else{
    #Use saved cookies (espnCookies.txt) to log in
    #Get cookies from Chrome->Settings->Cookies and Other Site Data, under ESPN
    cookies = c(`swid` = readLines("espn/espnCookies.txt", warn=FALSE)[1],
                 `espn_s2` =  readLines("espn/espnCookies.txt", warn=FALSE)[2] )
    cookie = paste(names(cookies), cookies, sep = "=", collapse = ";")
    playersGet = httr::GET(url = url, config = httr::config(cookie = cookie))
  }
  playersRaw = rawToChar(playersGet$content)
  players = jsonlite::fromJSON(playersRaw)
  return(players)
}



#Example Use of API
#leagueID = readLines("leagueIds.txt", warn=FALSE)[3]
#leaguePlayers = getPlayersData(leagueID,FALSE)
#playersDf = leaguePlayers$players$player
