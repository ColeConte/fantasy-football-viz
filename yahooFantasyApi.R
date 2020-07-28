#Yahoo Fantasy API Connection
#Helpful Resource That Also Has ESPN Connection Info: 
#https://dusty-turner.netlify.app/post/espn-yahoo-api-navigation/

# Setup the environment
setwd("~/Documents/GitHub/fantasy-football-viz")

initialSetup <- function(){
  #Run this function when you're first connecting to the API. Otherwise the credentials should be saved already.
  #Read your developer keys from yahooCreds.txt
  options("httr_oob_default" = T)
  cKey    = readLines("yahooCreds.txt", warn=FALSE)[1]
  cSecret = readLines("yahooCreds.txt", warn=FALSE)[2]
  yahoo   = httr::oauth_endpoints("yahoo")
  myapp   = httr::oauth_app("yahoo", key=cKey, secret=cSecret)
  yahoo   = httr::oauth_endpoint(authorize ="https://api.login.yahoo.com/oauth2/request_auth", access = "https://api.login.yahoo.com/oauth2/get_token", base_url = "https://fantasysports.yahooapis.com")
  myapp   = httr::oauth_app("yahoo", key=cKey, secret = cSecret,redirect_uri = "oob")
  
  #Authorize app
  httr::BROWSE(httr::oauth2.0_authorize_url(yahoo, myapp, scope="fspt-r", redirect_uri = myapp$redirect_uri))
  passcode = "YOUR_PASSWORD_HERE"
  
  #Get yahoo_token and sig and save to Fantasy.Rdata so that they can be loaded into your environment next time you start up
  yahoo_token = httr::oauth2.0_access_token(yahoo,myapp,code=passcode)
  sig         = httr::sign_oauth1.0(myapp, yahoo_token$oauth_token, yahoo_token$oauth_token_secret)
  save(sig,yahoo_token,file="Fantasy.Rdata")
}

getGameKey <- function(yahooToken){
  #Get Game Key: Game key is not saved to Fantasy.Rdata because it may change
  game_page = httr::GET("https://fantasysports.yahooapis.com/fantasy/v2/game/nfl",httr::add_headers(Authorization=paste0("Bearer ", yahoo_token$access_token)))
  XMLgame   = httr::content(game_page, as="parsed", encoding="utf-8")
  gameDoc   = XML::xmlTreeParse(XMLgame, useInternal=TRUE)
  gameList  = XML::xmlToList(XML::xmlRoot(gameDoc))
  gameKey   = gameList$game$game_key
}

requestAPI <- function(baseURL,leagueID,tag,gameKey){
  #Send a GET request to the Yahoo Fantasy API
  requestURL  = paste0(baseURL,gameKey,".l.",leagueID,tag)
  requestPage = httr::GET(requestURL, httr::add_headers(Authorization = paste0("Bearer ", yahoo_token$access_token)))
  XMLrequest  = httr::content(requestPage, as = "parsed", encoding = "utf-8")
  
  requestDoc  = XML::xmlTreeParse(XMLrequest, useInternal = TRUE)
  requestList = XML::xmlToList(XML::xmlRoot(requestDoc))
  return(requestList)
}

#Load saved authorization data, game key, and league IDs
load(file = "Fantasy.RData")
gameKey = getGameKey(yahoo_token)
bdoe  = readLines("leagueIds.txt", warn=FALSE)[1]
loeg  = readLines("leagueIds.txt", warn=FALSE)[2]

#Set up your request parameters
baseURL     = "https://fantasysports.yahooapis.com/fantasy/v2/league/"
leagueID    = bdoe
tag         = "/players"

#Make your API request and save it to a dataframe
df = requestAPI(baseURL,leagueID,tag,gameKey)











