if (!require(XML)) install.packages('XML')
library(XML)
if (!require(ggvis)) install.packages('ggvis')
library(ggvis)
if (!require(shiny)) install.packages('shiny')
library(shiny)

espnprojections = data.frame("PLAYER, TEAM POS"=character(),
                             "First.Name"=character(),
                             "OPP"=character(), 
                             "STATUS ET"=character(),
                             "C/A" = character(),
                             "PASSYDS" = double(),
                             "PASSTD" = double(),
                             "INT" = double(),
                             "RUSH" = double(),
                             "RUSHYDS" = double(),
                             "RUSHTD" = double(),
                             "REC" = double(),
                             "RECYDS" = double(),
                             "RECTD" = double(),
                             "PTS" = double()) 
for (row in seq(0,240,40)){
  rows = {readHTMLTable(paste0("http://games.espn.go.com/ffl/tools/projections?&startIndex=",row), head=TRUE, as.data.frame=TRUE, stringsAsFactors=FALSE)$playertable_0}
  rows = rows[-1,]
  names(rows) <- c("PLAYER, TEAM POS", "OPP", "STATUS ET", "C/A", "PASSYDS", "PASSTD", "INT", "RUSH", "RUSHYDS", "RUSHTD", "REC", "RECYDS", "RECTD", "PTS")
  rows["PASSYDS"] = lapply(rows["PASSYDS"],as.double)
  rows["INT"] = lapply(rows["INT"],as.double)
  rows["RUSH"] = lapply(rows["RUSH"],as.double)
  rows["RUSHYDS"] = lapply(rows["RUSHYDS"],as.double)
  rows["RUSHTD"] = lapply(rows["RUSHTD"],as.double)
  rows["REC"] = lapply(rows["REC"],as.double)
  rows["RECYDS"] = lapply(rows["RECYDS"],as.double)
  rows["RECTD"] = lapply(rows["RECTD"],as.double)
  rows["PTS"] = lapply(rows["PTS"],as.double)
  rows["PTS"] = rows["PTS"] + 0.5*rows["REC"]         #Half PPR fanduel adjustment
  espnprojections = rbind(espnprojections,rows)
}
fanduelprices = read.csv(file="FanDuel-NFL-2016-12-24-17365-players-list.csv",head=TRUE,sep=",",stringsAsFactors=FALSE)
fanduelprices$Team[fanduelprices$Team=="BAL"] = "Bal"
fanduelprices$Team[fanduelprices$Team=="CIN"] = "Cin"
fanduelprices$Team[fanduelprices$Team=="CLE"] = "Cle"
fanduelprices$Team[fanduelprices$Team=="PIT"] = "Pit"
fanduelprices$Team[fanduelprices$Team=="HOU"] = "Hou"
fanduelprices$Team[fanduelprices$Team=="IND"] = "Ind"
fanduelprices$Team[fanduelprices$Team=="JAC"] = "Jax"
fanduelprices$Team[fanduelprices$Team=="TEN"] = "Ten"
fanduelprices$Team[fanduelprices$Team=="BUF"] = "Buf"
fanduelprices$Team[fanduelprices$Team=="MIA"] = "Mia"
fanduelprices$Team[fanduelprices$Team=="DEN"] = "Den"
fanduelprices$Team[fanduelprices$Team=="OAK"] = "Oak"
fanduelprices$Team[fanduelprices$Team=="CHI"] = "Chi"
fanduelprices$Team[fanduelprices$Team=="DET"] = "Det"
fanduelprices$Team[fanduelprices$Team=="MIN"] = "Min"
fanduelprices$Team[fanduelprices$Team=="ATL"] = "Atl"
fanduelprices$Team[fanduelprices$Team=="CAR"] = "Car"
fanduelprices$Team[fanduelprices$Team=="DAL"] = "Dal"
fanduelprices$Team[fanduelprices$Team=="PHI"] = "Phi"
fanduelprices$Team[fanduelprices$Team=="WAS"] = "Was"
fanduelprices$Team[fanduelprices$Team=="ARI"] = "Ari"
fanduelprices$Team[fanduelprices$Team=="SEA"] = "Sea"

fanduelprices$Nickname = NULL
fanduelprices$Id = NULL

names(espnprojections)[names(espnprojections)=="PLAYER, TEAM POS"] = "Name"
names = strsplit(espnprojections$Name, " ")
for(i in 1:dim(espnprojections)[1]) {
  if (grepl("D/ST",names[[i]][2])) {
    espnprojections$First.Name[i] = names[[i]][1]
    espnprojections$Last.Name[i] = names[[i]][1]
  }
  else if (grepl(",",names[[i]][2])) {
    espnprojections$First.Name[i] = names[[i]][1]
    names[[i]][2] = sub(",","",names[[i]][2])
    espnprojections$Last.Name[i] = names[[i]][2]
  }
  else{ #e.g. Odell Beckham Jr. 
    espnprojections$First.Name[i] = names[[i]][1]
    names[[i]][3] = sub(",","",names[[i]][3])
    espnprojections$Last.Name[i] = paste(names[[i]][2],names[[i]][3], sep = " ")
  }
}
espnprojections$First.Name[espnprojections$Last.Name=="Ravens"] = "Baltimore"
espnprojections$First.Name[espnprojections$Last.Name=="Bengals"] = "Cincinatti"
espnprojections$First.Name[espnprojections$Last.Name=="Browns"] = "Cleveland"
espnprojections$First.Name[espnprojections$Last.Name=="Steelers"] = "Pittsburgh"
espnprojections$First.Name[espnprojections$Last.Name=="Bears"] = "Chicago"
espnprojections$First.Name[espnprojections$Last.Name=="Lions"] = "Detroit"
espnprojections$First.Name[espnprojections$Last.Name=="Packers"] = "Green Bay"
espnprojections$First.Name[espnprojections$Last.Name=="Vikings"] = "Minnesota"
espnprojections$First.Name[espnprojections$Last.Name=="Texans"] = "Houston"
espnprojections$First.Name[espnprojections$Last.Name=="Colts"] = "Indianapolis"
espnprojections$First.Name[espnprojections$Last.Name=="Jaguars"] = "Jacksonville"
espnprojections$First.Name[espnprojections$Last.Name=="Titans"] = "Tennessee"
espnprojections$First.Name[espnprojections$Last.Name=="Falcons"] = "Atlanta"
espnprojections$First.Name[espnprojections$Last.Name=="Panthers"] = "Carolina"
espnprojections$First.Name[espnprojections$Last.Name=="Saints"] = "New Orleans"
espnprojections$First.Name[espnprojections$Last.Name=="Buccaneers"] = "Tampa Bay"
espnprojections$First.Name[espnprojections$Last.Name=="Bills"] = "Buffalo"
espnprojections$First.Name[espnprojections$Last.Name=="Dolphins"] = "Miami"
espnprojections$First.Name[espnprojections$Last.Name=="Patriots"] = "New England"
espnprojections$First.Name[espnprojections$Last.Name=="Jets"] = "New York"
espnprojections$First.Name[espnprojections$Last.Name=="Cowboys"] = "Dallas"
espnprojections$First.Name[espnprojections$Last.Name=="Giants"] = "New York"
espnprojections$First.Name[espnprojections$Last.Name=="Eagles"] = "Philadelphia"
espnprojections$First.Name[espnprojections$Last.Name=="Redskins"] = "Washington"
espnprojections$First.Name[espnprojections$Last.Name=="Broncos"] = "Denver"
espnprojections$First.Name[espnprojections$Last.Name=="Chiefs"] = "Kansas City"
espnprojections$First.Name[espnprojections$Last.Name=="Raiders"] = "Oakland"
espnprojections$First.Name[espnprojections$Last.Name=="Chargers"] = "San Diego"
espnprojections$First.Name[espnprojections$Last.Name=="Cardinals"] = "Arizona"
espnprojections$First.Name[espnprojections$Last.Name=="Rams"] = "Los Angeles"
espnprojections$First.Name[espnprojections$Last.Name=="49ers"] = "San Francisco"
espnprojections$First.Name[espnprojections$Last.Name=="Seahawks"] = "Seattle"


#INNER JOIN need to join on team too (david johnson)
completetable = merge(fanduelprices, espnprojections, all = FALSE)

sum = 0.0
for(i in 1:dim(completetable)[1]) {
  completetable$Value[i] =  completetable$PTS[i] / completetable$Salary[i]
}
max = max(completetable$Value, na.rm=TRUE)     
min = min(completetable$Value, na.rm=TRUE)  
mean = mean(completetable$Value, na.rm=TRUE)

generate_color <- function(value,max,min,mean){
  red = 255
  green = 255
  if (value < mean) {
    green = 255 - ((((mean - value)/mean) / ((mean-min)/mean)) * 255)
  }
  else if (value > mean) {
    red =  255 - ((((value - mean)/mean) / ((max-mean)/mean)) * 255)
  }
  return (c(red,green))
}

for(i in 1:dim(completetable)[1]) {
  values <- generate_color(completetable$Value[i], max, min, mean)
  completetable$RGB[i] = rgb(values[1], values[2], 0, maxColorValue=255)
}

# start ggvis


# positional_stats_printout <- function(player){
#   if (player$position == "QB"){
#     return "a"
#   }
#   else{
#     return "a"
#   }
# }

on_hover <- function(x) {
  if(is.null(x)) return(NULL)
  row = completetable[completetable$id == x$id,]
  
  paste0("<p style='color:", row$RGB ,";'>", row$First.Name, " ", row$Last.Name,"</p>", row$Position, "<br /> $", row$Salary, "<br />Proj: ", row$PTS, "<br />", row$OPP)

}
completetable$id <- 1:nrow(completetable)
completetable = completetable[completetable$Position == 'WR',]

visualization = ggvis(completetable, ~PTS, ~Salary, key := ~id, fill := ~RGB) %>% add_tooltip(on_hover, "hover") %>% layer_points() %>% hide_legend('fill') #%>% scale_ordinal("fill", range = c("red", "green")) 

#   layer_model_predictions(model = "lm", se = TRUE) %>% 


