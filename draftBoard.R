#Cole Conte
#Fantasy Football Draft Interface


#Set up the environment
setwd("~/Documents/GitHub/fantasy-football-viz")
library(shinydashboard)
library(shiny)
source("espnFantasyApi.R")
source("dataDecoding.R")
source("getters.R")
source("vorp.R")

ui = dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Draft Board", tabName = "DraftBoard", icon = icon("glyphicon-th-list")),
      menuItem("Roster", tabName = "Roster", icon = icon("glyphicon-user"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "DraftBoard",
              h2("Draft Board"),
              DT::DTOutput("draftBoard")
              #fluidRow(
               # column(width=12,plotlyOutput("aaPlot"))
              #),
      ),
      tabItem(tabName = "Roster",
              h2("Roster")
      )
    )
  )
)

server <- function(input,output){
  #Connect to ESPN API 
  leagueID = readLines("leagueIds.txt", warn=FALSE)[3]
  leaguePlayers = getPlayersData(leagueID,FALSE)
  playersDf = leaguePlayers$players$player
  
  #Display player names and positions in a dynamic table
  pos = getPlayerPos(playersDf)
  pos$projection = sapply(pos$id,getPlayerProjections,playersDf=playersDf)
  pos$adp = sapply(pos$id,getADP,playersDf=playersDf)
  replVals = calculateReplacementValues(pos)
  pos$vorp = apply(pos,1,calculateVorp,replacementValues=replVals)
  output$draftBoard = DT::renderDT(pos[,-1], filter="top")
}

shinyApp(ui,server)
