#Cole Conte
#Fantasy Football Draft Interface

library(shinydashboard)

#Set up the environment
setwd("~/Documents/GitHub/fantasy-football-viz")
source("espnFantasyApi.R")

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
              DT::dataTableOutput("draftBoard")
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
  
  #Display player names and injury status in a dynamic table
  nameInj = playersDf[c("fullName","injured")]
  output$draftBoard = DT::renderDataTable({
    nameInj
  })
}

shiny::shinyApp(ui,server)
