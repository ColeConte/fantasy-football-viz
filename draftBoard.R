#Cole Conte
#Fantasy Football Draft Interface


#Set up the environment
setwd("~/Documents/GitHub/fantasy-football-viz")
library(shinydashboard)
library(shiny)
source("fantasyProsScrape.R")
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
  #Display player names and positions in a dynamic table
  playerProj = scrapeProjections()
  playerProj = playerProj["FPTS" > 50.0]
  replVals = calculateReplacementValues(playerProj)
  playerProj$vorp = apply(playerProj,1,calculateVorp,replacementValues=replVals)
  output$draftBoard = DT::renderDT(playerProj, filter="top")
}

shinyApp(ui,server)
