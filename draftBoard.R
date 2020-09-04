#Cole Conte
#Fantasy Football Draft Interface


#Set up the environment
setwd("~/Documents/GitHub/fantasy-football-viz")
library(shinydashboard)
library(shiny)
library(DT)
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
  #Remove irrelevant players
  playerProj = playerProj["FPTS" > 50.0]
  replVals = calculateReplacementValues(playerProj)
  playerProj$vorp = apply(playerProj,1,calculateVorp,replacementValues=replVals)
  #Add Vorp rankings and ADP to VORP differential
  order.vorp = order(playerProj$vorp,decreasing = T)
  playerProj$vorpRank[order.vorp] = 1:nrow(playerProj) 
  playerProj$differential = playerProj$Rank - playerProj$vorpRank 
  #Highlight high positive differentials
  output$draftBoard = renderDT(datatable(playerProj)%>% formatStyle("differential",background = styleInterval(c(-5,5), c("red","white","green"))), filter="top" ) 
}

shinyApp(ui,server)
