#Cole Conte
#Fantasy Football Draft Interface


#Set up the environment
setwd("~/Documents/GitHub/fantasy-football-viz")
library(shinydashboard)
library(shiny)
library(DT)
source("fantasyPros/fantasyProsScrape.R")
source("calc/vorp.R")

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
              DTOutput("draftBoard")
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
  replVals = calculateReplacementValues(playerProj)
  playerProj$vorp = apply(playerProj,1,calculateVorp,replacementValues=replVals)
  #Add Vorp rankings and ADP to VORP differential
  #order.vorp = order(playerProj$vorp,decreasing = T)
  #playerProj$vorpRank[order.vorp] = 1:nrow(playerProj) 
  #Filter down to players with listed budget
  playerProj = playerProj[!is.na(playerProj$Budget),]
  vorpLm = lm(Budget~poly(vorp,2),data=playerProj,na.action = na.pass)
  #print(fitted(vorpLm,na))
  playerProj$ProjectedBudgetVorp = round(fitted(vorpLm),2)
  playerProj$BudgetDiff = round(playerProj$ProjectedBudgetVorp - playerProj$Budget,2)
  output$draftBoard = renderDT(datatable(playerProj,filter="top",options=list(pageLength=100))%>% formatStyle("BudgetDiff",background = styleInterval(c(-3,3), c("red","white","green"))) ) 
  #playerProj$differential = playerProj$Rank - playerProj$vorpRank 
  #Highlight high positive differentials
  #output$draftBoard = renderDT(filter="top",options=list(pageLength = 100),datatable(playerProj))
}

shinyApp(ui,server)
