#Cole Conte
#Fantasy Football Draft Interface

library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
#setwd("Documents/GitHub/fantasy-football-viz")
#inv = read.csv("investments.csv")

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
              h2("Draft Board")
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
  #output$aaPlot = renderPlotly({
   # ggplotly(
    #  ggplot(inv,aes(x=Asset.Class,y=Current,fill=Asset.Class))+
     #   geom_col()+
      #  labs(title="Asset Allocation",xlab=element_blank())
    #)
  #})
}

shinyApp(ui,server)
