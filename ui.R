library(shiny)
library(shinydashboard)
library(DT)

dashboardPage(
  dashboardHeader(title = "Florida's COVID-19"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      box(width=8, 
          status="info", 
          title= "Map of Cases by County",
          solidHeader = TRUE,
          plotOutput("myplot")
      ),
      box(width=4, 
          status="warning", 
          title = "Data Frame of Cases by County",
          solidHeader = TRUE, 
          collapsible = TRUE, 
          DT::dataTableOutput("mydata")
      )
    )
  )
)
