library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  # Application title
  titlePanel("Approval markets predictions"),
  
  # Sidebar with a slider input for number of bins 
  fluidRow(
    column(
      4,
        textInput("pi_market", "Predictit market", value = "538.071618")
         ),
    column(4,
          textInput("rcp_market", "RCP poll", value = "president_trump_job_approval-6179")
           ),
    column(4, 
           textInput("rcp_historical", "RCP historical", value = "6179")
           )
  ),
  fluidRow(
    column(
      4,
      div(tableOutput("PItable"), style = "font-size:90%")
      
    ),
    column(8,
      div(tableOutput("RCPtable"), style = "font-size:80%")
    )
  ),
  fluidRow(
    column(
      12,
      plotlyOutput("RCPhistorical")
      
    )
  )
))