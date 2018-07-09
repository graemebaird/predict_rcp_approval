library(shiny)


shinyServer(function(input, output) {
  
  source("../codefiles/libraries.R")
  source("../codefiles/functions.R")
  
  output$PItable <- renderTable({
    
    ql.PRD(input$pi_market)
    
  })
  
  output$RCPtable <- renderTable({
    
    ql.RCP(input$rcp_market)
  
  })
  
  output$RCPhistorical <- renderPlot({
    
    updateRCP() %>%
      ggplot() + 
      geom_line(aes(Date,app6179))
    
  })
  
})
