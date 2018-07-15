
shinyServer(function(input, output) {
  
  source("libraries.R")
  source("functions.R")
  
  output$PItable <- renderTable({
    
    ql.PRD(input$pi_market)
    
  })
  
  output$RCPtable <- renderTable({
    
    ql.RCP(input$rcp_market)
  
  })
  
  output$RCPhistorical <- renderPlotly({
    
    p <- updateRCP() %>%
      ggplot() + 
      geom_line(aes(Date,app6179)) + 
      ylab("Approval") + 
      ggtitle("Average rating")
    ggplotly(p)
    
  })
  
  
})
