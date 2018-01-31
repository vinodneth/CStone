library(shiny)



shinyServer(function(input, output) {
  
    
  variable<-reactiveValues(value="NA")
  variable<-reactiveValues(txt="None")
  
  data <- observeEvent(input$smit, {
    if(input$inputText > 0) {
      shinyjs::disable("smit")
      variable$txt<-input$inputText
      variable$value<-PredictNgram(model,input$inputText)
      shinyjs::enable("smit")
      ##updateTabsetPanel(session, "tab",selected = "Output")
      }
    })
  
  
  output$predictedWords <- renderTable({
      
    variable$value
    })
  
  output$retText<-renderText({variable$txt})
   
})
  