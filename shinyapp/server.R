#setwd("~/Desktop/coursera-capstone/shiny-app")
library(shiny)
source("prediction.r")

shinyServer(function(input, output) {
    dataInput <- reactive(predict(input$entry))
#   output$top1 <- renderText({
#     #dataInput()[1] #  
#     ifelse(is.na(dataInput()[1])==F,paste("First prediction:", dataInput()[1]),"First prediction: the")
#   })
#   output$top2 <- renderText({
#     ifelse(is.na(dataInput()[2])==F,paste("Second prediction:", dataInput()[2]),"Second prediction: and")
#   })
#   output$top3 <- renderText({
#     ifelse(is.na(dataInput()[3])==F,paste("Third prediction:", dataInput()[3]),"Third prediction: or")
#   })
  output$text <- renderText({
    paste(ifelse(is.na(dataInput()[1])==F,dataInput()[1],"the"),
          ifelse(is.na(dataInput()[2])==F,dataInput()[2],"and"),
          ifelse(is.na(dataInput()[3])==F,dataInput()[3],"i"),sep = " | ")
  })
  output$sent <- renderText({
    input$entry
  })
  
})