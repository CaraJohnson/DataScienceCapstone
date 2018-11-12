# server.R file for Capstone Project - Next Word Prediction

library(shiny)
# packages for function
library(tm)
library(ngram)
library(stylo)

# Loading the function
#setwd("C:/Users/Cara/Documents/Coursera Data Science/Class 10 - Capstone/PredictWord ShinyApp/PredictWord")
source("PredictWord.R")

# Call on PredictWord function, output the predicted NextWord text value
shinyServer(function(input, output) {
   
  output$NextWord <- renderText({
    
    PredictWord(input$textInput)
    
  })
})
