# ui.R file for Capstone Project - Next Word Prediction


library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Data Science Specialization Capstone Project"),
    mainPanel(
            h4("Developed by Cara Johnson - 11/12/2018"),
            h2("Next Word Prediction"),
            textInput(inputId="textInput", label = h4("Enter a word, phrase, or sentence", value ="")),
            h4("Possible next word:"),
            textOutput("NextWord")
    )
  )
)
