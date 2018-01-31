library(shiny)
library(shinyjs)

shinyUI(
  fluidPage(shinyjs::useShinyjs(),
            titlePanel("Text Prediction Application"),
            sidebarPanel(
              textInput("inputText", h4("Text Input")),
              ##actionButton("smit", "Submit"),
              actionButton("smit", "Submit",  
                           style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
              br(),
              h5("Please enter text to predict and press Submit"),
              h5("Please click on Output tab to view the predicted word")),
            mainPanel(
              tabsetPanel(type = "tabs", 
                          tabPanel(h4("Overview"), 
                                   h4("Introduction"),
                                   p("This Text Prediction Application is a submission for the Coursera Data Science Specialization in collaboration with the Johns Hopkins University, Data Science Capstone module."),
                                   p("The SwiftKey Corpus provided as part of the course / assignment has been used for the prediction."),
                                   h4("Text Prediction Model"),
                                   p("The model used for this text prediction is the Stupid Back Off Which each word is modeled as transitional state with probability."),
                                   h4("Application Instruction"),
                                   p("1. Type some text into the text box under the \"Text input\" heading on the left pane"),
                                   p("2. \"Text input\" please restrict to  english language."),
                                   p("3. Click submit button. The predicted values would come on \"Output\" tab in the main panel"),
                                   br(),
                                   h4("Model and Prediction - Key points"),
                                   p("5% of the Swiftkey has been picked for preparation of model. The model includes uni, bi and tri grams"),
                                   p("The model has been prepared by preprocessing  - exclusion of numeric, limiting to en_US set & stripping whitespace ")),
                         tabPanel(h4("Output"),
                                   h4("You have entered,"),
                                   span(h4(textOutput("retText")), style="font-weight: bold; color: #4582ec"),
                                   br(),
                                   h4("The predicted words are as follows. The words, probability and matched ngram (1/2/3) provided below :"),
                                   span(h4(tableOutput("predictedWords")), style="font-weight: bold; color: #4582ec")
                          ))
            ))
)

