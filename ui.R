#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

## Import libraries 
#########################################################################################################
library(shiny)
library(leaflet)  # renderLeaflet function
library(shinyjs)


## UI Rendering
#########################################################################################################
shinyUI(fluidPage(
  useShinyjs(),
  
  # Application title
  titlePanel("Do Hult Students Love Cultural Diversity?"),
  h6("Created by Team 6 on February, 2020"),
  hr(),
  fillPage(
    tabsetPanel(
      tabPanel(
        "Overview",
        fluidPage(
          h3("What did they answer?"),
          plotOutput(outputId = "yes_no_plot", height = "100px"),
          h5(textOutput(outputId = "missing")), 
          hr(),
          h3("Where do they come from?"),
          leafletOutput(outputId = "map"),
          actionButton(inputId = "selected_all", "Selected all countries", value = TRUE)
        ),
      ),
      tabPanel(
        "Benefits & Difficulties",
        h3("What are the benefits and difficulties of having a diverse environment"),
        sidebarLayout(
          sidebarPanel(
            selectInput('selected_pros_cons', h4('Difficulties and Benefits'), choices = c('Difficulties','Benefits'))
          ),
          mainPanel(
            plotOutput('pros_cons')
          )
        )
      ),
      tabPanel(
        "Sentiments",
        fluidPage(
          h3("How do you feel when meeting new people?"),
          sidebarPanel(id="sidebar", width = 3, tags$style("#sidebar { visibility: hidden; }")),
          mainPanel( imageOutput("sentiment") )
        )
      ),
      tabPanel(
        "Culture Differences",
        h3("Did you find any certain differences between other cultures? Can you give us examples?"),
        sidebarLayout(
          sidebarPanel(
            selectInput('yesno', h4('Include the people who say:'), choices = c('All', 'Yes', 'No')),
            width = 3
          ),
          mainPanel(
            plotOutput('ngram'),
            width = 9
          )
        )
      ),
      tabPanel(
        "Naive Bayes Model",
        h3("Can We Predict If Studentes Think Being in Multicultural Environment is Good?"),
        sidebarLayout(
          sidebarPanel(
            h3("Naive Bayes Classification Model"),
            h5('Assume that the missing data is "NO" outcome.'),
            verbatimTextOutput(outputId = "nbm"),
            h3("Model Prediction"),
            verbatimTextOutput(outputId = "predict"),
            width = 6
          ),
          mainPanel(
            sliderInput(inputId = "training_number", h4("How many data used in training?"), 21, 28, value = 25),
            hr(),
            h3("Model Performance"),
            plotOutput(outputId = "predict_plot"),
            width = 6
          )
        )
      )
    )
  )
))
