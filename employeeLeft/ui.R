#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Human Resource Analytics"),
  h4("Web app to predict whether an employee will leave the company or not based on the inputs below."),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    
    sidebarPanel(
       numericInput("satisfaction",
                   "Enter the satisfaction level-(0 to 1)",value=0.25) ,
       br(),
       numericInput("evaluation",
                    "Enter the last evaluation (0 to 1)",value=0.50),
       br(),
       numericInput("project",
                    "Enter the number of projects-(1 to 10)",value=4),
       br(),
       numericInput("worked",
                    "Enter the number of Average hours worked-(100 to 300)",value=150) ,
       br(),
       numericInput("time",
                    "Enter the time spend at company in years-(1 to 10)",value=3) ,
       br(),
       
       actionButton("btn","Submit")
       
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      textOutput("class")
    )
  )    
))
