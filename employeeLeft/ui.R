
library(shiny)
require(tensorflow)
library(keras)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Human Resource Analytics"),
  h4("Web app to predict whether an employee will leave the company or not based on the inputs below."),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    
    sidebarPanel(
       sliderInput("satisfaction",
                   "Enter the satisfaction level:(0 to 1)",value=0.25,min=0,max=1,step=0.01) ,
       br(),
       sliderInput("evaluation",
                    "Enter the last evaluation:(0 to 1)",value=0.50,min=0,max=1,step=0.01),
       br(),
       sliderInput("project",
                    "Enter the number of projects done by employee:(1 to 10)",value=4,min=1,max=10,step=1),
       br(),
       numericInput("worked",
                    "Enter the number of Average hours worked:(100 to 300)",value=150) ,
       br(),
       numericInput("time",
                    "Enter the time spend at company in years:(1 to 10)",value=3) ,
       br(),
       
       actionButton("btn","Submit")
       
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h3("The predicted class label is"),
      hr(),
      h3(textOutput("class"),style="color:blue")
    )
  )    
))
