library(shiny)
library(keras)
require(tensorflow)
require(dplyr)




hrm<-read.csv('../Dataset/HR_comma_sep.csv')

hrm$Empid<-0

#random sampling
hrm$Empid = sample(x=1:nrow(hrm),size=nrow(hrm))



#loading the saved keras model
model<-load_model_hdf5('model1.h5')

shinyServer(function(input, output) {
  
  observeEvent(input$btn, {
    cat("\nEntered Inputs are:",input$satisfaction,"\n",input$evaluation,"\n",input$project,
        "\n",input$worked,"\n",input$time,"\n")
    
    print(output_class())
    
    
  })
  
  
  #take action whenever button is pressed
  output_class<-eventReactive(input$btn,{
    #taking the user's input data and converting it to a matrix to predict the class label
    inputdata<-matrix(data=c(input$satisfaction,input$evaluation,input$project,
                             input$worked,input$time),nrow=1,ncol=5)
    
    
    
    #predicted class
    pred.class<-predict_classes(model,inputdata,batch_size=32,verbose=0)
    class<-ifelse(pred.class==1,"Employee is likely to leave","Employee is not likely to leave")
    #will return the class label
    class
    
    
})
  
  
output$class<-renderText({
  
  #predicting the output class
  #calling the above function which is called when we click the submit button, which returns the 
  #predicted class label
  output_class()
  
  
})  

  
})
