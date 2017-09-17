library(shiny)
require(keras)


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
    class
    
    
    
})
  
  
output$class<-renderText({
  
  #predicting the output class
  output_class()
  
  
})  
   

  
})
