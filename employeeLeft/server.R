library(shiny)



set.seed(122)

#Generating Random Samples of data to create Training and Test Set-
#no-1 would be having prportion of 67% and 2 will be having around 33%
hrmnew<- sample(2, nrow(hrm), replace=TRUE, prob=c(0.67, 0.33))

#checking the distribution of random numbers
table(hrmnew)

#Splitting the data

#Training Data
#Saperating Inputs and Target Variables
hrm.train<-hrm[hrmnew==1,1:5]
hrm.trainTarget<-hrm[hrmnew==1,7]

#Converting the input Data to a Matrix becasue to use Keras the data should be in form of
#array or matrix
hrm.train<-as.matrix(hrm.train)


#Test Data- Inputs and Output Saperated
hrm.test<-hrm[hrmnew==2,1:5]
#Converting the Data to a Matrix becasue to use Keras the data should be in form of
#array or matrix
hrm.test<-as.matrix(hrm.test)

hrm.testTarget<-hrm[hrmnew==2,7]
#generating a target vector for test set to generate a confusion matrix for classifier
hrm.testTarget.vector<-hrm.testTarget<-hrm[hrmnew==2,7]


#converting Targets to one-hot encoding 
hrm.trainTarget<-to_categorical(hrm.trainTarget)
hrm.testTarget<- to_categorical(hrm.testTarget)

#Initializing a Empty Sequential Model
model<-keras_model_sequential()

#Now we are trying to predict whether a Employee is going to Leave his Job or not

#Defining the architecture of a simple Multi Layer Perceptron Model

#input shape for defining the dimentions of the Training Data with inputs = No of columns
model %>% layer_dense(units = 32 , activation = 'relu' , input_shape=c(5))  %>%
  layer_dense(units=10 , activation="relu") %>%
  
  #output layer with 2 columns with prob for each class 
  #softmax for computing class probabilities
  layer_dense(units = 2 ,activation="softmax")



model %>% compile(loss = "binary_crossentropy",
                  optimizer="adam",
                  metrics="accuracy")

#training the model
#fitting the Model
history<-model %>% fit(
  hrm.train,hrm.trainTarget,epochs=500,
  batch_size= 32 , verbose = 2,
  callbacks = callback_tensorboard(log_dir = "logs/run_a"),
  validation_split=0.2)


shinyServer(function(input, output) {
   

  
})
