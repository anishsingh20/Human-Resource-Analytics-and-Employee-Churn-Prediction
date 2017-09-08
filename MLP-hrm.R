#Applying Deep learning using keras in R

attach(hrm)
require(keras)
require(tensorflow)


#PHASE1-------------------------------DATA PRE-PROCESSING-------------------------------------

#Generating Training and Test Data
summary(hrm)

#Data preprocessing needed to make the data suitable ,clean,transform,nromalize it so as to
#feed it to the Model and get accurate results


#setting seed for reproducable results

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


#converting Targets to one-hot encoding 
hrm.trainTarget<-to_categorical(hrm.trainTarget)
hrm.testTarget<- to_categorical(hrm.testTarget)




#normalizing the variables

#making a function which takes a vector x and uses max-min normalization to normalize it
#to a range-0 to 1
normal<-function(x)
{
  norm_x<-(x-min(x))/(max(x)-min(x))
  norm_x
}

hrm.train[,3:5]<-normal(hrm.train[,3:5])











#_-------------------------------------------------------------------------------------------

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
  
#To check the architecture of the MLP model
summary(model)
get_layer(model,index=3)# to get the layers used
get_config(model) # to check the configuration
model$layers
model$input_layers
model$output # to list the output tensors


#Compiling and fitting the model

model %>% compile(loss = "binary_crossentropy",
                  optimizer="adam",
                  metrics="accuracy")

#fitting the Model

history<-model %>% fit(
              hrm.train,hrm.trainTarget,epochs=500,
              batch_size= 32 , verbose = 2,
              callbacks = callback_tensorboard(log_dir = "logs/run_a"),
              validation_split=0.2)
  
  
tensorboard() #for visualizing the Model's Metrics

#Visualizing the Model's metrics

plot(history$metrics$loss,type="l",col="red",xlab="Epochs",ylab="Error")
lines(history$metrics$val_loss , type="l",col="purple")
legend("topright",c("Train","Test"),lty=c(1,1),col=c("red","purple"))

#Plotting Accuracy
plot(history$metrics$acc,type="l",col="blue",xlab="Epochs",ylab="Accuracy")
lines(history$metrics$val_acc,type="l",col="green")
legend("bottomright",c("train","Test"),col=c("blue","green"),lty=c(1,1))

#Evaluating on Test Data
score<-model %>% evaluate(hrm.test ,hrm.testTarget , batch_size = 128,verbose=1)

print(score) #An accuracy of 95 % on Test data with Loss of 14%

#saving the Model
save_model_hdf5(model, "model1.h5")

model<-load_model_hdf5('model1.h5')



#----------------------------------------------


#Generating sample Training labels
trainY1<-sample(c(1,0),9980,replace = T,prob=c(0.70,0.30))
trainY2<-sample(c(0,1),9980,replace = T,prob=c(0.65,.45))

sample.trainTarget<-cbind(trainY1,trainY2)
colnames(sample.trainTarget)<-c("V1","V2")


model2<-keras_model_sequential()

model2 %>% layer_dense(units = 20 , activation = 'relu' , input_shape=c(5))  %>%
  layer_dense(units=10 , activation="relu") %>%
  
  #output layer with 2 columns with prob for each class 
  #softmax for computing class probabilities
  layer_dense(units = 2 ,activation="softmax")

summary(model2)


#compiling the MLP model -using Stochastic gradient descent as optimization strategy
model2 %>% compile(loss="binary_crossentropy",optimizer="adam",
                   metrics="accuracy")


history2<- model2 %>% fit(hrm.train,hrm.trainTarget,epochs=300,
                          batch_size= 32 , verbose = 2,
                          callbacks = callback_tensorboard(log_dir = "logs/run_e"))
tensorboard()

#generating random Test Target data
score<-model2 %>% evaluate(hrm.test ,hrm.testTarget , batch_size = 128,verbose=1)
sample_test1<-sample(c(0,1),nrow(hrm.testTarget),replace = T,prob=c(0.60,0.40))
sample_test2<-sample(c(1,0),nrow(hrm.testTarget),replace = T,prob=c(0.60,0.40))
sample_randtest<-cbind(sample_test2,sample_test1)
score2<-model2 %>% evaluate(hrm.test ,sample_randtest , batch_size = 128,verbose=1)

#accuracy of 93% on test set and loss of 19%

#saving the second model
save_model_hdf5(model2,"model2.h5")
plot(history2)


#----------------------------------

#Trial 3

model3<-keras_model_sequential()


model3 %>% layer_dense(units = 25 , activation = 'relu' , input_shape=c(5))  %>%
  layer_dense(units=12 , activation="relu") %>%
  
  #output layer with 2 columns with prob for each class 
  #softmax for computing class probabilities
  layer_dense(units = 2 ,activation="softmax")

summary(model3)

#compiling the MLP model -using Stochastic gradient descent as optimization strategy
model3 %>% compile(loss="binary_crossentropy",optimizer="rmsprop",
                   metrics="accuracy")


history3<- model3 %>% fit(hrm.train,hrm.trainTarget,epochs=350,
                          batch_size= 32 , verbose = 2,
                          callbacks = callback_tensorboard(log_dir = "logs/run_f"),
                          validation_split=0.1)



