#Applying Deep learning using keras in R

attach(hrm)
require(keras)
require(tensorflow)


#Generating Training and Test Data

#Converting the Data to a Matrix becasue to use Keras the data should be in form of
#array or matrix

hrm<-as.matrix(hrm)

#Setting the Column names to NULL
dimnames(hrm)<-NULL

summary(hrm)


#setting seed for reproducable results

set.seed(122)

#Generating Random Samples of data to create Training and Test Set
hrmnew<- sample(2, nrow(hrm), replace=TRUE, prob=c(0.67, 0.33))

#Splitting the data

#Training Data
#Saperating Inputs and Target Variables
hrm.train<-hrm[hrmnew==1,1:9]
hrm.trainTarget<-hrm[hrmnew==1,10]

#Test Data- Inputs and Output Saperated
hrm.test<-hrm[hrmnew==2,1:9]
hrm.testTarget<-hrm[hrmnew==2,10]


#converting Targets to one-hot encoding 
hrm.trainTarget<-to_categorical(hrm.trainTarget)
hrm.testTarget<- to_categorical(hrm.testTarget)

#Initializing a Empty Sequential Model
model<-keras_model_sequential()
