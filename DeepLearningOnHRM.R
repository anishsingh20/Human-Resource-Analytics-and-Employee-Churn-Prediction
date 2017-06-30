#Applying Deep learning using keras in R

attach(hrm)
require(keras)

#Generating Training and Test Data

#Converting the Data to a Matrix becasue to use Keras the data should be in form of
#array or matrix
hrm<-as.matrix(hrm)

#Setting the Column names to NULL
dimnames(hrm)<-NULL