#SMOTE

hrm.best<-hrm[,1:7]
hrm.best$Work_accident<-NULL
table(hrm.best$left)
hrm.best$left<-ifelse(hrm.best$left==1,1,2)
hrm.best$left<-factor(hrm.best$left)


hrm.best.bal<-SMOTE(left~.,hrm.best,perc.over =300,perc.under = 100)
table(hrm.best.bal$left)

ggplot(aes(x=left),data=hrm.best.bal) +
  geom_bar()


#dataset now balanced

#preparing for feeding to MLP

ind<- sample(2, nrow(hrm.best.bal), replace=TRUE, prob=c(0.67, 0.33))


hrm.bal.train<-hrm.best.bal[ind==1,1:5]
hrm.bal.trainTarget<-hrm.best.bal[ind==1,6]

#Converting the input Data to a Matrix becasue to use Keras the data should be in form of
#array or matrix
hrm.bal.train<-as.matrix(hrm.bal.train)


#Test Data- Inputs and Output Saperated
hrm.bal.test<-hrm.best.bal[ind==2,1:5]
#Converting the Data to a Matrix becasue to use Keras the data should be in form of
#array or matrix
hrm.bal.test<-as.matrix(hrm.bal.test)

hrm.bal.testTarget<-hrm.best.bal[ind==2,6]


#converting Targets to one-hot encoding 
hrm.bal.trainTarget<-to_categorical(hrm.bal.trainTarget)
hrm.bal.testTarget<- to_categorical(hrm.bal.testTarget)




