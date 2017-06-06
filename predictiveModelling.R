attach(hrm)

#Doing Predictive Analysitcs on the Dataset

#Predicting the Satisfaction Level of the Employees

#1) Using Linear Regression - Backward  Selection technique
mod1<-lm(satisfaction_level ~ .,data =hrm)
summary(mod1)

#Now using the important features in the new model
mod2<-lm(satisfaction_level ~ number_project+ average_montly_hours + time_spend_company,data =hrm)
summary(mod2)

set.seed(1000)

#training Data
train<-hrm[1:9999,]

mod3<-lm(satisfaction_level ~ number_project+ average_montly_hours + 
           time_spend_company + left,data =train )

summary(mod3)
AIC(mod3)

#BIC puts a penalty on Larger Models
BIC(mod3)
#plotting the Model 3

par(mfrow=c(2,2))
plot(mod3)

#Checking the Predictive accuracy of the model
testdf<-hrm[10000:14000,]#Test Data frame

#Predictions on Test set
pred1=data.frame(predict(mod3,newdata=testdf,type = 'response'))

#The Data frame consisting the actual values and the Predicted values
accuracy<-data.frame(select(testdf,satisfaction_level),pred1)
names(accuracy)<-c("Actual","Predicted")
accuracy<-accuracy %>% mutate(Residuals=abs(Actual-Predicted))

summary(accuracy$Residuals)

#Mean Squared Error on Test data
MSE = sum(accuracy$Residuals**2)/nrow(accuracy)
MSE*100
#Error = 5.1 %
#Predictive accuracy of The model is around 95%

#Plotting the Residuals Value for checking the Distribution of Residuals
#Residuals  should be Normally Distributed in Linear Regression
ggplot(aes(x = Residuals),data=accuracy) + 
  geom_histogram(color='black',fill='red',bins=40) + 
  labs(x = "Residuals Of Model 3",title= "Histogram of Residuals",y = 'Count') + 
  scale_x_continuous(limits =c(0,0.6),breaks=seq(0,0.6,0.05))


#Scatter Plot of  Actual vs Predicted values
ggplot(aes(x = Predicted,y = Actual),data = accuracy) + 
  geom_jitter() +
  labs(title=("Scatter plot of Predicted and Actual Values"))
#Actually the Actual and Predicted values have very slight correlation
cor.test(accuracy$Actual,accuracy$Predicted)






#Using K-NN to predict the Satisfaction Level

#Requiring the package which has K-NN regression
require(FNN)
?knn.reg

modKnn<-knn.reg(select(train,average_montly_hours,time_spend_company,number_project
                       ),select(testdf,average_montly_hours,time_spend_company,number_project
                                ),train[,1],k=10,algorithm=c("kd_tree"))

summary(modKnn)



#Without the Test SET - It performs the Leave One Out Cross validation
modKnn1<-knn.reg(select(train,average_montly_hours,time_spend_company,number_project,left),test = NULL,
train[,1],k=10,algorithm=c("kd_tree"))

#The residuals value
modKnn1$residuals

#The Deviance value i.e RSS(Residual Sum of Squares)
modKnn1$PRESS

#R squared value which explains the amount of variance explained
modKnn1$R2Pred

