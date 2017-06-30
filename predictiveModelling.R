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
MSE
stderr<-sqrt(MSE)
stderr
#StandardError = 23 %


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

modKnn<-knn.reg(select(train,average_montly_hours,time_spend_company,number_project,left
                       ),select(testdf,average_montly_hours,time_spend_company,number_project,left
                                ),train[,1],k=10,algorithm=c("kd_tree"))

#Predictions Data frame with Actual Target values and Predicted Target values
# for Test Data set
preddf<-data.frame(Actual =testdf$satisfaction_level,
                   Predicted = modKnn$pred)

preddf<-preddf %>% mutate(Residuals=abs(Actual-Predicted))

#Redisudals Distribution
summary(preddf$Residuals)

filter(preddf,Residuals==0)
#17 observations with exact prediction i.e Residual value=0

ggplot(aes(x = Residuals ),data  = preddf)  + 
  geom_histogram(color='black',fill='green',bins=40,binwidth = 0.01) + 
  scale_x_continuous(breaks=seq(0,0.7,0.05)) + 
  labs(x = "Residual Values i.e (Actual Target - Predicted Target)",y = "Counts",
       title = "Plot of Residuals Values for K-NN with 10 as K") + 
  coord_cartesian(ylim=c(0,400)) + 
  scale_y_continuous(breaks = seq(0,400,50))

#Mean squared Error value for K-NN model

RSS<-sum(preddf$Residual**2) #Residuam Sum of Squares-deviance value
MSEknn<-(RSS/nrow(preddf)) # Mean Squared Error
MSEknn
#Standardized Errors
StandardError<-sqrt(MSEknn)
StandardError
#Standard error of 20%
#lesser Error then Linear Regression Model
#Hence K-NN gave better results on the Test Set
#Prediction Accuracy = 7%





#Without the Test SET - It performs the Leave One Out Cross validation
modKnn1<-knn.reg(select(train,average_montly_hours,time_spend_company,number_project,left),test = NULL,
train[,1],k=10)

#The residuals value
modKnn1$residuals
summary(modKnn1$residuals)




#The Deviance value i.e RSS(Residual Sum of Squares)
modKnn1$PRESS

#R squared value which explains the amount of variance explained
modKnn1$R2Pred








