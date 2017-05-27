library(ggplot2)
library(dplyr)

hrm<-read.csv('HR_comma_sep.csv')

#Structure of the Dataset
str(hrm)
attach(hrm)

#converting left variable to factor variable 
hrm$left<-ifelse(left==1,'True','False')

hrm$left<-factor(hrm$left,levels=c("True","False"))
table(hrm$left)

#Summary Statistics of the dataset
summary(hrm)



by(hrm$satisfaction_level,hrm$salary,summary)

#Histogram

p1<-ggplot(aes(x=satisfaction_level),data=hrm) + 
  geom_histogram(color="black",fill="red",bins = 30) +
  labs(title="Satisfaction level Histogram",x='Satisfaction Level of Employees', y="Frequency")

p1


#Satisfaction level histogram facetted by sallary classes

p2 =  p1  + facet_wrap(~salary)
p2


by(satisfaction_level,left,summary)
#As peedicted the satifaction level of employees who left was lower

#Sstisfaction level vs left
ggplot(aes(x = satisfaction_level),data=hrm) + 
  geom_histogram(color='black',fill='green',bins=35) +
  xlab('Satisfaction Level') + 
  ylab("Frequency")  + 
  facet_wrap(~left)
  

#Boxplot for Satisfaction level vs left
ggplot(aes(x = left,y=satisfaction_level),data= hrm) + 
  geom_boxplot() + 
  ylab('Satisfaction Level') + 
  xlab("Employee left") + 
  labs(fill="Salary Classes")

#Boxplot for Satisfaction level vs left facetted by Salary Ranges
ggplot(aes(x = left,y=satisfaction_level),data= hrm) + 
  geom_boxplot() + 
  ylab('Satisfaction Level') + 
  xlab("Employee left") + 
  facet_wrap(~salary)

table(left , salary)

#Testing for the dependence between left and salary Ranges
#Both are categorial variables so we use Chisq Test statistic
chisq.test(left,salary)
#X-squared value is high and p-value is less i.e results are significant
#both variables are related


#Analysis on number of Projects

hrm$number_project<-factor(hrm$number_project)

ggplot(aes(x=number_project),data = hrm) + 
  geom_bar(color='black',fill='#234338') +
  xlab("Number of Projects") + 
  ylab("Frequency") + 
  labs(title="Barplot of Number of projects") 

#boxplot of number of projects vs  Average monthly hours at workplace of employees
p3=ggplot(aes(x=number_project, y = average_montly_hours),data=hrm)+
  geom_boxplot()

p4=p3+facet_wrap(~salary)
p4

p5=p3+facet_wrap(~left) + labs(title="Number projects Vs Avg monthly hours worked faceted by Left")
p5


#facetted by salary
ggplot(aes(x=number_project),data = hrm) + 
  geom_bar(color='black',fill='#834338') +
  xlab("Number of Projects") + 
  ylab("Frequency") + 
  labs(title="Barplot of Number of projects faceted by Salary") +
  facet_wrap(~salary)


#faceted by If a employee left or not
ggplot(aes(x=number_project),data = hrm) + 
  geom_bar(color='black',fill='#547398') +
  xlab("Number of Projects") + 
  ylab("Frequency") + 
  labs(title="Barplot of Number of projects faceted by Left")+  
  facet_wrap(~left)
  
