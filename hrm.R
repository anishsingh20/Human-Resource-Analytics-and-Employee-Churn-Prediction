library(ggplot2)
library(dplyr)
library(tidyr)

hrm<-read.csv('HR_comma_sep.csv')

#Structure of the Dataset
str(hrm)
attach(hrm)

#converting left variable to factor variable 
hrm$left<-ifelse(hrm$left==1,'True','False')

hrm$left<-factor(hrm$left,labels=c("True","False"))
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
p3

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


#Analysis of average monthly hours
summary(average_montly_hours)
#Somewhat Normally distributed

ggplot(aes(x= average_montly_hours),data = hrm)+
  geom_histogram(color='black',fill="yellow",bins = 30)


cor.test(satisfaction_level,average_montly_hours)
#No relation between both the variables

cor.test(time_spend_company,average_montly_hours)

ggplot(aes(y = average_montly_hours, x = as.factor(time_spend_company)),data=hrm)+
  geom_boxplot() + 
  xlab("No of years a Employee has worked in The company") + 
  ylab("Average Montly hours worked")


ggplot(aes(x = average_montly_hours),data =hrm ) + 
  geom_histogram(color='black',fill='#443332',bins = 30) + 
  facet_wrap(~left)

by(average_montly_hours , hrm$left ,summary)

ggplot(aes(y = average_montly_hours, x = hrm$left),data=hrm)+
  geom_boxplot() + 
  xlab("Employee left or not") + 
  ylab("Average Montly hours worked")

# A thing to notice is that employee who left the company worked more hours than those
# who did not leave.


#Anslysis for variable Time spend at company

table(hrm$time_spend_company)

ggplot(aes(x = factor(time_spend_company)),data = hrm) + 
  geom_bar(fill = 'purple',color='black') + 
  xlab("Time spend at compnay in years") + 
  ylab("Frequency")+
  labs(title = "Barplot of Time spend at Company")


#Time spend at company vs Left or not

ggplot(aes(x = factor(time_spend_company)),data = hrm) + 
  geom_bar(fill = 'grey',color='black') + 
  xlab("Time spend at compnay in years") + 
  ylab("Frequency")+
  labs(title = "Barplot of Time spend at Company faceted by Left")  +
  facet_wrap(~left)

by(time_spend_company , hrm$left , summary)

ggplot(aes(x = left , y
           = time_spend_company),data = hrm)+
  geom_boxplot()




#Time spend vs Satisfaction level of employees as they worked

by(satisfaction_level,factor(time_spend_company),summary)

cor.test(satisfaction_level,time_spend_company)
#both have a negetive correlation

#plots vs Time spend and Satisfaction level
ggplot(aes(x=factor(time_spend_company),y=satisfaction_level),data=hrm)+
  geom_boxplot() + 
  xlab("Time spend at company in years")+ 
  ylab("Satisfaction level")


#Time spend at compnay vs Promotion in last 5 years

table(Promotion=promotion_last_5years,Time_Spend=factor(time_spend_company))
#Employees who have had promotion are very less


ggplot(aes(x = factor(time_spend_company)),data = hrm)+
  geom_bar()+
  facet_wrap(~promotion_last_5years) + 
  scale_y_continuous(limits=c(0,4000),breaks=seq(0,4000,500))


#Time spend vs Department of Work

by(time_spend_company,sales,summary)



ggplot(aes(x = sales,y = time_spend_company),data = hrm) + 
  geom_boxplot() + 
  coord_flip()




#Analysis of Department of Work 


ggplot(aes(x =sales),data = hrm ) +
  geom_bar()  +
  xlab('Department') + 
  ylab('Counts') +
  coord_flip() 
#highest count is for Sales department then Technical  and least for 
#Management


#Department vs sallary

table(Dept = sales , Salary  = salary)


ggplot(aes(x =sales),data = hrm ) +
  geom_bar(aes(fill=salary))  +
  xlab('Department') + 
  ylab('Counts') +
  coord_flip()


ggplot(aes(x =sales),data = hrm ) +
  geom_bar(aes(fill=salary))  +
  xlab('Department') + 
  ylab('Counts') +
  labs(title = "Department and their count facetted by Salary ranges")+
  facet_wrap(~salary) + 
  coord_flip()

chisq.test(sales,salary)
#Department and Salary is dependent on each other . 


#Department vs which employee left

ggplot(aes(x = sales),data =hrm)  +
  geom_bar(aes(fill=left))

#finding proportions
prop.table(table(Dept = sales , left = left))*100


deptdf<-hrm %>% group_by(sales,left) %>% 
      summarise(count=n())

#making a data frame of Departments and the count of workers who left or not
deptdf<-spread(deptdf,left,count)

deptdf<-transform(deptdf,Perleft=(True/(True+False))*100 , PerWork=(False/(True+False))*100)
deptdf

chisq.test(sales , hrm$left)
#Hence both Department and left variables are realted


#Plot of Department vs Percentage of Employees who left
ggplot(aes(x=sales, y = Perleft),data = deptdf) + 
  geom_col(fill='#53ab85',color='#2f3f52') + 
  coord_flip()+
  xlab("Department") + 
  ylab("Percentage of Employees who left") + 
  labs(title="Plot of Department vs Percentage of Employee left")
#highest percentage of employees belonged to HR dept then accounting
# least for management dept who left
  
#Plot of Department vs Percentage of People Working
ggplot(aes(x=sales, y = PerWork),data = deptdf) + 
  geom_col(fill='#b6a2bf',color='#2f3f52') + 
  coord_flip()+
  xlab("Department") + 
  ylab("Percentage of Employees who Still Work") + 
  labs(title="Plot of Department vs Percentage of Employees Working")




#Department vs Satisfaction level

by(satisfaction_level,sales,summary)
#highest mean satisfaction for R&D and Management Dept

ggplot(aes(x = sales, y = satisfaction_level),data = hrm)+
  geom_boxplot() + 
  scale_y_sqrt()+
  xlab('Department') + 
  ylab('Satisfaction Level"') + 
  coord_flip()
#Highest Median Satisfaction for IT dept, R&D and , Management
#Least Median Satifaction level for HR and Accounting


#Analysis of Department vs Time spend at company

by(time_spend_company,sales,summary)

#Maximum  Mean Time spent by Managaement Employees

ggplot(aes(x = sales,y = time_spend_company),data = hrm) + 
  geom_boxplot() +
  xlab('Department') + 
  ylab("Time Spend at Company") + 
  coord_flip()


ggplot(aes(x = time_spend_company),data = hrm) + 
  geom_bar() +
   xlab("Time Spend at Company splitted by Department") + 
facet_wrap(~sales)
#In every department there is very less count of Employees
# working for over 5 years



#Department vs Time average monthly hours

by(average_montly_hours,sales , summary)


#Highest average working time for IT and Technical departments

ggplot(aes(x = sales , y = average_montly_hours),data =hrm) +
  geom_boxplot() + 
  xlab('Department of Work') + 
  ylab('Average Monthly Hourse of Work') + 
  coord_flip()

#Highest Median working time of Management department


#Department vs Work Accident

table(Work_accident)

table(sales,Work_accident)

ggplot(aes(x = sales),data = hrm) +
  geom_bar(aes(fill=factor(Work_accident))) + 
  coord_flip() + 
  labs(x = "Department",y ="Frequency", fill="Work Accidents" )


hrm$Work_accident<-factor(Work_accident,labels = c('False','True'))

accidentdf<-hrm %>% group_by(sales,Work_accident) %>%
  summarise(Count= n())

accidentdf<-spread(accidentdf,Work_accident,Count)

accidentdf<-transform(accidentdf,TrueRate=(True/(True+False))*100,FalseRate=(False/(True+False))*100)

#Plot of Departent vs Accidental Rate 
ggplot(aes(x = sales,y = TrueRate),data = accidentdf) + 
  geom_col(color='black',fill="#b266b2") + 
  xlab('Department') + 
  ylab('Accident Percentage') + 
  coord_flip()
#Hishest number of accidents in R and D department

ggplot(aes(x = sales,y = FalseRate),data = accidentdf) + 
  geom_col(color='black',fill="#d8b2d8") + 
  xlab('Department') + 
  ylab('No Accident Percentage') + 
  coord_flip()
#Maximum for HR department



#Department vs number_projects made

by(number_project,sales,summary)

ggplot(aes(x = sales, y =factor(number_project)),data = hrm) +
  geom_count() +
  xlab("Department") + 
  ylab("Number of projects") + 
  labs(title = "Plot of Department vs Number of projects and their count ")

   

#Department vs Promotion in last 5 years

table(sales , hrm$promotion_last_5years)

#TRansforming Promotion Column to Factor with True and False values
hrm$promotion_last_5years<-factor(promotion_last_5years,labels=c('False',"True"))

#Generating a promotions Data frame
promotiondf<-hrm %>% group_by(sales,promotion_last_5years) %>%
  summarise(Count = n())

#Sprading the data'
promotiondf<-promotiondf %>% spread(promotion_last_5years,Count)

#changing the names
names(promotiondf)<-c("Department","Nopromotion","Promotion")

#replacing NA valuw with 0
promotiondf[is.na(promotiondf)]<-0

promotiondf<-promotiondf %>% transform(PerPromotion=(Promotion/(Promotion+Nopromotion))*100,
                                    PerNopromotion = (Nopromotion/(Promotion + Nopromotion))*100)

#Most number of Promotions done in Management and Marketing Departments
#Least in IT , Technical and Product Manager

#Plotting Department vs Promotion Percentage
ggplot(aes(x =Department, y =PerPromotion ),data = promotiondf) + 
  geom_col(color='black',fill = '#453322') + 
  xlab("Department") + 
  ylab("Percentage of employees Promoted in last 5 years") + 
  coord_flip()
#Highest in Management Department

#Plotting Department vs No Promotion Percentage
ggplot(aes(x =Department, y =PerNopromotion ),data = promotiondf) + 
  geom_col(color="white",fill = "#665443") + 
  xlab("Department") + 
  ylab("Percentage of employees Not Promoted in last 5 years") + 
  coord_flip()
#No promotion in IT and Product Management Dept



  















