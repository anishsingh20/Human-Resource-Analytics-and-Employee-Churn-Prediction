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
