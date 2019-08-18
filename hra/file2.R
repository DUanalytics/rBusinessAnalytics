#file 2

#https://rpubs.com/Guryash/hranalytics
#data set ?
hr.df<- read.csv(paste("HR_comma_sep.csv", sep = ""))
fullmodel <- lm(satisfaction_level ~ salary + average_montly_hours + number_project + time_spend_company +  promotion_last_5years + last_evaluation + Work_accident + left,data=hr.df )

summary(fullmodel)


#revised Model
revisedmodel<- lm(satisfaction_level ~ average_montly_hours + number_project + time_spend_company + last_evaluation,data=hr.df)
summary(revisedmodel)



#3.1 Read the Data Set HR
hr.df<- read.csv(paste("HR_comma_sep.csv", sep = ""))
#3.2 DataSet Details
dim(hr.df)
## [1] 14999    10
#3.3 Describe DataSet
#Create a descriptive statistics (min, max, median etc) of each variable.

library(psych)
describe(hr.df)


#3.4 Create a Contigency Table for each variable in dataset.
mytable<-with(hr.df,table(salary))
mytable


mytable1<-with(hr.df,table(satisfaction_level))
mytable1

mytable3<-with(hr.df,table(number_project))
mytable3


mytable4<-with(hr.df,table(average_montly_hours))
mytable4

mytable5<-with(hr.df,table(time_spend_company))
mytable5

mytable6<-with(hr.df,table(Work_accident))
mytable6

mytable7<-with(hr.df,table(left))
mytable7

mytable8<-with(hr.df,table(promotion_last_5years))
mytable8

mytable9<-with(hr.df,table(sales))
mytable9

mytable10<-with(hr.df,table(salary))
mytable10

#3.5 Create two-way contingency tables for the categorical variables in your dataset.
mytable11<-xtabs(~number_project+time_spend_company,data=hr.df)
mytable11

mytable12<-xtabs(~satisfaction_level+salary,data=hr.df)
mytable12

mytable14<-xtabs(~average_montly_hours+salary,data=hr.df)
mytable14

mytable15<-xtabs(~Work_accident+salary,data=hr.df)
mytable15

mytable16<-xtabs(~promotion_last_5years+salary,data=hr.df)
mytable16

mytable17<-xtabs(~number_project+time_spend_company,data=hr.df)
mytable17

#3.6 Boxplot Creation
boxplot(satisfaction_level ~salary,data=hr.df, horizontal=TRUE, ylab="Salary Level", xlab="Satisfaction level", las=1,main="Analysis of Salary of Employee on the basis of their satisfaction level", col=c("red","blue","green") )

boxplot(satisfaction_level ~left, data=hr.df, horizontal=TRUE, ylab="Left", xlab="Satisfaction level", las=1,    main="Analysis of of Employee Left on the basis of their satisfaction level",col=c("Yellow","Orange"))

boxplot(number_project~left,data=hr.df, horizontal=TRUE, ylab="Left", xlab="No of Projects", las=1,    main="Analysis of of Employee Left on the basis of their Number of Projects", col=c("Red","Magenta"))

boxplot(average_montly_hours ~left, data=hr.df,horizontal=TRUE,  ylab="Left", xlab="Average Monthly Hours", las=1,main="Analysis of of Employee Left on the basis of their Average Monthly Hours",  col=c("Yellow","Orange"))

boxplot(Work_accident~left,data=hr.df, horizontal=TRUE,  ylab="Left", xlab="Work Accident", las=1,  main="Analysis of of Employee Left on the basis of their Work Accident",col=c("Yellow","Orange"))

boxplot(last_evaluation ~left,data=hr.df, horizontal=TRUE,ylab="Left", xlab="Last Evaluation", las=1, main="Analysis of of Employee Left on the basis of their Last Evaluation",col=c("Yellow","Orange"))


#3.7 Histogram for the variables
hist(hr.df$satisfaction_level, main=" Variation in Satisfaction Level ", xlab="Satisfaction Level",breaks=10,ylab="Frequency", col="red")

hist(hr.df$last_evaluation, main=" Variation in Last Evaluation ", xlab="Last Evaluation",breaks=10,ylab="Frequency", col="blue")

hist(hr.df$satisfaction_level, main=" Variation in Time Spent in the Company ", xlab="Time Spent in the Company",breaks=10,ylab="Frequency", col="yellow")

hist(hr.df$average_montly_hours, main=" Variation in Average Monthly Hours ", xlab="Average Monthly Hours",breaks=10,ylab="Frequency", col="red")

plot(y=hr.df$salary, x=hr.df$sales,    col="orange", main="Relationship Btw salary and sales",   ylab="Salary", xlab="Sales")

plot(y=hr.df$average_montly_hours, x=hr.df$sales,
     col="Violet",
     main="Relationship Btw Average Monthly Hours and sales",
     ylab="Average Monthly Hours", xlab="Sales")


library(corrplot)
## corrplot 0.84 loaded
correlationMatrix <- cor(hr.df[,c(1:8)])
corrplot(correlationMatrix, method="circle")

cor(hr.df[ ,c(1,2,3,4,5,6,7,8)])

#3.10 Visualize your correlation matrix using corrgram.
library(corrgram)
corrgram(hr.df, lower.panel = panel.shade, upper.panel = panel.pie, text.panel = panel.txt, main = "Corrgram of all  variables")

#3.11 Create a scatter plot matrix for your data set.
library(car)

scatterplotMatrix(formula = ~left + satisfaction_level + time_spend_company + Work_accident +average_montly_hours , data = hr.df,smooth= TRUE)

3.12 Run a suitable test to check your hypothesis for your suitable assumptions.
cor.test(hr.df$left,hr.df$satisfaction_level)

cor.test(hr.df$left,hr.df$time_spend_company)

cor.test(hr.df$left,hr.df$average_montly_hours)
cor.test(hr.df$left,hr.df$last_evaluation)
cor.test(hr.df$left,hr.df$number_project)
3.13 T- Test Hypothesis
t.test(hr.df$satisfaction_level~hr.df$left)
t.test(hr.df$time_spend_company~hr.df$left)
t.test(hr.df$average_montly_hours~hr.df$left)
t.test(hr.df$last_evaluation~hr.df$left)

