#https://analyticsprofile.com/business-analytics/how-to-optimise-digital-marketing-spend-using-linear-programming-in-r/

library(lpSolveAPI)
options(scipen = 999)
#No of Channels
model<-make.lp(ncol=4)
model
#cols -"Adword","Facebook","Email","Affiliated")
 
#maximise users
m1<-lp.control(model, sense="max", verbose="neutral")
model

#set.bounds is used to set lower and upper limits to the variables of the model.
value1 = round(c(1/250,1/200,1/150,1/100),3)
m2 <- set.objfn(model, obj=value1)
model

#set.bounds is used to set lower and upper limits to the variables of the model.
#Adwords :: B1/250 >=1000 : B1 >= 1000 * 250 = 250000
#FB :: B2/200 >=1000 : B2 >= 200000
#Email :: B3/150 >=500 : B3 >= 75000
#Affiliated :: B4/100>=500 : B4 >= 50000
m3 <- set.bounds(model, lower=c(250000,200000,75000,50000))
model

#FB and Adwords Budget Constraint:: B1+B2<=600000
m4 <- add.constraint(model, c(1,1,0,0), "<=",600000)
model

#Total Budget Constraint :: B1+B2+B3+B4 <=1000000
m5 <- add.constraint(model, c(1,1,1,1), type="<=",1000000)
model

#Minimum LTV Constraint
(value6 = round(c((1500/250-500/250),(800/200-500/200),(300/150-500/150),(100/100-500/100)),3))
      
m6 <- add.constraint(model, value, type=">=",0) 
model

rownames=c("FB & AdWord","Total budget","LTV")
colnames=c("Adword","Facebook","Email","Affiliated")
dimnames(model)=list(rownames,colnames)
model
name.lp(model,"Maximize Number Of Users")
model
write.lp(model, filename="lp_model.lp")
knitr::kable(readLines("lp_model.lp"))
solve(model)

get.variables(model)
get.objective(model)
#The maximum number of users in a year through all the channels of digital marketing is 6725.
print(model)
