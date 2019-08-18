#Linear Regression - Marketing

#http://www.learnbymarketing.com/tutorials/linear-regression-in-r/

#R linear regression uses the lm() function to create a regression model given some formula, in the form of Y~X+X2.  To look at the model, you use the summary() function.  To analyze the residuals, you pull out the $resid variable from your new model.  Residuals are the differences between the prediction and the actual results and you need to analyze these differences to find ways to improve your regression model.To do linear (simple and multiple) regression in R you need the built-in lm function. Here’s the data we will use, one year of marketing spend and company sales by month. 
url = 'http://www.learnbymarketing.com/wp-content/uploads/2014/12/data-marketing-budget-12mo.csv'
#data-marketing-budget-12mo.csv
dataset = read.csv(url, header=T,   colClasses = c("numeric", "numeric", "numeric"))
dataset

#The predictor (or independent) variable for our linear regression will be Spend (notice the capitalized S) and the dependent variable (the one we’re trying to predict) will be Sales (again, capital S).The lm function really just needs a formula (Y~X) and then a data source.  We’ll use Sales~Spend, data=dataset and we’ll call the resulting linear model “fit”.
simple.fit = lm(Sales~Spend, data=dataset)
summary(simple.fit)
multi.fit = lm(Sales~Spend+Month, data=dataset)
summary(multi.fit)

#Notice on the multi.fit line the Spend variables is accompanied by the Month variable and a plus sign (+).  The plus sign includes the Month variable in the model as a predictor (independent) variable. The summary function outputs the results of the linear regression model.

#Both models have significant models (see the F-Statistic for Regression) and the Multiple R-squared and Adjusted R-squared are both exceptionally high (keep in mind, this is a simplified example).  We also see that all of the variables are significant (as indicated by the “**”)

#Interpreting R’s Regression Output----
#Residuals: The section summarizes the residuals, the error between the prediction of the model and the actual results.  Smaller residuals are better.
#Coefficients: For each variable and the intercept, a weight is produced and that weight has other attributes like the standard error, a t-test value and significance.
#Estimate: This is the weight given to the variable.  In the simple regression case (one variable plus the intercept), for every one dollar increase in Spend, the model predicts an increase of $10.6222.
#Std. Error: Tells you how precisely was the estimate measured.  It’s really only useful for calculating the t-value.
#t-value and Pr(>[t]): The t-value is calculated by taking the coefficient divided by the Std. Error.  It is then used to test whether or not the coefficient is significantly different from zero.  If it isn’t significant, then the coefficient really isn’t adding anything to the model and could be dropped or investigated further.  Pr(>|t|) is the significance level.
#Performance Measures: Three sets of measurements are provided.
#Residual Standard Error: This is the standard deviation of the residuals.  Smaller is better.
#Multiple / Adjusted R-Square: For one variable, the distinction doesn’t really matter.  R-squared shows the amount of variance explained by the model.  Adjusted R-Square takes into account the number of variables and is most useful for multiple-regression.
#F-Statistic: The F-test checks if at least one variable’s weight is significantly different than zero.  This is a global test to help asses a model.  If the p-value is not significant (e.g. greater than 0.05) than your model is essentially not doing anything.
#http://www.learnbymarketing.com/tutorials/explaining-the-lm-summary-in-r/

layout(matrix(c(1,2,3,4),2,2,byrow=T))
plot(multi.fit$fitted, rstudent(multi.fit),     main="Multi Fit Studentized Residuals",     xlab="Predictions",ylab="Studentized Resid",     ylim=c(-2.5,2.5))
abline(h=0, lty=2)
plot(dataset$Month, multi.fit$resid,     main="Residuals by Month", xlab="Month",ylab="Residuals")
abline(h=0,lty=2)
hist(multi.fit$resid,main="Histogram of Residuals")
qqnorm(multi.fit$resid)
qqline(multi.fit$resid)


#Residuals are Normally Distributed
#Histogram of residuals does not look normally distributed. However, the QQ-Plot shows only a handful of points off of the normal line.
#We fail to reject the Jarque-Bera null hypothesis (p-value = 0.5059)
library(fBasics)
jarqueberaTest(multi.fit$resid) #Test residuals for normality
#Null Hypothesis: Skewness and Kurtosis are equal to zero
#Residuals X-squared: 1.3627 p Value: 0.5059

#Residuals are independent
#We fail to reject the Durbin-Watson test’s null hypothesis (p-value 0.3133)
library(lmtest) #dwtest
dwtest(multi.fit) #Test for independence of residuals
#Null Hypothesis: Errors are serially UNcorrelated
#Results: DW = 2.1077, p-value = 0.3133

#Residuals have constant variance
#Constant variance can be checked by looking at the “Studentized” residuals – normalized based on the standard deviation.  “Studentizing” lets you compare residuals across models.
#The Multi Fit Studentized Residuals plot shows that there aren’t any obvious outliers.  If a point is well beyond the other points in the plot, then you might want to investigate.  Based on the plot above, I think we’re okay to assume the constant variance assumption.  More data would definitely help fill in some of the gaps.