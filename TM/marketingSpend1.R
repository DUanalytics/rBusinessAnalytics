#Regression analysis - Marketing

#http://www.sthda.com/english/articles/40-regression-analysis/168-multiple-linear-regression-in-r/
#We’ll use the marketing data set [datarium package], which contains the impact of the amount of money spent on three advertising medias (youtube, facebook and newspaper) on sales.
library(tidyverse)  #data manipulation and visualisation
#Multiple linear regression is an extension of simple linear regression used to predict an outcome variable (y) on the basis of multiple distinct predictor variables (x).
#With three predictor variables (x), the prediction of y is expressed by the following equation:  y = b0 + b1*x1 + b2*x2 + b3*x3
#The “b” values are called the regression weights (or beta coefficients). They measure the association between the predictor variable and the outcome. “b_j” can be interpreted as the average effect on y of a one unit increase in “x_j”, holding all other predictors fixed.
#----------------------------------------
data("marketing", package = "datarium")
head(marketing, 4)

#Building model -We want to build a model for estimating sales based on the advertising budget invested in youtube, facebook and newspaper, as follow:
#sales = b0 + b1*youtube + b2*facebook + b3*newspaper
#You can compute the model coefficients in R as follow:
  
model <- lm(sales ~ youtube + facebook + newspaper, data = marketing)
summary(model)

#Interpretation----
#The first step in interpreting the multiple regression analysis is to examine the F-statistic and the associated p-value, at the bottom of model summary.
#In our example, it can be seen that p-value of the F-statistic is < 2.2e-16, which is highly significant. This means that, at least, one of the predictor variables is significantly related to the outcome variable.
#To see which predictor variables are significant, you can examine the coefficients table, which shows the estimate of regression beta coefficients and the associated t-statitic p-values:
  
summary(model)$coefficient

#or a given the predictor, the t-statistic evaluates whether or not there is significant association between the predictor and the outcome variable, that is whether the beta coefficient of the predictor is significantly different from zero.

#It can be seen that, changing in youtube and facebook advertising budget are significantly associated to changes in sales while changes in newspaper budget is not significantly associated with sales.

#For a given predictor variable, the coefficient (b) can be interpreted as the average effect on y of a one unit increase in predictor, holding all other predictors fixed.
#For example, for a fixed amount of youtube and newspaper advertising budget, spending an additional 1 000 dollars on facebook advertising leads to an increase in sales by approximately 0.1885*1000 = 189 sale units, on average.
#The youtube coefficient suggests that for every 1000 dollars increase in youtube advertising budget, holding all other predictors constant, we can expect an increase of 0.045*1000 = 45 sales units, on average.
#We found that newspaper is not significant in the multiple regression model. This means that, for a fixed amount of youtube and newspaper advertising budget, changes in the newspaper advertising budget will not significantly affect sales units.
#As the newspaper variable is not significant, it is possible to remove it from the model:
model2  <- lm(sales ~ youtube + facebook, data = marketing)
summary(model2)
#model equation can be written as follow:
#sales = 3.5 + 0.045*youtube + 0.187*facebook.
#confidence interval of the model coefficient can be extracted as follow:
confint(model2)


#Model accuracy assessment
#As we have seen in simple linear regression, the overall quality of the model can be assessed by examining the R-squared (R2) and Residual Standard Error (RSE).
#R-squared:  In multiple linear regression, the R2 represents the correlation coefficient between the observed values of the outcome variable (y) and the fitted (i.e., predicted) values of y. For this reason, the value of R will always be positive and will range from zero to one.
#R2 represents the proportion of variance, in the outcome variable y, that may be predicted by knowing the value of the x variables. An R2 value close to 1 indicates that the model explains a large portion of the variance in the outcome variable.
#A problem with the R2, is that, it will always increase when more variables are added to the model, even if those variables are only weakly associated with the response (James et al. 2014). A solution is to adjust the R2 by taking into account the number of predictor variables.
#The adjustment in the “Adjusted R Square” value in the summary output is a correction for the number of x variables included in the prediction model.

#In our example, with youtube and facebook predictor variables, the adjusted R2 = 0.89, meaning that “89% of the variance in the measure of sales can be predicted by youtube and facebook advertising budgets.
#This model is better than the simple linear model with only youtube, which had an adjusted R2 of 0.61.

#Residual Standard Error (RSE), or sigma:
#The RSE estimate gives a measure of error of prediction. The lower the RSE, the more accurate the model (on the data in hand).
#The error rate can be estimated by dividing the RSE by the mean outcome variable:
sigma(model2)/mean(marketing$sales)



#Misc
#if you have many predictors variable in your data, you don’t necessarily need to type their name when computing the model.
#To compute multiple regression using all of the predictors in the data set, simply type this:
model3 <- lm(sales ~., data = marketing)
#If you want to perform the regression using all of the variables except one, say newspaper, type this:
model4 <- lm(sales ~. -newspaper, data = marketing)
#Alternatively, you can use the update function:
model4b <- update(model4,  ~. -newspaper)
