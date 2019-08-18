# CART Models

library(rpart)
library(rpart.plot)
library(forecast)
#install.packages("https://cran.r-project.org/bin/windows/contrib/3.3/RGtk2_2.20.31.zip", repos=NULL)
library(RGtk2)
#install.packages("rattle")
library(rattle)


# Set the working directory to folder where you have placed the Input Data
#setwd("E:\\HHges - Mkt Anyts\\Day3 Material\\Decision_Tree\\CART\\CART-Regression Tree")

msales = read.csv(file = "./data/MAdata - msales.csv",
          header = T, skip=1)
dim(msales)
# Summarize the dataset
summary(msales)
names(msales)
str(msales)
df = msales
# Look at the average Annual_Sales()
ncol(df)
for(i in 2:ncol(df))
{
  if(length(unique(df[,i])) <= 5)
  {
    Annual_Sales = aggregate(x = df$sales, by = list(df[,i]), FUN = mean)
    print(colnames(df)[i])
    print(Annual_Sales)
    print("**********************************")
  }
}
#one by one
aggregate(df$sales, by=list(df$loc), FUN=sum)

# Random Sampling
set.seed(777) 
Index = sample(x = 1:nrow(df), size = 0.7*nrow(df))
head(Index)
# Create Train dataset
dfTrain = df[Index, ]
nrow(dfTrain)
summary(dfTrain)

# Create Test dataset
dfTest = df[-Index, ]
nrow(dfTest)
summary(dfTest)


################# Modeling #################################

# Build a full model with default settings
library(rpart)
set.seed(123) # To ensure reproducibility of xerrors (cross validated errors while estimating complexity paramter for tree pruning)
Model1 = rpart(sales ~ . , data = dfTrain[,-1], 
                      method = "anova")
summary(Model1)
# Plot the Regression Tree
library(rpart.plot)
rpart.plot(Model1)
rpart.plot(Model1, type = 4,fallen.leaves = T, cex = 0.6)
title("CartFullModel")

# fancyRpartPlot() function to plot the same model
library(rattle)
fancyRpartPlot(Model1, main = "CartFullModel", cex = 0.6) 

# The following code also produces the same output, but in a windowed form
windows()
fancyRpartPlot(Model1, main = "CartFullModel", cex = 0.6)
# CP
printcp(Model1)

# This produces a plot which may help particpants to look for a model depending on R-Square values produced at various splits
rsq.rpart(Model1)

########################### Using CP to expand / Prune the tree #################################################
# Lets change rpart.control() to specify certain attributes for tree building
RpartControl = rpart.control(cp = 0.005)
set.seed(123)
Model2 = rpart(sales ~ . , data = dfTrain[,-1], 
       method = "anova", control = rpart.control(cp = 0.005))

Model2

Model2$where
trainingnodes = rownames(Model2$frame) [ Model2$where]
trainingnodes

summary(Model2)
rpart.plot(Model2, type = 4,fallen.leaves = T, cex = 0.6)

printcp(Model2)
rsq.rpart(Model2)




#Prediction 
# Predict on testset
ModelTest1 = predict(object = Model1,
            newdata = dfTest, type = "vector")
ModelTest1
# Calculate RMSE and MAPE manually
# Participants can calculate RMSE and MAPE using various available functions in R, but that may not
# communicate effectively the mathematical aspect behind the calculations

# RMSE
(Diff = ModelTest1 - dfTest$sales)

(rmse1 = sqrt(mean(Diff^2)))

(mape = mean(abs(Diff)/dfTest$sales))

library(Metrics)
Metrics::rmse(dfTest$sales, ModelTest1)
Metrics::mape(dfTest$sales, ModelTest1)


library(forecast)
(ModelAccuarcy = forecast::accuracy(ModelTest1, x = dfTest$sales))

windows()
fancyRpartPlot(Model1, main = "Final CART Regression Tree", cex = 0.6, sub = "Model 12")















Act_vs_Pred = CartFullModelPredictTest - df2Utest$sales # Differnce
Act_vs_Pred_Square = Act_vs_Pred^2 # Square
Act_vs_Pred_Square_Mean = mean(Act_vs_Pred_Square) # Mean
Act_vs_Pred_Square_Mean_SqRoot = sqrt(Act_vs_Pred_Square_Mean) # Square Root
Act_vs_Pred_Square_Mean_SqRoot

# MAPE : Mean absoluted %

Act_vs_Pred_Abs = abs(CartFullModelPredictTest - df2Utest$sales) # Absolute Differnce
Act_vs_Pred_Abs_Percent = Act_vs_Pred_Abs/df2Utest$sales # Percent Error
Act_vs_Pred_Abs_Percent_Mean = mean(Act_vs_Pred_Abs_Percent)*100 # Mean
Act_vs_Pred_Abs_Percent_Mean

# Validate RMSE and MAPE calculation with a function in R
library(forecast)
ModelAccuarcy = forecast::accuracy(f = CartFullModelPredictTest, x = df2Utest$sales)

windows()
fancyRpartPlot(model = CartModel_1, main = "Final CART Regression Tree", cex = 0.6, sub = "Model 12")
