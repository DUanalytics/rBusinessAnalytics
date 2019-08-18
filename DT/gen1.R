df1 = data.frame(rollno=c(1,2,3), marks=c(NA,10, 9))
df2 = data.frame(rollno=c(1,2,3), marks=c(100,10, 9))
df1
df2
for(i in 1:dim(df1)[1]){
  df1[i,is.na(df1[i,])] <- df2[i,]
}
df1
df2

df1[,is.na(df1[i,])]


# Create data set with missing values
naDF <- data.frame(a = sample(c(1,2), 100, rep=TRUE), 
                   b = sample(c(3,4), 100, rep=TRUE), 
                   fNA = sample(c(100, 200, 300, 400, NA), 100, rep=TRUE))

# Created full data set
fillDF <- data.frame(a = c(1,2,1,2), 
                     b = c(3,3,4,4),
                     fFull = c(100, 200, 300, 400))
naDF

# Fill in missing f's from naDF with values from fillDF
FilledInData <- FillIn(D1 = naDF, D2 = fillDF, 
                       Var1 = "fNA", Var2 = "fFull", KeyVar = c("a", "b"))
