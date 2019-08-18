#Linear Filtering on TS
?stats::filter
#https://stackoverflow.com/questions/14372880/simple-examples-of-filter-function-recursive-option-specifically
#filter as stepping through your original vector, applying the weights and summing at each step. The recursive filter is just like the convolution filter, except the weights f1, ..., fn automatically become c(1, f1, ..., fn), and at each step 1 is applied to the current value, while f1, ..., fn are applied to the last n values from the new corrected vector being created, instead of the original values. With convolution, (with default sides = 2), the weights straddle the current value, with next n/2 original values on one side, and the previous n/2 original values on the other. 

?filter
#Applies linear filtering to a univariate time series or to each series separately of a multivariate time series.
filter(x, filter, method = c("convolution", "recursive"),   sides = 2, circular = FALSE, init)

x <- 1:100
stats::filter(x, rep(1, 3))
stats::filter(x, rep(1, 3), sides = 1)
stats::filter(x, rep(1, 3), sides = 1, circular = TRUE)

stats::filter(presidents, rep(1, 3))

# Set some values for filter components
f1 <- 1; f2 <- 1; f3 <- 1;

# basic convolution filter
filter(1:5,f1,method="convolution")


#---------------
# basic convolution filter
filter(1:5,f1,method="convolution")
#[1] 1 2 3 4 5

#equivalent to:
x[1] * f1 
x[2] * f1 
x[3] * f1 
x[4] * f1 
x[5] * f1 

# convolution with 2 coefficients in filter
stats::filter(1:5,c(f1,f2),method="convolution")
#[1]  3  5  7  9 NA

#equivalent to:
x[1] * f2 + x[2] * f1
x[2] * f2 + x[3] * f1
x[3] * f2 + x[4] * f1 
x[4] * f2 + x[5] * f1 
x[5] * f2 + x[6] * f1

# convolution with 3 coefficients in filter
stats::filter(1:5,c(f1,f2,f3),method="convolution")
#[1] NA  6  9 12 NA

#equivalent to:
NA  * f3 + x[1] * f2 + x[2] * f1  #x[0] = doesn't exist/NA
x[1] * f3 + x[2] * f2 + x[3] * f1
x[2] * f3 + x[3] * f2 + x[4] * f1 
x[3] * f3 + x[4] * f2 + x[5] * f1 
x[4] * f3 + x[5] * f2 + x[6] * f1


#-----------
stats::filter(1:5, f1, method="recursive")
#[1]  1  3  6 10 15

#equivalent to:
x[1]
x[2] + f1*x[1]
x[3] + f1*x[2] + f1^2*x[1]
x[4] + f1*x[3] + f1^2*x[2] + f1^3*x[1]
x[5] + f1*x[4] + f1^2*x[3] + f1^3*x[2] + f1^4*x[1]
