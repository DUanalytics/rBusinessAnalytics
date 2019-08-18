#Ad Stock Rate
#https://github.com/AnalyticsArtist/BlogFiles/blob/master/Advertising%20Adstock%20Transformation.R

# Define Adstock Rate
adstock_rate <- 0.50
max_memory   <- 2

# Create Data
advertising = c(117.913, 120.112, 125.828, 115.354, 177.090, 141.647, 137.892,   0.000,   0.000,   0.000,   0.000, 0.000,   0.000,   0.000,   0.000,   0.000,   0.000, 158.511, 109.385,  91.084,  79.253, 102.706, 78.494, 135.114, 114.549,  87.337, 107.829, 125.020,  82.956,  60.813,  83.149,   0.000,   0.000, 0.000,   0.000,   0.000,   0.000, 129.515, 105.486, 111.494, 107.099,   0.000,   0.000,   0.000,0.000,   0.000,   0.000,   0.000,   0.000,   0.000,   0.000,   0.000)

advertising
# Calculate Advertising Adstock
# Credit: http://stackoverflow.com/questions/14372880/simple-examples-of-filter-function-recursive-option-specifically

# Calculate Advertising Adstock
(learn_rates <- rep(adstock_rate, max_memory+1) ^ c(0:max_memory))
#highest, half, qtr, 0

(adstocked_advertising <- stats::filter(c(rep(0, max_memory), advertising), learn_rates, method="convolution"))
length(adstocked_advertising)
length(advertising)

cbind(advertising[1:10], adstocked_advertising[1:10])
advertising[1] * 0.5 + advertising[2] 
adstocked_advertising <- adstocked_advertising[!is.na(adstocked_advertising)]

# Alternative Method Using Loops Proposed by Linh Tran
adstocked_advertising = numeric(length(advertising))
adstocked_advertising[1] = advertising[1]
for(i in 2:length(advertising)){
  adstocked_advertising[i] = advertising[i] + adstock_rate * adstocked_advertising[i-1]
}

# Graph Data
plot(seq(1,length(advertising)), advertising, type="h", 
     xlab="Time (Usually in Weeks)", ylab="Advertising", 
     ylim=c(0, max(c(advertising, adstocked_advertising))), 
     frame.plot=FALSE)
lines(adstocked_advertising)
