#Marketing Mix
#http://rpubs.com/nihil0/mmm01

library(bayesm)
data("cheese")
?cheese
# A data frame with 5555 observations on the following 4 variables:
# $RETAILER	a list of 88 retailers
# $VOLUME	unit sales
# $DISP	percent ACV on display (a measure of advertising display activity)
# $PRICE	in U.S. dollars
summary(cheese)
dim(cheese)
str(cheese)
head(cheese)

#----
library(dplyr)

#The data consists of weekly observations of the volume of sales of sliced cheese over different retailers in the United States. Here, VOLUME corresponds to yt, DISP to xt and PRICE to  pt

adstockTransform <- function(x){
  stats::filter( 1/(1+exp(-2*x)), 0.25, method = "recursive")
}

#We are not yet looking at hierarchical models, so we focus on one retailer, say Dominick’s in Chicago. We then apply the required transformations to obtain the regression dataset.

mmm.data <- cheese %>% filter(RETAILER == 'CHICAGO - DOMINICK') %>% select(-RETAILER) %>%   mutate(log.volume = log(VOLUME), log.price = log(PRICE), adstock= adstockTransform(DISP)) %>%  select(-VOLUME, -DISP, -PRICE)

mmm.data

fit <- lm(log.volume ~ adstock + log.price, data=mmm.data)
summary(fit)


#Regression coefficients are statistically significant with R2=0.8982. as expected, we can infer that ad exposure has a net positive effect on sales volumes. Furthermore, we find that the price elasticity is estimated to be -2.96, indicating that increasing the price has a negative impact on sales.