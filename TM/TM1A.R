#TM solution

library(dplyr)
library(data.table)
library(tidyr)
library(ggplot2)
library(IDPmisc)
library(data.table)

rm(list = ls())


####Defining Adstock Function########################################
adstock<-function(x,rate=0){
  return(as.numeric(stats::filter(x=x,filter=rate,method="recursive")))
}


AdstockRate<-function(Data,Impact,Ads){
  modFit<-nls(data=Data,Impact~a+b*adstock(Ads,rate),  start=c(a=1,b=1,rate=0))
  if(summary(modFit)$coefficients[3,1]>0){
    AdstockRate=summary(modFit)$coefficients[3,1]
  }
  else{
    library(minpack.lm)
    nls.out<-nlsLM(Impact~a+b*adstock(Ads,rate),data=Data,start=list(a=1,b=1,rate=0),lower=c(a=-Inf,b=-Inf,rate=0),upper=c(a=Inf,b=Inf,rate=1)) 
    AdstockRate=summary(nls.out)$coefficients[3,1]
  }
  return(AdstockRate)
}

AdstockRate1<-function(Data,Impact,Ads){
  modFit<-nls(data=Data,Impact~a+b*adstock(Ads,rate),
              start=c(a=1,b=1,rate=0), control = list(maxiter = 500))
  if(summary(modFit)$coefficients[3,1]>0){
    AdstockRate=summary(modFit)$coefficients[3,1]
  }
  else{
    library(minpack.lm)
    nls.out<-nlsLM(Impact~a+b*adstock(Ads,rate),data=Data,start=list(a=1,b=1,rate=0),
                   lower=c(a=-Inf,b=-Inf,rate=0),upper=c(a=Inf,b=Inf,rate=1)) 
    AdstockRate=summary(nls.out)$coefficients[3,1]
  }
  return(AdstockRate)
}

#####Loading the Spend Data#######################
channel <- read.csv("E:/mdata/ChannelDataUPD.csv", header = TRUE, skip=1)
str(channel)
channel$Week <- as.Date(channel$Week)

######Users##########################################
adstock_Users=data.frame(matrix(ncol=0,nrow=157))
adstock_Users$week <- channel$Week
View(adstock_Users)

#Social
adstock_rate <- AdstockRate(channel, channel$Users, channel$Social)
max_memory <- 3
learn_rates <- rep(adstock_rate, max_memory+1) ^ c(0:max_memory)
adstocked_advertising <- stats::filter(c(rep(0, max_memory), channel$Social), learn_rates, method="convolution")
adstocked_advertising <- adstocked_advertising[!is.na(adstocked_advertising)]
adstock_Users$Social <- adstocked_advertising

#OnDemand
adstock_rate1 <- AdstockRate(channel, channel$Users, channel$OnDemand)
max_memory <- 3
learn_rates <- rep(1, max_memory+1) ^ c(0:max_memory)
adstocked_advertising <- stats::filter(c(rep(0, max_memory), channel$OnDemand), learn_rates, method="convolution")
adstocked_advertising <- adstocked_advertising[!is.na(adstocked_advertising)]
adstock_Users$OnDemand <- adstocked_advertising

#TV
adstock_rate3 <- AdstockRate(channel, channel$Users, channel$TV)
max_memory <- 4
learn_rates <- rep(adstock_rate3, max_memory+1) ^ c(0:max_memory)
adstocked_advertising <- stats::filter(c(rep(0, max_memory), channel$TV), learn_rates, method="convolution")
adstocked_advertising <- adstocked_advertising[!is.na(adstocked_advertising)]
adstock_Users$TV <- adstocked_advertising

#OOH
adstock_rate4 <- AdstockRate(channel, channel$Users, channel$OOH)
max_memory <- 3
learn_rates <- rep(adstock_rate4, max_memory+1) ^ c(0:max_memory)
adstocked_advertising <- stats::filter(c(rep(0, max_memory), channel$OOH), learn_rates, method="convolution")
adstocked_advertising <- adstocked_advertising[!is.na(adstocked_advertising)]
adstock_Users$OOH <- adstocked_advertising

#Display
adstock_rate5 <- AdstockRate(channel, channel$Users, channel$Display)
max_memory <- 3
learn_rates <- rep(adstock_rate5, max_memory+1) ^ c(0:max_memory)
adstocked_advertising <- stats::filter(c(rep(0, max_memory), channel$Display), learn_rates, method="convolution")
adstocked_advertising <- adstocked_advertising[!is.na(adstocked_advertising)]
adstock_Users$Display <- adstocked_advertising

#Search
adstock_rate6 <- AdstockRate(channel, channel$Users, channel$SEM)
max_memory <- 1
learn_rates <- rep(adstock_rate6, max_memory+1) ^ c(0:max_memory)
adstocked_advertising <- stats::filter(c(rep(0, max_memory), channel$SEM), learn_rates, method="convolution")
adstocked_advertising <- adstocked_advertising[!is.na(adstocked_advertising)]
adstock_Users$SEM <- adstocked_advertising

#Email
adstock_rate7 <- AdstockRate(channel, channel$Users, channel$Email)
max_memory <- 1
learn_rates <- rep(adstock_rate7, max_memory+1) ^ c(0:max_memory)
adstocked_advertising <- stats::filter(c(rep(0, max_memory), channel$Email), learn_rates, method="convolution")
adstocked_advertising <- adstocked_advertising[!is.na(adstocked_advertising)]
adstock_Users$Email <- adstocked_advertising

#Radio
library(minpack.lm)
adstock_rate8 <- AdstockRate(channel, channel$Users, channel$Radio)
max_memory <- 3
learn_rates <- rep(adstock_rate8, max_memory+1) ^ c(0:max_memory)
adstocked_advertising <- stats::filter(c(rep(0, max_memory), channel$Radio), learn_rates, method="convolution")
adstocked_advertising <- adstocked_advertising[!is.na(adstocked_advertising)]
adstock_Users$Radio <- adstocked_advertising


#Target Varibale
adstock_Users$Users <- channel$Users

#Adstock Rates
Users_adstock <- data.frame(adstock_rate, adstock_rate1, adstock_rate3, adstock_rate4, adstock_rate5, adstock_rate6, adstock_rate7, adstock_rate8)
setnames(Users_adstock, old = c('adstock_rate', 'adstock_rate1', 'adstock_rate3', 'adstock_rate4', 'adstock_rate5', 'adstock_rate6', 'adstock_rate7', 'adstock_rate8'), 
         new = c('Social', 'OnDemand', 'TV', 'OOH', 'Display', 'SEM', 'Email', 'Radio'))

Users_adstock <- melt(Users_adstock)
Users_adstock$value <- round(Users_adstock$value, 2)
setnames(Users_adstock, old = c('variable', 'value'), new = c('Channels', 'Users_Rates'))
adstocks <- Users_adstock

#Normalizing the adstock variables
adstock_Users$Social <- adstock_Users$Social / 2907096.38
adstock_Users$OnDemand <- adstock_Users$OnDemand / 2907096.38
adstock_Users$TV <- adstock_Users$TV / 2907096.38
adstock_Users$OOH <- adstock_Users$OOH / 2907096.38
adstock_Users$Display <- adstock_Users$Display / 2907096.38
adstock_Users$SEM <- adstock_Users$SEM / 2907096.38
adstock_Users$Radio <- adstock_Users$Radio / 2907096.38
adstock_Users$Email <- adstock_Users$Email / 2907096.38

#Creating the dummy variables for the campaign variables
adstock_Users$Brand_Tool <- ifelse(channel$Brand_Tool>0, 1, 0)
adstock_Users$Justine_Smith <- ifelse(channel$Justine_Smith>0, 1, 0)
adstock_Users$AO_App <- ifelse(channel$AO_App>0, 1, 0)
adstock_Users$AO_Fan_Gen <- ifelse(channel$AO_Fan_Gen>0, 1, 0)
adstock_Users$Jamie_Comedian <- ifelse(channel$Jamie_Comedian>0, 1, 0)
adstock_Users$Contextual <- ifelse(channel$Contextual>0, 1, 0)
adstock_Users$Facebook_BAU <- ifelse(channel$Facebook_BAU>0, 1, 0)
adstock_Users$Facebook_TMJ <- ifelse(channel$Facebook_TMJ>0, 1, 0)



#Seasonality and Trend Decomposition
ts_standard <- ts(adstock_Users$Users, frequency = 52, start=c(2016,27))
decompsition_standard <- decompose(ts_standard)
adstock_Users$seasonal <- decompsition_standard$seasonal


#Fitting The Model

adstock_Users <- read.csv("adstock_Users.csv", header = TRUE)
model_Users <- lm(Users ~ (Social + OnDemand + TV + OOH + Display + SEM + Email + Radio) ^ 3 + seasonal, data = adstock_Users[,-1])
model_Users <- lm(Users ~., data = adstock_Users[,-1])
model_step <- step(model_Users)
summary(model_step)


#Calulate Contributions
contri_Users=data.frame(matrix(ncol=0,nrow=157))
contri_Users$week <- channel$Week
contri_Users$Users <- channel$Users

contri_Users$Social <- abs(coef(model_Users)[2]) * adstock_Users$Social
contri_Users$onDemand <- abs(coef(model_Users)[3])  * adstock_Users$OnDemand
contri_Users$TV <- abs(coef(model_Users)[4]) * adstock_Users$TV
contri_Users$OOH <- abs(coef(model_Users)[5]) * adstock_Users$OOH
contri_Users$Display <- abs(coef(model_Users)[6])  * adstock_Users$Display
contri_Users$SEM <- abs(coef(model_Users)[7])  * adstock_Users$SEM
contri_Users$Email <- abs(coef(model_Users)[8])  * adstock_Users$Email
contri_Users$Radio <- abs(coef(model_Users)[8])  * adstock_Users$Radio



channel_Users <- channel %>%
  dplyr::select('Week', 'Users', 'Social', 'OnDemand', 'TV', 'OOH', 'Display', 'SEM', 'Email', 'Radio')


mdf_Users <- tidyr::gather(contri_Users, variable, value, -week, -Users)
sumdata_Users=data.frame(value=apply(contri_Users[-(1:2)],2,sum, na.rm=TRUE))
sumdata_Users$key=rownames(sumdata_Users)
sumdata_Users$cost <- apply(channel_Users[-c(1,2)],2,sum, na.rm=TRUE)
sumdata_Users$cpc<- as.numeric(format(round(sumdata_Users$cost/sumdata_Users$value, 2), nsmall = 2))



cost_Users<- ggplot(data=NaRV.omit(sumdata_Users), aes(x=stats::reorder(key, cpc), y=cpc, fill=key)) +
  geom_bar(colour="black", stat="identity", width = 0.6) +
  expand_limits(y = c(0,(as.integer(max(sumdata_Users$cpc)) + 1))) +
  geom_text(aes(label=paste0("$", cpc)), size = 4, hjust = -0.3, vjust = 0.5, position=     "stack") +
  ggtitle("Cost Per Conversion") +
  xlab("Channels") +
  ylab("Cost In Dollars") +
  theme_grey() +
  labs(fill = "Channels") + 
  coord_flip() + 
  theme(
    plot.title = element_text(color="black", size=16, face="bold", hjust = 0.5),
    axis.title.x = element_text(color="black", size=10, face="bold"),
    axis.title.y = element_text(color="black", size=10, face="bold"), 
    legend.position = "bottom", legend.background = element_rect(color = "black", 
                                                                 fill = "grey90", size = 0.5, linetype = "solid"), legend.direction = "horizontal")

contribution_Users <- ggplot(data=filter(sumdata_Users, value>0), aes(x=reorder(key, value), y=value, fill=key)) +
  geom_bar(colour="black", stat="identity", width = 0.6) +
  expand_limits(y = c(0,400)) +
  geom_text(aes(label=(prettyNum((round(value)), big.mark = ",", preserve.width = "none"))), size = 4, hjust = -0.1, vjust = 0.5, position=     "stack") +
  ggtitle("Absolute Advertising Channel Contribution") +
  xlab("Channels") +
  ylab("Number of Transactions") +
  theme_grey() +
  labs(fill = "Channels") + 
  coord_flip() + 
  theme(
    plot.title = element_text(color="black", size=16, face="bold", hjust = 0.5),
    axis.title.x = element_text(color="black", size=10, face="bold"),
    axis.title.y = element_text(color="black", size=10, face="bold"), 
    legend.position = "bottom", legend.background = element_rect(color = "black", 
                                                                 fill = "grey90", size = 0.5, linetype = "solid"), legend.direction = "horizontal")

timeline_Users<-ggplot(mdf_Users, aes(x=as.Date(mdf_Users$week), y=mdf_Users$value)) + 
  geom_area(aes(fill = variable), position = 'stack', color = 'black') + 
  geom_line(aes(y = Users), mdf_Users, lty = 2) +
  ggtitle("Weekly Subscriptions Timeline") +
  labs(fill = "Channels") +
  xlab("Time Period") + ylab("Weekly Subscriptions") +
  theme_grey() +
  theme(
    plot.title = element_text(color="black", size=16, face="bold", hjust = 0.5),
    axis.title.x = element_text(color="black", size=12, face="bold"),
    axis.title.y = element_text(color="black", size=12, face="bold")) 


#Saving the contributions and adstocked variables
write.csv(adstock_Users, "adstock_Users.csv")
write.csv(contri_Users, "Contri_Users.csv")


###################################################New.Users################################################################################################
adstock_New.Users=data.frame(matrix(ncol=0,nrow=157))
adstock_New.Users$week <- channel$Week
View(adstock_New.Users)

#Social
adstock_rate <- AdstockRate(channel, channel$New.Users, channel$Social)
max_memory <- 3
learn_rates <- rep(adstock_rate, max_memory+1) ^ c(0:max_memory)
adstocked_advertising <- stats::filter(c(rep(0, max_memory), channel$Social), learn_rates, method="convolution")
adstocked_advertising <- adstocked_advertising[!is.na(adstocked_advertising)]
adstock_New.Users$Social <- adstocked_advertising

#OnDemand
adstock_rate1 <- AdstockRate(channel, channel$New.Users, channel$OnDemand)
max_memory <- 3
learn_rates <- rep(adstock_rate1, max_memory+1) ^ c(0:max_memory)
adstocked_advertising <- stats::filter(c(rep(0, max_memory), channel$OnDemand), learn_rates, method="convolution")
adstocked_advertising <- adstocked_advertising[!is.na(adstocked_advertising)]
adstock_New.Users$OnDemand <- adstocked_advertising

#TV
adstock_rate3 <- AdstockRate(channel, channel$New.Users, channel$TV)
max_memory <- 4
learn_rates <- rep(adstock_rate3, max_memory+1) ^ c(0:max_memory)
adstocked_advertising <- stats::filter(c(rep(0, max_memory), channel$TV), learn_rates, method="convolution")
adstocked_advertising <- adstocked_advertising[!is.na(adstocked_advertising)]
adstock_New.Users$TV <- adstocked_advertising

#OOH
adstock_rate4 <- AdstockRate(channel, channel$New.Users, channel$OOH)
max_memory <- 3
learn_rates <- rep(adstock_rate4, max_memory+1) ^ c(0:max_memory)
adstocked_advertising <- stats::filter(c(rep(0, max_memory), channel$OOH), learn_rates, method="convolution")
adstocked_advertising <- adstocked_advertising[!is.na(adstocked_advertising)]
adstock_New.Users$OOH <- adstocked_advertising

#Display
adstock_rate5 <- AdstockRate(channel, channel$New.Users, channel$Display)
max_memory <- 3
learn_rates <- rep(adstock_rate5, max_memory+1) ^ c(0:max_memory)
adstocked_advertising <- stats::filter(c(rep(0, max_memory), channel$Display), learn_rates, method="convolution")
adstocked_advertising <- adstocked_advertising[!is.na(adstocked_advertising)]
adstock_New.Users$Display <- adstocked_advertising

#Search
adstock_rate6 <- AdstockRate(channel, channel$New.Users, channel$SEM)
max_memory <- 1
learn_rates <- rep(adstock_rate6, max_memory+1) ^ c(0:max_memory)
adstocked_advertising <- stats::filter(c(rep(0, max_memory), channel$SEM), learn_rates, method="convolution")
adstocked_advertising <- adstocked_advertising[!is.na(adstocked_advertising)]
adstock_New.Users$SEM <- adstocked_advertising

#Email
adstock_rate7 <- AdstockRate(channel, channel$New.Users, channel$Email)
max_memory <- 1
learn_rates <- rep(adstock_rate7, max_memory+1) ^ c(0:max_memory)
adstocked_advertising <- stats::filter(c(rep(0, max_memory), channel$Email), learn_rates, method="convolution")
adstocked_advertising <- adstocked_advertising[!is.na(adstocked_advertising)]
adstock_New.Users$Email <- adstocked_advertising

#Radio
library(minpack.lm)
adstock_rate8 <- AdstockRate(channel, channel$New.Users, channel$Radio)
max_memory <- 3
learn_rates <- rep(adstock_rate8, max_memory+1) ^ c(0:max_memory)
adstocked_advertising <- stats::filter(c(rep(0, max_memory), channel$Radio), learn_rates, method="convolution")
adstocked_advertising <- adstocked_advertising[!is.na(adstocked_advertising)]
adstock_New.Users$Radio <- adstocked_advertising


#Target Varibale
adstock_New.Users$New.Users <- channel$New.Users

#Adstock Rates
New.Users_adstock <- data.frame(adstock_rate, adstock_rate1, adstock_rate3, adstock_rate4, adstock_rate5, adstock_rate6, adstock_rate7, adstock_rate8)
setnames(New.Users_adstock, old = c('adstock_rate', 'adstock_rate1', 'adstock_rate3', 'adstock_rate4', 'adstock_rate5', 'adstock_rate6', 'adstock_rate7', 'adstock_rate8'), 
         new = c('Social', 'OnDemand', 'TV', 'OOH', 'Display', 'SEM', 'Email', 'Radio'))

New.Users_adstock <- melt(New.Users_adstock)
New.Users_adstock$value <- round(New.Users_adstock$value, 2)
setnames(New.Users_adstock, old = c('variable', 'value'), new = c('Channels', 'New.Users_Rates'))
adstocks$New.Users <- New.Users_adstock$New.Users_Rates

# #Normalizing the adstock variables
# adstock_New.Users$Social <- adstock_New.Users$Social / 2907096.38
# adstock_New.Users$OnDemand <- adstock_New.Users$OnDemand / 2907096.38
# adstock_New.Users$TV <- adstock_New.Users$TV / 2907096.38
# adstock_New.Users$OOH <- adstock_New.Users$OOH / 2907096.38
# adstock_New.Users$Display <- adstock_New.Users$Display / 2907096.38
# adstock_New.Users$SEM <- adstock_New.Users$SEM / 2907096.38
# adstock_New.Users$Radio <- adstock_New.Users$Radio / 2907096.38
# adstock_New.Users$Email <- adstock_New.Users$Email / 2907096.38

#Creating the dummy variables for the campaign variables
adstock_New.Users$Brand_Tool <- ifelse(channel$Brand_Tool>0, 1, 0)
adstock_New.Users$Justine_Smith <- ifelse(channel$Justine_Smith>0, 1, 0)
adstock_New.Users$AO_App <- ifelse(channel$AO_App>0, 1, 0)
adstock_New.Users$AO_Fan_Gen <- ifelse(channel$AO_Fan_Gen>0, 1, 0)
adstock_New.Users$Jamie_Comedian <- ifelse(channel$Jamie_Comedian>0, 1, 0)
adstock_New.Users$Contextual <- ifelse(channel$Contextual>0, 1, 0)
adstock_New.Users$Facebook_BAU <- ifelse(channel$Facebook_BAU>0, 1, 0)
adstock_New.Users$Facebook_TMJ <- ifelse(channel$Facebook_TMJ>0, 1, 0)



#Seasonality and Trend Decomposition
ts_standard <- ts(adstock_New.Users$New.Users, frequency = 52, start=c(2016,27))
decompsition_standard <- decompose(ts_standard)
adstock_New.Users$seasonal <- decompsition_standard$seasonal


#Fitting The Model

model_New.Users <- lm(New.Users ~ (Social + OnDemand + TV + OOH + Display + SEM + Email + Radio ) ^ 3 + seasona, data = adstock_New.Users[,-1])
model_New.Users <- lm(New.Users ~., data = adstock_New.Users[,-1])
model_step <- step(model_New.Users)
summary(model_New.Users)


#Calulate Contributions
contri_New.Users=data.frame(matrix(ncol=0,nrow=157))
contri_New.Users$week <- channel$Week
contri_New.Users$New.Users <- channel$New.Users

contri_New.Users$Social <- abs(coef(model_New.Users)[2]) * adstock_New.Users$Social
contri_New.Users$OnDemand <- abs(coef(model_New.Users)[3])  * adstock_New.Users$OnDemand
contri_New.Users$TV <- abs(coef(model_New.Users)[4]) * adstock_New.Users$TV
contri_New.Users$OOH <- abs(coef(model_New.Users)[5]) * adstock_New.Users$OOH
contri_New.Users$Display <- abs(coef(model_New.Users)[6])  * adstock_New.Users$Display
contri_New.Users$SEM <- abs(coef(model_New.Users)[7])  * adstock_New.Users$SEM
contri_New.Users$Email <- abs(coef(model_New.Users)[8])  * adstock_New.Users$Email
contri_New.Users$Radio <- abs(coef(model_New.Users)[8])  * adstock_New.Users$Radio



channel_New.Users <- channel %>%
  dplyr::select('Week', 'New.Users', 'Social', 'OnDemand', 'TV', 'OOH', 'Display', 'SEM', 'Email', 'Radio')


mdf_New.Users <- tidyr::gather(contri_New.Users, variable, value, -week, -New.Users)
sumdata_New.Users=data.frame(value=apply(contri_New.Users[-(1:2)],2,sum, na.rm=TRUE))
sumdata_New.Users$key=rownames(sumdata_New.Users)
sumdata_New.Users$cost <- apply(channel_New.Users[-c(1,2)],2,sum, na.rm=TRUE)
sumdata_New.Users$cpc<- as.numeric(format(round(sumdata_New.Users$cost/sumdata_New.Users$value, 2), nsmall = 2))



cost_New.Users<- ggplot(data=NaRV.omit(sumdata_New.Users), aes(x=stats::reorder(key, cpc), y=cpc, fill=key)) +
  geom_bar(colour="black", stat="identity", width = 0.6) +
  expand_limits(y = c(0,(as.integer(max(sumdata_New.Users$cpc)) + 1))) +
  geom_text(aes(label=paste0("$", cpc)), size = 4, hjust = -0.3, vjust = 0.5, position=     "stack") +
  ggtitle("Cost Per Conversion") +
  xlab("Channels") +
  ylab("Cost In Dollars") +
  theme_grey() +
  labs(fill = "Channels") + 
  coord_flip() + 
  theme(
    plot.title = element_text(color="black", size=16, face="bold", hjust = 0.5),
    axis.title.x = element_text(color="black", size=10, face="bold"),
    axis.title.y = element_text(color="black", size=10, face="bold"), 
    legend.position = "bottom", legend.background = element_rect(color = "black", 
                                                                 fill = "grey90", size = 0.5, linetype = "solid"), legend.direction = "horizontal")

contribution_New.Users <- ggplot(data=filter(sumdata_New.Users, value>0), aes(x=reorder(key, value), y=value, fill=key)) +
  geom_bar(colour="black", stat="identity", width = 0.6) +
  expand_limits(y = c(0,400)) +
  geom_text(aes(label=(prettyNum((round(value)), big.mark = ",", preserve.width = "none"))), size = 4, hjust = -0.1, vjust = 0.5, position=     "stack") +
  ggtitle("Absolute Advertising Channel Contribution") +
  xlab("Channels") +
  ylab("Number of Transactions") +
  theme_grey() +
  labs(fill = "Channels") + 
  coord_flip() + 
  theme(
    plot.title = element_text(color="black", size=16, face="bold", hjust = 0.5),
    axis.title.x = element_text(color="black", size=10, face="bold"),
    axis.title.y = element_text(color="black", size=10, face="bold"), 
    legend.position = "bottom", legend.background = element_rect(color = "black", 
                                                                 fill = "grey90", size = 0.5, linetype = "solid"), legend.direction = "horizontal")

timeline_New.Users<-ggplot(mdf_New.Users, aes(x=as.Date(mdf_New.Users$week), y=mdf_New.Users$value)) + 
  geom_area(aes(fill = variable), position = 'stack', color = 'black') + 
  geom_line(aes(y = New.Users), mdf_New.Users, lty = 2) +
  ggtitle("Weekly New Users Timeline") +
  labs(fill = "Channels") +
  xlab("Time Period") + ylab("Weekly New Users") +
  theme_grey() +
  theme(
    plot.title = element_text(color="black", size=16, face="bold", hjust = 0.5),
    axis.title.x = element_text(color="black", size=12, face="bold"),
    axis.title.y = element_text(color="black", size=12, face="bold")) 


#Saving the contributions and adstocked variables
write.csv(adstock_New.Users, "adstock_New.Users.csv")
write.csv(contri_New.Users, "Contri_New.Users.csv")

###################################################Applications################################################################################################
adstock_Applications=data.frame(matrix(ncol=0,nrow=157))
adstock_Applications$week <- channel$Week
View(adstock_Applications)

#Social
adstock_rate <- AdstockRate(channel, channel$Applications, channel$Social)
max_memory <- 3
learn_rates <- rep(adstock_rate, max_memory+1) ^ c(0:max_memory)
adstocked_advertising <- stats::filter(c(rep(0, max_memory), channel$Social), learn_rates, method="convolution")
adstocked_advertising <- adstocked_advertising[!is.na(adstocked_advertising)]
adstock_Applications$Social <- adstocked_advertising

#OnDemand
adstock_rate1 <- AdstockRate(channel, channel$Applications, channel$OnDemand)
max_memory <- 3
learn_rates <- rep(adstock_rate1, max_memory+1) ^ c(0:max_memory)
adstocked_advertising <- stats::filter(c(rep(0, max_memory), channel$OnDemand), learn_rates, method="convolution")
adstocked_advertising <- adstocked_advertising[!is.na(adstocked_advertising)]
adstock_Applications$OnDemand <- adstocked_advertising

#TV
adstock_rate3 <- AdstockRate(channel, channel$Applications, channel$TV)
max_memory <- 4
learn_rates <- rep(adstock_rate3, max_memory+1) ^ c(0:max_memory)
adstocked_advertising <- stats::filter(c(rep(0, max_memory), channel$TV), learn_rates, method="convolution")
adstocked_advertising <- adstocked_advertising[!is.na(adstocked_advertising)]
adstock_Applications$TV <- adstocked_advertising

#OOH
adstock_rate4 <- AdstockRate(channel, channel$Applications, channel$OOH)
max_memory <- 3
learn_rates <- rep(adstock_rate4, max_memory+1) ^ c(0:max_memory)
adstocked_advertising <- stats::filter(c(rep(0, max_memory), channel$OOH), learn_rates, method="convolution")
adstocked_advertising <- adstocked_advertising[!is.na(adstocked_advertising)]
adstock_Applications$OOH <- adstocked_advertising

#Display
adstock_rate5 <- AdstockRate(channel, channel$Applications, channel$Display)
max_memory <- 3
learn_rates <- rep(adstock_rate5, max_memory+1) ^ c(0:max_memory)
adstocked_advertising <- stats::filter(c(rep(0, max_memory), channel$Display), learn_rates, method="convolution")
adstocked_advertising <- adstocked_advertising[!is.na(adstocked_advertising)]
adstock_Applications$Display <- adstocked_advertising

#Search
adstock_rate6 <- AdstockRate(channel, channel$Applications, channel$SEM)
max_memory <- 1
learn_rates <- rep(adstock_rate6, max_memory+1) ^ c(0:max_memory)
adstocked_advertising <- stats::filter(c(rep(0, max_memory), channel$SEM), learn_rates, method="convolution")
adstocked_advertising <- adstocked_advertising[!is.na(adstocked_advertising)]
adstock_Applications$SEM <- adstocked_advertising

#Email
adstock_rate7 <- AdstockRate(channel, channel$Applications, channel$Email)
max_memory <- 1
learn_rates <- rep(adstock_rate7, max_memory+1) ^ c(0:max_memory)
adstocked_advertising <- stats::filter(c(rep(0, max_memory), channel$Email), learn_rates, method="convolution")
adstocked_advertising <- adstocked_advertising[!is.na(adstocked_advertising)]
adstock_Applications$Email <- adstocked_advertising

#Radio
library(minpack.lm)
adstock_rate8 <- AdstockRate1(channel, channel$Applications, channel$Radio)
max_memory <- 3
learn_rates <- rep(adstock_rate8, max_memory+1) ^ c(0:max_memory)
adstocked_advertising <- stats::filter(c(rep(0, max_memory), channel$Radio), learn_rates, method="convolution")
adstocked_advertising <- adstocked_advertising[!is.na(adstocked_advertising)]
adstock_Applications$Radio <- adstocked_advertising


#Target Varibale
adstock_Applications$Applications <- channel$Applications

#Adstock Rates
Applications_adstock <- data.frame(adstock_rate, adstock_rate1, adstock_rate3, adstock_rate4, adstock_rate5, adstock_rate6, adstock_rate7, adstock_rate8)
setnames(Applications_adstock, old = c('adstock_rate', 'adstock_rate1', 'adstock_rate3', 'adstock_rate4', 'adstock_rate5', 'adstock_rate6', 'adstock_rate7', 'adstock_rate8'), 
         new = c('Social', 'OnDemand', 'TV', 'OOH', 'Display', 'SEM', 'Email', 'Radio'))

Applications_adstock <- melt(Applications_adstock)
Applications_adstock$value <- round(Applications_adstock$value, 2)
setnames(Applications_adstock, old = c('variable', 'value'), new = c('Channels', 'Applications_Rates'))
adstocks$Applications <- Applications_adstock$Applications_Rates

# #Normalizing the adstock variables
# adstock_Applications$Social <- adstock_Applications$Social / 2907096.38
# adstock_Applications$OnDemand <- adstock_Applications$OnDemand / 2907096.38
# adstock_Applications$TV <- adstock_Applications$TV / 2907096.38
# adstock_Applications$OOH <- adstock_Applications$OOH / 2907096.38
# adstock_Applications$Display <- adstock_Applications$Display / 2907096.38
# adstock_Applications$SEM <- adstock_Applications$SEM / 2907096.38
# adstock_Applications$Radio <- adstock_Applications$Radio / 2907096.38
# adstock_Applications$Email <- adstock_Applications$Email / 2907096.38

#Creating the dummy variables for the campaign variables
adstock_Applications$Brand_Tool <- ifelse(channel$Brand_Tool>0, 1, 0)
adstock_Applications$Justine_Smith <- ifelse(channel$Justine_Smith>0, 1, 0)
adstock_Applications$AO_App <- ifelse(channel$AO_App>0, 1, 0)
adstock_Applications$AO_Fan_Gen <- ifelse(channel$AO_Fan_Gen>0, 1, 0)
adstock_Applications$Jamie_Comedian <- ifelse(channel$Jamie_Comedian>0, 1, 0)
adstock_Applications$Contextual <- ifelse(channel$Contextual>0, 1, 0)
adstock_Applications$Facebook_BAU <- ifelse(channel$Facebook_BAU>0, 1, 0)
adstock_Applications$Facebook_TMJ <- ifelse(channel$Facebook_TMJ>0, 1, 0)



#Seasonality and Trend Decomposition
ts_standard <- ts(adstock_Applications$Applications, frequency = 52, start=c(2016,27))
decompsition_standard <- decompose(ts_standard)
adstock_Applications$seasonal <- decompsition_standard$seasonal



#Fitting The Model
model_Applications <- lm(Applications ~ (Social + OnDemand + TV + OOH + Display + SEM + Email + Radio + seasonal) ^ 3, data = adstock_Applications[,-1])
model_Applications <- lm(Applications ~., data = adstock_Applications[,-1])
model_step <- step(model_Applications)
summary(model_step)


#Calulate Contributions
contri_Applications=data.frame(matrix(ncol=0,nrow=157))
contri_Applications$week <- channel$Week
contri_Applications$Applications <- channel$Applications

contri_Applications$Social <- abs(coef(model_Applications)[2]) * adstock_Applications$Social
contri_Applications$OnDemand <- abs(coef(model_Applications)[3])  * adstock_Applications$OnDemand
contri_Applications$TV <- abs(coef(model_Applications)[4]) * adstock_Applications$TV
contri_Applications$OOH <- abs(coef(model_Applications)[5]) * adstock_Applications$OOH
contri_Applications$Display <- abs(coef(model_Applications)[6])  * adstock_Applications$Display
contri_Applications$SEM <- abs(coef(model_Applications)[7])  * adstock_Applications$SEM
contri_Applications$Email <- abs(coef(model_Applications)[8])  * adstock_Applications$Email
contri_Applications$Radio <- abs(coef(model_Applications)[8])  * adstock_Applications$Radio



channel_Applications <- channel %>%
  dplyr::select('Week', 'Applications', 'Social', 'OnDemand', 'TV', 'OOH', 'Display', 'SEM', 'Email', 'Radio')


mdf_Applications <- tidyr::gather(contri_Applications, variable, value, -week, -Applications)
sumdata_Applications=data.frame(value=apply(contri_Applications[-(1:2)],2,sum, na.rm=TRUE))
sumdata_Applications$key=rownames(sumdata_Applications)
sumdata_Applications$cost <- apply(channel_Applications[-c(1,2)],2,sum, na.rm=TRUE)
sumdata_Applications$cpc<- as.numeric(format(round(sumdata_Applications$cost/sumdata_Applications$value, 2), nsmall = 2))



cost_Applications<- ggplot(data=NaRV.omit(sumdata_Applications), aes(x=stats::reorder(key, cpc), y=cpc, fill=key)) +
  geom_bar(colour="black", stat="identity", width = 0.6) +
  expand_limits(y = c(0,(as.integer(max(sumdata_Applications$cpc)) + 1))) +
  geom_text(aes(label=paste0("$", cpc)), size = 4, hjust = -0.3, vjust = 0.5, position=     "stack") +
  ggtitle("Cost Per Conversion") +
  xlab("Channels") +
  ylab("Cost In Dollars") +
  theme_grey() +
  labs(fill = "Channels") + 
  coord_flip() + 
  theme(
    plot.title = element_text(color="black", size=16, face="bold", hjust = 0.5),
    axis.title.x = element_text(color="black", size=10, face="bold"),
    axis.title.y = element_text(color="black", size=10, face="bold"), 
    legend.position = "bottom", legend.background = element_rect(color = "black", 
                                                                 fill = "grey90", size = 0.5, linetype = "solid"), legend.direction = "horizontal")

contribution_Applications <- ggplot(data=filter(sumdata_Applications, value>0), aes(x=reorder(key, value), y=value, fill=key)) +
  geom_bar(colour="black", stat="identity", width = 0.6) +
  expand_limits(y = c(0,400)) +
  geom_text(aes(label=(prettyNum((round(value)), big.mark = ",", preserve.width = "none"))), size = 4, hjust = -0.1, vjust = 0.5, position=     "stack") +
  ggtitle("Absolute Advertising Channel Contribution") +
  xlab("Channels") +
  ylab("Number of Transactions") +
  theme_grey() +
  labs(fill = "Channels") + 
  coord_flip() + 
  theme(
    plot.title = element_text(color="black", size=16, face="bold", hjust = 0.5),
    axis.title.x = element_text(color="black", size=10, face="bold"),
    axis.title.y = element_text(color="black", size=10, face="bold"), 
    legend.position = "bottom", legend.background = element_rect(color = "black", 
                                                                 fill = "grey90", size = 0.5, linetype = "solid"), legend.direction = "horizontal")

timeline_Applications<-ggplot(mdf_Applications, aes(x=as.Date(mdf_Applications$week), y=mdf_Applications$value)) + 
  geom_area(aes(fill = variable), position = 'stack', color = 'black') + 
  geom_line(aes(y = Applications), mdf_Applications, lty = 2) +
  ggtitle("Weekly New Users Timeline") +
  labs(fill = "Channels") +
  xlab("Time Period") + ylab("Weekly New Users") +
  theme_grey() +
  theme(
    plot.title = element_text(color="black", size=16, face="bold", hjust = 0.5),
    axis.title.x = element_text(color="black", size=12, face="bold"),
    axis.title.y = element_text(color="black", size=12, face="bold")) 


#Saving the contributions and adstocked variables
write.csv(adstock_Applications, "adstock_Applications.csv")
write.csv(contri_Applications, "Contri_Applications.csv")



###################################################Searches################################################################################################
adstock_Searches=data.frame(matrix(ncol=0,nrow=157))
adstock_Searches$week <- channel$Week
View(adstock_Searches)

#Social
adstock_rate <- AdstockRate(channel, channel$Searches, channel$Social)
max_memory <- 3
learn_rates <- rep(adstock_rate, max_memory+1) ^ c(0:max_memory)
adstocked_advertising <- stats::filter(c(rep(0, max_memory), channel$Social), learn_rates, method="convolution")
adstocked_advertising <- adstocked_advertising[!is.na(adstocked_advertising)]
adstock_Searches$Social <- adstocked_advertising

#OnDemand
adstock_rate1 <- AdstockRate(channel, channel$Searches, channel$OnDemand)
max_memory <- 3
learn_rates <- rep(adstock_rate1, max_memory+1) ^ c(0:max_memory)
adstocked_advertising <- stats::filter(c(rep(0, max_memory), channel$OnDemand), learn_rates, method="convolution")
adstocked_advertising <- adstocked_advertising[!is.na(adstocked_advertising)]
adstock_Searches$OnDemand <- adstocked_advertising

#TV
adstock_rate3 <- AdstockRate(channel, channel$Searches, channel$TV)
max_memory <- 4
learn_rates <- rep(adstock_rate3, max_memory+1) ^ c(0:max_memory)
adstocked_advertising <- stats::filter(c(rep(0, max_memory), channel$TV), learn_rates, method="convolution")
adstocked_advertising <- adstocked_advertising[!is.na(adstocked_advertising)]
adstock_Searches$TV <- adstocked_advertising

#OOH
adstock_rate4 <- AdstockRate(channel, channel$Searches, channel$OOH)
max_memory <- 3
learn_rates <- rep(adstock_rate4, max_memory+1) ^ c(0:max_memory)
adstocked_advertising <- stats::filter(c(rep(0, max_memory), channel$OOH), learn_rates, method="convolution")
adstocked_advertising <- adstocked_advertising[!is.na(adstocked_advertising)]
adstock_Searches$OOH <- adstocked_advertising

#Display
adstock_rate5 <- AdstockRate(channel, channel$Searches, channel$Display)
max_memory <- 3
learn_rates <- rep(adstock_rate5, max_memory+1) ^ c(0:max_memory)
adstocked_advertising <- stats::filter(c(rep(0, max_memory), channel$Display), learn_rates, method="convolution")
adstocked_advertising <- adstocked_advertising[!is.na(adstocked_advertising)]
adstock_Searches$Display <- adstocked_advertising

#Search
adstock_rate6 <- AdstockRate(channel, channel$Searches, channel$SEM)
max_memory <- 1
learn_rates <- rep(adstock_rate6, max_memory+1) ^ c(0:max_memory)
adstocked_advertising <- stats::filter(c(rep(0, max_memory), channel$SEM), learn_rates, method="convolution")
adstocked_advertising <- adstocked_advertising[!is.na(adstocked_advertising)]
adstock_Searches$SEM <- adstocked_advertising

#Email
adstock_rate7 <- AdstockRate(channel, channel$Searches, channel$Email)
max_memory <- 0
learn_rates <- rep(adstock_rate7, max_memory+1) ^ c(0:max_memory)
adstocked_advertising <- stats::filter(c(rep(0, max_memory), channel$Email), learn_rates, method="convolution")
adstocked_advertising <- adstocked_advertising[!is.na(adstocked_advertising)]
adstock_Searches$Email <- adstocked_advertising

#Radio
library(minpack.lm)
adstock_rate8 <- AdstockRate1(channel, channel$Searches, channel$Radio)
max_memory <- 3
learn_rates <- rep(adstock_rate8, max_memory+1) ^ c(0:max_memory)
adstocked_advertising <- stats::filter(c(rep(0, max_memory), channel$Radio), learn_rates, method="convolution")
adstocked_advertising <- adstocked_advertising[!is.na(adstocked_advertising)]
adstock_Searches$Radio <- adstocked_advertising


#Target Varibale
adstock_Searches$Searches <- channel$Searches

#Adstock Rates
Searches_adstock <- data.frame(adstock_rate, adstock_rate1, adstock_rate3, adstock_rate4, adstock_rate5, adstock_rate6, adstock_rate7, adstock_rate8)
setnames(Searches_adstock, old = c('adstock_rate', 'adstock_rate1', 'adstock_rate3', 'adstock_rate4', 'adstock_rate5', 'adstock_rate6', 'adstock_rate7', 'adstock_rate8'), 
         new = c('Social', 'OnDemand', 'TV', 'OOH', 'Display', 'SEM', 'Email', 'Radio'))

Searches_adstock <- melt(Searches_adstock)
Searches_adstock$value <- round(Searches_adstock$value, 2)
setnames(Searches_adstock, old = c('variable', 'value'), new = c('Channels', 'Searches_Rates'))
adstocks$Searches <- Searches_adstock$Searches_Rates

# #Normalizing the adstock variables
# adstock_Searches$Social <- adstock_Searches$Social / 2907096.38
# adstock_Searches$OnDemand <- adstock_Searches$OnDemand / 2907096.38
# adstock_Searches$TV <- adstock_Searches$TV / 2907096.38
# adstock_Searches$OOH <- adstock_Searches$OOH / 2907096.38
# adstock_Searches$Display <- adstock_Searches$Display / 2907096.38
# adstock_Searches$SEM <- adstock_Searches$SEM / 2907096.38
# adstock_Searches$Radio <- adstock_Searches$Radio / 2907096.38
# adstock_Searches$Email <- adstock_Searches$Email / 2907096.38

#Creating the dummy variables for the campaign variables
adstock_Searches$Brand_Tool <- ifelse(channel$Brand_Tool>0, 1, 0)
adstock_Searches$Justine_Smith <- ifelse(channel$Justine_Smith>0, 1, 0)
adstock_Searches$AO_App <- ifelse(channel$AO_App>0, 1, 0)
adstock_Searches$AO_Fan_Gen <- ifelse(channel$AO_Fan_Gen>0, 1, 0)
adstock_Searches$Jamie_Comedian <- ifelse(channel$Jamie_Comedian>0, 1, 0)
adstock_Searches$Contextual <- ifelse(channel$Contextual>0, 1, 0)
adstock_Searches$Facebook_BAU <- ifelse(channel$Facebook_BAU>0, 1, 0)
adstock_Searches$Facebook_TMJ <- ifelse(channel$Facebook_TMJ>0, 1, 0)



#Seasonality and Trend Decomposition
ts_standard <- ts(adstock_Searches$Searches, frequency = 52, start=c(2016,27))
decompsition_standard <- decompose(ts_standard)
adstock_Searches$seasonal <- decompsition_standard$seasonal


#Fitting The Model
model_Searches <- lm(Searches ~ (Social + OnDemand + TV + OOH + Display + SEM + Email + Radio + seasonal), data = adstock_Searches[,-1])
model_Searches <- lm(Searches ~., data = adstock_Searches[,-1])
model_step <- step(model_Searches)
summary(model_step)


#Calulate Contributions
contri_Searches=data.frame(matrix(ncol=0,nrow=157))
contri_Searches$week <- channel$Week
contri_Searches$Searches <- channel$Searches

contri_Searches$Social <- abs(coef(model_Searches)[2]) * adstock_Searches$Social
contri_Searches$OnDemand <- abs(coef(model_Searches)[3])  * adstock_Searches$OnDemand
contri_Searches$TV <- abs(coef(model_Searches)[4]) * adstock_Searches$TV
contri_Searches$OOH <- abs(coef(model_Searches)[5]) * adstock_Searches$OOH
contri_Searches$Display <- abs(coef(model_Searches)[6])  * adstock_Searches$Display
contri_Searches$SEM <- abs(coef(model_Searches)[7])  * adstock_Searches$SEM
contri_Searches$Email <- abs(coef(model_Searches)[8])  * adstock_Searches$Email
contri_Searches$Radio <- abs(coef(model_Searches)[8])  * adstock_Searches$Radio



channel_Searches <- channel %>%
  dplyr::select('Week', 'Searches', 'Social', 'OnDemand', 'TV', 'OOH', 'Display', 'SEM', 'Email', 'Radio')


mdf_Searches <- tidyr::gather(contri_Searches, variable, value, -week, -Searches)
sumdata_Searches=data.frame(value=apply(contri_Searches[-(1:2)],2,sum, na.rm=TRUE))
sumdata_Searches$key=rownames(sumdata_Searches)
sumdata_Searches$cost <- apply(channel_Searches[-c(1,2)],2,sum, na.rm=TRUE)
sumdata_Searches$cpc<- as.numeric(format(round(sumdata_Searches$cost/sumdata_Searches$value, 2), nsmall = 2))



cost_Searches<- ggplot(data=NaRV.omit(sumdata_Searches[-7,]), aes(x=stats::reorder(key, cpc), y=cpc, fill=key)) +
  geom_bar(colour="black", stat="identity", width = 0.6) +
  expand_limits(y = c(0,(as.integer(max(sumdata_Searches$cpc)) + 1))) +
  geom_text(aes(label=paste0("$", cpc)), size = 4, hjust = -0.3, vjust = 0.5, position=     "stack") +
  ggtitle("Cost Per Conversion") +
  xlab("Channels") +
  ylab("Cost In Dollars") +
  theme_grey() +
  labs(fill = "Channels") + 
  coord_flip() + 
  theme(
    plot.title = element_text(color="black", size=16, face="bold", hjust = 0.5),
    axis.title.x = element_text(color="black", size=10, face="bold"),
    axis.title.y = element_text(color="black", size=10, face="bold"), 
    legend.position = "bottom", legend.background = element_rect(color = "black", 
                                                                 fill = "grey90", size = 0.5, linetype = "solid"), legend.direction = "horizontal")

contribution_Searches <- ggplot(data=filter(sumdata_Searches, value>0), aes(x=reorder(key, value), y=value, fill=key)) +
  geom_bar(colour="black", stat="identity", width = 0.6) +
  expand_limits(y = c(0,400)) +
  geom_text(aes(label=(prettyNum((round(value)), big.mark = ",", preserve.width = "none"))), size = 4, hjust = -0.1, vjust = 0.5, position=     "stack") +
  ggtitle("Absolute Advertising Channel Contribution") +
  xlab("Channels") +
  ylab("Number of Transactions") +
  theme_grey() +
  labs(fill = "Channels") + 
  coord_flip() + 
  theme(
    plot.title = element_text(color="black", size=16, face="bold", hjust = 0.5),
    axis.title.x = element_text(color="black", size=10, face="bold"),
    axis.title.y = element_text(color="black", size=10, face="bold"), 
    legend.position = "bottom", legend.background = element_rect(color = "black", 
                                                                 fill = "grey90", size = 0.5, linetype = "solid"), legend.direction = "horizontal")

timeline_Searches<-ggplot(mdf_Searches, aes(x=as.Date(mdf_Searches$week), y=mdf_Searches$value)) + 
  geom_area(aes(fill = variable), position = 'stack', color = 'black') + 
  geom_line(aes(y = Searches), mdf_Searches, lty = 2) +
  ggtitle("Weekly Searches Timeline") +
  labs(fill = "Channels") +
  xlab("Time Period") + ylab("Weekly Searches") +
  theme_grey() +
  theme(
    plot.title = element_text(color="black", size=16, face="bold", hjust = 0.5),
    axis.title.x = element_text(color="black", size=12, face="bold"),
    axis.title.y = element_text(color="black", size=12, face="bold")) 


#Saving the contributions and adstocked variables
# write.csv(adstock_Searches, "adstock_Searches.csv")
# write.csv(contri_Searches, "Contri_Searches.csv")
# 
# 
# write.csv(adstocks, "adstock_rates.csv")
