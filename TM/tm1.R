#Marketing Campaigns

library(ggplot2)
library(dplyr)
library(reshape2)

#read data
#mm1 <- read.csv('E:/mdata/ChannelDataUPD.csv', skip=1)
#mm1$Week = as.Date(mm1$Week)
#mm1$SEM = as.numeric(mm1$SEM)
#head(mm1[,c('Week',channel)])
#

str(mm1)
(colC1 = c('Date', rep('numeric',20)))
mm1 <- read.csv('E:/mdata/ChannelDataUPD.csv', skip=1, colClasses = colC1, stringsAsFactors = F)

dim(mm1)
head(mm1)
names(mm1)
str(mm1)
mm1
(target = names(mm1[,2:5]))
(channel = names(mm1[,6:13]))
(campaign = names(mm1[,14:20]))
head(mm1[,c('Week',channel)])
data=mm1
#
rownames(data) = mm1$Week
#data = data[,-1]
head(data)
head(data[,target])
head(data[,channel])
head(data[,campaign])

#groupby summary
data2a = melt(data[,c('Week',target)], id.vars='Week')
head(data2a)
data2a$category = 'target'
head(data2a)

data2b = melt(data[,c('Week',channel)], id.vars='Week')
head(data2b)
data2b$category = 'channel'
head(data2b)

data2c = melt(data[,c('Week',campaign)], id.vars='Week')
head(data2c)
data2c$category = 'campaign'
head(data2c)

data3 = rbind(data2a, data2b, data2c)
head(data3)

data3b <- data3 %>% group_by(category, variable) %>% summarise(sumTotal = sum(value, na.rm=T))

data3b %>% filter(!category %in% c('campaign', 'target'))  %>% ggplot(., aes(x=reorder(variable,sumTotal), y=sumTotal, fill=variable)) + geom_bar(stat='identity') + geom_text(aes(label=round(sumTotal,0))) +  ggtitle('Campaign Total : All Years') + labs(x='Total Amount', y='Channel Category')  + coord_flip() + theme(legend.position = 'bottom') + guides(fill=guide_legend(nrow=1,byrow=TRUE))

library(lubridate)
#group_by(month=floor_date(date, "month"))
as.Date(data3$Week, format="%Y")
year(as.Date(data3$Week, "%Y"))
year(data3$Week)

data3b <- data3 %>% group_by(category, variable) %>% summarise(sumTotal = sum(value, na.rm=T))
head(data3b)

data3b %>% filter(!category %in% c('campaign', 'target'))  %>% ggplot(., aes(x=reorder(variable,sumTotal), y=sumTotal, fill=variable)) + geom_bar(stat='identity') + geom_text(aes(label=round(sumTotal,0))) +  ggtitle('Campaign Total : All Years') + labs(x='Total Amount', y='Channel Category')  + coord_flip() + theme(legend.position = 'bottom') + guides(fill=guide_legend(nrow=1,byrow=TRUE))

#

data3c <- data3 %>% group_by(year=year(data3$Week), category, variable) %>% summarise(sumTotal = sum(value, na.rm=T))
head(data3c)

options(scipen = 999)
data3c %>% filter(!category %in% c('campaign', 'target'))  %>% ggplot(., aes(x=reorder(variable,sumTotal), y=sumTotal, fill=variable)) + geom_bar(stat='identity') + geom_text(aes(label=round(sumTotal,0))) +  ggtitle('Campaign Total : Years Wise') + labs(x='Total Amount', y='Channel Category')  + coord_flip() + theme(legend.position = 'bottom') + guides(fill=guide_legend(nrow=1,byrow=TRUE)) + facet_wrap( ~ year)

#Cost per Conversion
campaign
head(data3b)
(users = data3b %>% filter(variable=='Users') %>% pull(sumTotal))
data3e = data3b %>% filter(variable %in% c(channel)) %>% mutate(conversion = sumTotal/users)
head(data3e)
data3e

data3e %>% ggplot(., aes(x=reorder(variable, channel), y=conversion, fill=variable)) + geom_bar(stat='identity') + geom_text(aes(label=round(conversion,3))) +  ggtitle('Campaign Conversion : All Years') + labs(x='Conversion Amount', y='Channel Category')  + coord_flip() + theme(legend.position = 'bottom') + guides(fill=guide_legend(nrow=1,byrow=TRUE))


#--------------------
#Cost per Conversion
data4 <- data[,c('Week', 'Users', campaign)]
names(data4)

#----------------------------


# General Model-----
fit1 = lm(data=data, sales ~ channel1 + channel2 + channel3)
summary(fit1)

#--------
data
names(data) # "Users" "New.Users" "Applications"  "Searches"
(col1 = c('Users', channel))
tm1 = data[,col1]
tm1
fit1 = lm(Users ~ . , data=tm1)
summary(fit1)

(col2 = c('New.Users', channel))
tm2 = data[,col2]
head(tm2)
fit2 = lm(New.Users ~ . , data=tm2)
summary(fit2)

col3 = c('Applications', channel)
tm3 = data[,col3]
head(tm3)
fit3 = lm(Applications ~ . , data=tm3)
summary(fit3)

col3 = c('Applications', channel)
tm3 = data[,col3]
head(tm3)
fit3 = lm(Applications ~ . , data=tm3)
summary(fit3)

col4 = c('Searches', channel)
tm4 = data[,col4]
head(tm4)
fit4 = lm(Searches ~ . , data=tm4)
summary(fit4)

head(tm4)
fit4b = lm(Searches ~ . - (Display + Radio + OOH), data=tm4)
summary(fit4b)

#mutivariate
pv = c("Users", "New.Users", "Applications","Searches")
col5 = c(pv, channel)
tm5 = data[, col5]
head(tm5)
tm5
fit5 = lm(cbind(Users, New.Users, Applications, Searches) ~ . , data= tm5)
summary(fit5)[1]
summary(fit1)
