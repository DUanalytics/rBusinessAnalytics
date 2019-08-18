###

#install.packages("party")
library(party)
#install.packages("partykit")
library(partykit)

##Read the data in the file
df<-read.csv("./data/MAdata - npsfood.csv", skip=1)
df
names(df)
table(df$npscat)
?ctree
fit <- ctree(npscat ~ .- cid - NPS_Score, data=df)
plot(fit, main="Conditional Inference Tree for NPS ")
plot(fit, gp = gpar(fontsize = 5), inner_panel=node_inner,
     ip_args=list( abbreviate = FALSE,id = FALSE))
summary(fit)
partykit:::.list.rules.party(fit)
names(df)
predict(fit, newdata=df[1:10,-c(1,2)])
df$npscat[1:10]
