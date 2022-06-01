
library(ggplot2)
set.seed(9999) # Random seed to make subset reproducible
dsmall <- diamonds[sample(nrow(diamonds), 1000), ] 
attach(dsmall)
unique(dsmall$cut)
#contracts(clarity) = contr.treatment(8)
#contracts(color) = contr.treatment(7)
#contracts(cut) = contr.treatment(5)
fit = lm(price ~ carat+color+cut + clarity)
fit = lm(log(price) ~ log(carat)+color+cut + clarity)

plot(density(dsmall$price))
plot(density(log(dsmall$price)))

#2 
getwd()
setwd('/Users/mac/My Drive/Documents/Collection/2-SEM_1/FIT3152/Tutorial/Week6')
body.dat <- read.csv("body.dat.csv")
attach(body.dat)
t.test(Height[Gender == "Male"],
       Height[Gender == "Female"], "greater",
       conf.level=0.95
       )

t.test(Height[Gender == "Male"],
       Height[Gender == "Female"], "greater",
       conf.level=0.95
)

t.test(Weight[Gender == "Male"],
       Weight[Gender == "Female"], "greater",
       conf.level=0.99
)


#q4
Data<-read.csv('winequality-red.csv')
lm(quality~,data=Data)

#q6
ad<-read.csv("advertising.csv")
ad$budget = ad$TV+ad$Radio+ad$Newspaper
plot(ad$budget,ad$Sales)

#b+c
model <- lm(ad$Sales~ad$budget)
summary(model)
abline(model)

#d
model2<-lm(ad$Sales~ad$TV+ad$Radio+ad$Newspaper)
summary(model2)
#par(mfrow=c(2,2))
plot(model2)
