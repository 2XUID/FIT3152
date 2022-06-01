rm(list = ls())
zoo = read.csv("zoo.data.csv",stringsAsFactors = T)
zoo[,2:18]<-lapply(zoo[,2:18],factor)

library(tree)
z.fit<-tree(type~.-animal_name,data=zoo)
summary(z.fit)
plot(z.fit)
text(z.fit,pretty=0)

set.seed(9999)
train.row = sample(1)