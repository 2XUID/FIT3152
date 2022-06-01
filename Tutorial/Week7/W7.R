#install.packages("tree")
library(tree)
#a
data(mtcars)
attach(mtcars)
attach(mtcars)
summary(mpg)
cons = ifelse(mpg >= 19.20, "yes", "no")
carsclass = cbind(cons, mtcars)
head(carsclass)
#b
set.seed(999)
train.row = sample(1:nrow(carsclass),
                  0.7*nrow(carsclass))
c.train = carsclass[train.row,]
c.test = carsclass[-train.row]

sample(1:nrow(carsclass),0.7*nrow(carsclass))

#c
cl.train=c.train[sample(nrow(c.train),100,replace = TRUE),]
