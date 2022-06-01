library(dplyr)
library(rpart)
library(rpart.plot)
library(corrplot)
library(randomForest)
library(adabag)
library(tree)
library(e1071)
library(ROCR)

setwd("C:/Users/aud/My Drive/Documents/Assignment/2-SEM_1/FIT3152/A2 (20%)")
rm(list = ls())
WAUS <- read.csv("WarmerTomorrow2022.csv")
L <- as.data.frame(c(1:49))
set.seed(30874157) # Your Student ID is the random seed
L <- L[sample(nrow(L), 10, replace = FALSE),] # sample 10 locations
WAUS <- WAUS[(WAUS$Location %in% L),]
WAUS <- WAUS[sample(nrow(WAUS), 2000, replace = FALSE),] # sample 2000 rows
#the WAUS without NA value object
WAUS.WithoutNA = na.omit(WAUS)
#setting 4 decimals
options(digits=4)

#----------
#Question 1
#----------

#delete Warmer Tomorrow NA value
bad.WAUS = is.na(WAUS$WarmerTomorrow)
WAUS.WTWithoutNA=WAUS[!bad.WAUS,]

#proportion warmer vs not warmer
count.WT = WAUS.WTWithoutNA %>% count(WarmerTomorrow)
proportion = count.WT$n[2]/(count.WT$n[1]+count.WT$n[2])
proportion
remove(WAUS.WTWithoutNA)
#present the mean of every elements relate to warmer
WAUS.WithoutNA %>% summarise_if(is.numeric, mean)
#present the std
WAUS.WithoutNA %>% summarise_if(is.numeric, sd)

#Calculate the correlation of them to find any element should remove
A2_Correlation <- na.omit(subset(WAUS, select = -c(WindGustDir,
                                                   WindDir9am,
                                                   WindDir3pm,
                                                   WarmerTomorrow)))
A2_Correlation = cor(A2_Correlation)
corrplot(A2_Correlation, 
         method = 'square', 
         order = 'alphabet', 
         type = 'lower',
         tl.col= "black", 
         tl.srt= 45, 
         diag = FALSE)

#----------
#Question 2
#----------

#Remove Day, Year, Month, Location
#Remove NA value
WAUS.WithoutNA$Day<-NULL
WAUS.WithoutNA$Month<-NULL
WAUS.WithoutNA$Year<-NULL

#factor
WAUS.WithoutNA$WindGustDir = as.factor(WAUS.WithoutNA$WindGustDir)
WAUS.WithoutNA$WindDir9am = as.factor(WAUS.WithoutNA$WindDir9am)
WAUS.WithoutNA$WindDir3pm = as.factor(WAUS.WithoutNA$WindDir3pm)
WAUS.WithoutNA$WarmerTomorrow = as.factor(WAUS.WithoutNA$WarmerTomorrow)

#----------
#Question 3
#----------

set.seed(30874157) #Student ID as random seed 
train.row = sample(1:nrow(WAUS.WithoutNA), 0.7*nrow(WAUS.WithoutNA))
WAUS.train = WAUS.WithoutNA[train.row,]
WAUS.test = WAUS.WithoutNA[-train.row,]
remove(A2_Correlation)
remove(count.WT)

#----------
#Question 4
#----------

#decision tree
WAUS.DecisionTree = tree(WarmerTomorrow~., data = WAUS.train)
plot(WAUS.DecisionTree)
WAUS.DecisionTree
summary(WAUS.DecisionTree)

#naive bayes
WAUS.NavieBayes = naiveBayes(WarmerTomorrow~., data = WAUS.train)

#bagging
WAUS.Bagging = bagging(WarmerTomorrow ~. , data = WAUS.train)

#boosting
WAUS.Boosting = boosting(WarmerTomorrow ~. , data = WAUS.train)

#random Forest
WAUS.RandomForest = randomForest(WarmerTomorrow ~. , 
                                 data = WAUS.train,
                                 na.action = na.exclude)

#----------
#Question 5
#----------

#prepare predict data
WAUS.predict.DecisionTree = predict(WAUS.DecisionTree,
                                    WAUS.test,type = "class")
WAUS.predict.NavieBayes = predict(WAUS.NavieBayes, WAUS.test)
WAUS.predict.Bagging = predict.bagging(WAUS.Bagging,WAUS.test)
WAUS.predict.Boosting = predict.boosting(WAUS.Boosting, WAUS.test)
WAUS.predict.RandomForest = predict(WAUS.RandomForest, WAUS.test)

#confusion matrix of them
confmat.DecisionTree = table(actual = WAUS.test$WarmerTomorrow, 
                             predicted = WAUS.predict.DecisionTree)
confmat.NavieBayes = table(actual = WAUS.test$WarmerTomorrow, 
                           predicted = WAUS.predict.NavieBayes)
confmat.Bagging = WAUS.predict.Bagging$confusion
confmat.Boosting = WAUS.predict.Boosting$confusion
confmat.RandomForest = table(actual = WAUS.test$WarmerTomorrow, 
                             predicted = WAUS.predict.RandomForest)

#print out confusion matrix
confmat.DecisionTree
confmat.NavieBayes
confmat.Bagging
confmat.Boosting
confmat.RandomForest

#calculate accuracy
accuracy.DecisionTree = (sum(diag(confmat.DecisionTree))/
                         sum(confmat.DecisionTree))
accuracy.NavieBayes = (sum(diag(confmat.NavieBayes))/
                       sum(confmat.NavieBayes))
accuracy.Bagging = (sum(diag(confmat.Bagging))/
                    sum(confmat.Bagging))
accuracy.Boosting =(sum(diag(confmat.Boosting))/
                    sum(confmat.Boosting))
accuracy.RandomForest =(sum(diag(confmat.RandomForest))/
                        sum(confmat.RandomForest))

#print out accuracy (Bagging and RandomForest)
accuracy.DecisionTree
accuracy.NavieBayes
accuracy.Bagging
accuracy.Boosting
accuracy.RandomForest

#----------
#Question 6
#----------

#####ROC#####
###Decision Tree
WAUS.pred.tree = predict(WAUS.DecisionTree, WAUS.test, type = "vector")
prediction.DT = prediction( WAUS.pred.tree[,2], WAUS.test$WarmerTomorrow)
performance.DT = performance(prediction.DT,"tpr","fpr")
plot(performance.DT, col='red')

###Navie Bayes
WAUS.pred.bayes = predict(WAUS.NavieBayes, WAUS.test, type = "raw")
predict.bayes = prediction( WAUS.pred.bayes[,2], WAUS.test$WarmerTomorrow)
performance.NB = performance(predict.bayes,"tpr","fpr")
plot(performance.NB, add=TRUE, col='yellow')

###Bagging
predict.Bagging = prediction( WAUS.predict.Bagging$prob[,2], 
                                WAUS.test$WarmerTomorrow)
performance.Bagging = performance(predict.Bagging,"tpr","fpr")
plot(performance.Bagging, add=TRUE, col='blue')

###Boosting
predict.Boosting = prediction( WAUS.predict.Boosting$prob[,2], 
                                 WAUS.test$WarmerTomorrow)
performance.Boosting = performance(predict.Boosting,"tpr","fpr")
plot(performance.Boosting, add=TRUE, col='orange')

###Random Forest
WAUS.pred.forest = predict(WAUS.RandomForest, WAUS.test, type = "prob")
predict.RF = prediction( WAUS.pred.forest[,2], WAUS.test$WarmerTomorrow)
performance.RF = performance(predict.RF,"tpr","fpr")
plot(performance.RF, add=TRUE, col='black')
abline(0,1)
legend("bottomright",
       legend=c("Decision Tree", 
                "Naive Bayes", 
                "Bagging", 
                "Boosting", 
                "Random Forest"),
       fill=c('red',
              'yellow',
              'blue',
              'orange',
              'black'))

#####AUC#####(Bagging is the best)
###Decision Tree
waus.AUC.DT = performance(predict.bayes, "auc")
value.DT = as.numeric(waus.AUC.DT@y.values)

###Navie Bayes
waus.AUC.NB = performance(predict.bayes, "auc")
value.NB = as.numeric(waus.AUC.NB@y.values)


###Bagging
waus.AUC.Bagging = performance(predict.Bagging, "auc")
value.Bagging = as.numeric(waus.AUC.Bagging@y.values)


###Boosting
waus.AUC.Boosting = performance(predict.Boosting, "auc")
value.Boosting = as.numeric(waus.AUC.Boosting@y.values)


###Random Forest
waus.AUC.RF = performance(predict.RF, "auc")
value.RF = as.numeric(waus.AUC.RF@y.values)

#print result
value.DT
value.NB
value.Bagging
value.Boosting
value.RF

#----------
#Question 7
#----------
#Bagging

#----------
#Question 8
#----------
###Decision Tree
plot(WAUS.DecisionTree)
text(WAUS.DecisionTree, pretty = 0,cex = 0.7)
title("WAUS.DecisionTree")
summary(WAUS.DecisionTree)

###Navie Bayes (Not found)

###Bagging 
sort(WAUS.Bagging$importance)

###Boosting 
sort(WAUS.Boosting$importance)

###Random Forest
WAUS.RandomForest$importance[order(WAUS.RandomForest$importance),]

#----------
#Question 9
#----------

set.seed(30874157) #Student ID as random seed 
WAUS.Q10 = WAUS.WithoutNA
WAUS.Q10 = WAUS.Q10[,c(5,7,9,10,15,21)]
WAUS.Q10 = na.omit(WAUS.Q10)

train.row = sample(1:nrow(WAUS.Q10), 0.7*nrow(WAUS.Q10))
WAUS.train.Q10 = WAUS.Q10[train.row,]
WAUS.test.Q10 = WAUS.Q10[-train.row,]
WAUS.DecisionTree2 = tree(WarmerTomorrow~., data = WAUS.train.Q10,method = "vector")
plot(WAUS.DecisionTree2)
text(WAUS.DecisionTree2,pretty = 0,cex = 0.7)

#testing in origin data
WAUS.predict.DecisionTree2 = predict(WAUS.DecisionTree2,
                                    WAUS.test,type = "class")
#confmat
confmat.DecisionTree2 = table(actual = WAUS.test$WarmerTomorrow, 
                             predicted = WAUS.predict.DecisionTree2)
#accuracy
accuracy.DecisionTree2 = (sum(diag(confmat.DecisionTree2))/
                           sum(confmat.DecisionTree2))
accuracy.DecisionTree2
WAUS.pred.tree2 = predict(WAUS.DecisionTree2, WAUS.test, type = "vector")
prediction.DT2 = prediction( WAUS.pred.tree2[,2], WAUS.test$WarmerTomorrow)
performance.DT2 = performance(prediction.DT2,"tpr","fpr")

#AUC
waus.AUC.DT2 = performance(predict.bayes, "auc")
as.numeric(waus.AUC.DT@y.values)

#----------
#Question 10
#----------
######Decision Tree######
test.WAUS.fit=cv.tree(WAUS.DecisionTree, FUN=prune.misclass)
test.WAUS.fit
#After testing, best = 4 has lowest dev
prune.WAUS.DecisionTree = prune.misclass(WAUS.DecisionTree, best=9)
summary(prune.WAUS.DecisionTree)
plot(prune.WAUS.DecisionTree)
text(prune.WAUS.DecisionTree, pretty=0)
title("prune.WAUS.DecisionTree")
#test accuracy after pruning
WAUS.prune.predict = predict(prune.WAUS.DecisionTree,WAUS.test,type = "class")
confmat.prune.DecisionTree = table(predicted = WAUS.prune.predict, 
                                   actual = WAUS.test$WarmerTomorrow)
confmat.prune.DecisionTree
confmat.DecisionTree
accuracy.prune.DecisionTree = (sum(diag(confmat.prune.DecisionTree))/
                               sum(confmat.prune.DecisionTree))
accuracy.prune.DecisionTree
accuracy.DecisionTree



######Bagging Cross Validation######
WAUS.Bagging.cv = bagging.cv(WarmerTomorrow ~ ., 
                             v = 10, 
                             WAUS.train)
confmat.Bagging = WAUS.Bagging.cv$confusion
confmat.Bagging
accuracy.Bagging.cv = (sum(diag(confmat.Bagging))/
                       sum(confmat.Bagging))
#Compare accuracy
accuracy.Bagging
accuracy.Bagging.cv


######Boosting Cross Validation######
WAUS.Boosting.cv = boosting.cv(WarmerTomorrow ~ ., 
                             v = 10, 
                             WAUS.train)
confmat.Boosting = WAUS.Boosting.cv$confusion
confmat.Boosting
accuracy.Boosting.cv = (sum(diag(confmat.Boosting))/
                         sum(confmat.Boosting))
#Compare accuracy
accuracy.Boosting
accuracy.Boosting.cv


######Random Forest######
rf.cv = rfcv(WAUS.train[,-c(19)], 
             WAUS.train[,c(19)], 
             cv.fold=10, 
             scale="log", 
             step=0.5, 
             mtry=function(p) max(1, floor(sqrt(p))), 
             recursive=FALSE)
rf.cv$error.cv
with(rf.cv, plot(n.var, error.cv))
#mtry = 10
WAUS.CV.RandomForest = randomForest(WarmerTomorrow~., 
                                   WAUS.train, 
                                   na.action = na.exclude, 
                                   mtry=10)
WAUS.CV.RandomForest
WAUS.predict.CV.RandomForest = predict(WAUS.CV.RandomForest, WAUS.test)
confmat.CV.RandomForest = table(actual = WAUS.test$WarmerTomorrow, 
                                predicted = WAUS.predict.CV.RandomForest)
confmat.RandomForest
confmat.CV.RandomForest
accuracy.CV.RandomForest =(sum(diag(confmat.CV.RandomForest))/
                           sum(confmat.CV.RandomForest))
accuracy.RandomForest
accuracy.CV.RandomForest

#----------
#Question 11
#----------

library(neuralnet)
ANN.WAUS = WAUS
ANN.WAUS = na.omit(ANN.WAUS)
ANN.m =  as.data.frame(model.matrix(~WindGustDir+WindDir9am+WindDir3pm, data=ANN.WAUS))
#factor
ANN.WAUS = cbind(ANN.WAUS,ANN.m)
ANN.WAUS$WarmerTomorrow = as.numeric(ANN.WAUS$WarmerTomorrow)
ANN.WAUS

ANN.WAUS = ANN.WAUS[,-c(1:4,7,10:17,19:23)]
ANN.WAUS$`(Intercept)`<-NULL
#ANN.WAUS = ANN.WAUS %>% relocate(WarmerTomorrow, .after = WindDir3pmWNW)

ANN.WAUS
set.seed(30874157)
train.row = sample(1:nrow(ANN.WAUS), 0.7*nrow(ANN.WAUS))
ANN.WAUS.train = ANN.WAUS[train.row,]
ANN.WAUS.test = ANN.WAUS[-train.row,]

#WindGustDir++WindDir9am+WindDir3pm

neuralnet.WAUS = neuralnet(WarmerTomorrow~ 
                             MinTemp+MaxTemp+Evaporation+Sunshine+Pressure9am +
                             WindGustDirENE+WindGustDirESE+WindGustDirN+
                             WindGustDirNE+WindGustDirNNE+WindGustDirNNW+
                             WindGustDirNW+WindGustDirS+WindGustDirSE+
                             WindGustDirSSE+WindGustDirSSW+WindGustDirSW+
                             WindGustDirW+WindGustDirWNW+WindGustDirWSW+
                             WindDir9amENE+WindDir9amESE+WindDir9amN+
                             WindDir9amNE+WindDir9amNNE+WindDir9amNNW+
                             WindDir9amNW+WindDir9amS+WindDir9amSE+
                             WindDir9amSSE+WindDir9amSSW+WindDir9amSW+
                             WindDir9amW+WindDir9amWNW+WindDir9amWSW+
                             WindDir3pmENE+WindDir3pmESE+WindDir3pmN+
                             WindDir3pmNE+WindDir3pmNNE+WindDir3pmNNW+
                             WindDir3pmNW+WindDir3pmS+WindDir3pmSE+
                             WindDir3pmSSE+WindDir3pmSSW+WindDir3pmSW+
                             WindDir3pmW+WindDir3pmWNW+WindDir3pmWSW,
                           ANN.WAUS.train,
                           hidden = 6,
                           linear.output = FALSE)
#tidy data
neuralnet.WAUS$result.matrix
neuralnet.WAUS.predict = neuralnet::compute(neuralnet.WAUS,ANN.WAUS.test)
neuralnet.WAUS.predict$net.result
neuralnet.WAUS.predict = round(neuralnet.WAUS.predict$net.result,0)

# Create confusion matrix and calculate the accuracy
neuralnet.confmat = table(actual=ANN.WAUS.test$WarmerTomorrow, 
                          predicted=neuralnet.WAUS.predict)
neuralnet.confmat
neuralnet.WAUS.accuracy = (sum(diag(neuralnet.confmat))/
                           sum(neuralnet.confmat))
neuralnet.WAUS.accuracy
plot(neuralnet.WAUS)
