# set working directory to desktop
# setwd("~/Desktop")
# clean up the environment before starting
rm(list = ls())
options(digits=4)

# install.packages("e1071")
library(e1071)

pttrain <- read.csv("playtennistrain.csv")
pttest <- read.csv("playtennistest.csv")
tmodel = naiveBayes(Play ~. -Day, data = pttrain)
# outputs as confidence levels
tbpredict.r = predict(tmodel, pttest, type = 'raw')
tbpredict.r
# outputs as classifications
tbpredict = predict(tmodel, pttest)
tbpredict
table(pttest$Play, tbpredict)


# install.packages(("ROCR"))
library(ROCR)

## computing a simple ROC curve (x-axis: fpr, y-axis: tpr)
# labels are actual values, predictors are probability of play
tpred <- prediction( tbpredict.r[,2], pttest$Play)
tperf <- performance(tpred,"tpr","fpr")
plot(tperf)
abline(0,1)

inputs = cbind( tbpredict.r[,2], pttest$Play)
inputs


## computing a simple ROC curve (x-axis: fpr, y-axis: tpr)
pconfidence = c(0.3, 0.6, 0.4, 0.8, 0.4, 0.7, 1)
plabels = c(0, 1, 0, 1, 0, 0, 1)
# transform the inputs into a prediction object
cpred <- prediction(pconfidence, plabels)
# calculate the performance functions
cperf <- performance(cpred,"tpr","fpr")
 plot(cperf)
 abline(0,1)
# calc auc
cauc = performance(cpred, "auc")
print(as.numeric(cauc@y.values))
# calculate and plot lift
clift = performance(cpred, "lift")
 plot(clift)
 print(clift@y.values)

