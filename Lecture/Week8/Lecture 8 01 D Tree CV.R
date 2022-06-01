# set working directory to desktop
# clean up the environment before starting
rm(list = ls())
#install.packages("tree")
library(tree)
#library(rpart)

options(digits=4)

ptt <- read.csv("playtennistrain.csv", stringsAsFactors = T)
set.seed(9999) #random seed
# resampling with replacement to create a larger training set
pttrain = ptt[sample(nrow(ptt), 100, replace = TRUE),]
# fitting the model
ptfit = tree::tree(Play ~. -Day, data = pttrain)
ptfit
summary(ptfit)
plot(ptfit)
text(ptfit, pretty = 0)
# making predictions from test data
pttest <- read.csv("playtennistest.csv", stringsAsFactors = T)
tpredict = predict(ptfit, pttest, type = "class")
tpredict
pttest$Play
table(actual = pttest$Play, predicted = tpredict)

# now do a cross validation test at different tree sizes

# Note that CV is not working properly on Version 3.6.3 for JB

testptfit = cv.tree(ptfit, FUN = prune.misclass, K = 10)

testptfit
prune.ptfit = prune.misclass(ptfit, best = 3)
summary(prune.ptfit)
plot(prune.ptfit)
text(prune.ptfit, pretty = 0)


ppredict = predict(prune.ptfit, pttest, type = "class")
table(actual = pttest$Play, predicted = ppredict)
ppredict = predict(prune.ptfit, pttest, type = "vector")
ppredict

