# set working directory to desktop
# clean up the environment before starting
rm(list = ls())
options(digits=4)

# install.packages("e1071")
library(e1071)

pttrain <- read.csv("playtennistrain.csv")
pttest <- read.csv("playtennistest.csv")
tmodel = naiveBayes(Play ~. -Day, data = pttrain)

tbpredict.r = predict(tmodel, pttest, type = 'raw')
tbpredict.r
tbpredict = predict(tmodel, pttest)
tbpredict
table(actual = pttest$Play, predicted = tbpredict)
