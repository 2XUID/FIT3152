#  _____          _                                   _         _    _            
# |  __ \        | |             /\                  | |       | |  (_)           
# | |  | |  __ _ | |_  __ _     /  \    _ __    __ _ | | _   _ | |_  _   ___  ___ 
# | |  | | / _` || __|/ _` |   / /\ \  | '_ \  / _` || || | | || __|| | / __|/ __|
# | |__| || (_| || |_| (_| |  / ____ \ | | | || (_| || || |_| || |_ | || (__ \__ \
# |_____/  \__,_| \__|\__,_| /_/    \_\|_| |_| \__,_||_| \__, | \__||_| \___||___/
#                                                         __/ |                   
#                                                        |___/                    
#                      _                                        _      _    _ 
#     /\              (_)                                      | |    | |  | |
#    /  \    ___  ___  _   __ _  _ __   _ __ ___    ___  _ __  | |_   | |  | |
#   / /\ \  / __|/ __|| | / _` || '_ \ | '_ ` _ \  / _ \| '_ \ | __|  | |  | |
#  / ____ \ \__ \\__ \| || (_| || | | || | | | | ||  __/| | | || |_   | |  | |
# /_/    \_\|___/|___/|_| \__, ||_| |_||_| |_| |_| \___||_| |_| \__|  |_|  |_|
#                          __/ |                                         
#                         |___/          
#
#----------------------------------------------------------------------------------------------------------------
# Load necessary libraries
library(dplyr)
library(rpart)
library(tree) # for Decision Tree
library(e1071) # for Naive Bayes
library(randomForest) # for Random Forest
library(adabag) # for Bagging and Boosting
library(neuralnet) # for ANN
library(ROCR) # for AUC and ROC

# Clear the environment
rm(list = ls())

# Clear the output console (same with Ctrl + L)
cat("\014")

# Set the number of significant digits
options(digits=4)

# Set the directory where the file is located at to "Desktop"
dir = "~/Desktop" 
setwd(dir)

# Extract 2000 datapoints for 10 locations in Australia (randomly selected)
WAUS = read.csv("WAUS2019.csv")
L = as.data.frame(c(1:49))
set.seed(28279174) # Your Student ID is the random seed
L = L[sample(nrow(L), 10, replace = FALSE),] # sample 10 locations
WAUS = WAUS[(WAUS$Location %in% L),]
WAUS = WAUS[sample(nrow(WAUS), 2000, replace = FALSE),] # sample 2000 rows
#----------------------------------------------------------------------------------------------------------------
# Question 1
#----------------------------------------------------------------------------------------------------------------
str(WAUS) # check the basic structure of each field
summary(WAUS) # check the range of all fields

# Check the proportion of Yes and No in RainToday and WarmerTomorrow columns
raintmr_freq_table = as.data.frame(xtabs(~ WarmerTomorrow, WAUS))
raintdy_freq_table = as.data.frame(xtabs(~ RainToday, WAUS))
cbind(raintmr_freq_table, Percent=prop.table(raintmr_freq_table$Freq)*100)
cbind(raintdy_freq_table, Percent=prop.table(raintdy_freq_table$Freq)*100)

# Check the sum of NA values for each column
sapply(WAUS, function(col) sum(is.na(col)) ) 

# Calculate the standard deviation for each column
sapply(WAUS[,-c(4,10,12:13,24:25)], sd, na.rm = T)

# Removal of columns
# ::: Remove columns with too many NA values (roughly 25% of datapoints)
# ::: Remove Date (Day, Month, Year) columns
# ::: Remove columns that are unable to handle (roughly 10% of datapoints and they are sparsely distributed)
WAUS[,c("Evaporation", "Sunshine", "Cloud9am", "Cloud3pm")] = NULL
WAUS[,c("Day", "Month", "Year")] = NULL
WAUS[,c("WindGustDir", "WindDir9am", "WindDir3pm","Pressure9am", "Pressure3pm")] = NULL

# Change the representation of categorical data (strings) into numeric type
WAUS = WAUS %>% 
  mutate(WarmerTomorrow = ifelse(WarmerTomorrow == "Yes", 1, 0)) %>%
  mutate(RainToday = ifelse(RainToday == "Yes", 1, 0))

# Investigate the relationship between Rainfall and RainToday
# ::: When Rainfall is less than or equal to 1mm, there would be no rain happened on that day
# ::: Same goes the other way round
# ::: So I decided to fill NA value of Rainfall with default value of 0 if there is no rain on that day
# ::: As the range of 0 - 1 is relatively narrow, it would be acceptable just to put it as a default value of 0.
# ::: If there is rain on that day, the mean of Rainfall would then be filled.
WAUS[is.na(WAUS$Rainfall),][,c("Rainfall","RainToday")] 
WAUS[is.na(WAUS$RainToday),][,c("Rainfall","RainToday")]

mean_Rainfall = apply(WAUS[,c("Rainfall"), drop=F], 2, mean, na.rm=T) # Calculate the mean for Rainfall column

WAUS = WAUS %>%
  mutate(Rainfall = ifelse(is.na(Rainfall), ifelse(RainToday == "No", 0, mean_Rainfall), Rainfall)) %>%
  mutate(RainToday = ifelse(is.na(RainToday), ifelse(Rainfall <= 1, 0, 1), RainToday))

# Impute Min/MaxTemp using Temp9am/3pm
WAUS = WAUS %>%
  mutate(MaxTemp = ifelse(is.na(MaxTemp) & !(is.na(Temp9am) & is.na(Temp3pm)), max(Temp9am, Temp3pm, na.rm=T), MaxTemp)) %>%
  mutate(MinTemp = ifelse(is.na(MinTemp) & !(is.na(Temp9am) & is.na(Temp3pm)), min(Temp9am, Temp3pm, na.rm=T), MinTemp))

# Impute WindGustSpeed using WindSpeed9am/3pm
WAUS = WAUS %>%
  mutate(WindGustSpeed = ifelse(is.na(WindGustSpeed) & !(is.na(WindSpeed9am) & is.na(WindSpeed3pm)), max(WindSpeed9am, WindSpeed3pm, na.rm=T), WindGustSpeed))

# Remove rows with NA value
WAUS = WAUS[complete.cases(WAUS),]

# Convert categorical data columns with numeric type to categorical type 
WAUS$WarmerTomorrow = as.factor(WAUS$WarmerTomorrow)
WAUS$RainToday = as.factor(WAUS$RainToday)
WAUS$Location = as.factor(WAUS$Location)

#----------------------------------------------------------------------------------------------------------------
# Question 2
#----------------------------------------------------------------------------------------------------------------
set.seed(28279174) #Student ID as random seed 
train.row = sample(1:nrow(WAUS), 0.7*nrow(WAUS)) # Split into 70% and 30%
WAUS.train = WAUS[train.row,] # Assign 70% to train data
WAUS.test = WAUS[-train.row,] # Assign 30% to test data
print(paste0("Number of train datapoints: ", nrow(WAUS.train)))
print(paste0("Number of test datapoints: ", nrow(WAUS.test)))

#----------------------------------------------------------------------------------------------------------------
# Question 3
#----------------------------------------------------------------------------------------------------------------
# Fit each classifier to train data

#################
# Decision Tree #
#################
dt.fit = tree(WarmerTomorrow ~ ., data=WAUS.train)
summary(dt.fit)

###############
# Naive Bayes #
###############
nb.fit = naiveBayes(WarmerTomorrow ~ ., data=WAUS.train)
summary(nb.fit)

###########
# Bagging #
###########
set.seed(28279174)
bag.fit = bagging(WarmerTomorrow ~ ., data=WAUS.train, mfinal=5)
summary(bag.fit)

############
# Boosting #
############
set.seed(28279174)
boost.fit = boosting(WarmerTomorrow ~ ., data=WAUS.train, Mfinal=5)
summary(boost.fit)

#################
# Random Forest #
#################
set.seed(28279174)
rf.fit = randomForest(WarmerTomorrow ~ ., data=WAUS.train)
summary(rf.fit)

#----------------------------------------------------------------------------------------------------------------
# Question 4
#----------------------------------------------------------------------------------------------------------------
# Generate confusion matrix and calculate accuracy 
#      TP + TN
# ----------------- = Accuracy
# TP + TN + FP + FN

#################
# Decision Tree #
#################
dt.predict = predict(dt.fit, WAUS.test, type="class") 
dt.cm = table(actual=WAUS.test$WarmerTomorrow, predicted=dt.predict) # confusion matrix
dt.cm 
dt.acc = round(sum(diag(dt.cm))/sum(dt.cm), 4)
print(paste0("Accuracy for Decision Tree: ", dt.acc))

###############
# Naive Bayes #
###############
nb.predict = predict(nb.fit, WAUS.test)
nb.cm = table(actual=WAUS.test$WarmerTomorrow, predicted=nb.predict) # confusion matrix
nb.cm 
nb.acc = round(sum(diag(nb.cm))/sum(nb.cm), 4)
print(paste0("Accuracy for Naive Bayes: ", nb.acc))

###########
# Bagging #
###########
bag.predict = predict.bagging(bag.fit, WAUS.test)
bag.predict$confusion # confusion matrix
bag.acc = round((1 - bag.predict$error), 4)
print(paste0("Accuracy for Bagging: ", bag.acc))

############
# Boosting #
############
boost.predict = predict.boosting(boost.fit, WAUS.test)
boost.predict$confusion # confusion matrix
boost.acc = round((1 - boost.predict$error), 4)
print(paste0("Accuracy for Boosting: ", boost.acc))

#################
# Random Forest #
#################
rf.predict = predict(rf.fit, WAUS.test)
rf.cm = table(actual=WAUS.test$WarmerTomorrow, predicted=rf.predict) # confusion matrix
rf.cm 
rf.acc = round(sum(diag(rf.cm))/sum(rf.cm), 4)
print(paste0("Accuracy for Random Forest: ", rf.acc))

#----------------------------------------------------------------------------------------------------------------
# Question 5
#----------------------------------------------------------------------------------------------------------------
# ROC & AUC

#################
# Decision Tree #
#################
dt.predict2 = predict(dt.fit, WAUS.test) 

###############
# Naive Bayes #
###############
nb.predict2 = predict(nb.fit, WAUS.test, type="raw")

###########
# Bagging #
###########
bag.predict2 = bag.predict$prob

############
# Boosting #
############
boost.predict2 = boost.predict$prob

#################
# Random Forest #
#################
rf.predict2 = predict(rf.fit, WAUS.test, type="prob")

# Plot ROC curve and calculate AUC for each classifier 
num_models = 5 # number of models used
model_names = c("Decision Tree", "Naive Bayes", "Bagging", "Boosting", "Random Forest")
cust_colors = c("red", "blue", "dark green","purple", "dark orange") # list of colors
preds = cbind(dt.predict2, nb.predict2, bag.predict2, boost.predict2, rf.predict2) # list of prediction prob in different models 
auc = c()
for (i in 1:num_models) {
  pred = prediction(preds[,i*2], WAUS.test$WarmerTomorrow)
  plot(performance(pred, "tpr", "fpr"), 
       add=(i!=1), col=cust_colors[i], main="ROC Curve For Each Classifier") # ROC
  cauc = performance(pred, "auc") # AUC
  cauc2 = round(as.numeric(cauc@y.values), 4) # Round up to 4 decimal places
  auc = append(auc, cauc2)
  print(paste0(cauc@y.name," (AUC) for ", model_names[i], ": ", cauc2))
}

# Add straight line to represent random guess
abline(0,1)

# Add legend for better visualization on different ROC curves
legend("bottomright", inset=c(0.02,0.02), model_names, col=cust_colors, lty=1, cex=.5)

#----------------------------------------------------------------------------------------------------------------
# Question 6
#----------------------------------------------------------------------------------------------------------------
# Combine results from Question 4 and 5
acc = c(dt.acc, nb.acc, bag.acc, boost.acc, rf.acc)
summary_table = rbind(Accuracy=acc, AUC=auc)
colnames(summary_table) = model_names
summary_table

#----------------------------------------------------------------------------------------------------------------
# Question 7
#----------------------------------------------------------------------------------------------------------------
#################
# Decision Tree #
#################
summary(dt.fit)

###############
# Naive Bayes #
###############
# Cannot determine the importance of variables used in Naive Bayes as it only calculates the probabilities of each variable given

###########
# Bagging #
###########
sort(bag.fit$importance, decreasing=T)

############
# Boosting #
############
sort(boost.fit$importance, decreasing=T)

#################
# Random Forest #
#################
rf.fit$importance[order(-rf.fit$importance),]

#----------------------------------------------------------------------------------------------------------------
# Question 8
#----------------------------------------------------------------------------------------------------------------
# Experiment on improving classifier(s) by changing setting or through certain processes

# Here I try to experiment with 3 classifiers which are Decision Tree and Random Forest.
# Why? Because they are the best 2 out of 5 which achieved pretty good result in Accuracy and AUC.
# (Note: Remember to include seed when running the process of random forest cross validation below.)

#################
# Decision Tree #
#################
# Visualize the decision tree (see how we can prune)
plot(dt.fit) 
text(dt.fit, pretty=0)

# Use K-fold cv function to generate attributes for observation use
test_dt.fit = cv.tree(dt.fit, FUN = prune.misclass)
test_dt.fit

# Choose the smallest $dev and $k for pruning process
pruned_dt.fit = prune.misclass(dt.fit, best = 2)
summary(pruned_dt.fit) 

# < Observation >
# You would get k = 0 and it simply means model performance won't get changed 
# although the tree is simplified into one with 2 leaf nodes in the end.

# View the pruned decision tree
plot(pruned_dt.fit) 
text(pruned_dt.fit, pretty=0)

# Calculate accuracy
pruned_dt.predict = predict(pruned_dt.fit, WAUS.test, type="class") 
pruned_dt.cm = table(actual=WAUS.test$WarmerTomorrow, predicted=pruned_dt.predict) # confusion matrix
pruned_dt.cm 
pruned_dt.acc = round(sum(diag(pruned_dt.cm))/sum(pruned_dt.cm), 4)
print(paste0("Accuracy for Decision Tree after pruning: ", pruned_dt.acc))

#################
# Random Forest #
#################
set.seed(28279174)

# Run cross validation for random forest
rain_rfcv = rfcv(trainx=WAUS.train[,c(1:11)], trainy=WAUS.train[,c(13)], step=0.8)
rain_rfcv$error.cv

# < Observation >
# When doing sampling, the number of variables = 10 or 8 has the lowest error rate (0.1570) 

# Fit random forest model to train data
set.seed(28279174)
new_rf.fit = randomForest(WarmerTomorrow ~ ., data=WAUS.train[,c(1:11, 13)], mtry = 7)

# Calculate accuracy
new_rf.predict = predict(new_rf.fit, WAUS.test)
new_rf.cm = table(actual=WAUS.test$WarmerTomorrow, predicted=new_rf.predict) # confusion matrix
new_rf.cm 
new_rf.acc = round(sum(diag(new_rf.cm))/sum(new_rf.cm), 4)
print(paste0("Accuracy for Random Forest after CV: ", new_rf.acc))

# < Findings for random forest> 
# Original accuracy = 0.842
# When mtry = 7, accuracy = 0.8698
# When mtry = 9, accuracy = 0.866

# Generate summary table for comparison purpose
new_acc = c(pruned_dt.acc, new_rf.acc)
new_summary_table = rbind(Initial_Accuracy=acc[c(1,5)], Improved_Accuracy=new_acc)
colnames(new_summary_table) = model_names[c(1,5)]
new_summary_table

#----------------------------------------------------------------------------------------------------------------
# Question 9
#----------------------------------------------------------------------------------------------------------------
# Make indicator
new_WAUS = WAUS # make a copy of current dataset to avoid modification on it
new_WAUS$NoRainTmr = new_WAUS$WarmerTomorrow == 0
new_WAUS$HasRainTmr = new_WAUS$WarmerTomorrow == 1
location_categorical_vars = model.matrix(~ Location + 0, data=new_WAUS) # "+0" is to solve the problem that one of the variables is being ignored

# Exclude some unimportant variables
new_WAUS = new_WAUS[,-c(1:3,5:7,11:12)] 

# Normalise data (0 - 1)
normalise_ = function(col){
  return((col - min(col))/(max(col) - min(col)))
}

# Apply normalise function to all predictors (column 1 - 10)that are not categorical
predictors = apply(new_WAUS[,c(1:4)], 2, normalise_) 
new_WAUS = cbind(predictors, location_categorical_vars, new_WAUS[,c(5:7)])

# Check the attribute of each variable
summary(new_WAUS)

# Split data into train and test
set.seed(28279174) #Student ID as random seed 
train.row = sample(1:nrow(new_WAUS), 0.7*nrow(new_WAUS)) # Split into 70% and 30%
new_WAUS.train = new_WAUS[train.row,] # Assign 70% to train data
new_WAUS.test = new_WAUS[-train.row,] # Assign 30% to test data

# Build ANN
# ::: Here I did not use Location and RainToday as the level of their importance 
# ::: in classifiers above is not significant.
# ::: attribute {hidden} - the number of neurons for the hidden layer
# ::: attribute {linear.output} - determine whether activation function should be applied. If false, then apply.
set.seed(28279174)
nn.fit = neuralnet(NoRainTmr + HasRainTmr ~ . - WarmerTomorrow, new_WAUS.train, hidden = 1, linear.output=F)

# Visualize ANN
plot(nn.fit)

# Print the weights
nn.fit$result.matrix

# Make prediction using test set
nn.predict = compute(nn.fit, new_WAUS.test[,-c(15:17)]) 

# Make prediction
nn.predr = round(nn.predict$net.result, 0)
nn.predrdf = as.data.frame(as.table(nn.predr))

# Remove rows classified 0 - leave only classified 1
nn.predrdfs = nn.predrdf[!nn.predrdf$Freq==0,]
nn.predrdfs$Freq = NULL
colnames(nn.predrdfs) = c("Obs", "WarmerTomorrow")
nn.predrdfs = nn.predrdfs[order(nn.predrdfs$Obs),]

# Create confusion matrix and calculate the accuracy
nn.cm = table(observed=new_WAUS.test$WarmerTomorrow, predicted=nn.predrdfs$WarmerTomorrow)
nn.cm
nn.acc = round(sum(diag(nn.cm))/sum(nn.cm)*100, 2)
print(paste0("Accuracy for ANN:", nn.acc, "%"))

# exclude RainToday
# num of hidden neurons = 1, acc = 86.04%
# num of hidden neurons = 2, acc = 84.91%
# num of hidden neurons = 3, acc = 85.28%
# num of hidden neurons = c(1,1), acc = 85.85%
# num of hidden neurons = c(3,2), acc = 85.85%
# num of hidden neurons = c(5,1), acc = 85.85%

# exclude RainToday + Location
# num of hidden neurons = 1, acc = 85.28%
# num of hidden neurons = 2, acc = 85.09%
# num of hidden neurons = 3, acc = 85.28%
# num of hidden neurons = 4, acc = 84.91%

# exclude RainToday + WindSpeed9am + WindSpeed3pm
# num of hidden neurons = 1, acc = 86.04%

# exclude RainToday + WindSpeed9am + WindSpeed3pm + WindGustSpeed
# num of hidden neurons = 1, acc = 86.23%

# exclude RainToday + WindSpeed9am + WindSpeed3pm + WindGustSpeed + MinTemp + MaxTemp + Temp9am + Temp3pm
# num of hidden neurons = 1, acc = 86.23%

# exclude RainToday + WindSpeed9am + WindSpeed3pm + WindGustSpeed + MinTemp + MaxTemp + Temp3pm
# num of hidden neurons = 1, acc = 86.79%

#----------------------------------------------------------------------------------------------------------------
# Question 10
#----------------------------------------------------------------------------------------------------------------

################################################
# Please refer to "Kong Zheng.Ho.28279174.pdf" #
################################################