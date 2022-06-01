# Clean up the environment before starting
rm(list = ls())

# Toothbrush example
Toothbrush <- read.csv("Toothbrush.csv")
attach(Toothbrush) # note ‘attach’ function
plot(Price, Function)
fit = lm(Function ~ Price) # regression of y on x
fit
plot(Price, Function)
abline(fit)
attributes(fit)
fit$residuals
fit$coefficients[1]
fit$coefficients[2]
hist(fit$residuals)
qqnorm(fit$residuals)
qqline(fit$residuals)
plot(Price, fit$residuals)
par(mfrow =c(2,2)) # creates a 2 x 2 matrix for plots
plot(fit)
summary(fit)
predict.lm(fit, newdata = data.frame(Price=c(6,7,8)), int="conf")


# Clean up the environment again
# Concrete example
rm(list = ls())
Concrete <- read.csv("Concrete_regression.csv")
attach(Concrete)
fit <- lm(Strength ~ Cement + Water)
fit
summary(fit)
#install.packages("scatterplot3d") to do the 3D plot
library(scatterplot3d)
sur <-scatterplot3d(Water, Cement, Strength, pch=16)
fit <- lm(Strength ~ Water + Cement) 
sur$plane3d(fit)

# Now fit to all the data and build new model
fit <- lm(Strength ~ . , data = Concrete) # note “.” = all
fit
summary(fit)

# Clean up the environment again
rm(list = ls())
library(ggplot2)
set.seed(9999) # Random seed
dsmall <- diamonds[sample(nrow(diamonds), 1000), ] # sample of 1000 rows
qplot(carat, price, data = dsmall, color = color, size = clarity, alpha = cut)
qplot(log(carat), log(price), data = dsmall, color = color, size = clarity, alpha = cut)
qplot(log(carat), log(price), data = dsmall, size = clarity)
attach(dsmall)
contrasts(clarity) = contr.treatment(8) # 8 levels
d.fit <- lm(log(price) ~ log(carat) + clarity)
d.fit
# making the page width a bit narrower for a better fit
options(width = 50)
d.fit
# back to a wider page
options(width = 60)
summary(d.fit)
contrasts(clarity)
qplot(log(carat), log(price), data = dsmall, size = clarity) + geom_abline(intercept = 7.8, slope = 1.8)

