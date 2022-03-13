#install.packages("MASS")
#install.packages("ISLR")
#install.packages("car")
# including all libraries
library(MASS)
library(ISLR)
library(car)

# viewing data set
fix(Boston)
names(Boston)
head(Boston)

# fit a simple linear regression model with medv as the response and lstat as the predictor
lm.fit = lm(medv~lstat,data = Boston)
# needed for plotting
attach(Boston)

# information about the model
lm.fit
summary(lm.fit)
names(lm.fit)

# access values from names(lm.fit)
coef(lm.fit)
# obtain confidence interval for the coefficient estimates
confint(lm.fit)

# produce confidence intervals and prediction intervals
predict(lm.fit,data.frame(lstat = c(5,10,15)),interval = "confidence")
predict(lm.fit,data.frame(lstat = c(5,10,15)),interval = "prediction")

# plot lstat and medv
plot(lstat,medv)
abline(lm.fit)
abline(lm.fit,lwd = 3)
abline(lm.fit,lwd = 3,col = "red")

# different plotting options
#plot(lstat,medv,col = "red")
#plot(lstat,medv,pch = 20)
#plot(lstat,medv,pch = "+")
#plot(1:20,1:20,pch = 1:20)

# diagnostic plots
par(mfrow = c(2,2))
plot(lm.fit)

# compute and plot residuals
plot(predict(lm.fit),residuals(lm.fit))
plot(predict(lm.fit),rstudent(lm.fit))

# plot leverage statistics
plot(hatvalues(lm.fit))
# identifies which observation has the largest leverage statistic
which.max(hatvalues(lm.fit))

# multiple linear regression model with medv as the response and lstat and age as the predictor
lm.fit = lm(medv~lstat + age,data = Boston)
summary(lm.fit)

# multiple linear regression model with medv as the response and the rest as predictors
lm.fit = lm(medv~.,data = Boston)
summary(lm.fit)

# used to compute variance inflation factors
vif(lm.fit)

# multiple linear regression model with medv as the response and the rest except age as predictors
lm.fit1 = lm(medv~.-age,data = Boston)
summary(lm.fit1)

# alternative way of creating a linear regression using all predictors except age
#lm.fit1 = update(lm.fit,~.-age)
#summary(lm.fit1)
