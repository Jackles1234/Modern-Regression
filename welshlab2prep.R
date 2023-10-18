library(MASS)
head(Boston)
attach (Boston) #Sets boston as default set
lm.fit <- lm(medv ~ lstat)  #least squares  regression set lm(y~x,data)
lm.fit
summary(lm.fit)
names(lm.fit) #find out what other pieces of info are stored in lmfit
coef(lm.fit)  #extracts quantities by name
confint(lm.fit) #obtains conidence interval for coeff estimates
predict(lm.fit, data.frame(lstat = (c(5, 10, 15))), interval = "confidence")
#used to produce confidence intervals and prediction intervals for x + y
predict(lm.fit, data.frame(lstat = (c(5, 10, 15))), interval = "prediction")
#95% conf for lstat of 10 is (24.47, 25.63) and 95% prediciton interval is (12.828, 37.28)
plot(lstat, medv)
abline(lm.fit, col = "blue")  #gives lsr line
abline(lm.fit, lwd = 3)   #lwd = width of regression increased by 3
abline(lm.fit, lwd = 3, col = "red")
plot(lstat, medv, col = "red")
plot(lstat, medv, pch = 20)   #pch = different plotting symbols
plot(lstat, medv, pch = "+")
plot(1:20, 1:20, pch = 1:20)
par(mfrow = c(2,2)) #divdes hte lotting region into 2x2 grid of panels
plot(lm.fit)
plot(predict(lm.fit), residuals(lm.fit))  #computes residuals
plot(predict(lm.fit), rstudent(lm.fit))   #rstudent returns studenized residuals . Plots against fitted values

plot(hatvalues(lm.fit)) #leverage stats can be computed for any number of predictors
which.max(hatvalues(lm.fit))  #identifies index of the largest element of a vector

