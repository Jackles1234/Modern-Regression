#1
library(ISLR2)
attach(Auto)
install.packages("corrplot")
library(corrplot)
auto1 = (Auto[-9])
#2
plot(Auto)
correlations<-cor(auto1)
correlations
corrplot(correlations, "shade")

#3
lm.fit <- lm(mpg~ . -name, data= Auto)
summary(lm.fit)
#5
#vif(lm.fit)
par(mfrow = c(2,2))
plot(lm.fit)
abline(lm.fit)

vif(lm.fit)
#6
lm.fit1 = lm(mpg~ weight*origin)
summary(lm.fit1)
par(mfrow = c(2,2))
plot(lm.fit1)

