#Question 1
library(ISLR2)
attach(mtcars)
head(mtcars)

#a
cor(mtcars$mpg, mtcars$cyl)
#-0.852162
cor(mtcars$mpg, mtcars$wt)
#-0.8676594

#b
lm.fit <-lm(mpg ~ cyl+ wt, data = mtcars)
summary(lm.fit)
confint(lm.fit)

#e
par(mfrow=c(2,2))
plot(lm.fit)

#f
predict(lm.fit, data.frame(cyl = 192, wt = 3.46), interval = "confidence")
predict(lm.fit, data.frame(cyl = 192, wt = 3.46), interval = "prediction")

#G
plot(predict(lm.fit), residuals(lm.fit)) 

#2.
library(ISLR2)
attach(Boston)
head(Boston)

#a
med = median(crim)
crim01 <- rep(0, nrow(Boston))
crim01[crim>med] = 1
crim01
head(Boston)
Boston <- Boston[, -1]
Boston <-data.frame(crim01, Boston)
head(Boston)
attach(Boston)

#b

dim(Boston)
train <- Boston[1:405,]
dim(train)
test <- Boston[406:506, ]
dim(test)

#c
head(Boston)
lr.fit.train <- glm(crim01 ~ zn + indus + chas + nox + rm + age + dis + rad + tax + ptratio + lstat + medv, data = train, family = binomial)
summary(lr.fit.train)

#D
lr.prob.test <- predict(lr.fit.train, test, type = "response")
lr.pred.test <- rep(0, 101)
lr.pred.test[lr.prob.test > .5] <- 1
test.target <- crim01[406:506]  
table(lr.pred.test, test.target)
mean(lr.pred.test == test.target)
