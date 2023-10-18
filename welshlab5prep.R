library(ISLR2)
set.seed(1)
#seed so we get the same rng 
train <- sample(392, 196)
lm.fit <- lm(mpg ~ horsepower, data = Auto, subset = train)
#-train will index anything not in the train dataset
attach(Auto)
mean((mpg - predict(lm.fit, Auto))[-train]^2)
#calculates mse
#poly() estimates the test errror for the qaud and cube regression
lm.fit2 <- lm(mpg ~ poly(horsepower, 2), data = Auto, subset = train)
mean((mpg - predict(lm.fit2, Auto))[-train]^2)

lm.fit3 <- lm(mpg ~ poly(horsepower, 3), data = Auto, subset = train)
mean((mpg - predict(lm.fit3, Auto))[-train]^2)
#18.71646 vs 18.79401

set.seed(2)
train <- sample(392, 196)
mean ((mpg - predict (lm.fit , Auto))[-train ]^2)
lm.fit2 <- lm(mpg ~ poly(horsepower, 2), data = Auto, subset = train)
mean((mpg - predict(lm.fit2, Auto))[-train]^2)

lm.fit3 <- lm(mpg ~ poly(horsepower, 3), data =Auto, subset = train)
mean((mpg - predict(lm.fit3, Auto))[-train]^2)
#20.43036 vs 20.38533

#5.3.2

#LOOCV estimate can be computed using glm and cv.glm()
glm.fit <- glm(mpg ~ horsepower, data = Auto)
coef(glm.fit)

lm.fit <- lm(mpg ~ horsepower, data = Auto)
coef(lm.fit)
#Same results

library(boot)
glm.fit <- glm(mpg ~ horsepower, data = Auto)
cv.err <- cv.glm(Auto, glm.fit)
cv.err$delta


#produces a list with teh two numbers in teh delta vector
#Our cross-validation estimate for the test error is ~ 24.23

cv.error <- rep(0, 10)
for (i in 1:10) {
  glm.fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error[i] <- cv.glm(Auto, glm.fit)$delta[1]
}

cv.error

#drop between test mse between linear and quadratic fits

#5.3.3

#cv.glm can be used to imlement k-fold CV. 
set.seed(17)
cv.error.10 <- rep(0, 10)
for (i in 1:10){
  glm.fit<-glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error.10[i] <- cv.glm(Auto, glm.fit, K = 10)$delta[1]
}
cv.error.10
#Compuatation time is shorter than LOOCV