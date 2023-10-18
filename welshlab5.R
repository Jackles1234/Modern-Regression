#1
library(ISLR2)
attach(Carseats)
dim(Carseats)
head(Carseats)

#400 rows, 11 cols

#2
lm.fit <- lm(Sales ~ Income + Price + Advertising)

#3
set.seed(1)
train <- sample(400, 320)
lm.fit <- lm(Sales ~ Income + Price + Advertising, data = Carseats, subset = train)
mean((Sales - predict(lm.fit, Carseats))[-train]^2)
# 5.953015

#4
library(boot)
glm.fit <- glm(Sales ~ Income + Price + Advertising, data = Carseats)
cv.err <- cv.glm(Carseats, glm.fit)
cv.err$delta
#[1] 5.731485 5.731343
#The two numbers are nearly identical 

#5
cv.error <- rep(0, 10)
for(i in 1:10){
  glm.fit <- glm(Sales ~ poly(Income + Price + Advertising, i), data = Carseats)
  cv.error[i] <- cv.glm(Carseats, glm.fit)$delta[1]
}
cv.error
#1] 7.918097 7.943413 7.899518 7.926112 7.955943
#[6] 8.004765 8.017110 8.095867 8.072014 8.165740

#The MSE is gradually improving when using higher order polynomials. 

#6
#Most likely