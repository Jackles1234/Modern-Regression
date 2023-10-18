#A
library(ISLR2)
head(Carseats)
dim(Carseats)
# 400  12
sum(is.na(Carseats))
#B
attach(Carseats)
train <- sample(1:nrow(Carseats), nrow(Carseats)/2)
Carseats.test <- Carseats[-train, ]
Sales.test <- Sales[-train]

#C
tree.carseats <- tree(Sales ~ ., Carseats, subset = train)
plot(tree.carseats)
text(tree.carseats, pretty = 0)
# The tree seems to start at ShelveLoc and Price, going down to some of the more obscure variables.
#19 terminal nodes
#17 internal nodes

#D
yhat <- predict(tree.carseats, newdata = Carseats[-train, ])
mean((yhat - Sales.test)^2)

#MSE = 5.301027

#E
cv.carseats <- cv.tree(tree.carseats)
plot(cv.carseats$size, cv.carseats$dev, type = "b")

#$size
#[1] 19 18 17 16 14 13 12 11  9  7  5
#[12]  4  3  2  1

#$dev
#[1] 1137.856 1154.053 1156.298
#[4] 1184.387 1188.660 1198.494
#[10] 1093.573 1122.667 1137.392

#7 is the most optimal

#F
prune.carseats <- prune.tree(tree.carseats, best = 7)
plot(prune.carseats)
text(prune.carseats, pretty = 0)
yhat <- predict(prune.carseats, newdata = Carseats[-train, ])
mean((yhat - Sales.test)^2)
#MSE = 5.412611
#No, it did not improve the MSE

#G
library(randomForest)
set.seed(1)
rf.carseats <- randomForest(Sales ~., data = Carseats, subset = train, mtry = 10, importance = TRUE)
yhat.rf <- predict(rf.carseats, newdata = Carseats[-train, ])
mean((yhat.rf - Sales.test)^2)
#MSE = 2.949105

#H
importance(rf.carseats)
varImpPlot(rf.carseats)
#ShelvLoc and Price seem to be the most important variables

#I

rf.carseats <- randomForest(Sales ~., data = Carseats, subset = train, mtry = 6, importance = TRUE)
yhat.rf <- predict(rf.carseats, newdata = Carseats[-train, ])
mean((yhat.rf - Sales.test)^2)
#MSE = 2.898812

rf.carseats <- randomForest(Sales ~., data = Carseats, subset = train, mtry = 3, importance = TRUE)
yhat.rf <- predict(rf.carseats, newdata = Carseats[-train, ])
mean((yhat.rf - Sales.test)^2)

#MSE = 3.323913

#when changing the m value, it seems that around 6-10 gives a MSE ~3. 
#However, going below 6 is when the MSE goes above 3 drastically. 
#So between 6-10 variables, the MSE remains relatively the same, but dipping below 6 causes the MSE to shoot up.