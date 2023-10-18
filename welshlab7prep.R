library(tree)
library(ISLR2)
attach(Carseats)
High <- factor(ifelse(Sales <= 8, "No", "Yes"))
Carseats <- data.frame (Carseats , High)
tree.carseats <- tree(High ~. -Sales, Carseats)
summary(tree.carseats)
#training error = 9%
#plot to display the tree and text for labels
plot(tree.carseats)
text(tree.carseats, pretty = 0)
#most important indicator of sales is shelving location
tree.carseats
#Need to estimate test error rather than simply computing training error
#type = class instructs to reutnr class prediction
set.seed(2)
train <- s+ample(1: nrow(Carseats), 200)
Carseats.test <- Carseats[-train, ]
High.test <- High[-train]
tree.carseats <- tree(High ~ . - Sales, Carseats, subset = train)
tree.pred <- predict(tree.carseats, Carseats.test, type = "class")
table(tree.pred, High.test)
#Next see if pruning the tree might lead to improved results.
#cv.tree perfroms Cross-validatiom to determine optimal complexity

set.seed(7)
cv.carseats <- cv.tree(tree.carseats, FUN = prune.misclass)
names(cv.carseats)
cv.carseats
#dev corresponds to number of cross validation errors
par(mfrow = c(1,2))
plot(cv.carseats$size, cv.carseats$dev, type = "b")
plot(cv.carseats$k, cv.carseats$dev, type = "b")
#prune.misclass() func in order to prune the tree ot obtain nine node tree
prune.carseats <- prune.misclass(tree.carseats, best = 9)
plot(prune.carseats)
text(prune.carseats, pretty = 0)

#pruned tree perform on test data. Apply predict func
tree.pred <- predict(prune.carseats, Carseats.test, type = "class")
table(tree.pred, High.test)
(97+58)/200
#Increasing value of best, we obtain a larger prunned tree with lower accuracy
prune.carseats <- prune.misclass(tree.carseats, best = 14)
plot(prune.carseats)
text(prune.carseats, pretty = 0)
tree.pred <- predict(prune.carseats, Carseats.test, type = "class")
table(tree.pred, High.test)
(102 + 52)/200

#8.3.2
set.seed(1)
train <- sample(1:nrow(Boston), nrow(Boston/2))
tree.boston <- tree(medv ~., Boston, subset = train)
summary(tree.boston)
#Summary shows only four variables have been used in constructing the tree. 
plot(tree.boston)
text(tree.boston, pretty = 0)
#Now use the cv.tree func to see whther pruning the tree will improve preformance
cv.boston <- cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type = "b")
#using prune.tree() to prune 
prune.boston <- prune.tree(tree.boston, best = 5)
plot(prune.boston)
text(prune.boston, pretty = 0)
#use the unpruned tree to make predictions on test set
yhat <- predict(tree.boston, newdata = Boston[-train, ])
boston.test <- Boston[-train, "medv"]
plot(yhat, boston.test)
abline (0, 1)
mean((yhat - boston.test)^2)

####8.3.3####

#random forests
library(randomForest)
set.seed(1)
bag.boston <- randomForest(medv ~ ., data = Boston, subset = train, mtry = 12, importance = TRUE)
bag.boston
#mtry = 12 indidactes that all 12 predictors should be considered. 
yhat.bag <- predict(bag.boston, newdata = Boston[-train, ])
plot(yhat.bag, boston.test)
abline(0,1)
mean((yhat.bag - boston.test)^2)
#Growing a random forest proceeds in exactly teh same way  except we use smaller mtry value
set.seed(1)
rf.boston <- randomForest(medv ~., data = Boston, subset = train, mtry = 6, importance = TRUE)
yhat.rf <- predict(rf.boston, newdata = Boston[-train, ])
mean((yhat.rf - boston.test)^2)
#test set MSE is 20.07. 
#importance func we can view importance of each variable
importance(rf.boston)
varImpPlot(rf.boston)
