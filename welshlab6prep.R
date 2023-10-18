library(ISLR2)
View(Hitters)
names(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))
#Remove Missing Variables
Hitters <- na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))
#regsubsets performs best subset selections by 
#identifying best model. Best is determined with RSS
library(leaps)
regfit.full <- regsubsets(Salary ~., Hitters)
summary(regfit.full)
#asterisk indicates given variable is included in model
#nvmax option returns as many variables as desired
regfit.full <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19)
reg.summary <- summary(regfit.full)
names(reg.summary)
#R^2
reg.summary$rsq
#plotting RSS adjusting R^@, Cp and BIC
par(mfrow = c(2,2))
plot(reg.summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
plot(reg.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted Rsq", type = "l")
#points command is like plot() except it plots points
#which.max ids the max of a vector
which.max(reg.summary$adjr2)
points(11, reg.summary$adjr2[11], col = " red ", cex = 2,pch = 20)

plot(reg.summary$cp, xlab = "Number of variables", ylab = "Cp", type = "l")
which.min(reg.summary$cp)
points(10, reg.summary$cp[10], col = "red", cex = 2, pch = 20)
which.min(reg.summart$bic)
plot(reg.summary$bic, xlab = "Number of variables", ylab = "BIC", type = "l")
points (6, reg.summary$bic[6], col = " red ", cex = 2,pch = 20)
#regsubsets() func has built-in plot() which can display selected vars given predictors
plot(regfit.full, scale = "r2")
plot(regfit.full, scale = "adjr2")
plot(regfit.full, scale = "Cp")
plot(regfit.full, scale = "bic")
#coeg func estimates associated with this model
coef(regfit.full, 6)

#### FORWARD AND BACKRWARD STEPWISE SLECTION####
#Regsubsets can be used for forward and backward stewise selection

regfit.fwd <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19, method = "forward")
summary(regfit.fwd)
regfit.bwd <- regsubsets(Salary ~ ., data = Hitters ,nvmax = 19, method = "backward")
summary(regfit.bwd)
coef(regfit.full, 7)                            
coef(regfit.fwd, 7)
coef(regfit.bwd, 7)

#### CHOOSING AMONG MODELS USING THE SET VALIDATION APPROACH AND CROSS VALIDATION####

set.seed(1)
train <- sample(c(TRUE, FALSE), nrow(Hitters), replace = TRUE)
test <- (!train)
regfit.best <- regsubsets(Salary ~ ., data = Hitters[train, ], nvmax = 19)
#testing data
test.mat <- model.matrix(Salary ~ ., data = Hitters[test, ])
#model.matrix builds X matrix from data
val.errors <- rep(NA, 19)
for(i in 1:19){
  coefi <- coef(regfit.best, id = i)
  pred <- test.mat[, names(coefi)] %*% coefi
  val.errors[i] <- mean((Hitters$Salary[test] - pred)^2)
}
#We find hte best model is the one that contains seven variables
val.errors
which.min(val.errors)
coef(regfit.best, 7)
#predict for regsubsets
predict.regsubsets <- function(object, newdata, id, ...){
  form <- as.formula(object$call[[2]])
  mat <- model.matrix (form , newdata)
  coefi <- coef (object , id = id)
  xvars <- names (coefi)
  mat[, xvars] %*% coefi
}
#select best seven variable model using variable obtained in training
regfit.best <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19)
coef(regfit.best, 7)
#Different set of variables w/ full data has different variables then in training
#choose model of different sizes using cross validation
k <- 10
n <- nrow(Hitters)
set.seed(1)
folds <- sample(rep(1:k, length = n))
cv.errors <- matrix(NA, k, 19, dimnames = list(NULL, paste(1:19)))
#Loop that performs cross validation and make predictions
for (j in 1:k){
  best.fit <- regsubsets(Salary ~ ., data = Hitters[folds != j, ], nvmax = 19)
  for (i in 1:19){
    pred <- predict(best.fit, Hitters[folds == j, ], id = i)
    cv.errors[j, i] <- mean((Hitters$Salary[folds == j] - pred)^2)
  }
}
#gives us 10x19 matrix
#apply() func to average over cols of this matrix
mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors
# we see cross validation selects 10-variable model
reg.best <- regsubsets(Salary ~ ., data = Hitters ,nvmax = 19)
coef(reg.best, 10) 

####6.5.2####

#perfrom ridge regress and lasso in order to predict slary on hitters data
x <- model.matrix(Salary ~., Hitters)[, -1]
y <- Hitters$Salary
#Ridge Regression
library(glmnet)
grid <- 10^seq(10, -2, length = 100)
ridge.mod <- glmnet(x, y, alpha = 0, lambda = grid)
#automatically selects lambda values (10^10, 10^-2)
dim(coef(ridge.mod))
ridge.mod$lambda[50]
coef(ridge.mod)[, 50]
sqrt(sum(coef(ridge.mod)[-1, 50]^2))
#larger l2 norm associated with smaller lambda
ridge.mod$lambda[60]
coef(ridge.mod)[, 60]
sqrt(sum(coef(ridge.mod)[-1, 60]^2))
#use predict func or a number of purposes. ridge regression with lambda 50
predict(ridge.mod, s= 50, type = "coefficients")[1:20, ]
#split samples into training and testing
set.seed(1)
train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train)
y.test <- y[test]
#Next fit a ridge regression model on the training set and eval MSE using lambda =4
ridge.mod <- glmnet(x[train, ], y[train], alpha = 0, lambda = grid, thresh = 1e-12)
ridge.pred <- predict(ridge.mod, s = 4, newx = x[test, ])
mean((ridge.pred - y.test)^2)
#test mse is 142199.2
#we can compute the test set mse like this
mean((mean(y[train]) - y.test)^2)
#we can also get same result here
ridge.pred <- predict (ridge.mod , s = 1e10 , newx = x[test , ])
mean ((ridge.pred - y.test)^2)

#lamba for lead to lower test MSE
#now we perform ridge regression with lambda =4
ridge.pred <- predict(ridge.mod, s = 0, newx = x[test, ], exact = T, x = x[train, ], y = y[train])
mean((ridge.pred - y.test)^2)
lm(y ~ x, subset = train)
predict(ridge.mod, s = 0, exact = T, type = "coefficients", x = x[train, ], y = y[train])[1:20, ]
#it would be better to use a cross validation to choose lambda
#nfolds can change from ten fold cross validation
set.seed(1)
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 0)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam
#test mse associate with new lambda = 326
ridge.pred <- predict(ridge.mod , s = bestlam ,newx = x[test , ])
mean ((ridge.pred - y.test)^2)
#improvement over the test MSE
out <- glmnet(x, y, alpha = 0)
predict(out, type = "coefficients", s = bestlam)[1:20]

#The Lasso
lasso.mod <- glmnet(x[train, ], y[train], alpha = 1, lambda = grid)
plot(lasso.mod)
#perform cross validation and compute associated test error
set.seed(1)
cv.out <- cv.glmnet(x[train , ], y[train], alpha = 1)
plot (cv.out)
bestlam <- scv.out$lambda.min
lasso.pred <- predict(lasso.mod , s = bestlam ,newx = x[test , ])
mean ((lasso.pred - y.test)^2)
#this is lower than the test set MSE
#However, Lass has advantage over ridge regression.
#coefficient estimates are sparse.
#8/19 coefficients are 0
out <- glmnet (x, y, alpha = 1, lambda = grid)
lasso.coef <- predict(out , type = "coefficients",s = bestlam)[1:20, ]
lasso.coef

                         
                      