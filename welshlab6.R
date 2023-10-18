library(ISLR2)
dim(College)
head(College)
attach(College)
x <- model.matrix(Apps ~., College)[, -1]

#2
set.seed(1)
train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train)
y.test <- y[test]

#3
library (leaps)
regfit.full <- regsubsets (Apps ~ ., College)
reg.summary <-summary(regfit.full)
reg.summary$rsq

#4
library(glmnet)
y <- College$Apps
grid <- 10^seq(10, -2, length = 100)
ridge.mod <- glmnet(x, y, alpha = 0, lambda = grid)
dim(coef(ridge.mod))
ridge.mod$lambda[50]
coef(ridge.mod)[, 50]
sqrt(sum(coef(ridge.mod)[-1, 50]^2))

#5
out <- glmnet (x, y, alpha = 0)
predict(ridge.mod, s = 50, type = "coefficients")[1:18, ]

#6
lasso.mod <- glmnet(x[train, ], y[train], alpha = 1, lambda = grid)
set.seed(1)
cv.out <- cv.glmnet(x[train , ], y[train], alpha = 1)
plot (cv.out)
bestlam <- cv.out$lambda.min
lasso.pred <- predict(lasso.mod , s = bestlam ,newx = x[test , ])
mean ((lasso.pred - y.test)^2)

#7
out2 <- glmnet (x, y, alpha = 1, lambda = grid)
lasso.coef <- predict (out2 , type = "coefficients", s = bestlam)[1:18, ]
lasso.coef
