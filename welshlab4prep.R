#4.7.1

library(ISLR2)
names(Smarket)
dim(Smarket)
summary(Smarket)
pairs(Smarket)

cor(Smarket[,-9])

detach(Weekly)
attach(Smarket)

#4.7.2

glm.fits <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Smarket, family = binomial)
summary(glm.fits)
#GLM used to fit many types of gneralized linear models
#Ther smallest p-value is Lag1, but its relativly large so correlation cannot be established

#Coef used to access coefficients for this model

coef(glm.fits)
summary(glm.fits)$coef
summary(glm.fits)$coef[, 4]

#predict func can be used to predict probability the market will go up

glm.probs <- predict(glm.fits, type = "response")
glm.probs[1:10]
contrasts(Direction)
#contrasts idnicates that R has craeted a dummy variable with a 1 for up

glm.pred <- rep("Down", 1250)
glm.pred[glm.probs > .5] = "Up"

#We must convert these predicted probablities into class label, up or down

table(glm.pred, Direction)
#Gives us table representation of correct predictions and actual
mean(glm.pred == Direction)
#Gives us the percent correct

train <- (Year <2005)
Smarket.2005 <- Smarket[!train, ]
dim(Smarket.2005)
Direction.2005 <- Direction[!train]
#set the training set adn teh testing set

glm.fits <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Smarket, family = binomial, subset = train)
glm.probs <- predict(glm.fits, Smarket.2005, type = "response")
# Trained and tested both of our models

glm.pred <- rep("Down", 252)
glm.pred[glm.probs > .5] <- "Up"
table(glm.pred, Direction.2005)

mean(glm.pred == Direction.2005)
mean(glm.pred != Direction.2005)

#This is showing the data from the training and testing set.

glm.fits <- glm(Direction ~ Lag1 + Lag2, data = Smarket, family = binomial, subset = train)
glm.probs <- predict(glm.fits, Smarket.2005, type = "response")
glm.pred <- rep("Down", 252)
glm.pred[glm.probs > .5] <- "Up"
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)

predict(glm.fits, newdata = data.frame(Lag1 = c(1.2, 1.5), Lag2 = c(1.1, -.8)), type = "response")
glm.prob.test <- predict(glm.fits)
        