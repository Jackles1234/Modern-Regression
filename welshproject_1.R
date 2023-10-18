games <- read.csv('games.csv')
head(games)

attach(games)
#Does do reviews effect the price of a game?
#Creating price_dif variable and adding it to the dataset
price_dif <- (price_final - price_original)
games1 <- data.frame(price_dif, games)
#SLR
lm.fit <- lm(price_dif ~ positive_ratio)
summary(lm.fit)
#Correlation shown

#Confidence interval  
median(positive_ratio)
predict(lm.fit, data.frame(positive_ratio = 81), interval = "confidence")

#Creating dummy variable for qualitative variable
#All negative or mixed reviews = 0 and all positive reviews = 1
rev1 <- rep(0, nrow(games1))
rev1['Positive'== rating | 'Very Positive' == rating | 'Mostly Positive' == rating] = 1
games1 <- data.frame(rev1, games1)
head(games1)

#do the reviews itself effect?
#change values so that all negatives = negative, and all positives = positive
#MLR
lm.fit2 <- lm(price_dif ~ positive_ratio +  rev1, data = games1)
summary(lm.fit2)


#Turning the rev variable back into qualitiative for LR
rev2 <- rep('Positive Rating', nrow(games1))
rev2[rev1 ==0] = 'Low Rating'
games1 <- data.frame(rev2, games1)
head(games1)


#Regression Lines:
plot(price_dif, positive_ratio)
abline(lm.fit, col = "blue")

plot(price_dif, rev0)
abline(lm.fit2, col = "blue")


#Diagnostic Plots:
par(mfrow=c(2,2))
plot(lm.fit)

plot(lm.fit2)


#GLM stuff
dim(games1)
train <-games1[1:36854, ]
test <- games1[36855:46068, ]
glm.fit <- glm(price_dif ~ positive_ratio + rev1, data = train)

#Prediction Models
glm.probs <- predict(glm.fit, test, type = "response")
glm.pred.test <- rep(0, 9213)
#}
summary(glm.probs)
#-0.5976 is the median
glm.pred.test[glm.probs > -0.5976 ] <- 1
test.target <- price_dif[36855:46068]
table(glm.pred.test, test.target)
1- mean(glm.pred.test == test.target) 
