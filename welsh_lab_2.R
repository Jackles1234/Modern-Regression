carseats = read.csv("Carseats.csv", stringsAsFactors = TRUE)
dim(carseats) 
head(Carseats)
attach(Carseats)
lm.fit <- lm(Sales ~ Price)
summary(lm.fit)
cor(Sales, Price)
predict(lm.fit, data.frame(Price=coef(lm.fit)), interval = "confidence")
predict(lm.fit, data.frame(Sales = 192))
predict(lm.fit, data.frame(Sales = 192), interval = "confidence")
predict(lm.fit, data.frame(Sales = 192), interval = "prediction")
plot(Price, Sales)
abline(lm.fit, lwd = 3, col = "blue")
par(mfrow = c(2,2))
plot(lm.fit)


