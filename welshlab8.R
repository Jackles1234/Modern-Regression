#a
library(ISLR2)
library(e1071)
attach(OJ)
head(OJ)
dim(OJ)


#b
#dat <- data.frame(x = OJ("Purchase"), y = as.factor(OJ("WeekofPurchase",  "StoreID", "PriceCH", "PriceMM", "DiscCH", "DiscMM", "SpecialCH",  "SpecialMM", "LoyalCH", "SalePriceMM", "SalePriceCH", "PriceDiff", "Store7", "PctDiscMM", "PctDiscCH", "ListPriceDiff", "STORE")))
train <- OJ[0:800, ]
test <- OJ[801:1070, ]

#c

svm.fit <- svm(Purchase ~ ., data = train, kernel = "linear", cost = .01)
summary(svm.fit)
plot(svm.fit, test)

#d
pred <- predict(svm.fit, test[,-1],type = "class")
100*mean(pred==test[,1])
#79.25926

#Tuned
tune.out <- tune(svm, Purchase ~., data = train, kernal = "linear", ranges = list(cost = c( 0.01, 0.1, 1, 2.5, 5, 7.5, 10)))
summary(tune.out)
table(true = test$Purchase, pred = predict(tune.out$best.model, newdata = test))
35 + 14
142+14+35+79
49/270
#Regular: 20.74074% test error
#Tuned: 18% test error

#e
svm.fit2 <- svm(Purchase ~ ., data = train, kernel = "radial", gamma = 1, cost = .01)
summary(svm.fit2)
plot(svm.fit2, test)

pred1 <- predict(svm.fit2, test[,-1],type = "class")
100*mean(pred1==test[,1])
#57.777781

tune.out1 <- tune(svm, Purchase ~., data = train, kernel = "radial", gamma = 1, ranges = list(cost = c( 0.01, 0.1, 1, 2.5, 5, 7.5, 10)))
summary(tune.out1)
table(true = test$Purchase, pred1 = predict(tune.out1$best.model, newdata = test))

17+40
139+17+40+74
57/270
#Regular: 42.22222
#Tuned: 0.2111111 error rate
#Tuned Linear model seemed to perform the best.

#f
detach(OJ)
attach(USArrests)
head(USArrests)
dim(USArrests)

#g
hc.complete <- hclust(dist(USArrests), method = "complete")

#h
plot(hc.complete , main = "Complete Linkage", xlab = "", sub = "", cex = .9)
cutree(hc.complete , 3) 
#1: Alabama, Alaska, Arizona, California, Delaware, Florida, Illinois, Maryland, Michigan, Mississippi, Nevada, New mexico, New York, North Carolina, South Carolina, 
#2: Arkansas, Colorado, Georgia, Massachusetts, Missouri, New Jersey, Oklahoma, Oregon, Texas, Virginia, Washington, Wyoming
#3: Connecticut, Hawaii, Idaho, Indiana, Iowa, Kansas, Kentucky, Maine, Montana, Minnesota, Nebraska, New Hampshire, North Dakota, Ohio, Pennsylvania, South Dakota, Utah, Vermont, West Virgina, Wisconsin

#i
xsc <- scale(USArrests)
hc.complete2 <- hclust(dist(xsc), method = "complete")
plot(hc.complete2 , main = "Scaled Complete Linkage", xlab = "", sub = "", cex = .9)
cutree(hc.complete2 , 3)

#Alabama         Alaska        Arizona 
#1              1              2 
#Arkansas     California       Colorado 
#3              2              2 
#Connecticut       Delaware        Florida 
#3              3              2 
#Georgia         Hawaii          Idaho 
#1              3              3 
#Illinois        Indiana           Iowa 
#2              3              3 
#Kansas       Kentucky      Louisiana 
#3              3              1 
#Maine       Maryland  Massachusetts 
#3              2              3 
#Michigan      Minnesota    Mississippi 
#2              3              1 
#Missouri        Montana       Nebraska 
#3              3              3 
#Nevada  New Hampshire     New Jersey 
#2              3              3 
#New Mexico       New York North Carolina 
#2              2              1 
#North Dakota           Ohio       Oklahoma 
#3              3              3 
#Oregon   Pennsylvania   Rhode Island 
#3              3              3 
#South Carolina   South Dakota      Tennessee 
#1              3              1 
#Texas           Utah        Vermont 
#2              3              3 
#Virginia     Washington  West Virginia 
#3              3              3 
#Wisconsin        Wyoming 
#3              3 

#j

#Overall the model became more complex when scaling. 





