x <- c(1, 3, 2)#C = concatenate
x
x = c(1, 6,2)
y = c(1, 4, 3)
length(x)
length(y) #adding vectors
x + y

ls()
rm(x, y) #deletes vars
ls()
rm(list = ls())   #Deletes all objs at once
?matrix

x = matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)
x
# or
x = matrix(c(1,2,3,4), 2, 2)
x
matrix(c(1,2,3,4),2,2, byrow = TRUE) 
#Byrow populates matix in order of rows
sqrt(x)
x^2
x = rnorm(50)
y = x + rnorm(50, mean = 50, sd = )
cor(x, y)
#rnorm(n, mean = _, sd = _) generates random vars
#cor() computes the correlation between them
set.seed(1303)
rnorm(50)
#set.seed() reproduces the exact same set of ran nums

set.seed(3)
y = rnorm(100)
mean(y)
var(y)
sqrt(var(y))
sd(y)

# _________________Graphs_______________________
#plot = primary way to plot data
#xlab = will result in label on x axis
x = rnorm(100)
y = rnorm(100)
plot(x, y)
plot(x, y, xlab= "X-Axis", ylab = "Y-Axis", main = "plot of X and Y")
#pdf = creates pdf
#jpg = creates jpg
pdf("Figure.pdf")
plot(x, y, col = "green")
dev.off()
#dev.off = indicates when done creating plot
#seq(a, b) = creates a sequence of nums between a, b
x <- seq(1, 10)
x
x <- 1:10#  does the same
x
x <- seq(-pi, pi,length = 50)

#contour() creates contour plot for 3-dimensional data

y <- x
f <- outer(x, y, function(x, y) cos(y)/ (1 + x^2))
contour(x, y, f)
contour(x, y, f, nlevels = 45, add = T)
fa <- (f - t(f))/ 2
contour(x, y, fa, nlevels = 15)

#image() works the smae way as countour()
#image() produces a color-coded plot. Color depends on z-valute
#persp() can be used to produce a 3-dim plot. 
#theta and phi control angles at which the plot is viewed
image(x, y, fa)
persp(x, y, fa)
persp(x, y, fa, theta = 30)
persp(x, y, fa, theta = 30, phi = 20)
persp(x, y, fa, theta = 30, phi = 70)
persp(x, y, fa, theta = 30, phi = 40)

#_______________Indexing Data___________________

A = matrix(1:16, 4, 4)
A
A[2, 3]
A[c(1, 3), c(2, 4)]
A[1:3, 2:4]
A[1:2, ]
A[, 1:2]
#single rows are vectors
A[1, ]
#use of negative sign tells R to keep all rows except the indicated index
A[-c(1,3), ]
A[-c(1,3), -c(1,3,4)]
#dim() outputs the numebr of rows and columns
dim(A)
#___________________Loading Data_________________

#read.table() imports a data set
#write.table() exports data

Auto <- read.table("Auto.data")
View(Auto)
head(Auto)

Auto <- read.table("Auto.data", header = T, na.strings = "?", stringsAsFactor = T)
View(Auto)
#StringAsFactors = T tells R that any variabble containing character strings should be interpreted as qualitative Variable
#read.csv() loads data from excel into R ands aves it as a csv
Auto <- read.csv("Auto.csv", na.strings = "?", stringsAsFactors = T)
View(Auto)
dim(Auto)
Auto[1:4, ]
#dim() function tells us that the data has 397 ovservations, or rows and nine variables, or columns
#na.omit() omits rows
Auto <- na.omit(Auto)
dim(Auto)
#names() to check the variable names are loaded correctlu
names(Auto)

#______Addional Graphical and Numerical Summaries_____

#plot() func to produce scatterplots of quant variables
plot(Auto$cylinders, Auto$mpg)
attach(Auto)
plot(cylinders, mpg)
#as.factor() converts quant ot qualit vars
cylinders <- as.factor(cylinders)
plot(cylinders, mpg)
plot(cylinders, mpg, col = "red")
plot(cylinders, mpg, col = "red", varwidth = T)
plot(cylinders, mpg, col = "red", varwidth = T, horizontal = T)
plot(cylinders, mpg, col = "red", varwidth = T, xlab = "cylinders", ylab = "MPG")
#hist() creates histogram
hist(mpg)
hist(mpg, col = 2)
hist(mpg, col = 2, breaks = 15)
#pairs creates a scatterplot
pairs(Auto)
#pairs (mpg + displacement + horsepower + weight + acceleration ,data = Auto)
#identify provides useful interactive method for identifying val of a partifuclar var
plot(horsepower, mpg)
identify(horsepower, mpg, name)
#summary provides a numerical summary of each var
summary(Auto)
rm(list = ls()) 
