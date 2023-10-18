college <- read.csv("College.csv", stringsAsFactors = TRUE)
View(college)

dim(college)
college2 = college[, -1]
View(college2)
summary(college2)
pairs(college2)
pairs(~ Apps + Accept + Enroll + Top10perc + Top25perc, data = college2)
#AppxAccept = postive correlation AcceptxApps = Positive Correlation
#AppsxEnroll = Positive Correlation,  AcceptxEnroll = Positive Correlation
#AppsxTop10Perc = Weak, positive correlation AcceptxTop10Perc = weak positive correlation EnrollxTop10Perc = weak positive correlation
#AppsxTop25Perc = Positive Correlation, AcceptxTop25perc = Positive Correlation, EnrollxTop25perc = weak positive correclation, Top10percxTop25perc = positive correlation

#AppsxEnroll positive correlation, AcceptxEnroll = positive correlation
#AppsxTop10perc = very weak positive correlation, AcceptxTop10Perc = very weak positive correlation, EnrollxTop10Perc = very weak positive correlation
#AppsxTop25perc = weak positive correlation, AcceptxTop25perc, very weak positive correlation, EnrollxTop25perc very weak correlation, Top25percxTop10perc=positive correlation

plot(college$Private, college$Outstate)
par(mfrow=c(1,2))
hist(rnorm(college$Apps), col = "green")
hist(rnorm(college$Accept), col = "red")
