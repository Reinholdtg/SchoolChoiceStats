data<-read.table(file.choose(),header=T) # PriorityRO.csv
names(data)<-c("Choice", "Student", "Count","Share", "Reserve")
attach(data)
detach(data)
head(data)
tail(data)
summary(data)

par(mfrow=c(1,1))


#------------------------------- FIRST CHOICE -------------------------------#
#--SUBSETS--#
over1 <- data[which(data$Choice==0),]
mino1 <- data[which(data$Choice==0 & data$Student=='-'),]
majo1 <- data[which(data$Choice==0 & data$Student=='+'),]

#VAR CAL#
o0 <- data[which(data$Choice==0 & data$Reserve==0),]
o0.5 <- data[which(data$Choice==0 & data$Reserve==.5),]
o1 <- data[which(data$Choice==0 & data$Reserve==1),]

mino0 <- data[which(data$Choice==0 & data$Reserve==0 & data$Student=='-'),]
mino0.5 <- data[which(data$Choice==0 & data$Reserve==.5 & data$Student=='-'),]
mino1 <- data[which(data$Choice==0 & data$Reserve==1 & data$Student=='-'),]

majo0 <- data[which(data$Choice==0 & data$Reserve==0 & data$Student=='+'),]
majo0.5 <- data[which(data$Choice==0 & data$Reserve==.5 & data$Student=='+'),]
majo1 <- data[which(data$Choice==0 & data$Reserve==1 & data$Student=='+'),]

#MEANS#
means_over1 <-by(2*over1$Share,over1$Reserve,mean)
mean_mino1<- by(2*mino1$Share,mino1$Reserve,mean)
mean_majo1<- by(2*majo1$Share,majo1$Reserve,mean)

aggregate(over1$Share*2, list(over1$Reserve), mean)
aggregate(mino1$Share*2, list(mino1$Reserve), mean)
aggregate(majo1$Share*2, list(majo1$Reserve), mean)


mean(over1$Share*2) #0.7845


#--OVERALL FIRST CHOICE--#
boxplot(2*Share~Reserve, data=over1, medcol = "red", main="First Choice - All Students", xlab="Reserve Size", ylab="Share of First Choice")
means_over1 <-by(2*over1$Share,over1$Reserve,mean)
points(means_over1, pch=17,col="orange")

fit_over1 <- lm(2*Share~Reserve+Student, data=over1)
fit_over1
summary(fit_over1)

par(mfrow=c(2,2))
plot(fit_over1)


#--ELIGIBLE FIRST CHOICE--#
boxplot(2*Share~Reserve, data=mino1,medcol = "red", main="First Choice - Eligible Students",xlab="Reserve Size", ylab="Share of First Choice")
mean_mino1<- by(2*mino1$Share,mino1$Reserve,mean)
points(mean_mino1, pch=17,col="orange")

fit_mino1 <- lm(2*Share~Reserve, data=mino1)
fit_mino1
summary(fit_mino1)

par(mfrow=c(2,2))
plot(fit_mino1)


#--INELIGIBLE FIRST CHOICE--#
boxplot(2*Share~Reserve, data=majo1, medcol = "red", main="First Choice - Ineligible Students",xlab="Reserve Size", ylab="Share of First Choice")
mean_majo1<- by(2*majo1$Share,majo1$Reserve,mean)
points(mean_majo1, pch=17,col="orange")

fit_majo1 <- lm(2*Share~Reserve, data=majo1)
fit_majo1
summary(fit_majo1)

par(mfrow=c(2,2))
plot(fit_majo1)


#--COMBINED--#
par(mfrow=c(3,1))
boxplot(2*Share~Reserve, data=over1, medcol = "red", main="First Choice - All Students", xlab="Reserve Size", ylab="Share of First Choice")
means_over1 <-by(2*over1$Share,over1$Reserve,mean)
points(means_over1, pch=17,col="orange")
boxplot(2*Share~Reserve, data=mino1,medcol = "red", main="First Choice - Eligible Students",xlab="Reserve Size", ylab="Share of First Choice")
mean_mino1<- by(2*mino1$Share,mino1$Reserve,mean)
points(mean_mino1, pch=17,col="orange")
boxplot(2*Share~Reserve, data=majo1, medcol = "red", main="First Choice - Ineligible Students",xlab="Reserve Size", ylab="Share of First Choice")
mean_majo1<- by(2*majo1$Share,majo1$Reserve,mean)
points(mean_majo1, pch=17,col="orange")

png(file="First Choice - RO.png",width=3150, height=4455, res=500)
par(mfrow=c(3,1))
boxplot(2*Share~Reserve, data=over1, medcol = "red", main="First Choice - All Students", xlab="Reserve Size", ylab="Share of First Choice")
points(means_over1, pch=17,col="orange")
boxplot(2*Share~Reserve, data=mino1,medcol = "red", main="First Choice - Eligible Students",xlab="Reserve Size", ylab="Share of First Choice")
points(mean_mino1, pch=17,col="orange")
boxplot(2*Share~Reserve, data=majo1, medcol = "red", main="First Choice - Ineligible Students",xlab="Reserve Size", ylab="Share of First Choice")
points(mean_majo1, pch=17,col="orange")
dev.off()


var(o0$Share) 		#0.01300766
var(o0.5$Share)		#0.00270415
var(o1$Share)		#0.01299174

var(mino0$Share) 		#0.006962654
var(mino0.5$Share)	#0.002706365
var(mino1$Share)		#0.0009351612

var(majo0$Share) 		#0.0009383809
var(majo0.5$Share)	#0.002700995
var(majo1$Share)		#0.007151145


#------------------------------- FIFTH CHOICE -------------------------------#
#--SUBSETS--#
over5 <- data[which(data$Choice==4),]
mino5 <- data[which(data$Choice==4 & data$Student=='-'),]
majo5 <- data[which(data$Choice==4 & data$Student=='+'),]

#MEANS#
means_over5 <-by(2*over5$Share,over5$Reserve,mean)
mean_mino5<- by(2*mino5$Share,mino5$Reserve,mean)
mean_majo5<- by(2*majo5$Share,majo5$Reserve,mean)


#--OVERALL FIFTH CHOICE--#
boxplot(2*Share~Reserve, data=over5, medcol = "red", main="Fifth Choice - All Students", xlab="Reserve Size", ylab="Share of Fifth Choice")
means_over5 <-by(2*over5$Share,over5$Reserve,mean)
points(means_over5, pch=17,col="orange")

fit_over5 <- lm(2*Share~Reserve+Student, data=over5)
fit_over5
summary(fit_over5)


par(mfrow=c(2,2))
plot(fit_over5)


#--MINORITY FIFHT CHOICE--#
boxplot(2*Share~Reserve, data=mino5,medcol = "red", main="Fifth Choice - Eligible Students",xlab="Reserve Size", ylab="Share of Fifth Choice")
mean_mino5<- by(2*mino5$Share,mino5$Reserve,mean)
points(mean_mino1, pch=17,col="orange")

fit_mino5 <- lm(2*Share~Reserve, data=mino5)
fit_mino5
summary(fit_mino5)

par(mfrow=c(2,2))
plot(fit_mino5)


#--MAJORITY FIFTH CHOICE--#
boxplot(2*Share~Reserve, data=majo5, medcol = "red", main="Fifth Choice - Ineligible Students",xlab="Reserve Size", ylab="Share of Fifth Choice")
mean_majo5<- by(2*majo5$Share,majo5$Reserve,mean)
points(mean_majo5, pch=17,col="orange")

fit_majo5 <- lm(2*Share~Reserve, data=majo5)
fit_majo5
summary(fit_majo5)

par(mfrow=c(2,2))
plot(fit_majo5)


#--COMBINED--#
par(mfrow=c(3,1))
boxplot(2*Share~Reserve, data=over5, medcol = "red", main="a) Fifth Choice - All Students", xlab="Reserve Size", ylab="Share of Fifth Choice")
means_over5 <-by(2*over5$Share,over5$Reserve,mean)
points(means_over5, pch=17,col="orange")
boxplot(2*Share~Reserve, data=mino5,medcol = "red", main="b) Fifth Choice - Eligible Students",xlab="Reserve Size", ylab="Share of Fifth Choice")
mean_mino5<- by(2*mino5$Share,mino5$Reserve,mean)
points(mean_mino5, pch=17,col="orange")
boxplot(2*Share~Reserve, data=majo5, medcol = "red", main="c) Fifth Choice - Ineligible Students",xlab="Reserve Size", ylab="Share of Fifth Choice")
mean_majo5<- by(2*majo5$Share,majo5$Reserve,mean)
points(mean_majo5, pch=17,col="orange")

png(file="Fifth Choice - RO.png",width=3150, height=4455, res=500)
par(mfrow=c(3,1))
boxplot(2*Share~Reserve, data=over5, medcol = "red", main="a) Fifth Choice - All Students", xlab="Reserve Size", ylab="Share of Fifth Choice")
points(means_over5, pch=17,col="orange")
boxplot(2*Share~Reserve, data=mino5,medcol = "red", main="b) Fifth Choice - Eligible Students",xlab="Reserve Size", ylab="Share of Fifth Choice")
points(mean_mino5, pch=17,col="orange")
boxplot(2*Share~Reserve, data=majo5, medcol = "red", main="c) Fifth Choice - Ineligible Students",xlab="Reserve Size", ylab="Share of Fifth Choice")
points(mean_majo5, pch=17,col="orange")
dev.off()


#---FIRST CHOICE RATIO---#
cr <- c(0.6092,0.6148,0.6138,0.6186,0.6201,0.6220,0.6259,0.6248,0.6311,0.6325,0.6356,0.6394,0.6435,0.6453,0.6485,0.6515,0.6539,0.6590,0.6651,0.6716,0.6737,0.6782,0.6818,0.6894,0.6965,0.7001,0.7064,0.7143,0.7224,0.7281,0.7364,0.7433,0.7532,0.7590,0.7722,0.7820,0.7923,0.8020,0.8135,0.8254,0.8375,0.8515,0.8652,0.8815,0.8963,0.9130,0.9285,0.9470,0.9658,0.9841,1.0040,1.0261,1.0454,1.0658,1.0861,1.1055,1.1253,1.1451,1.1626,1.1829,1.2013,1.2194,1.2380,1.2550,1.2697,1.2897,1.3038,1.3212,1.3340,1.3502,1.3652,1.3773,1.3920,1.4070,1.4207,1.4301,1.4422,1.4545,1.4700,1.4784,1.4889,1.5014,1.5091,1.5149,1.5270,1.5431,1.5491,1.5491,1.5600,1.5697,1.5754,1.5821,1.5896,1.5978,1.6049,1.6102,1.6146,1.6236,1.6304,1.6440,1.6354)
r <- c(0,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1,0.11,0.12,0.13,0.14,0.15,0.16,0.17,0.18,0.19,0.2,0.21,0.22,0.23,0.24,0.25,0.26,0.27,0.28,0.29,0.3,0.31,0.32,0.33,0.34,0.35,0.36,0.37,0.38,0.39,0.4,0.41,0.42,0.43,0.44,0.45,0.46,0.47,0.48,0.49,0.5,0.51,0.52,0.53,0.54,0.55,0.56,0.57,0.58,0.59,0.6,0.61,0.62,0.63,0.64,0.65,0.66,0.67,0.68,0.69,0.7,0.71,0.72,0.73,0.74,0.75,0.76,0.77,0.78,0.79,0.8,0.81,0.82,0.83,0.84,0.85,0.86,0.87,0.88,0.89,0.9,0.91,0.92,0.93,0.94,0.95,0.96,0.97,0.98,0.99,1)

plot(r,1cr,main="First Choice Ratio",xlab="Reserve Size", ylab="Ratio")
lines(lowess(1cr~r),col="red")
abline(h=1)

png(file="First Choice Ratio - RO.png",width=4455, height=3150, res=500)
plot(r,cr ,main="First Choice Ratio",xlab="Reserve Size", ylab="Ratio")
lines(lowess(cr~r),col="red")
abline(h=1)
dev.off()
