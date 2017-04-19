data<-read.table(file.choose(),header=T)
names(data)<-c("Choice", "Student", "Count","Share", "Reserve")
attach(data)
detach(data)
head(data)
summary(data)

par(mfrow=c(1,1))

#------------------------------- FIRST CHOICE -------------------------------#

#--SUBSETS--#

over1 <- data[which(data$Choice==0),]
mino1 <- data[which(data$Choice==0 & data$Student=='-'),]
majo1 <- data[which(data$Choice==0 & data$Student=='+'),]

#var cal#
o0 <- data[which(data$Choice==0 & data$Reserve==0),]
o0.5 <- data[which(data$Choice==0 & data$Reserve==.5),]
o1 <- data[which(data$Choice==0 & data$Reserve==1),]

mino0 <- data[which(data$Choice==0 & data$Reserve==0 & data$Student=='-'),]
mino0.5 <- data[which(data$Choice==0 & data$Reserve==.5 & data$Student=='-'),]
mino1 <- data[which(data$Choice==0 & data$Reserve==1 & data$Student=='-'),]

majo0 <- data[which(data$Choice==0 & data$Reserve==0 & data$Student=='+'),]
majo0.5 <- data[which(data$Choice==0 & data$Reserve==.5 & data$Student=='+'),]
majo1 <- data[which(data$Choice==0 & data$Reserve==1 & data$Student=='+'),]


#--OVERALL FIRST CHOICE--#

boxplot(2*Share~Reserve, data=over1, medcol = "red", main="First Choice - All Students", xlab="Reserve Size", ylab="Share of First Choice")
means_over1 <-by(2*over1$Share,over1$Reserve,mean)
points(means_over1, pch=17,col="orange")

fit_over1 <- lm(2*Share~Reserve+Student, data=over1)
fit_over1
summary(fit_over1, degits=10)


par(mfrow=c(2,2))
plot(fit_over1)


#--MINORITY FIRST CHOICE--#

boxplot(2*Share~Reserve, data=mino1,medcol = "red", main="First Choice- Minority Students",xlab="Reserve Size", ylab="Share of First Choice")
mean_mino1<- by(2*mino1$Share,mino1$Reserve,mean)
points(mean_mino1, pch=17,col="orange")

fit_mino1 <- lm(2*Share~Reserve, data=mino1)
summary(fit_mino1)


par(mfrow=c(2,2))
plot(fit_mino1)


#--MAJORITY FIRST CHOICE--#

boxplot(2*Share~Reserve, data=majo1, medcol = "red", main="First Choice - Majority Students",xlab="Reserve Size", ylab="Share of First Choice")
mean_majo1<- by(2*majo1$Share,majo1$Reserve,mean)
points(mean_majo1, pch=17,col="orange")

fit_majo1 <- lm(2*Share~Reserve, data=majo1)
summary(fit_majo1)


par(mfrow=c(2,2))
plot(fit_majo1)


#--COMBINED--#
par(mfrow=c(3,1))
boxplot(2*Share~Reserve, data=over1, medcol = "red", main="a) First Choice - All Students", xlab="Reserve Size", ylab="Share of First Choice")
means_over1 <-by(2*over1$Share,over1$Reserve,mean)
points(means_over1, pch=17,col="orange")
boxplot(2*Share~Reserve, data=mino1,medcol = "red", main="b) First Choice - Minority Students",xlab="Reserve Size", ylab="Share of First Choice")
mean_mino1<- by(2*mino1$Share,mino1$Reserve,mean)
points(mean_mino1, pch=17,col="orange")
boxplot(2*Share~Reserve, data=majo1, medcol = "red", main="c) First Choice - Majority Students",xlab="Reserve Size", ylab="Share of First Choice")
mean_majo1<- by(2*majo1$Share,majo1$Reserve,mean)
points(mean_majo1, pch=17,col="orange")


png(file="First Choice - OR.png",width=2100, height=2970, res=350)
par(mfrow=c(3,1))
boxplot(2*Share~Reserve, data=over1, medcol = "red", main="a) First Choice - All Students", xlab="Reserve Size", ylab="Share of First Choice")
points(means_over1, pch=17,col="orange")
boxplot(2*Share~Reserve, data=mino1,medcol = "red", main="b) First Choice - Minority Students",xlab="Reserve Size", ylab="Share of First Choice")
points(mean_mino1, pch=17,col="orange")
boxplot(2*Share~Reserve, data=majo1, medcol = "red", main="c) First Choice - Majority Students",xlab="Reserve Size", ylab="Share of First Choice")
points(mean_majo1, pch=17,col="orange")
dev.off()


var(o0$Share) 		#0.01302894
var(o0.5$Share)		#0.002730047
var(o1$Share)		#0.002664661

var(mino0$Share) 		#0.006984529
var(mino0.5$Share)	#0.002738521
var(mino1$Share)		#0.002659087

var(majo0$Share) 		#0.0009175993
var(majo0.5$Share)	#0.002721304
var(majo1$Share)		#0.002669791

#------------------------------- FIFTH CHOICE -------------------------------#

#--SUBSETS--#

over5 <- data[which(data$Choice==4),]
mino5 <- data[which(data$Choice==4 & data$Student=='-'),]
majo5 <- data[which(data$Choice==4 & data$Student=='+'),]


#--OVERALL FIFTH CHOICE--#

boxplot(2*Share~Reserve, data=over5, medcol = "red", main="a) Fifth Choice - All Students", xlab="Reserve Size", ylab="Share of Fifth Choice")
means_over5 <-by(2*over5$Share,over5$Reserve,mean)
points(means_over5, pch=17,col="orange")

fit_over5 <- lm(2*Share~Reserve+Student, data=over5)
summary(fit_over5)


par(mfrow=c(2,2))
plot(fit_over5)


#--MINORITY FIFHT CHOICE--#

boxplot(2*Share~Reserve, data=mino5,medcol = "red", main="b) Fifth Choice - Minority Students",xlab="Reserve Size", ylab="Share of Fifth Choice")
mean_mino5<- by(2*mino5$Share,mino5$Reserve,mean)
points(mean_mino1, pch=17,col="orange")

fit_mino5 <- lm(2*Share~Reserve, data=mino5)
summary(fit_mino5)

par(mfrow=c(2,2))
plot(fit_mino5)


#--MAJORITY FIFTH CHOICE--#

boxplot(2*Share~Reserve, data=majo5, medcol = "red", main="c) Fifth Choice - Majority Students",xlab="Reserve Size", ylab="Share of Fifth Choice")
mean_majo5<- by(2*majo5$Share,majo5$Reserve,mean)
points(mean_majo5, pch=17,col="orange")

fit_majo5 <- lm(2*Share~Reserve, data=majo5)
summary(fit_majo5)

par(mfrow=c(2,2))
plot(fit_majo5)

#--COMBINED--#
par(mfrow=c(3,1))
boxplot(2*Share~Reserve, data=over5, medcol = "red", main="a) Fifth Choice - All Students", xlab="Reserve Size", ylab="Share of Fifth Choice")
means_over5 <-by(2*over5$Share,over5$Reserve,mean)
points(means_over5, pch=17,col="orange")
boxplot(2*Share~Reserve, data=mino5,medcol = "red", main="b) Fifth Choice - Minority Students",xlab="Reserve Size", ylab="Share of Fifth Choice")
mean_mino5<- by(2*mino5$Share,mino5$Reserve,mean)
points(mean_mino5, pch=17,col="orange")
boxplot(2*Share~Reserve, data=majo5, medcol = "red", main="c) Fifth Choice - Majority Students",xlab="Reserve Size", ylab="Share of Fifth Choice")
mean_majo5<- by(2*majo5$Share,majo5$Reserve,mean)
points(mean_majo5, pch=17,col="orange")

png(file="Fifth Choice - OR.png",width=2100, height=2970, res=350)
par(mfrow=c(3,1))
boxplot(2*Share~Reserve, data=over5, medcol = "red", main="a) Fifth Choice - All Students", xlab="Reserve Size", ylab="Share of Fifth Choice")
points(means_over5, pch=17,col="orange")
boxplot(2*Share~Reserve, data=mino5,medcol = "red", main="b) Fifth Choice - Minority Students",xlab="Reserve Size", ylab="Share of Fifth Choice")
points(mean_mino5, pch=17,col="orange")
boxplot(2*Share~Reserve, data=majo5, medcol = "red", main="c) Fifth Choice - Majority Students",xlab="Reserve Size", ylab="Share of Fifth Choice")
points(mean_majo5, pch=17,col="orange")
dev.off()


