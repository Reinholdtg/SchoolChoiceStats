#------------------RESERVE-OPEN PRECEDENCE ORDER------------------#
data_a1<-read.table(file.choose(),header=T) # PriorityRO.csv
names(data_a1)<-c("Choice", "Student", "Count","Share", "Reserve")
attach(data_a1)
detach(data_a1)
head(data_a1)

data_a1$Precedence <- rep("a1",nrow(data_a1))
write.table(data_a1, file='a1.csv',row.names=FALSE,col.names=TRUE)


#------------------TOGGLE PRECEDENCE ORDER------------------#
data_a2<-read.table(file.choose(),header=T) # PriorityT.csv
names(data_a2)<-c("Choice", "Student", "Count","Share", "Reserve")
attach(data_a2)
detach(data_a2)
head(data_a2)

data_a2$Precedence <- rep("a2",nrow(data_a2))
write.table(data_a2, file='a2.csv',row.names=FALSE,col.names=TRUE)


#------------------OPEN-RESERVE PRECEDENCE ORDER------------------#
data_a3<-read.table(file.choose(),header=T) # PriorityOR.csv
names(data_a3)<-c("Choice", "Student", "Count","Share", "Reserve")
attach(data_a3)
detach(data_a3)
head(data_a3)

data_a3$Precedence <- rep("a3",nrow(data_a3))
write.table(data_a3, file='a3.csv',row.names=FALSE,col.names=TRUE)


#----------------------COMBINE----------------------#
setwd('C:/Users/SRG023/Documents/combine/')

file_name <- list.files()
temp <- lapply (file_name, read.csv, sep=' ', header=T, strip.white=T) # READING ALL THE TEXT FILES
all<-rbind.fill(temp)
head(aa)
tail(aa)

write.table(all, file='all.csv',row.names=FALSE,col.names=TRUE)


#---------ALL---------#
data<-read.table(file.choose(),header=T) # PriorityAll.csv
names(data)<-c("Choice", "Student", "Count", "Share", "Reserve", "Precedence")
attach(data)
head(data)
tail(data)


#------------------------------- FIRST CHOICE -------------------------------#
over1 <- data[which(data$Choice==0),]
mino1 <- data[which(data$Choice==0 & data$Student=='-'),]
majo1 <- data[which(data$Choice==0 & data$Student=='+'),]

aggregate(over1$Share*2, list(over1$Reserve), mean)
aggregate(mino1$Share*2, list(mino1$Reserve), mean)
aggregate(majo1$Share*2, list(majo1$Reserve), mean)


#--------PLOTS AND FITS--------#
#--OVERALL FIRST CHOICE--#
boxplot(2*Share~Reserve, data=over1, medcol = "red", main="First Choice - All Students", xlab="Reserve Size", ylab="Share of First Choice")
means_over1 <-by(2*over1$Share,over1$Reserve,mean)
points(means_over1, pch=17,col="orange")

cor(over1$Student,over1$Share)

fit_over1 <- lm(2*Share~Reserve+Student+Precedence, data=over1)
fit_over1
summary(fit_over1)

over11<-within(over1, Precedence <- relevel(Precedence, ref = "a3"))


#--ELIGIBLE FIRST CHOICE--#
boxplot(2*Share~Reserve, data=mino1,medcol = "red", main="First Choice - Eligible Students",xlab="Reserve Size", ylab="Share of First Choice")
mean_mino1<- by(2*mino1$Share,mino1$Reserve,mean)
points(mean_mino1, pch=17,col="orange")

fit_mino1 <- lm(2*Share~Reserve+Precedence, data=mino1)
summary(fit_mino1)


#--INELIGILBLE FIRST CHOICE--#
boxplot(2*Share~Reserve, data=majo1, medcol = "red", main="First Choice - Ineligible Students",xlab="Reserve Size", ylab="Share of First Choice")
mean_majo1<- by(2*majo1$Share,majo1$Reserve,mean)
points(mean_majo1, pch=17,col="orange")

fit_majo1 <- lm(2*Share~Reserve+Precedence, data=majo1)
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


#---FIRST CHOICE RATIO---#
RO <- c(0.6092,0.6148,0.6138,0.6186,0.6201,0.6220,0.6259,0.6248,0.6311,0.6325,0.6356,0.6394,0.6435,0.6453,0.6485,0.6515,0.6539,0.6590,0.6651,0.6716,0.6737,0.6782,0.6818,0.6894,0.6965,0.7001,0.7064,0.7143,0.7224,0.7281,0.7364,0.7433,0.7532,0.7590,0.7722,0.7820,0.7923,0.8020,0.8135,0.8254,0.8375,0.8515,0.8652,0.8815,0.8963,0.9130,0.9285,0.9470,0.9658,0.9841,1.0040,1.0261,1.0454,1.0658,1.0861,1.1055,1.1253,1.1451,1.1626,1.1829,1.2013,1.2194,1.2380,1.2550,1.2697,1.2897,1.3038,1.3212,1.3340,1.3502,1.3652,1.3773,1.3920,1.4070,1.4207,1.4301,1.4422,1.4545,1.4700,1.4784,1.4889,1.5014,1.5091,1.5149,1.5270,1.5431,1.5491,1.5491,1.5600,1.5697,1.5754,1.5821,1.5896,1.5978,1.6049,1.6102,1.6146,1.6236,1.6304,1.6440,1.6354)
OR <- c(0.6093,0.6131,0.6130,0.6186,0.6211,0.6214,0.6234,0.6270,0.6288,0.6315,0.6331,0.6381,0.6397,0.6446,0.6486,0.6522,0.6558,0.6620,0.6644,0.6704,0.6747,0.6782,0.6843,0.6880,0.6962,0.7025,0.7088,0.7146,0.7229,0.7276,0.7366,0.7437,0.7531,0.7608,0.7708,0.7809,0.7928,0.8027,0.8132,0.8268,0.8380,0.8522,0.8658,0.8808,0.8961,0.9128,0.9301,0.9472,0.9661,0.9839,1.0028,1.0262,1.0667,1.0667,1.0858,1.1054,1.1261,1.1446,1.1636,1.1823,1.2026,1.2196,1.2353,1.2569,1.2734,1.2867,1.3024,1.3178,1.3364,1.3506,1.3645,1.3773,1.3936,1.4052,1.4195,1.4296,1.4438,1.4585,1.4711,1.4776,1.4910,1.4988,1.5108,1.5155,1.5274,1.5367,1.5468,1.5553,1.5607,1.5697,1.5806,1.5912,1.5933,1.5958,1.6073,1.6110,1.6181,1.6243,1.6237,1.6317,1.6409)
T <- c(0.60872,0.61442,0.61631,0.61631,0.62114,0.62089,0.62644,0.62579,0.62963,0.63117,0.63704,0.63766,0.63991,0.64452,0.65045,0.65044,0.65424,0.66047,0.66387,0.67041,0.67300,0.67932,0.68281,0.68909,0.69570,0.70235,0.70752,0.71457,0.72023,0.72914,0.73679,0.74445,0.75175,0.76066,0.77120,0.77961,0.79171,0.80303,0.81337,0.82524,0.83732,0.85270,0.86648,0.88151,0.89560,0.91328,0.92934,0.94739,0.96599,0.98372,1.00266,1.00240,1.00279,1.00279,1.00189,1.00260,1.00314,1.00219,1.00241,1.00249,1.00265,1.00227,1.00187,1.00291,1.00204,1.00249,1.00193,1.00236,1.00253,1.00272,1.00261,1.00274,1.00290,1.00281,1.00241,1.00243,1.00310,1.00185,1.00242,1.00315,1.00263,1.00218,1.00295,1.00253,1.00234,1.00276,1.00239,1.00287,1.00154,1.00304,1.00172,1.00234,1.00216,1.00228,1.00273,1.00240,1.00268,1.00268,1.00232,1.00304,1.00325)

r <- c(0,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1,0.11,0.12,0.13,0.14,0.15,0.16,0.17,0.18,0.19,0.2,0.21,0.22,0.23,0.24,0.25,0.26,0.27,0.28,0.29,0.3,0.31,0.32,0.33,0.34,0.35,0.36,0.37,0.38,0.39,0.4,0.41,0.42,0.43,0.44,0.45,0.46,0.47,0.48,0.49,0.5,0.51,0.52,0.53,0.54,0.55,0.56,0.57,0.58,0.59,0.6,0.61,0.62,0.63,0.64,0.65,0.66,0.67,0.68,0.69,0.7,0.71,0.72,0.73,0.74,0.75,0.76,0.77,0.78,0.79,0.8,0.81,0.82,0.83,0.84,0.85,0.86,0.87,0.88,0.89,0.9,0.91,0.92,0.93,0.94,0.95,0.96,0.97,0.98,0.99,1)

plot(r, RO, col="blue", main="First Choice Ratio",xlab="Reserve Size", ylab="Ratio")
points(r, T, col="red")
points(r, OR, col="green")
legend(1, 90, legend=c(“Reserve-Open”,”Toggle”,”Open-Reserve”))
abline(h=1)

png(file="First Choice Ratio - All.png",width=4455, height=3150, res=500)
plot(r, RO, col="blue", main="First Choice Ratio",xlab="Reserve Size", ylab="Ratio")
points(r, T, col="red")
points(r, OR, col="green")
abline(h=1)
dev.off()

