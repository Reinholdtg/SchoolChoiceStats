#-------------------------------RESERVE-OPEN PRECEDENCE ORDER-------------------------------#
data_a1<-read.table(file.choose(),header=T)
attach(data_a1)

dat<-data.frame(MinoShare,MajoShare)

data_a1$Variance <- apply(dat,1,var)
data_a1$Ratio <- MinoShare/MajoShare
data_a1$Precedence <- rep("a1",nrow(data_a1))

head(data_a1)
tail(data_a1)

names(data_a1)<-c("School", "MinoCount", "MajoCount","MinoShare", "MajoShare", "Reserve", "Variance", "Ratio","Precedence")

plot(Variance~Reserve, data=data_a1, main="Variance of Student Assignment", xlab="Reserve Size", ylab="Variance")

write.table(data_a1, file='a1.csv',row.names=FALSE,col.names=TRUE)
detach(data_a1)


#-------------------------------Toggle PRECEDENCE ORDER-------------------------------#
data_a2<-read.table(file.choose(),header=T)
attach(data_a2)

dat<-data.frame(MinoShare,MajoShare)

data_a2$Variance <- apply(dat,1,var)
data_a2$Ratio <- MinoShare/MajoShare
data_a2$Precedence <- rep("a2",nrow(data_a2))

head(data_a2)
tail(data_a2)

names(data_a2)<-c("School", "MinoCount", "MajoCount","MinoShare", "MajoShare", "Reserve", "Variance", "Ratio","Precedence")

plot(Variance~Reserve, data=data_a2, main="Variance of Student Assignment", xlab="Reserve Size", ylab="Variance")

write.table(data_a2, file='a2.csv',row.names=FALSE,col.names=TRUE)
detach(data_a2)


#-------------------------------OPEN-RESERVE PRECEDENCE ORDER-------------------------------#
data_a3<-read.table(file.choose(),header=T)
attach(data_a3)

dat<-data.frame(MinoShare,MajoShare)

data_a3$Variance <- apply(dat,1,var)
data_a3$Ratio <- MinoShare/MajoShare
data_a3$Precedence <- rep("a3",nrow(data_a3))

head(data_a3)
tail(data_a3)

names(data_a3)<-c("School", "MinoCount", "MajoCount","MinoShare", "MajoShare", "Reserve", "Variance", "Ratio","Precedence")

plot(Variance~Reserve, data=data_a3, main="Variance of Student Assignment", xlab="Reserve Size", ylab="Variance")

write.table(data_a3, file='a3.csv',row.names=FALSE,col.names=TRUE)
detach(data_a3)


#-------------------------------COMBINED-------------------------------#
setwd('C:/Users/SRG023/Documents/combine/')

file_name <- list.files()
temp <- lapply (file_name, read.csv, sep=' ', header=T, strip.white=T) # READING ALL THE TEXT FILES
all<-rbind.fill(temp)
head(all)
tail(all)

write.table(all, file='AllStat.csv',row.names=FALSE,col.names=TRUE)

data<-read.table(file.choose(),header=T)
attach(data)
names(data)<-c("School", "MinoCount", "MajoCount","MinoShare", "MajoShare", "Reserve", "Variance", "Ratio","Precedence")
head(data)
tail(data)

data$Distance <- abs(MinoShare-MajoShare)
write.table(data, file='AllStat.csv',row.names=FALSE,col.names=TRUE)

#-------DATA READY-------#
data<-read.table(file.choose(),header=T)
attach(data)
detach(data)
names(data)<-c("School", "MinoCount", "MajoCount","MinoShare", "MajoShare", "Reserve", "Variance", "Ratio","Precedence", "Distance")
head(data)
tail(data)

#-------SUBSETS & MEANS-------#
data1 <- data[which(data$Precedence=='a1'),]
data2 <- data[which(data$Precedence=='a2'),]
data3 <- data[which(data$Precedence=='a3'),]


data_0 <- data[which(data$Reserve==0),]

data_.45 <- data[which(data$Reserve==0.45),]
data_.46 <- data[which(data$Reserve==0.46),]
data_.47 <- data[which(data$Reserve==0.47),]
data_.48 <- data[which(data$Reserve==0.48),]
data_.49 <- data[which(data$Reserve==0.49),]
data_.5 <- data[which(data$Reserve==0.5),]
data_.51 <- data[which(data$Reserve==0.51),]
data_.52 <- data[which(data$Reserve==0.52),]
data_.53 <- data[which(data$Reserve==0.53),]
data_.54 <- data[which(data$Reserve==0.54),]
data_.55 <- data[which(data$Reserve==0.55),]

data_1 <- data[which(data$Reserve==1),]

data_t <- data[which(data$Reserve>.5),]

means_var_data1 <-by(data1$Variance,data1$Reserve,mean)
means_var_data2 <-by(data2$Variance,data2$Reserve,mean)
means_var_data3 <-by(data3$Variance,data3$Reserve,mean)

means_dis_data1 <-by(data1$Distance,data1$Reserve,mean)
means_dis_data2 <-by(data2$Distance,data2$Reserve,mean)
means_dis_data3 <-by(data3$Distance,data3$Reserve,mean)


#-------MEANS & VARIANCE-------#
summary(Distance, data=data1)
summary(Distance, data=data2)
summary(Distance, data=data3)


aggregate(data$Distance, list(data$Precedence),mean)

aggregate(data_0$Distance, list(data_0$Precedence),mean)

aggregate(data_.45$Distance, list(data_.45$Precedence),mean)
aggregate(data_.46$Distance, list(data_.46$Precedence),mean)
aggregate(data_.47$Distance, list(data_.47$Precedence),mean)
aggregate(data_.48$Distance, list(data_.48$Precedence),mean)
aggregate(data_.49$Distance, list(data_.49$Precedence),mean)
aggregate(data_.5$Distance, list(data_.5$Precedence),mean)
aggregate(data_.51$Distance, list(data_.51$Precedence),mean)
aggregate(data_.52$Distance, list(data_.52$Precedence),mean)
aggregate(data_.53$Distance, list(data_.53$Precedence),mean)
aggregate(data_.54$Distance, list(data_.54$Precedence),mean)
aggregate(data_.55$Distance, list(data_.55$Precedence),mean)

aggregate(data_1$Distance, list(data_1$Precedence),mean)

aggregate(data_t$Distance, list(data_t$Precedence),mean)

aggregate(data$Distance, list(data$Precedence),var)

aggregate(data_.5$Distance, list(data_.5$Precedence),var)

aggregate(data_.45$Distance, list(data_.45$Precedence),var)
aggregate(data_.46$Distance, list(data_.46$Precedence),var)
aggregate(data_.47$Distance, list(data_.47$Precedence),var)
aggregate(data_.48$Distance, list(data_.48$Precedence),var)
aggregate(data_.49$Distance, list(data_.49$Precedence),var)
aggregate(data_.5$Distance, list(data_.5$Precedence),var)
aggregate(data_.51$Distance, list(data_.51$Precedence),var)
aggregate(data_.52$Distance, list(data_.52$Precedence),var)
aggregate(data_.53$Distance, list(data_.53$Precedence),var)
aggregate(data_.54$Distance, list(data_.54$Precedence),var)
aggregate(data_.55$Distance, list(data_.55$Precedence),var)


#-------VARIANCE PLOTS-------#
par(mfrow=c(3,1))
plot(Variance~Reserve, data=data1, main="a) Variance of Student Assignment by \nthe Reserve-Open Precedence Order", xlab="Reserve Size", ylab="Variance")
plot(Variance~Reserve, data=data2, main="b) Variance of Student Assignment by \nthe Toggle Precedence Order", xlab="Reserve Size", ylab="Variance")
plot(Variance~Reserve, data=data3, main="c) Variance of Student Assignment by \nthe Open-Reserve Precedence Order", xlab="Reserve Size", ylab="Variance")

par(mfrow=c(3,1))
boxplot(Variance~Reserve, data=data1, medcol = "red", main="a) Variance of Student Assignment by \nthe Reserve-Open Precedence Order", xlab="Reserve Size", ylab="Variance")
points(means_var_data1, pch=17,col="orange")
boxplot(Variance~Reserve, data=data2, medcol = "red", main="b) Variance of Student Assignment by \nthe Toggle Precedence Order", xlab="Reserve Size", ylab="Variance")
points(means_var_data2, pch=17,col="orange")
boxplot(Variance~Reserve, data=data3, medcol = "red", main="c) Variance of Student Assignment by \nthe Open-Reserve Precedence Order", xlab="Reserve Size", ylab="Variance")
points(means_var_data3, pch=17,col="orange")


#-------DISTANCE PLOTS-------#
par(mfrow=c(3,1))
plot(Distance~Reserve, data=data1, main="a) Distance Between Student Assignment by \nthe Reserve-Open Precedence Order", xlab="Reserve Size", ylab="Distance")
plot(Distance~Reserve, data=data2, main="b) Distance Between Student Assignment by \nthe Toggle Precedence Order", xlab="Reserve Size", ylab="Distance")
plot(Distance~Reserve, data=data3, main="c) Distance Between Student Assignment by \nthe Open-Reserve Precedence Order", xlab="Reserve Size", ylab="Distance")

par(mfrow=c(3,1))
boxplot(Distance~Reserve, data=data1, medcol = "red", main="a) Distance Between Student Assignment by \nthe Reserve-Open Precedence Order", xlab="Reserve Size", ylab="Distance")
points(means_dis_data1, pch=17,col="orange")
boxplot(Distance~Reserve, data=data2, medcol = "red", main="b) Distance Between Student Assignment by \nthe Toggle Precedence Order", xlab="Reserve Size", ylab="Distance")
points(means_dis_data2, pch=17,col="orange")
boxplot(Distance~Reserve, data=data3, medcol = "red", main="c) Distance Between Student Assignment by \nthe Open-Reserve Precedence Order", xlab="Reserve Size", ylab="Distance")
points(means_dis_data3, pch=17,col="orange")


#-------SAVE PLOTS-------#
png(file="Variance plot - All.png",width=3150, height=4455, res=500)
par(mfrow=c(3,1))
plot(Variance~Reserve, data=data1, main="a) Variance of Student Assignment by \nthe Reserve-Open", xlab="Reserve Size", ylab="Variance")
plot(Variance~Reserve, data=data2, main="b) Variance of Student Assignment by \nthe Toggle", xlab="Reserve Size", ylab="Variance")
plot(Variance~Reserve, data=data3, main="c) Variance of Student Assignment by \nthe Open-Reserve", xlab="Reserve Size", ylab="Variance")
dev.off()

png(file="Variance boxplot - All.png",width=3150, height=4455, res=500)
par(mfrow=c(3,1))
boxplot(Variance~Reserve, data=data1, medcol = "red", main="a) Variance of Student Assignment by \nthe Reserve-Open Precedence Order", xlab="Reserve Size", ylab="Variance")
points(means_var_data1, pch=17,col="orange")
boxplot(Variance~Reserve, data=data2, medcol = "red", main="b) Variance of Student Assignment by \nthe Toggle Precedence Order", xlab="Reserve Size", ylab="Variance")
points(means_var_data2, pch=17,col="orange")
boxplot(Variance~Reserve, data=data3, medcol = "red", main="c) Variance of Student Assignment by \nthe Open-Reserve Precedence Order", xlab="Reserve Size", ylab="Variance")
points(means_var_data3, pch=17,col="orange")
dev.off()

png(file="Distance plot - All.png",width=3150, height=4455, res=500)
par(mfrow=c(3,1))
plot(Distance~Reserve, data=data1, main="a) Distance Between Student Assignment by \nthe Reserve-Open Precedence Order", xlab="Reserve Size", ylab="Distance")
plot(Distance~Reserve, data=data2, main="b) Distance Between Student Assignment by \nthe Toggle Precedence Order", xlab="Reserve Size", ylab="Distance")
plot(Distance~Reserve, data=data3, main="c) Distance Between Student Assignment by \nthe Open-Reserve Precedence Order", xlab="Reserve Size", ylab="Distance")
dev.off()

png(file="Distance boxplot - All.png",width=3150, height=4455, res=500)
par(mfrow=c(3,1))
boxplot(Distance~Reserve, data=data1, medcol = "red", main="a) Distance Between Student Assignment by \nthe Reserve-Open Precedence Order", xlab="Reserve Size", ylab="Distance")
points(means_dis_data1, pch=17,col="orange")
boxplot(Distance~Reserve, data=data2, medcol = "red", main="b) Distance Between Student Assignment by \nthe Toggle Precedence Order", xlab="Reserve Size", ylab="Distance")
points(means_dis_data2, pch=17,col="orange")
boxplot(Distance~Reserve, data=data3, medcol = "red", main="c) Distance Between Student Assignment by \nthe Open-Reserve Precedence Order", xlab="Reserve Size", ylab="Distance")
points(means_dis_data3, pch=17,col="orange")
dev.off()

