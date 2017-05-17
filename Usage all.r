#-------------------------------RESERVE-OPEN PRECEDENCE ORDER-------------------------------#
data_a1<-read.table(file.choose(),header=T) # UsageRO.csv
attach(data_a1)

data_a1$Precedence <- rep("a1",nrow(data_a1))

head(data_a1)
tail(data_a1)

names(data_a1)<-c("Eligible", "ReserveSeat", "Count","Share", "Reserve", "Precedence")

write.table(data_a1, file='a1.csv',row.names=FALSE,col.names=TRUE)
detach(data_a1)


#-------------------------------Toggle PRECEDENCE ORDER-------------------------------#
data_a2<-read.table(file.choose(),header=T) # UsageT.csv
attach(data_a2)

data_a2$Precedence <- rep("a2",nrow(data_a2))

head(data_a2)
tail(data_a2)

names(data_a2)<-c("Eligible", "ReserveSeat", "Count","Share", "Reserve", "Precedence")

write.table(data_a2, file='a2.csv',row.names=FALSE,col.names=TRUE)
detach(data_a2)


#-------------------------------OPEN-RESERVE PRECEDENCE ORDER-------------------------------#
data_a3<-read.table(file.choose(),header=T) # UsageOR.csv
attach(data_a3)

data_a3$Precedence <- rep("a3",nrow(data_a3))

head(data_a3)
tail(data_a3)

names(data_a3)<-c("Eligible", "ReserveSeat", "Count","Share", "Reserve", "Precedence")

write.table(data_a3, file='a3.csv',row.names=FALSE,col.names=TRUE)
detach(data_a3)


#-------DATA READY-------#
data<-read.table(file.choose(),header=T) # UsageAll.csv
attach(data)
names(data)<-c("Eligible", "ReserveSeat", "Count","Share", "Reserve", "Precedence")
head(data)
tail(data)
detach(data)


#-------SUBSETS & MEANS-------#
data1<- data[which(data$Precedence=='a1' & data$Eligible==1 & data$ReserveSeat==1),]
data2<- data[which(data$Precedence=='a2' & data$Eligible==1 & data$ReserveSeat==1),]
data3<- data[which(data$Precedence=='a3' & data$Eligible==1 & data$ReserveSeat==1),]
head(data1)
tail(data1)

data3open<- data[which(data$Precedence=='a3' & data$Eligible==1 & data$ReserveSeat==0),]

m_data1 <- data.frame(data1$Reserve, m_del=ave(data1$Share, data1$Reserve), stringsAsFactors=F)
m_data2 <- data.frame(data2$Reserve, m_del=ave(data2$Share, data2$Reserve), stringsAsFactors=F)
m_data3 <- data.frame(data3$Reserve, m_del=ave(data3$Share, data3$Reserve), stringsAsFactors=F)


#-------VARIANCE & MEANS------#
aggregate(data1$Share, list(data1$Reserve), mean)
aggregate(data1$Share, list(data1$Reserve), var)

aggregate(data2$Share, list(data2$Reserve), mean)
aggregate(data2$Share, list(data2$Reserve), var)

aggregate(data3$Share, list(data3$Reserve), mean)
aggregate(data3$Share, list(data3$Reserve), var)

aggregate(data3open$Share, list(data3open$Reserve), mean)
aggregate(data3open$Share, list(data3open$Reserve), var)

mean(data1$Share)
mean(data2$Share)
mean(data3$Share)


#-------PLOTS-------#
par(mfrow=c(3,1))
barplot(names.arg=c(0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1,0.11,0.12,0.13,0.14,0.15,0.16,0.17,0.18,0.19,0.2,0.21,0.22,0.23,0.24,0.25,0.26,0.27,0.28,0.29,0.3,0.31,0.32,0.33,0.34,0.35,0.36,0.37,0.38,0.39,0.4,0.41,0.42,0.43,0.44,0.45,0.46,0.47,0.48,0.49,
0.5,0.51,0.52,0.53,0.54,0.55,0.56,0.57,0.58,0.59,0.6,0.61,0.62,0.63,0.64,0.65,0.66,0.67,0.68,0.69,0.7,0.71,0.72,0.73,0.74,0.75,0.76,0.77,0.78,0.79,0.8,0.81,0.82,0.83,0.84,0.85,0.86,0.87,0.88,0.89,0.9,0.91,0.92,0.93,0.94,0.95,0.96,0.97,0.98,0.99,1),
unique(m_data1)$m_del, names=unique(m_data1)$Reserve,
main="b) Average - Reserve-Open", xlab="Reserve Size", ylab="Average Share of Eligible Students")
abline(h=0)
abline(h=1)

barplot(names.arg=c(0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1,0.11,0.12,0.13,0.14,0.15,0.16,0.17,0.18,0.19,0.2,0.21,0.22,0.23,0.24,0.25,0.26,0.27,0.28,0.29,0.3,0.31,0.32,0.33,0.34,0.35,0.36,0.37,0.38,0.39,0.4,0.41,0.42,0.43,0.44,0.45,0.46,0.47,0.48,0.49,
0.5,0.51,0.52,0.53,0.54,0.55,0.56,0.57,0.58,0.59,0.6,0.61,0.62,0.63,0.64,0.65,0.66,0.67,0.68,0.69,0.7,0.71,0.72,0.73,0.74,0.75,0.76,0.77,0.78,0.79,0.8,0.81,0.82,0.83,0.84,0.85,0.86,0.87,0.88,0.89,0.9,0.91,0.92,0.93,0.94,0.95,0.96,0.97,0.98,0.99,1),
unique(m_data2)$m_del, names=unique(m_data2)$Reserve,
main="d) Average - Toggle", xlab="Reserve Size", ylab="Average Share of Eligible Students")
abline(h=0)
abline(h=1)

barplot(names.arg=c(0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1,0.11,0.12,0.13,0.14,0.15,0.16,0.17,0.18,0.19,0.2,0.21,0.22,0.23,0.24,0.25,0.26,0.27,0.28,0.29,0.3,0.31,0.32,0.33,0.34,0.35,0.36,0.37,0.38,0.39,0.4,0.41,0.42,0.43,0.44,0.45,0.46,0.47,0.48,0.49,
0.5,0.51,0.52,0.53,0.54,0.55,0.56,0.57,0.58,0.59,0.6,0.61,0.62,0.63,0.64,0.65,0.66,0.67,0.68,0.69,0.7,0.71,0.72,0.73,0.74,0.75,0.76,0.77,0.78,0.79,0.8,0.81,0.82,0.83,0.84,0.85,0.86,0.87,0.88,0.89,0.9,0.91,0.92,0.93,0.94,0.95,0.96,0.97,0.98,0.99,1),
unique(m_data3)$m_del, names=unique(m_data3)$Reserve,
main="f) Average - Open-Reserve", xlab="Reserve Size", ylab="Average Share of Eligible Students")
abline(h=0)
abline(h=1)

par(mfrow=c(3,1))
plot(Share~Reserve, data=data1,
main="a) Eligible Students Assigned to Reserve Seats \nby the Reserve-Open Precedene Order", xlab="Reserve Size", ylab="Share of Eligible Students")
abline(0,2, col="green")
abline(.05,2, col="yellow")
abline(.1,2, col="red")
plot(Share~Reserve, data=data2,
main="b) Eligible Students Assigned to Reserve Seats \nby the Toggle Precedene Order", xlab="Reserve Size", ylab="Share of Eligible Students")
abline(0,2, col="green")
abline(.05,2, col="yellow")
abline(.1,2, col="red")
plot(Share~Reserve, data=data3,
main="c) Eligible Students Assigned to Reserve Seats \nby the Open-Reserve Precedene Order", xlab="Reserve Size", ylab="Share of Eligible Students")
abline(0,2, col="green")
abline(.05,2, col="yellow")
abline(.1,2, col="red")


#-------SAVE PLOTS-------#
png(file="Utilization plots - All.png",width=3150, height=4455, res=500)
par(mfrow=c(3,2))
plot(Share~Reserve, data=data1,
main="a) Distribution - Reserve-Open", xlab="Reserve Size", ylab="Share of Eligible Students")
abline(0,2, col="green")
abline(.05,2, col="yellow")
abline(.1,2, col="red")

barplot(names.arg=c(0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1,0.11,0.12,0.13,0.14,0.15,0.16,0.17,0.18,0.19,0.2,0.21,0.22,0.23,0.24,0.25,0.26,0.27,0.28,0.29,0.3,0.31,0.32,0.33,0.34,0.35,0.36,0.37,0.38,0.39,0.4,0.41,0.42,0.43,0.44,0.45,0.46,0.47,0.48,0.49,
0.5,0.51,0.52,0.53,0.54,0.55,0.56,0.57,0.58,0.59,0.6,0.61,0.62,0.63,0.64,0.65,0.66,0.67,0.68,0.69,0.7,0.71,0.72,0.73,0.74,0.75,0.76,0.77,0.78,0.79,0.8,0.81,0.82,0.83,0.84,0.85,0.86,0.87,0.88,0.89,0.9,0.91,0.92,0.93,0.94,0.95,0.96,0.97,0.98,0.99,1),
unique(m_data1)$m_del, names=unique(m_data1)$Reserve,
main="b) Average - Reserve-Open", xlab="Reserve Size", ylab="Average Share of Eligible Students")
abline(h=0)
abline(h=1)

plot(Share~Reserve, data=data2,
main="c) Distribution - Toggle", xlab="Reserve Size", ylab="Share of Eligible Students")
abline(0,2, col="green")
abline(.05,2, col="yellow")
abline(.1,2, col="red")

barplot(names.arg=c(0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1,0.11,0.12,0.13,0.14,0.15,0.16,0.17,0.18,0.19,0.2,0.21,0.22,0.23,0.24,0.25,0.26,0.27,0.28,0.29,0.3,0.31,0.32,0.33,0.34,0.35,0.36,0.37,0.38,0.39,0.4,0.41,0.42,0.43,0.44,0.45,0.46,0.47,0.48,0.49,
0.5,0.51,0.52,0.53,0.54,0.55,0.56,0.57,0.58,0.59,0.6,0.61,0.62,0.63,0.64,0.65,0.66,0.67,0.68,0.69,0.7,0.71,0.72,0.73,0.74,0.75,0.76,0.77,0.78,0.79,0.8,0.81,0.82,0.83,0.84,0.85,0.86,0.87,0.88,0.89,0.9,0.91,0.92,0.93,0.94,0.95,0.96,0.97,0.98,0.99,1),
unique(m_data2)$m_del, names=unique(m_data2)$Reserve,
main="d) Average - Toggle", xlab="Reserve Size", ylab="Average Share of Eligible Students")
abline(h=0)
abline(h=1)

plot(Share~Reserve, data=data3,
main="e) Distribution - Open-Reserve", xlab="Reserve Size", ylab="Share of Eligible Students")
abline(0,2, col="green")
abline(.05,2, col="yellow")
abline(.1,2, col="red")

barplot(names.arg=c(0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1,0.11,0.12,0.13,0.14,0.15,0.16,0.17,0.18,0.19,0.2,0.21,0.22,0.23,0.24,0.25,0.26,0.27,0.28,0.29,0.3,0.31,0.32,0.33,0.34,0.35,0.36,0.37,0.38,0.39,0.4,0.41,0.42,0.43,0.44,0.45,0.46,0.47,0.48,0.49,
0.5,0.51,0.52,0.53,0.54,0.55,0.56,0.57,0.58,0.59,0.6,0.61,0.62,0.63,0.64,0.65,0.66,0.67,0.68,0.69,0.7,0.71,0.72,0.73,0.74,0.75,0.76,0.77,0.78,0.79,0.8,0.81,0.82,0.83,0.84,0.85,0.86,0.87,0.88,0.89,0.9,0.91,0.92,0.93,0.94,0.95,0.96,0.97,0.98,0.99,1),
unique(m_data3)$m_del, names=unique(m_data3)$Reserve,
main="f) Average - Open-Reserve", xlab="Reserve Size", ylab="Average Share of Eligible Students")
abline(h=0)
abline(h=1)
dev.off()

