#-------------------------------RESERVE-OPEN PRECEDENCE ORDER-------------------------------#
data_a1<-read.table(file.choose(),header=T)
attach(data_a1)

data_a1$Precedence <- rep("a1",nrow(data_a1))

head(data_a1)
tail(data_a1)

names(data_a1)<-c("Minority", "ReserveSeat", "Count","Share", "Reserve", "Precedence")

write.table(data_a1, file='a1.csv',row.names=FALSE,col.names=TRUE)
detach(data_a1)


#-------------------------------Toggle PRECEDENCE ORDER-------------------------------#
data_a2<-read.table(file.choose(),header=T)
attach(data_a2)

data_a2$Precedence <- rep("a2",nrow(data_a2))

head(data_a2)
tail(data_a2)

names(data_a2)<-c("Minority", "ReserveSeat", "Count","Share", "Reserve", "Precedence")

write.table(data_a2, file='a2.csv',row.names=FALSE,col.names=TRUE)
detach(data_a2)


#-------------------------------OPEN-RESERVE PRECEDENCE ORDER-------------------------------#
data_a3<-read.table(file.choose(),header=T)
attach(data_a3)

data_a3$Precedence <- rep("a3",nrow(data_a3))

head(data_a3)
tail(data_a3)

names(data_a3)<-c("Minority", "ReserveSeat", "Count","Share", "Reserve", "Precedence")

write.table(data_a3, file='a3.csv',row.names=FALSE,col.names=TRUE)
detach(data_a3)


#-------DATA READY-------#
data<-read.table(file.choose(),header=T)
attach(data)
names(data)<-c("Minority", "ReserveSeat", "Count","Share", "Reserve", "Precedence")
head(data)
tail(data)
detach(data)


#-------SUBSETS & MEANS-------#
data1<- data[which(data$Precedence=='a1' & data$Minority==1 & data$ReserveSeat==1),]
data2<- data[which(data$Precedence=='a2' & data$Minority==1 & data$ReserveSeat==1),]
data3<- data[which(data$Precedence=='a3' & data$Minority==1 & data$ReserveSeat==1),]

head(data1)
tail(data1)


#-------SAVE PLOTS-------#
png(file="Utilization plot - All.png",width=2100, height=2970, res=350)
par(mfrow=c(3,1))
plot(Share/2~Reserve, data=data1, main="a) Reserve Seat Utilitization by \nthe Reserve-Open Precedene Order", xlab="Reserve Size", ylab="Utilization")
abline(0,1, col="red")
plot(Share/2~Reserve, data=data2, main="b) Reserve Seat Utilitization by \nthe Toggle Precedene Order", xlab="Reserve Size", ylab="Utilization")
abline(0,1, col="red")
plot(Share/2~Reserve, data=data3, main="c) Reserve Seat Utilitization by \nthe Open-Reserve Precedene Order", xlab="Reserve Size", ylab="Utilization")
abline(0,1, col="red")
dev.off()
