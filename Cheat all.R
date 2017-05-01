#-------------------------------RESERVE-OPEN PRECEDENCE ORDER-------------------------------#
data_a1<-read.table(file.choose(),header=T)
attach(data_a1)

data_a1$Precedence <- rep("a1",nrow(data_a1))

head(data_a1)
tail(data_a1)

names(data_a1)<-c("Eligible", "Cheat", "Count", "Reserve", "Precedence")

write.table(data_a1, file='a1.csv',row.names=FALSE,col.names=TRUE)
detach(data_a1)


#-------------------------------Toggle PRECEDENCE ORDER-------------------------------#
data_a2<-read.table(file.choose(),header=T)
attach(data_a2)

data_a2$Precedence <- rep("a2",nrow(data_a2))

head(data_a2)
tail(data_a2)

names(data_a2)<-c("Eligible", "Cheat", "Count", "Reserve", "Precedence")

write.table(data_a2, file='a2.csv',row.names=FALSE,col.names=TRUE)
detach(data_a2)


#-------------------------------OPEN-RESERVE PRECEDENCE ORDER-------------------------------#
data_a3<-read.table(file.choose(),header=T)
attach(data_a3)

data_a3$Precedence <- rep("a3",nrow(data_a3))

head(data_a3)
tail(data_a3)

names(data_a3)<-c("Eligible", "Cheat", "Count", "Reserve", "Precedence")

write.table(data_a3, file='a3.csv',row.names=FALSE,col.names=TRUE)
detach(data_a3)


#-------DATA READY-------#
data<-read.table(file.choose(),header=T)
attach(data)
names(data)<-c("Eligible", "Cheat", "Count", "Reserve", "Precedence")
head(data)
tail(data)
detach(data)


#-------SUBSETS-------#
data_a1   <- data[which(data$Precedence=='a1' & data$Cheat==1),]
data_a2   <- data[which(data$Precedence=='a2' & data$Cheat==1),]
data_a3   <- data[which(data$Precedence=='a3' & data$Cheat==1),]

data_a_0  <- data[which(data$Cheat==1 & data$Reserve==0),] #NO AA
data_e_0  <- data[which(data$Eligible==1 & data$Cheat==1 & data$Reserve==0),] #NO AA
data_in_0 <- data[which(data$Eligible==0 & data$Cheat==1 & data$Reserve==0),] #NO AA
#The precedence order does/should not matter, when there are no affirmative action.

data_e1   <- data[which(data$Precedence=='a1' & data$Eligible==1 & data$Cheat==1),]
data_e2   <- data[which(data$Precedence=='a2' & data$Eligible==1 & data$Cheat==1),]
data_e3   <- data[which(data$Precedence=='a3' & data$Eligible==1 & data$Cheat==1),]

data_in1  <- data[which(data$Precedence=='a1' & data$Eligible==0 & data$Cheat==1),]
data_in2  <- data[which(data$Precedence=='a2' & data$Eligible==0 & data$Cheat==1),]
data_in3  <- data[which(data$Precedence=='a3' & data$Eligible==0 & data$Cheat==1),]
head(data1)
tail(data1)

data_a2_.5 <- data[which(data$Precedence=='a2' & data$Cheat==1 & data$Reserve>.5),]
data_e2_.5 <- data[which(data$Precedence=='a2' & data$Cheat==1 & data$Eligible==1 & data$Reserve>.5),]
data_in2_.5 <- data[which(data$Precedence=='a2' & data$Cheat==1 & data$Eligible==0 & data$Reserve>.5),]



#-------MEANS-------#
means_data_e1  <-by(data_e1$Count/500,data_e1$Reserve,mean)
means_data_in1 <-by(data_in1$Count/500,data_in1$Reserve,mean)
means_data_e2  <-by(data_e2$Count/500,data_e2$Reserve,mean)
means_data_in2 <-by(data_in2$Count/500,data_in2$Reserve,mean)
means_data_e3  <-by(data_e3$Count/500,data_e3$Reserve,mean)
means_data_in3 <-by(data_in3$Count/500,data_in3$Reserve,mean)

mean(data_e1$Count/500)
mean(data_in1$Count/500)
mean(data_e2$Count/500)
mean(data_in2$Count/500)
mean(data_e3$Count/500)
mean(data_in3$Count/500)

mean(data_a1$Count/500)
mean(data_a2$Count/500)
mean(data_a3$Count/500)

mean(data_a_0$Count/500)  #NO AA
mean(data_e_0$Count/500)  #NO AA
mean(data_in_0$Count/500) #NO AA

aggregate(data_a1$Count/500, list(data_a1$Reserve), mean)
aggregate(data_a2$Count/500, list(data_a2$Reserve), mean)
aggregate(data_a3$Count/500, list(data_a3$Reserve), mean)

mean(data_a_0$Count/500)-mean(data_a1$Count/500)
mean(data_a_0$Count/500)-mean(data_a2$Count/500)
mean(data_a_0$Count/500)-mean(data_a3$Count/500)

table(data_a_0$Eligible)

mean(data_a2_.5$Count/500)
mean(data_e2_.5$Count/500)
mean(data_in2_.5$Count/500)


#-------PLOTS-------#
par(mfrow=c(3,2))
boxplot(Count/500~Reserve, data=data_e1, medcol = "red", main="a) Untruthful Incentives - Eligible Students \nby the Reserve-Open Precedene Order", xlab="Reserve Size", ylab="Share")
points(means_data_e1, pch=17,col="orange")
boxplot(Count/500~Reserve, data=data_in1, medcol = "red", main="b) Untruthful Incentives - Ineligible Students \nby the Reserve-Open Precedene Order", xlab="Reserve Size", ylab="Share")
points(means_data_in1, pch=17,col="orange")
boxplot(Count/500~Reserve, data=data_e2, medcol = "red", main="c) Untruthful Incentives - Eligible Students \nby the Toggle Precedene Order", xlab="Reserve Size", ylab="Share")
points(means_data_e2, pch=17,col="orange")
boxplot(Count/500~Reserve, data=data_in2, medcol = "red", main="d) Untruthful Incentives - Ineligible Students \nby the Toggle Precedene Order", xlab="Reserve Size", ylab="Share")
points(means_data_in2, pch=17,col="orange")
boxplot(Count/500~Reserve, data=data_e3, medcol = "red", main="e) Untruthful Incentives - Eligible Students \nby the Open-Reserve Precedene Order", xlab="Reserve Size", ylab="Share")
points(means_data_e3, pch=17,col="orange")
boxplot(Count/500~Reserve, data=data_in3, medcol = "red", main="f) Untruthful Incentives - Ineligible Students \nby the Open-Reserve Precedene Order", xlab="Reserve Size", ylab="Share")
points(means_data_in3, pch=17,col="orange")


#-------SAVE PLOTS-------#
png(file="Untruthful Incentives plot - All.png",width=3150, height=4455, res=500)
par(mfrow=c(3,2))
boxplot(Count/500~Reserve, data=data_e1, medcol = "red", main="a) Untruthful Incentives - Eligible Students \nby the Reserve-Open Precedene Order", xlab="Reserve Size", ylab="Share")
points(means_data_e1, pch=17,col="orange")
boxplot(Count/500~Reserve, data=data_in1, medcol = "red", main="b) Untruthful Incentives - Ineligible Students \nby the Reserve-Open Precedene Order", xlab="Reserve Size", ylab="Share")
points(means_data_in1, pch=17,col="orange")
boxplot(Count/500~Reserve, data=data_e2, medcol = "red", main="c) Untruthful Incentives - Eligible Students \nby the Toggle Precedene Order", xlab="Reserve Size", ylab="Share")
points(means_data_e2, pch=17,col="orange")
boxplot(Count/500~Reserve, data=data_in2, medcol = "red", main="d) Untruthful Incentives - Ineligible Students \nby the Toggle Precedene Order", xlab="Reserve Size", ylab="Share")
points(means_data_in2, pch=17,col="orange")
boxplot(Count/500~Reserve, data=data_e3, medcol = "red", main="e) Untruthful Incentives - Eligible Students \nby the Open-Reserve Precedene Order", xlab="Reserve Size", ylab="Share")
points(means_data_e3, pch=17,col="orange")
boxplot(Count/500~Reserve, data=data_in3, medcol = "red", main="f) Untruthful Incentives - Ineligible Students \nby the Open-Reserve Precedene Order", xlab="Reserve Size", ylab="Share")
points(means_data_in3, pch=17,col="orange")
dev.off()
