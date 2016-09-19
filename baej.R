#After all the data

##########
#
##Look at the time variable
#
##########
#all data; after observing notice that time is more of a categorical variable 
#(not a continuous scale doesn't say much)
hist(totactsnd$TIMEHR)
boxplot(totactsnd$TIMEMIN)
#check if ampm is significant (not really)
table(totactsnd$AMPM)
#After graphing does not seem like hours/ampm really effects the information
storeRowAM <- totactsnd[which(totactsnd$AMPM=="AM"),]
storeRowPM <- totactsnd[which(totactsnd$AMPM=="PM"),]
storeRowXAM <- xdmg[which(xdmg$AMPM=="AM"),]
storeRowXPM <- xdmg[which(xdmg$AMPM=="PM"),]
par(mfrow=c(1,2))
  plot(1:12, tapply(storeRowAM$ACCDMG, storeRowAM$TIMEHR, sum), type = "l", xlab = "Time AM", ylab = "Total Damage ($)", main = "Total Accident Damage per Year")
  plot(1:12, tapply(storeRowPM$ACCDMG, storeRowPM$TIMEHR, sum), type = "l", xlab = "Time PM", ylab = "Total Damage ($)", main = "Total Accident Damage per Year")
par(mfrow=c(1,1))
par(mfrow=c(1,2))
  plot(1:12, tapply(storeRowXAM$ACCDMG, storeRowXAM$TIMEHR, sum), type = "l", xlab = "Time AM", ylab = "Total Damage ($)", main = "Total Accident Damage per Year")
  plot(1:12, tapply(storeRowXPM$ACCDMG, storeRowXPM$TIMEHR, sum), type = "l", xlab = "Time PM", ylab = "Total Damage ($)", main = "Total Accident Damage per Year")
par(mfrow=c(1,1))

par(mfrow=c(1,2))
  plot(1:12, tapply(storeRowAM$Casualty, storeRowAM$TIMEHR, sum), type = "l", xlab = "Time AM", ylab = "Casualty", main = "Total Casualty per Year")
  plot(1:12, tapply(storeRowPM$Casualty, storeRowPM$TIMEHR, sum), type = "l", xlab = "Time PM", ylab = "Casualty", main = "Total Casualty per Year")
par(mfrow=c(1,1))
par(mfrow=c(1,2))
  plot(1:12, tapply(storeRowXAM$Casualty, storeRowXAM$TIMEHR, sum), type = "l", xlab = "Time AM", ylab = "Casualty", main = "Total Casualty per Year")
  plot(1:12, tapply(storeRowXPM$Casualty, storeRowXPM$TIMEHR, sum), type = "l", xlab = "Time PM", ylab = "Casualty", main = "Total Casualty per Year")
par(mfrow=c(1,1))

#########
#
#Look at the cars, carsdmg, carshzd variable
#
#########
uva.pairs(totactsnd[,c("ACCDMG", "Casualty", "CARS", "CARSDMG", "CARSHZD")])
uva.pairs(xdmg[,c("ACCDMG", "Casualty", "CARS", "CARSDMG", "CARSHZD")])
