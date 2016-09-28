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
#Aggregate hours by doing 1-24 schedule
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

########
# Evacuate, TEMP, TRNSPD, TONS
########
uva.pairs(totactsnd[,c("ACCDMG", "Casualty", "EVACUATE", "TEMP", "TONS")])
uva.pairs(xdmg[,c("ACCDMG", "Casualty", "EVACUATE", "TEMP", "TONS")])

######
# Type hypothesis: Derailment is biggest factor
######

#Barplot shows that derailment is the most frequent in derailment
barplot(table(xdmg$TYPE), main = "Accident Type in extreme data", xlab="TYPE", ylab="Frequency")
#look at cost in derailments
plot(1:13, tapply(xdmg$ACCDMG, xdmg$TYPE, sum), type = "h", xlab = "Type", ylab = "Cost", main = "Total Cost per Type extreme")
plot(1:13, tapply(xdmg$ACCDMG, xdmg$TYPE, mean), type = "h", xlab = "Type", ylab = "Cost", main = "Average Cost per Type extreme")
plot(1:13, tapply(xdmg$Casualty, xdmg$TYPE, sum), type = "h", xlab = "Type", ylab = "Casualty", main = "Total Casualty per Type extreme")
plot(1:13, tapply(xdmg$Casualty, xdmg$TYPE, mean), type = "h", xlab = "Type", ylab = "Casualty", main = "Average Casualty per Type extreme")
#notice that explosions have highest average cost; derailments have a value where it makes cost extremely high
boxplot(xdmg[which(xdmg$TYPE==1),]$ACCDMG)$stats
library(lattice)
bwplot(as.factor(TYPE)~ACCDMG, data = xdmg, main = "Box Plots of Total Damage", xlab = "Damage ($)", ylab = "TYPE")
bwplot(as.factor(TYPE)~Casualty, data=xdmg, main="BoxPlots of Casualty", xlab="Casualty", ylab="TYPE")
max(xdmg[which(xdmg$TYPE==1),]$ACCDMG)
which(xdmg$ACCDMG>=16000000)

#get max values
xdmg[154,]$TYPE
xdmg[4802,]$TYPE
xdmg[5666,]
xdmg[5667,]
#remove 5667 because it is a duplicate and skews the extreme damage
#xdmg <- xdmg[-5667,]
#Even after removing and viewing the boxplots, derailment is still the greatest in total cost, but type for
#explosive is greatest in average but only two data points so cannot look at it
length(which(xdmg$TYPE==10))

#DO NOT USE THIS
#attempt to remove both of the other outliers for type
#xdmg <- xdmg[-5666,]
#xdmg <- xdmg[-4802,]
#even after removing the two outliers no change in information so reset and just remove one of the values thats duplicate

#look at derailment information only because rest of the samples are small for extreme damage
table(xdmg$TYPE)
derail <- xdmg[which(xdmg$TYPE==1),]

#Start to build quantitative models
#PCA model shows same relationships between 
derail.pca <- princomp(derail[,c("CARSDMG","EQPDMG", "TRKDMG","ACCDMG","Casualty", "TOTKLD", "TOTINJ")], cor = T)
biplot(derail.pca)
derail$TRKDNSTY

#Multiple linear models
derail.lm<-lm(ACCDMG~.,data=derail[,c('ACCDMG','TEMP','TRNSPD','TONS','CARS', 'DRUG', 'ALCOHOL')])
summary(derail.lm)
nrow(derail[which(derail$DRUG>=1),])
#we find that trnspd/tons are significant at the 0.01 level and drugs is significant at the 0.05 level; cars is 0.1
#remake lm to test for only these factors
derail.lm2 <- lm(ACCDMG~., data=derail[,c('ACCDMG','TRNSPD','TONS','CARS','DRUG')])
summary(derail.lm2)
  
#Step models: Have to remove drug because na values
which(is.na(derail$DRUG))
derail.lm3 <- lm(ACCDMG~., data=derail[,c('ACCDMG','TEMP','TRNSPD','TONS','CARS')])
summary(derail.lm3)
#step model
derail.lm3.step<-step(derail.lm3, trace=F)
summary(derail.lm3.step)

#Partial F test
anova(derail.lm3.step,derail.lm3)
#the same thus using step doesn't really help (actually hurts because can't evaluate information from drug attribute)
