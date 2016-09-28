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
storeRowXAM <- xdmgnd[which(xdmgnd$AMPM=="AM"),]
storeRowXPM <- xdmgnd[which(xdmgnd$AMPM=="PM"),]
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
uva.pairs(xdmgnd[,c("ACCDMG", "Casualty", "CARS", "CARSDMG", "CARSHZD")])

########
# Evacuate, TEMP, TRNSPD, TONS
########
uva.pairs(totactsnd[,c("ACCDMG", "Casualty", "EVACUATE", "TEMP", "TONS")])
uva.pairs(xdmgnd[,c("ACCDMG", "Casualty", "EVACUATE", "TEMP", "TONS")])

######
# Type hypothesis: Derailment is biggest factor
######
#Make type a factor:
xdmgnd$TYPE <- factor(xdmgnd$TYPE)

#Barplot shows that derailment is the most frequent in derailment
barplot(table(xdmgnd$TYPE), main = "Accident Type in extreme data", xlab="TYPE", ylab="Frequency")
#look at cost in derailments
plot(1:13, tapply(xdmgnd$ACCDMG, xdmgnd$TYPE, sum), type = "h", xlab = "Type", ylab = "Cost", main = "Total Cost per Type extreme")
plot(1:13, tapply(xdmgnd$ACCDMG, xdmgnd$TYPE, mean), type = "h", xlab = "Type", ylab = "Cost", main = "Average Cost per Type extreme")
plot(1:13, tapply(xdmgnd$Casualty, xdmgnd$TYPE, sum), type = "h", xlab = "Type", ylab = "Casualty", main = "Total Casualty per Type extreme")
plot(1:13, tapply(xdmgnd$Casualty, xdmgnd$TYPE, mean), type = "h", xlab = "Type", ylab = "Casualty", main = "Average Casualty per Type extreme")
#notice that explosions have highest average cost; derailments have a value where it makes cost extremely high
boxplot(xdmgnd[which(xdmgnd$TYPE==1),]$ACCDMG)$stats
library(lattice)
bwplot(as.factor(TYPE)~ACCDMG, data = xdmgnd, main = "Box Plots of Total Damage", xlab = "Damage ($)", ylab = "TYPE")
bwplot(as.factor(TYPE)~Casualty, data=xdmgnd, main="BoxPlots of Casualty", xlab="Casualty", ylab="TYPE")
max(xdmgnd[which(xdmgnd$TYPE==1),]$ACCDMG)
which(xdmgnd$ACCDMG>=16000000)

#Even after removing and viewing the boxplots, derailment is still the greatest in total cost, but type for
#explosive is greatest in average but only two data points so cannot look at it
length(which(xdmgnd$TYPE==10))

#DO NOT USE THIS
#attempt to remove both of the other outliers for type
#xdmgnd <- xdmgnd[-5666,]
#xdmgnd <- xdmgnd[-4802,]
#even after removing the two outliers no change in information so reset and just remove one of the values thats duplicate

#look at derailment information only because rest of the samples are small for extreme damage
table(xdmgnd$TYPE)
Derail <- rep(0, nrow(xdmgnd))
Derail[which(xdmgnd$TYPE == 1)] <- 1 
Derail <- as.factor(Derail)
contrasts(Derail)

#Start to build quantitative models
#PCA model shows same relationships between 
xdmgnd.pca <- princomp(xdmgnd[,c("CARSDMG","EQPDMG", "TRKDMG","ACCDMG","Casualty", "TOTKLD", "TOTINJ")], cor = T)
biplot(xdmgnd.pca)


#the same thus using step doesn't really help 

#Move on to qualitative view of Type
Derail <- rep(0, nrow(xdmgndnd))
Derail[which(xdmgndnd$TYPE == 1)] <- 1 
Derail <- as.factor(Derail)
contrasts(Derail)

xdmgnd.lm<-lm(ACCDMG~Derail,data=xdmgnd)
summary(xdmgnd.lm)
#There are types that are significant at a p-value <0 

#Continuing analysis with ANCOVA Model
xdmgnd.lm1 <-lm(ACCDMG~Derail+TEMP + TRNSPD + TONS + CARS + HEADEND1,data=xdmgnd)
summary(xdmgnd.lm1)
xdmgnd.lm2<-lm(ACCDMG~(Derail+TEMP + TRNSPD + TONS + CARS + HEADEND1)^2,data=xdmgnd)
summary(xdmgnd.lm2)
#Partial F Test
anova(xdmgnd.lm1,xdmgnd.lm2)

#Use the xdmgnd.lm2 model because significant
summary(xdmgnd.lm2)
library(MASS)
L<-boxcox(xdmgnd.lm2, plotit = F)$x[which.max(boxcox(xdmgnd.lm2, plotit = F)$y)] 
xdmgnd.lm2.boxcox<-lm(ACCDMG^L ~(Derail+TEMP+TRNSPD+TONS+CARS+HEADEND1)^2,data=xdmgnd)

par(mfrow=c(2,2))
plot(xdmgnd.lm2, labels.id = NULL)
par(mfrow=c(1,1))

par(mfrow=c(2,2))
plot(xdmgnd.lm2.boxcox, labels.id = NULL)
par(mfrow=c(1,1))

summary(xdmgnd.lm2.boxcox)
#Based on the model, we can see that derailments do not increase the severity of accidents because the p-value is 
#0.12797.  However the interaction between derailments and train speed decreases severity because the coefficient
#is negative and the p-value is significant for the interaction between derailment and train speed.