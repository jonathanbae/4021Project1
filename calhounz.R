
xdmg$Cause <-as.factor(xdmg$Cause)
contrasts(xdmg$Cause)

xdmg.lm1 <-lm(ACCDMG~Cause, data = xdmg)
summary(xdmg.lm1)
# Looks like cause M is the most significant.
contrasts(xdmg$Cause)<-matrix(c(0,1,0,0,0, 0,0,1,0,0, 0,0,0,1,0,  0,0,0,0,1),nrow=5)
colnames(contrasts(xdmg$Cause)) <-matrix(c("H","M","S","T"),ncol=4)
contrasts(xdmg$Cause)

xdmg.lm2 <- lm(ACCDMG~Cause+TRNSPD+TONS+CARS, data = xdmg)
xdmg.lm2.step <-step(xdmg.lm2)
summary(xdmg.lm2.step)
summary(xdmg.lm1)
mean(xdmg$ACCDMG)

## So freight trains are a huge factor.

xdmg$freight <- (xdmg$TYPEQ == 1)

xyplot(ACCDMG~TRNSPD | freight, data = xdmg, type = c("p", "r"))
library(lattice)
xdmg$human <-(xdmg$Cause == "E")

xdmg$derail <-(xdmg$TYPE == "1")
badacts$derail <-(badacts$TYPE == "1")

TRNSPD.factor<-badacts$TRNSPD
TRNSPD.factor[which(badacts$TRNSPD>=50)]<-'high train speed'
TRNSPD.factor[which(badacts$TRNSPD<50)]<-'low train speed'

interaction.plot(TRNSPD.factor, badacts$derail, badacts$Casualty, main="Trainspeed and Derailment Interaction", ylab = "Mean of Casualties", xlab = "Trainspeed")

TRNSPD.factor<-xdmg$TRNSPD
TRNSPD.factor[which(xdmg$TRNSPD>=50)]<-'high train speed'
TRNSPD.factor[which(xdmg$TRNSPD<50)]<-'low train speed'

interaction.plot(xdmg$freight, xdmg$derail, xdmg$ACCDMG)

interaction.plot(xdmg$derail, CARS.factor,xdmg$ACCDMG)

interaction.plot(xdmg$derail,TRNSPD.factor,xdmg$ACCDMG)
