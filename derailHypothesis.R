##########################################################
#Find features for ACCDMG
#Start to build quantitative models
#PCA model shows same relationships between 
xdmgnd.pca <- princomp(xdmgnd[,c("ACCDMG","TRNSPD", "TONS", "CARS", "TIMEHR", "TEMP", "HEADEND1")], cor = T )
biplot(xdmgnd.pca,main="Accident Damage PCA")
badactsnd.pca <- princomp(badactsnd[,c("Casualty","TRNSPD", "TONS", "CARS", "TIMEHR", "TEMP","HEADEND1")], cor = T )
biplot(badactsnd.pca,main="Casualty Damage PCA")
###########################################################
################################### ACCDMG
#Move on to qualitative view of Type
Derail <- rep(0, nrow(xdmgnd))
Derail[which(xdmgnd$TYPE == 1)] <- 1 
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

##############################################
###############################################
#badactsndalties

Derail <- rep(0, nrow(badactsnd))
Derail[which(badactsnd$TYPE == 1)] <- 1 
Derail <- as.factor(Derail)
contrasts(Derail)

badactsnd.lm<-lm(Casualty~Derail,data=badactsnd)
summary(badactsnd.lm)
#There are types that are significant at a p-value <0 

#Continuing analysis with ANCOVA Model
badactsnd.lm1 <-lm(Casualty~Derail+TEMP + TRNSPD,data=badactsnd)
summary(badactsnd.lm1)
badactsnd.lm2<-lm(Casualty~(Derail+TEMP + TRNSPD)^2,data=badactsnd)
summary(badactsnd.lm2)
#Partial F Test
anova(badactsnd.lm1,badactsnd.lm2)

#Use the xdmgnd.lm2 model because significant
summary(badactsnd.lm2)
library(MASS)
L<-boxcox(badactsnd.lm2, plotit = F)$x[which.max(boxcox(badactsnd.lm2, plotit = F)$y)] 
badactsnd.lm2.boxcox<-lm(Casualty^L ~(Derail+TEMP + TRNSPD)^2,data=badactsnd)

par(mfrow=c(2,2))
plot(badactsnd.lm2, labels.id = NULL)
par(mfrow=c(1,1))

par(mfrow=c(2,2))
plot(badactsnd.lm2.boxcox, labels.id = NULL)
par(mfrow=c(1,1))

summary(badactsnd.lm2)
