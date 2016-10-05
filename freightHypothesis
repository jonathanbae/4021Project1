###########
#
# This document focuses on freight train analysis
#
###########

#First thing, look at quantitative variables that may be of use

uva.pairs(totactsnd[,c("ACCDMG", "Casualty", "CARS", "CARSDMG", "CARSHZD")])
uva.pairs(xdmgnd[,c("ACCDMG", "Casualty", "CARS", "CARSDMG", "CARSHZD")])

# We see that carsdmg and carshzd correlates most.

uva.pairs(xdmgnd[,c("ACCDMG", "Casualty", "EVACUATE", "TEMP", "TONS")])

#we see that tons is probably best

uva.pairs(xdmgnd[,c("ACCDMG", "TONS", "TRNSPD", "CARSDMG", "CARSHZD")])

#We see that Tons, TRNSPD, and CARSHZD are best.

uva.pairs(xdmgnd[,c("ACCDMG", "Casualty","TONS", "TRNSPD", "TEMP","CARSDMG", "CARSHZD")])


#####
# Let's create our first linear models.

xdmgnd.lm1 <- lm(ACCDMG~TONS+TRNSPD+TEMP+CARS+CARSDMG+CARSHZD+HEADEND1, data = xdmgnd)
badactsnd.lm1 <-lm(Casualty~TONS+TRNSPD+TEMP+CARS+CARSDMG+CARSHZD+HEADEND1, data = badactsnd)

# Use stepwise to reduce to best variables
xdmgnd.lm2.step <- step(xdmgnd.lm1)
badactsnd.lm2.step <- step(badactsnd.lm1)

# Summary of results from step method
summary(xdmgnd.lm2.step)
summary(badactsnd.lm2.step)

# Compare models to their step models
BIC(xdmgnd.lm1)
BIC(xdmgnd.lm2.step)

BIC(xdmgnd.lm2.step)
BIC(badactsnd.lm2.step)

# Let's look at diagnostics of both.
par(mfrow=c(2,2))
plot(xdmgnd.lm1, labels.id = NULL)
plot(badactsnd.lm1, labels.id = NULL)
par(mfrow=c(1,1)) 

#Perform boxcox transform to both models
library(MASS)
# XDMGND
L<-boxcox(xdmgnd.lm1, plotit = F)$x[which.max(boxcox(xdmgnd.lm1, plotit = F)$y)] 
xdmgnd.lm2.boxcox<-lm(ACCDMG^L~TONS+TRNSPD+TEMP+CARS+CARSDMG+CARSHZD+HEADEND1,data=xdmgnd)

#BADACTSND
L2<-boxcox(badactsnd.lm1, plotit = F)$x[which.max(boxcox(badactsnd.lm1, plotit = F)$y)] 
badactsnd.lm2.boxcox<-lm(Casualty^L2~TONS+TRNSPD+TEMP+CARS+CARSDMG+CARSHZD+HEADEND1,data=badactsnd)

xdmgnd.lm3.boxcox.step <-step(xdmgnd.lm2.boxcox)
badactsnd.lm3.boxcox.step <-step(badactsnd.lm2.boxcox)

# NOW we want to determine if there is more causality for one than the other.
summary(xdmgnd.lm3.boxcox.step)
summary(badactsnd.lm3.boxcox.step)

###############
###############
# Now we move onto analysis of categorical variables.
###############
#first, we need to recode train types (next two lines explain why if you enter)
xdmgnd$TYPEQ <-as.factor(xdmgnd$TYPEQ)
contrasts(xdmgnd$TYPEQ)
xdmgnd$TYPEQADJ <- rep(NA, nrow(xdmgnd))
xdmgnd$TYPEQADJ[which(substr(xdmgnd$TYPEQ, 1, 1) == "1")] <- "1"
xdmgnd$TYPEQADJ[which(substr(xdmgnd$TYPEQ, 1, 1) == "2")] <- "2"
xdmgnd$TYPEQADJ[which(substr(xdmgnd$TYPEQ, 1, 1) == "3")] <- "3"
xdmgnd$TYPEQADJ[which(substr(xdmgnd$TYPEQ, 1, 1) == "4")] <- "4"
xdmgnd$TYPEQADJ[which(substr(xdmgnd$TYPEQ, 1, 1) == "5")] <- "5"
xdmgnd$TYPEQADJ[which(substr(xdmgnd$TYPEQ, 1, 1) == "6")] <- "6"
xdmgnd$TYPEQADJ[which(substr(xdmgnd$TYPEQ, 1, 1) == "7")] <- "7"
xdmgnd$TYPEQADJ[which(substr(xdmgnd$TYPEQ, 1, 1) == "8")] <- "8"
xdmgnd$TYPEQADJ[which(substr(xdmgnd$TYPEQ, 1, 1) == "9")] <- "9"

xdmgnd$TYPEQADJ <- as.factor(xdmgnd$TYPEQADJ)

xdmgnd.lm4 <-lm(ACCDMG~TYPEQADJ, data = xdmgnd)

summary(xdmgnd.lm4)

# Now we need to recode the variable
contrasts(xdmgnd$TYPEQADJ)<-matrix(c(1,0,0,0,0,0,0,0,0, 
                                     0,0,1,0,0,0,0,0,0, 
                                     0,0,0,1,0,0,0,0,0,
                                     0,0,0,0,1,0,0,0,0,
                                     0,0,0,0,0,1,0,0,0,
                                     0,0,0,0,0,0,1,0,0,
                                     0,0,0,0,0,0,0,1,0,
                                     0,0,0,0,0,0,0,0,1),nrow=9)

contrasts(xdmgnd$TYPEQADJ)
colnames(contrasts(xdmgnd$TYPEQADJ)) <-matrix(c("1","3","4","5","6","7","8","9"),ncol=8)

# Now redo linear model
xdmgnd.lm4 <-lm(ACCDMG~TYPEQADJ, data = xdmgnd)
summary(xdmgnd.lm4)

####
# Same recoding for badactsnd

badactsnd$TYPEQADJ <- rep(NA, nrow(badactsnd))
badactsnd$TYPEQADJ[which(substr(badactsnd$TYPEQ, 1, 1) == "1")] <- "1"
badactsnd$TYPEQADJ[which(substr(badactsnd$TYPEQ, 1, 1) == "2")] <- "2"
badactsnd$TYPEQADJ[which(substr(badactsnd$TYPEQ, 1, 1) == "3")] <- "3"
badactsnd$TYPEQADJ[which(substr(badactsnd$TYPEQ, 1, 1) == "4")] <- "4"
badactsnd$TYPEQADJ[which(substr(badactsnd$TYPEQ, 1, 1) == "5")] <- "5"
badactsnd$TYPEQADJ[which(substr(badactsnd$TYPEQ, 1, 1) == "6")] <- "6"
badactsnd$TYPEQADJ[which(substr(badactsnd$TYPEQ, 1, 1) == "7")] <- "7"
badactsnd$TYPEQADJ[which(substr(badactsnd$TYPEQ, 1, 1) == "8")] <- "8"
badactsnd$TYPEQADJ[which(substr(badactsnd$TYPEQ, 1, 1) == "9")] <- "9"

badactsnd$TYPEQADJ <- as.factor(badactsnd$TYPEQADJ)

badactsnd.lm4 <-lm(Casualty~TYPEQADJ, data = badactsnd)

summary(badactsnd.lm4)

# Now we need to recode the variable
contrasts(xdmgnd$TYPEQADJ)<-matrix(c(1,0,0,0,0,0,0,0,0, 
                                     0,0,1,0,0,0,0,0,0, 
                                     0,0,0,1,0,0,0,0,0,
                                     0,0,0,0,1,0,0,0,0,
                                     0,0,0,0,0,1,0,0,0,
                                     0,0,0,0,0,0,1,0,0,
                                     0,0,0,0,0,0,0,1,0,
                                     0,0,0,0,0,0,0,0,1),nrow=9)

contrasts(xdmgnd$TYPEQADJ)
colnames(contrasts(xdmgnd$TYPEQADJ)) <-matrix(c("1","3","4","5","6","7","8","9"),ncol=8)



table(xdmgnd$TYPEQ)

# What percentage of trains are freight accidents?
table(totactsnd$TYPEQ)
nrow(totactsnd)
(1805+576)/43081

18855/43081

#######################################
#
# NOW: To add freight to our linear model
#
#######################################

xdmgnd$freight <- (xdmgnd$TYPEQ == 1)
contrasts(xdmgnd$freight)
summary(xdmgnd.lm3.boxcox.step)

xdmgnd.lm4 <- lm(ACCDMG^L~(TONS+TRNSPD+CARS+CARSDMG+HEADEND1+freight)^2 , data = xdmgnd)
summary(xdmgnd.lm4)
xdmgnd.lm4.step <- step(xdmgnd.lm4)
summary(xdmgnd.lm4.step)

# Now lets focus on specific interactions
# We want to look at freight with tons, train speed, and head end locomotives.

#With tons
TONS.factor<-xdmgnd$TONS
TONS.factor[which(xdmgnd$TONS < mean(xdmgnd$TONS))]<-'low tons'
TONS.factor[which(xdmgnd$TONS>= mean(xdmgnd$TONS))]<-'high tons'

interaction.plot(xdmgnd$freight, TONS.factor ,xdmgnd$ACCDMG,main="Interaction of Tons and Freight", xlab = "Freight", ylab="Average Accident Damage ($)")

# With train speed

TRNSPD.factor<-xdmgnd$TRNSPD
TRNSPD.factor[which(xdmgnd$TRNSPD<50)]<-'low train speed'
TRNSPD.factor[which(xdmgnd$TRNSPD>=50)]<-'high train speed'

interaction.plot(xdmgnd$freight, TRNSPD.factor, TONS.factor, xdmgnd$ACCDMG)

# With head end locomotives

HEADEND1.factor <-xdmgnd$HEADEND1
HEADEND1.factor[which(xdmgnd$HEADEND1 < mean(xdmgnd$HEADEND1))]<-'fewer headend'
HEADEND1.factor[which(xdmgnd$HEADEND1>= mean(xdmgnd$HEADEND1))]<-'more headend'

interaction.plot(xdmgnd$freight, HEADEND1.factor, xdmgnd$ACCDMG)

## 
## Looks like the only meaningful interaction is between freight trains and tons.
##
