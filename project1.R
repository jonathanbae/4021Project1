#Jonathan Bae
setwd("~/School/Fall 2016/SYS 4021/Data/Train Data")
source("D:/Users/Bae/Documents/School/Fall 2016/SYS 4021/InClass/Train Data/AccidentInput.R")
source("D:/Users/Bae/Documents/School/Fall 2016/SYS 4021/InClass/Train Data/PCAplots.R")
source("D:/Users/Bae/Documents/School/Fall 2016/SYS 4021/InClass/Train Data/SPM_Panel.R")
source("D:/Users/Bae/Documents/School/Fall 2016/SYS 4021/InClass/Train Data/TestSet-1.R")
path <- "D:/Users/Bae/Documents/School/Fall 2016/SYS 4021/Data/Train Data"
#Thomas Harrison
setwd("/Users/ThomasHarrison/Desktop/SYS 4021/Data")
source("/Users/ThomasHarrison/Desktop/SYS 4021/Code/AccidentInput.R")
path <- "/Users/ThomasHarrison/Desktop/SYS 4021/Data"
#Zach Calhoun -- I sourced everything
setwd("C:/Users/Student/Desktop/Fall 2016/SYS 4021/Univariate")
source("AccidentInput.R")
source("PCAplots.R")
source("SPM_Panel.R")
path <- "C:/Users/Student/Desktop/Fall 2016/SYS 4021/Univariate/path"

#Run all code
#acts is the array of all the years separately
acts <- file.inputl(path) 
comvar <- intersect(colnames(acts[[1]]), colnames(acts[[8]]))
#totacts is the array of all data
totacts <- combine.data(acts, comvar)
dim(totacts)

#############################
#ZC
#   Remove duplicates to clean data, create cause variable so no repeaat, create casualty variable
#
#############################

####
#Create a Casualty variable (TOTINJ+TOTKLD)
totacts$Casualty <- totacts$TOTKLD + totacts$TOTINJ

totacts$Cause <- rep(NA, nrow(totacts))
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "M")] <- "M"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "T")] <- "T"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "S")] <- "S"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "H")] <- "H"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "E")] <- "E"
totacts$Cause <- factor(totacts$Cause)
table(totacts$Cause)

totactsnd <- totacts[!(duplicated(totacts[, c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])),]
dim(totactsnd)

#############################
#ZC
#   Set up extreme points (in case we want to use)
#
#############################

dmgbox <- boxplot(totactsnd$ACCDMG)
xdmg <- totactsnd[totactsnd$ACCDMG > dmgbox$stats[5],]

#Not sure if this is necessary
rownames(xdmg) <- NULL

#############################
#
#   Look at causes better 
#
#   DO NOT NEED THIS SECTION
#
#############################

totactsnd$Cause <- rep(NA, nrow(totactsnd))

totactsnd$Cause[which(substr(totactsnd$CAUSE, 1, 1) == "M")] <- "M"
totactsnd$Cause[which(substr(totactsnd$CAUSE, 1, 1) == "T")] <- "T"
totactsnd$Cause[which(substr(totactsnd$CAUSE, 1, 1) == "S")] <- "S"
totactsnd$Cause[which(substr(totactsnd$CAUSE, 1, 1) == "H")] <- "H"
totactsnd$Cause[which(substr(totactsnd$CAUSE, 1, 1) == "E")] <- "E"

# This new variable, Cause, has to be a factor

totactsnd$Cause <- factor(totactsnd$Cause)

table(totactsnd$Cause)

##DOES THE SAME FOR xdmg
xdmg$Cause <- rep(NA, nrow(xdmg))

xdmg$Cause[which(substr(xdmg$CAUSE, 1, 1) == "M")] <- "M"
xdmg$Cause[which(substr(xdmg$CAUSE, 1, 1) == "T")] <- "T"
xdmg$Cause[which(substr(xdmg$CAUSE, 1, 1) == "S")] <- "S"
xdmg$Cause[which(substr(xdmg$CAUSE, 1, 1) == "H")] <- "H"
xdmg$Cause[which(substr(xdmg$CAUSE, 1, 1) == "E")] <- "E"

# This new variable, Cause, has to be a factor

xdmg$Cause <- factor(xdmg$Cause)

table(xdmg$Cause)

#############################
#
#   Let's determine which response variables correlate with ACCDMG
# NOT SURE IF THE BELOW IS CORRECT -- I HAVE HAD ISSUES
#
#############################

xdmg.lm1<-lm(ACCDMG~.,data=xdmg[,c('ACCDMG','TEMP','TRNSPD','TONS','CARS')])
##                                   ,'LOADF1','TIMEHR','LOADP1','EQPDMG'
  #                                 ,'TRKDMG','ENGHR','CDTRHR')])

#Stepwise will decide which response variables demonstrate promise.
#WARNING: May take a while to look at all combinations.
xdmg.lm1.step<-step(xdmg.lm1, trace = F)
summary(xdmg.lm1)
summary(xdmg.lm1.step)

########
#
# Look at Cause
#
########
#normal
barplot(table(totactsnd$TYPE), main = "Accident Type in extreme data", xlab="TYPE", ylab="Frequency")


