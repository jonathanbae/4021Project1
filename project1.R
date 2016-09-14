#Jonathan Bae
setwd("~/School/Fall 2016/SYS 4021/Data/Train Data")
source("D:/Users/Bae/Documents/School/Fall 2016/SYS 4021/InClass/Train Data/AccidentInput.R")
path <- "D:/Users/Bae/Documents/School/Fall 2016/SYS 4021/Data/Train Data"
#Thomas Harrison
setwd("")
source("")
path <- ""
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
#   Remove duplicates to clean data
#
#############################

dim(totacts)
totactsnd <- totacts[!(duplicated(totacts[, c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])),]
dim(totactsnd)

#############################
#ZC
#   Set up extreme points (in case we want to use)
#
#############################

dmgbox <- boxplot(totacts$ACCDMG)
xdmg <- totacts[totacts$ACCDMG > dmgbox$stats[5],]
