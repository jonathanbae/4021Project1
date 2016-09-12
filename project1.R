#Jonathan Bae
setwd("~/School/Fall 2016/SYS 4021/Data/Train Data")
source("D:/Users/Bae/Documents/School/Fall 2016/SYS 4021/InClass/Train Data/AccidentInput.R")
path <- "D:/Users/Bae/Documents/School/Fall 2016/SYS 4021/Data/Train Data"
#Thomas Harrison
setwd("")
source("")
path <- ""
#Zach Calhoun
setwd("")
source("")
path <- ""

#Run all code
#acts is the array of all the years separately
acts <- file.inputl(path) 
comvar <- intersect(colnames(acts[[1]]), colnames(acts[[8]]))
#totacts is the array of all data
totacts <- combine.data(acts, comvar)
dim(totacts)
