########
#
# Use this code to create data frames and clean data properly
# NOTE: Works once you have totacts established
########
acts <- file.inputl(path) 
comvar <- intersect(colnames(acts[[1]]), colnames(acts[[8]]))
#totacts is the array of all data
totacts <- combine.data(acts, comvar)

# Define causes for analysis
totacts$Cause <- rep(NA, nrow(totacts))
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "M")] <- "M"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "T")] <- "T"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "S")] <- "S"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "H")] <- "H"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "E")] <- "E"
totacts$Cause <- factor(totacts$Cause)

# Define badacts and remove duplicates
totacts$Casualty <- totacts$TOTKLD + totacts$TOTINJ
badacts <- subset(totacts,totacts$Casualty > 0)
badactsnd <- badacts[!(duplicated(badacts[, c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])),]
# should have 2709 observations, let's remove that one with 1001 casualties
which(badactsnd$Casualty == max(badactsnd$Casualty))
# the incident number is 220, which I remove below
badactsnd <- badactsnd[-220,]

# Define extreme accidents and remove duplicates
dmgbox <- boxplot(totacts$ACCDMG)
xdmg <- totacts[totacts$ACCDMG > dmgbox$stats[5],]
xdmgnd <- xdmg[!(duplicated(xdmg[, c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])),]

# should have 5750 observations, let's remove that one with $31.5 M in damage
which(xdmgnd$ACCDMG==max(xdmgnd$ACCDMG))
# You can see below that there are 2 incidents involved with this accident, so I removed both
xdmgnd <- xdmgnd[-5739,]
xdmgnd <- xdmgnd[-5738,]
boxplot(xdmgnd$ACCDMG)

