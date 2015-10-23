best <- function(state,outcome){

outcomedata <- read.csv("outcome-of-care-measures.csv", stringsAsFactors=FALSE)
outcomedata <- outcomedata[order(outcomedata$Hospital.Name),]
statenames<-unique(outcomedata$State)
outc <- c("heart attack","heart failure","pneumonia")
outcome <- tolower(outcome)

if(state %in% statenames){
statedata <- subset(outcomedata,outcomedata$State == state)
if(outcome == outc[1]){
m <-min(statedata[,11],na.rm=TRUE)
ind <-  match(m,statedata[,11])
hospital <- statedata[ind,2]
return(hospital)
}else if(outcome == outc[2]){
m <-min(statedata[,17],na.rm=TRUE)
ind <-  match(m,statedata[,17])
hospital <- statedata[ind,2]
return(hospital)
}else if(outcome == outc[3]){
statedata[,23]<-as.numeric(statedata[,23])
m <-min(statedata[,23],na.rm=TRUE)
ind <-  match(m,statedata[,23])
hospital <- statedata[ind,2]
return(hospital)
}else{
stop("Invalid outcome")
}
}
else{
stop("Invalid state")
}
}
