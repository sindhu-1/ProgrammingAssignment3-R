rankhospital <- function(state,outcome,num){

outcomedata <- read.csv("outcome-of-care-measures.csv", stringsAsFactors=FALSE)
outcomedata <- outcomedata[order(outcomedata$Hospital.Name),]
statenames<-unique(outcomedata$State)
outc <- c("heart attack","heart failure","pneumonia")
outcome <- tolower(outcome)

if(state %in% statenames){
statedata <- subset(outcomedata,outcomedata$State == state)
if(outcome == outc[1]){
statedata[,11]<-as.numeric(statedata[,11])
statedata <-statedata[order(statedata[,11]),]
statedata<-statedata[complete.cases(statedata[,11]),]
}else if(outcome == outc[2]){
statedata[,17]<-as.numeric(statedata[,17])
statedata <-statedata[order(statedata[,17]),]
statedata<-statedata[complete.cases(statedata[,17]),]
}else if(outcome == outc[3]){
statedata[,23]<-as.numeric(statedata[,23])
statedata <-statedata[order(statedata[,23]),]
statedata<-statedata[complete.cases(statedata[,23]),]
}else{
stop("Invalid outcome")
}
}
else{
stop("Invalid state")
}
if(is.numeric(num)){
hospital <- statedata[num,2]
}else if(num =="best"){
num <- 1
hospital <- statedata[num,2]
}else if(num =="worst"){
num <- nrow(statedata)
hospital <- statedata[num,2]
}
return(hospital)
}






