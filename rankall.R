rankall <- function(outcome,num="best"){

outcomedata <- read.csv("outcome-of-care-measures.csv", stringsAsFactors=FALSE)
outcomesubdata <- data.frame(HospitalName=outcomedata[,2],State=outcomedata[,7],heartattack=outcomedata[,11],heartfailure=outcomedata[,17],pneumonia=outcomedata[,23],stringsAsFactors=FALSE)
outcomesubdata[,3] <- suppressWarnings(as.numeric(outcomesubdata[,3]))
outcomesubdata[,4] <- suppressWarnings(as.numeric(outcomesubdata[,4]))
outcomesubdata[,5] <- suppressWarnings(as.numeric(outcomesubdata[,5]))
statenames<-unique(outcomesubdata$State)
statenames <- as.data.frame(statenames)
names(statenames)[1]<-"State"
statenames[,1]<-as.character(statenames[,1])
outc <- c("heart attack","heart failure","pneumonia")
outcome <- tolower(outcome)
allstates <-NULL
stateind<-NULL
hospital <- NULL
#outcome<- "pneumonia"
for(x in 1:54){
statedata <- subset(outcomesubdata,outcomesubdata$State == statenames[x,])
if(outcome == outc[1]){
statedata <-statedata[complete.cases(statedata[,3]),]
statedata1 <- statedata[order(statedata[,3],statedata[,1],statedata[,2]),]
statedata1<-cbind(statedata1,rank=seq(1,nrow(statedata1)))

}else if(outcome == outc[2]){
statedata <-statedata[complete.cases(statedata[,4]),]
statedata1 <- statedata[order(statedata[,4],statedata[,1],statedata[,2]),]
statedata1 <-cbind(statedata1,rank=seq(1,nrow(statedata1)))
}else if(outcome == outc[3]){
statedata <-statedata[complete.cases(statedata[,5]),]
statedata1 <-statedata[order(statedata[,5],statedata[,1],statedata[,2]),]
statedata1 <-cbind(statedata1,rank=seq(1,nrow(statedata1)))
}else{
stop("Invalid outcome")
}
allstates <- rbind(allstates,statedata1)
 }
if(is.numeric(num)){
hospital1 <- subset(allstates,allstates$rank == num)
hospital <- merge(statenames,hospital1,by.x="State",all.x=TRUE)
hospital <- hospital[order(hospital$State),]
hospitalrank <- cbind(hospital=hospital[,2],state=hospital[,1]) 
}else if(num =="worst"){
for(i in 1:nrow(statenames)){
statedataW <- subset(allstates,allstates$State == statenames[i,])
n <- as.numeric(table(statedataW$State== statenames[i,])["TRUE"])
hospital1 <- statedataW[n,]
hospital<- rbind(hospital,hospital1)
}
hospital <- merge(statenames,hospital,by.x="State",all.x=TRUE)
hospital <- hospital[order(hospital$State),]
hospitalrank<- cbind(hospital=hospital[,2],state=hospital[,1])
}else if(num =="best"){
num <- 1
hospital <- subset(allstates,allstates$rank == num)
hospital <- merge(statenames,hospital,by.x="State",all.x=TRUE)
hospital <- hospital[order(hospital$State),]
hospitalrank<- cbind(hospital=hospital[,2],state=hospital[,1]) 
}
return(as.data.frame(hospitalrank))
}



