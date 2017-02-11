rankall <- function(outcome, num = "best") {
        ## Read outcome data
        outcomes<-read.csv("outcome-of-care-measures.csv",na.strings="Not Available",stringsAsFactors=FALSE)
        
        ## Renaming columns for easier reference
        names(outcomes)[2]<-"hospital"
        names(outcomes)[7]<-"state"
        names(outcomes)[11]<-"heart attack"
        names(outcomes)[17]<-"heart failure"
        names(outcomes)[23]<-"pneumonia"
        
        unique_state<-outcomes[,"state"]
        unique_state<-unique(unique_state)
        unique_state<-sort(unique_state)
        
        bigdf = NULL
        
        ## Check that state and outcome are valid
        
        if (outcome != "heart attack" && outcome != "heart failure" && outcome != "pneumonia") {
                
                stop("invalid outcome")
                
        } else {
                
                if (is.numeric(num)){
                        
                        for (i in seq_along(unique_state)) {
                                
                                outcomes <- outcomes[order(outcomes[[outcome]],outcomes[,2]),]
                                testdata <- outcomes[outcomes[,7] == unique_state[i],]
                                
                                selected_hospital <- testdata[num,2]
                                
                                newdf<-data.frame(selected_hospital,unique_state[i])
                                
                                if (exists("bigdf")==FALSE) {
                                        
                                        bigdf <- newdf
                                        
                                } else {
                                        
                                        bigdf<-rbind(bigdf,newdf)
                                        
                                }
                                
                                
                        }
                        
                        
                }  else if(num == "worst"){
                        
                        for (i in seq_along(unique_state)) {
                                
                                outcomes <- outcomes[order(outcomes[[outcome]],rev(outcomes[,2]),decreasing = TRUE),]
                                outcomes <- outcomes[!is.na(outcomes[[outcome]]),]
                                
                                testdata <- outcomes[outcomes[,7] == unique_state[i],]
                                
                                selected_hospital <- testdata[1,2]
                                
                                newdf<-data.frame(selected_hospital,unique_state[i])
                                
                                if (exists("bigdf")==FALSE) {
                                        
                                        bigdf <- newdf
                                        
                                } else {
                                        
                                        bigdf<-rbind(bigdf,newdf)
                                        
                                }
                                
                        }
                        
                } else {
                        
                        for (i in seq_along(unique_state)) {
                                
                                outcomes <- outcomes[order(outcomes[[outcome]],outcomes[,2]),]
                                testdata <- outcomes[outcomes[,7] == unique_state[i],]
                                
                                selected_hospital <- testdata[1,2]
                                
                                newdf<-data.frame(selected_hospital,unique_state[i])
                                
                                if (exists("bigdf")==FALSE) {
                                        
                                        bigdf <- newdf
                                        
                                } else {
                                        
                                        bigdf<-rbind(bigdf,newdf)
                                        
                                }
                                
                                
                        }   
                        
                } 
                
        }
        
        names(bigdf)[1]<-"hospital"
        names(bigdf)[2]<-"state"
        bigdf
        
        
        ## For each state, find the hospital of the given rank
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
}

