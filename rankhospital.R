rankhospital <- function(state, outcome, num = "best") {
        
        ## Read outcome data
        outcomes<-read.csv("outcome-of-care-measures.csv",na.strings="Not Available",stringsAsFactors=FALSE)
        
        ## Renaming columns for easier reference
        names(outcomes)[2]<-"hospital"
        names(outcomes)[7]<-"state"
        names(outcomes)[11]<-"heart attack"
        names(outcomes)[17]<-"heart failure"
        names(outcomes)[23]<-"pneumonia"
        
        ## Check that state and outcome are valid
        checkdata <- outcomes[outcomes$state ==state,]
        
        if(nrow(checkdata)==0) {
                
                stop("invalid state")
                
        } else if (outcome != "heart attack" && outcome != "heart failure" && outcome != "pneumonia") {
                
                stop("invalid outcome")
                
        } else {
                
                ## Return hospital name in that state with the given rank 30-day death rate
                if(is.numeric(num)){
                        
                        #Will return NA if num exceeds the number of rows in this dataset
                        outcomes <- outcomes[order(outcomes[[outcome]],outcomes[,2]),]
                        testdata <- outcomes[outcomes[,7] ==state,]
                        answer <- testdata[num,2]
                        
                } else if(num == "worst"){
                        
                        #rev() used to sort the specified outcome column in descending order
                        outcomes <- outcomes[order(outcomes[[outcome]],rev(outcomes[,2]),decreasing = TRUE),]
                        testdata <- outcomes[outcomes[,7] ==state,]
                        answer <- testdata[1,2]
                        
                } else {
                        
                        #default answer
                        outcomes <- outcomes[order(outcomes[,7],outcomes[[outcome]],outcomes[,2]),]
                        testdata <- outcomes[outcomes[,7] ==state,]
                        answer <- testdata[1,2]
                        
                }
                
                
        }       
        
        print(answer)
}
