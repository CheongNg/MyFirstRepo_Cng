corr<-function(directory,threshold = 0){
        
        for (i in 1:332) {                                         
                
                ## format the i as file id to match file
                three_digit_id<- if (i < 10) {
                        paste("00",i,sep="")}
                else if (i < 100) {
                        paste("0",i,sep="")}
                else {i}
                
                ## Setting dynamic path and reading csv file        
                path<-paste('C:/Users/Cheong/Desktop/',directory,'/',three_digit_id,'.csv',sep="")
                
                mydata<-read.csv(file=path)
                
                #filter dataset for NAs
                newdata<-mydata[!is.na(mydata$sulfate)&!is.na(mydata$nitrate),]
                
                #filter for files that is above the threshold
                if (nrow(newdata)>threshold){
                       
                        cor_points<-cor(newdata$sulfate,newdata$nitrate) 
                
                                if (exists("vtr_cor_points")==FALSE) {
                                        
                                        #Start the vector if it does not exist
                                        vtr_cor_points <- c(cor_points)
                                        
                                } else {
                                        
                                        #Add new correlation points to vector
                                        vtr_cor_points<-c(vtr_cor_points,cor_points)
                                        
                                }        
                        
                } else {
                        
                        NULL
                }
                
        }
        #Print out the vector
        vtr_cor_points
}


