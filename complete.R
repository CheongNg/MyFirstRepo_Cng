complete <- function(directory,id=1:332){
        
        for (i in id) {                                         
                
                ## format the i as file id to match file
                three_digit_id<- if (i < 10) {
                        paste("00",i,sep="")}
                else if (i < 100) {
                        paste("0",i,sep="")}
                else {i}
                
                ## Setting dynamic path and reading csv file        
                path<-paste('C:/Users/Cheong/Desktop/',directory,'/',three_digit_id,'.csv',sep="")
                
                mydata<-read.csv(file=path)
                
                #filter data sets for NAs
                newdata<-mydata[!is.na(mydata$sulfate)&!is.na(mydata$nitrate),]
                
                #create a df based on the loop count and number of rows of the filtered dataset
                new_df<-data.frame(id=i, nob=nrow(newdata))
                
                        if (exists("combined_df")==FALSE) {
                                
                                #create a main df if it does not exist
                                combined_df <- new_df
                                
                        } else {
                                
                                #Append the new df to the main df
                                combined_df <-rbind(combined_df,new_df)
                                
                        }
                
                }
        
        #print out the combined data frame
        print(combined_df)
}
