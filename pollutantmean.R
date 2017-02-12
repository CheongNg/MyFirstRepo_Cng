pollutantmean<-function(directory,pollutant,id=1:332){
#Intro:
#user defined function to calculate mean from a specific set of files and path
#This function will cycle through a continuous set of files, collect data from the files and calculate the mean of the data.

        ##loop to look cycle through the files
        for (i in id) {                                         
        
        ## format the i as file id to match file
        three_digit_id<- if (i < 10) {
                                paste("00",i,sep="")}
                                else if (i < 100) {
                                        paste("0",i,sep="")}
                                else {i}

        ## Setting dynamic path and reading csv file        
        path<-paste('C:/Users/Cheong/Desktop/',directory,'/',three_digit_id,'.csv',sep="")
        newdata<-read.csv(file=path)
        
                ## Combining data frames, if statement applied for the 1st loop
                if (exists("mydata")==FALSE) {
                        
                        mydata <- newdata
                        
                } else {
                
                mydata<-rbind(mydata,newdata)
                
                }

        }
        
        ## Extracting the correct subset to calculate mean
        x<-mydata[pollutant]
        bad<-is.na(x)
        new_x<-x[!bad]
        mean(new_x)

}