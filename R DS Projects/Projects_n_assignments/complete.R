complete <- function(directory,id=1:332){
        setwd(directory)
        
        
        nobs <- numeric()
        id_vec <- numeric()
        for (i in id){
               
                id_val <- "001"
                if (i < 10)
                        id_val <- paste("00",i,sep='')
                else if(i < 100)
                        id_val <- paste("0",i,sep='')
                else
                        id_val <- i
                
                csv_file <- paste(id_val,".csv",sep='')
                df <- read.csv(csv_file)
               
                
                cc <- complete.cases(df)
                cc_count <- sum(cc)
                nobs <- c(nobs,cc_count)
                id_vec <- c(id_vec,i)
        }
        
        df <- data.frame(id = id_vec, nobs )
        
}
        
        
        