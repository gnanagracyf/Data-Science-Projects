corr <- function(directory, threshold = 0,id=1:332){
        setwd(directory)
        
        corr_vec <- numeric()
        for (i in id){
                #print(i)
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
                #print(cc_count)
                if ( cc_count > threshold){
                        #print(i)
                        #print(cc_count)
                        cor_coeff <- cor(df$sulfate,df$nitrate,use="complete.obs")
                        corr_vec <- c(corr_vec,cor_coeff)
                }
              
                        
        }
        c <- corr_vec
        
}