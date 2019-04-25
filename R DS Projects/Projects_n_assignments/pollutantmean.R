pollutantmean <- function(directory, pollutant, id = 1:332){
        setwd(directory)
        
        fin_data <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("Date", "sulfate", "nitrate", "ID"))
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
                csv_data <- read.csv(csv_file,strip.white = TRUE)
                fin_data <- rbind(fin_data,csv_data)
        }
        
        if (pollutant == "sulfate")
                mean(fin_data$sulfate, na.rm = TRUE)
        else
                mean(fin_data$nitrate, na.rm = TRUE)
}

