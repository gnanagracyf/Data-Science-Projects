rankall <- function(outcome, num = "best") {
        ## Read outcome data
        outcomedf <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings="Not Available", stringsAsFactors=FALSE)
        ## Check that state and outcome are valid
        possible_outcome <- c("heart attack","heart failure","pneumonia")
              
        if (!outcome %in% possible_outcome){
                stop("invalid outcome")
        }
        
        ## For each state, find the hospital of the given rank
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        if (outcome == "heart attack"){
                my_data <- c("Hospital.Name","State","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack")
                df <- outcomedf[my_data]
                final_df <-  na.omit(df)
                final_df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(final_df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
                orderedData <- final_df[order(final_df$State,final_df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,final_df$Hospital.Name), ]
        }
        else if (outcome == "heart failure")
        {
                my_data <- c("Hospital.Name","State","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")
                df <- outcomedf[my_data]
                final_df <-  na.omit(df)
                final_df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(final_df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
                orderedData <- final_df[order(final_df$State,final_df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,final_df$Hospital.Name), ]
                
                
        }
        else{
                my_data <- c("Hospital.Name","State","Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
                df <- outcomedf[my_data]
                final_df <-  na.omit(df)
                final_df$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(final_df$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
                orderedData <- final_df[order(final_df$State,final_df$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,final_df$Hospital.Name), ]
                
                
        }
        
        if ( num == "best")
                result_df <- lapply(split(orderedData$Hospital.Name,orderedData$State),function(x) x[1])
        else if ( num == "worst")
                result_df <- lapply(split(orderedData$Hospital.Name,orderedData$State),function(x) tail(x,1))
        else 
                result_df <- lapply(split(orderedData$Hospital.Name,orderedData$State),function(x) x[num])
        
        
        #result_df <- lapply(split(orderedData$Hospital.Name,orderedData$State),function(x) x[n])
        hospital_val = unlist(result_df,use.names = FALSE)
        state_val = names(result_df)
        df <- data.frame(hospital = hospital_val, state = state_val)
        df
}
