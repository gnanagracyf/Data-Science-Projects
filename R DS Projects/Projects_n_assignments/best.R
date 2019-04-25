#na.strings="Not Available" and stringsAsFactors=FALSE user this while reading so that we ccan sort num n alpha easily
best <- function(state, outcome) {
        ## Read outcome data
        outcomedf <- read.csv("outcome-of-care-measures.csv", colClasses = "character", ?data, stringsAsFactors=FALSE)
        ## Check that state and outcome are valid
        possible_outcome <- c("heart attack","heart failure","pneumonia")
        
        if (!state %in% outcomedf$State){
                stop("invalid State")
        }       
        if (!outcome %in% possible_outcome){
                stop("invalid outcome")
        }
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        if (outcome == "heart attack"){
                my_data <- c("Hospital.Name","State","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack")
                df <- outcomedf[my_data]
                res_df <- df[df$State == state, ]
                final_df <-  na.omit(res_df)
                final_df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(final_df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
                orderedData <- final_df[order(final_df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,final_df$Hospital.Name), ]
                
        }
        else if (outcome == "heart failure")
        {
                my_data <- c("Hospital.Name","State","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")
                df <- outcomedf[my_data]
                res_df <- df[df$State == state, ]
                final_df <-  na.omit(res_df)
                final_df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(final_df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
                orderedData <- final_df[order(final_df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,final_df$Hospital.Name), ]
                
        }
        else{
                my_data <- c("Hospital.Name","State","Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
                df <- outcomedf[my_data]
                res_df <- df[df$State == state, ]
                final_df <-  na.omit(res_df)
                final_df$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(final_df$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
                orderedData <- final_df[order(final_df$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,final_df$Hospital.Name), ]
                
        }
  
        orderedData$Hospital.Name[1]
        
        
}
