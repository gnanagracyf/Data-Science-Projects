best <- function(state, outcome) {
        ## Read outcome data
        outcomedf <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
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
                result <- lapply(split(outcomedf$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,outcomedf$State),min,na.rm=TRUE)
                res_val <- result[state]
                res_df <- outcomedf[outcomedf$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == res_val & outcomedf$State == state, ]
        }
        else if (outcome == "heart failure")
        {
                result <- lapply(split(outcomedf$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,outcomedf$State),min,na.rm=TRUE)
                res_val <- result[state]
                res_df <- outcomedf[outcomedf$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure == res_val & outcomedf$State == state, ]
        }
        else{
                result <- lapply(split(outcomedf$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,outcomedf$State),min,na.rm=TRUE)
                res_val <- result[state]
                res_df <- outcomedf[outcomedf$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia == res_val & outcomedf$State == state, ]
        }
        res_HN <- res_df$Hospital.Name
        if ( length(res_HN) > 1) {
                HN <- sort(res_HN)[1]
        }
        else
                HN <- res_HN
        HN        
}
