best <- function(state,colName) {
        data <- read.csv("outcome-of-care-measures.csv")
        diseases <- c("heart attack","heart failure","pneumonia")
        
        testState <- state%in%data$State        
        if (testState == FALSE) {
                stop("invalid state")
        }
        
        testCol <- colName%in%diseases        
        if (testCol == FALSE) {
                stop("invalid outcome")
        }
        
        subset <- data[data$State == state,]
        if (colName == "heart attack") {
                row <- which.min(suppressWarnings(as.numeric(as.character(subset$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))))
                return(as.character(subset[row,"Hospital.Name"]))
        }
        
        if (colName == "heart failure") {
                row <- which.min(suppressWarnings(as.numeric(as.character(subset$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))))
                return(as.character(subset[row,"Hospital.Name"]))
        }
        
        if (colName == "pneumonia") {
                row <- which.min(suppressWarnings(as.numeric(as.character(subset$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))))
                return(as.character(subset[row,"Hospital.Name"]))
        }

}