rankhospital <- function(state,outcome,num) {
        data <- read.csv("outcome-of-care-measures.csv")
        diseases <- c("heart attack","heart failure","pneumonia")
        
        subset <- data[data$State == state,]
                
        testState <- state%in%data$State        
        if (testState == FALSE) {
                stop("invalid state")
        }
        
        testCol <- outcome%in%diseases        
        if (testCol == FALSE) {
                stop("invalid outcome")
        }
        
        if (is.numeric(num) == TRUE && num > nrow(subset)) {
                return("NA")
        }
                
        if (outcome == "heart attack") {
                subset$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- suppressWarnings(as.numeric(as.character(subset$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)))
                ranked = subset[order(subset$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,subset$Hospital.Name),]
                ##print(subset[nrow(subset),11])
                if (num == "best") {
                        return(as.character(ranked[1,"Hospital.Name"]))
                }
                if (num == "worst") {
                        sub2 <- ranked[!is.na(ranked$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]
                        return(as.character(ranked[nrow(sub2),"Hospital.Name"]))
                }
                if (is.numeric(num) == TRUE) {
                        return(as.character(ranked[num,"Hospital.Name"]))
                }
        }
        
        if (outcome == "heart failure") {
                subset$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- suppressWarnings(as.numeric(as.character(subset$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)))
                ranked = subset[order(subset$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,subset$Hospital.Name),]
                if (num == "best") {
                        return(as.character(ranked[1,"Hospital.Name"]))
                }
                if (num == "worst") {
                        sub2 <- ranked[ranked$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure != "NA",]
                        return(as.character(ranked[nrow(sub2),"Hospital.Name"]))
                }
                if (is.numeric(num) == TRUE) {
                        return(as.character(ranked[num,"Hospital.Name"]))
                }
        }
        
        if (outcome == "pneumonia") {
                subset$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- suppressWarnings(as.numeric(as.character(subset$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)))
                ranked = subset[order(subset$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,subset$Hospital.Name),]
                if (num == "best") {
                        return(as.character(ranked[1,"Hospital.Name"]))
                }
                if (num == "worst") {
                        sub2 <- ranked[ranked$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia != "NA",]
                        return(as.character(ranked[nrow(sub2),"Hospital.Name"]))
                }
                if (is.numeric(num) == TRUE) {
                        return(as.character(ranked[num,"Hospital.Name"]))
                }
        }
}