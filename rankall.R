rankall <- function(outcome,num = "best") {
        data <- read.csv ("outcome-of-care-measures.csv")
        diseases <- c("heart attack","heart failure","pneumonia")
        
        testCol <- outcome%in%diseases        
        if (testCol == FALSE) {
                stop("invalid outcome")
        }
        
        States <- unique(data$State)
        
        data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- suppressWarnings(as.numeric(as.character(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)))
        data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- suppressWarnings(as.numeric(as.character(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)))
        data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- suppressWarnings(as.numeric(as.character(data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)))
        
        rankedHA <- data[order(data$State,data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,data$Hospital.Name),]
        rankedHF <- data[order(data$State,data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,data$Hospital.Name),]
        rankedP <- data[order(data$State,data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,data$Hospital.Name),]
        
        results <- data.frame()
        
        if (outcome == "heart attack") {
                rankedHAs <- split(rankedHA,rankedHA$State)
                if (num == "best") {
                        for (i in seq_along(States)) {
                                results <- rbind(results,rankedHAs[[i]][1,c(2,7)])
                        }
                }
                if (num == "worst") {
                        for (i in seq_along(States)) {
                                sub2 <- rankedHA[!is.na(rankedHA$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]
                                sub2 <- split(sub2,sub2$State)
                                results <- rbind(results,rankedHAs[[i]][nrow(sub2[[i]]),c(2,7)])
                        }
                }
                if (is.numeric(num) == TRUE) {
                        for (i in seq_along(States)) {
                                results <- rbind(results,rankedHAs[[i]][num,c(2,7)])
                        }
                }
        }
        
        if (outcome == "heart failure") {
                rankedHFs <- split(rankedHF,rankedHF$State)
                if (num == "best") {
                        for (i in seq_along(States)) {
                                results <- rbind(results,rankedHFs[[i]][1,c(2,7)])
                        }
                }
                if (num == "worst") {
                        for (i in seq_along(States)) {
                                sub2 <- rankedHF[!is.na(rankedHA$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]
                                sub2 <- split(sub2,sub2$State)
                                results <- rbind(results,rankedHFs[[i]][nrow(sub2[[i]]),c(2,7)])
                        }
                }
                if (is.numeric(num) == TRUE) {
                        for (i in seq_along(States)) {
                                results <- rbind(results,rankedHFs[[i]][num,c(2,7)])
                        }
                }
        }
        
        if (outcome == "pneumonia") {
                rankedPs <- split(rankedP,rankedP$State)
                if (num == "best") {
                        for (i in seq_along(States)) {
                                results <- rbind(results,rankedPs[[i]][1,c(2,7)])
                        }
                }
                if (num == "worst") {
                        for (i in seq_along(States)) {
                                sub2 <- rankedP[!is.na(rankedP$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),]
                                sub2 <- split(sub2,sub2$State)
                                results <- rbind(results,rankedPs[[i]][nrow(sub2[[i]]),c(2,7)])
                        }
                }
                if (is.numeric(num) == TRUE) {
                        for (i in seq_along(States)) {
                                results <- rbind(results,rankedPs[[i]][num,c(2,7)])
                        }
                }
        }
        colnames(results) <- c("hospital","state")
        return(results)
        
}