##Caitlin Hart
## R Programming
## Programming Assignment 1, Part 2
## 11/16/14
## This function searches a list of .csv files and returns a data frame with the filenames
## in the first column and the number of complete observed cases in the second column
complete <- function(directory,id=1:332) {
  nobs <- vector("numeric",length = length(id))
  for (i in seq_along(id)) {
    
    if (nchar(as.character(id[i])) == 1) { 
      
      number = paste("00",as.character(id[i]),sep="")
      
    } else if (nchar(as.character(id[i])) == 2) {
      
      number = paste("0",as.character(id[i]),sep="")
      
    } else if (nchar(as.character(id[i])) == 3) {
      
      number = as.character(id[i])
      
    }
    file = paste(number,".csv",sep="")
    data <- read.csv(paste(directory,"/",file,sep = ""))
    nobs[i] <- sum(complete.cases(data))
  }
  ans <- data.frame(id,nobs)
  ans
}