##Caitlin Hart
## R Programming
## Programming Assignment 1, Part 1
## 11/15/14
## This code will read a data frame and calculate the mean of the column with given level
## name
## pollutantmean(directory,pollutant,id=1:332)
## directory = A character vector with one element indicating the directory for the csv data files
## pollutant = "sulfate" or "nitrate", for our case. Generally, the level name of the column
## for whitch you wish to calculate the mean.
## id = 1:332. A numeric id of the csv you wish to process. For this case, it is set to 1:322 by default

pollutantmean <- function(directory,pollutant,id=1:332) {
  data <- data.frame()
  for (i in seq_along(id)) {

    if (nchar(as.character(id[i])) == 1) { 
    
      number = paste("00",as.character(id[i]),sep="")
    
    } else if (nchar(as.character(id[i])) == 2) {
    
      number = paste("0",as.character(id[i]),sep="")
    
    } else if (nchar(as.character(id[i])) == 3) {
    
      number = as.character(id[i])
    
    }
    file = paste(directory,"/",number,".csv",sep = "")
    file
    data <- rbind(data,read.csv(file))
  }
  names(data)
  ans <- mean(data[,pollutant],na.rm=TRUE)
  ans

}