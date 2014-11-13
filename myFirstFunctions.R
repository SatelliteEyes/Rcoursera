add2 <- function(x,y){
  x+y
}

above10 <- function(x) {
  use <- x > 10
  x[use]
}

above<- function(x,n = 10) { ## this means n = 10 if n is not specified
  use <- x >n
  x[use]
}

columnmean <- function(y) {
  nc <- ncol(y)
  means<- numeric(nc)
  for(i in 1:nc) {
    means[i] <- mean(y[,i])
  }
  means
}