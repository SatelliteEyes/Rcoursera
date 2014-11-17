corr <- function(directory,threshold = 0) {
  file_list <- list.files(directory,full.names = TRUE)
  correlation <- vector("numeric")
  for (i in seq_along(file_list)) {
    data <- read.csv(file_list[i])
    nobs<- sum(complete.cases(data))
    if (nobs > threshold) {
      correlation <- rbind(correlation,cor(data[,"sulfate"],data["nitrate"],use = "pairwise.complete.obs"))
    }
  }
  correlation
}