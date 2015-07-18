complete <- function(directory, id = 1:332 ) {
  directory <- as.character(directory)
  if (directory == 'specdata' ){
    setwd("~/Desktop/Coursera R Programming/specdata")
    file_list <- list.files()}
  else
  {stop("directory name incorrect")}
  id <- as.numeric(id)
  result = matrix(data=NA,nrow= 332, ncol=2)
  colnames(result) <- c("id","nobs")
  for (i in 1:length(id)){
    dataset <- read.csv(file_list[id[i]],header=TRUE,sep=",")
    dataset <- na.omit(dataset)
    result[i,1] <- id[i]
    result[i,2] <- nrow(dataset)
    }
  result <- as.data.frame(result)
  result <- na.omit(result)
  return(result)
}