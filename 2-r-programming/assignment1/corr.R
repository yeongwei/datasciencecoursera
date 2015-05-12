corr <- function (directory, threshold = 0) {
  if (length(directory) != 1) {
    print("Error! Directory vector not lenght of 1.")
    return(NA)
  }
  
  if (!file.exists(directory)) {
    print("Error! Is your working directory correct ?")
    return(NA)
  }
  
  if (class(threshold) != "numeric" && class(threshold) != "integer") {
    print("Error! Threshold vector must be class integer.")
    return(NA)
  }
  
  if (threshold%%1 != 0) {
    print("Error! Threshold must be class integer.")
    return(NA)
  }
  
  if (length(threshold) != 1) {
    print("Error! Threshold vector is not length 1.")
    return(NA)
  }
  
  completeData <- complete(directory)
  completeData <- completeData[completeData$nobs > threshold,]
  #print(completeData)
  accumulator <- vector("numeric", length = 0)
  for (x in completeData[["id"]]) {
    fileId <- x
    if (fileId < 10) {
      fileId <- paste("00", fileId, sep = "")
    } else if (fileId > 9 & fileId < 100) {
      fileId <- paste("0", fileId, sep = "")
    }
      
    csvFullFileName <- paste(directory, "/", fileId, ".csv", sep = "")
    csvContent <- read.csv(csvFullFileName)
    csvContent <- csvContent[complete.cases(csvContent),]
    #print(nrow(csvContent))
    accumulator<-c(accumulator, cor(csvContent["sulfate"], csvContent["nitrate"]))
  }
  
  return(accumulator)
}