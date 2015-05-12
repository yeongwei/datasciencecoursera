pollutantmean <- function (directory, pollutant, id = 1:332) {
  
  if (length(directory) != 1) {
    print("Error! Directory vector not lenght of 1.")
    return(NA)
  }
  
  if (!file.exists(directory)) {
    print("Error! Is your working directory correct ?")
    return(NA)
  }
  
  if (length(pollutant) != 1) {
    print("Error! Pollutant vector not lenght of 1.")
    return(NA)
  }
  
  if(!pollutant == "sulfate" & !pollutant == "nitrate") {
    print("Error! Invalid pollutant option.")
    return(NA)
  }

  if (class(id) != "numeric" & class(id) != "integer") {
    print("Error! Id needs to be a numeric vector.")
    return(NA)
  }
  
  if (any(id < 1) | any(id > 332)) {
    print("Error! Id out of range.")
    return(NA)
  }
  
  accumulator <- c()
  for (i in id) {
    fileId <- i
    if (fileId < 10) {
      fileId <- paste("00", fileId, sep = "")
    } else if (fileId > 9 & fileId < 100) {
      fileId <- paste("0", fileId, sep = "")
    }
    
    csvFullFileName <- paste(directory, "/", fileId, ".csv", sep = "")
    csvContent <- read.csv(csvFullFileName)
    relevantRecords <- csvContent[,pollutant]
    goodRecords <- relevantRecords[!is.na(relevantRecords)]
    accumulator <- c(accumulator, goodRecords) 
  }
  mean(accumulator)
}