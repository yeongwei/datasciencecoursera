complete <- function (directory, id = 1:332) {
  
  if (length(directory) != 1) {
    print("Error! Directory vector not length of 1.")
    return(NA)
  }
  
  if (!file.exists(directory)) {
    print("Error! Is your working directory correct ?")
    return(NA)
  }

  if (class(id) != "numeric" & class(id) != "integer") {
    print("Error! Id vector not numeric / integer.")
    return(NA)
  }
  
  if (any(id < 1) | any(id > 332)) {
    print("Error! Id out of range.")
    return(NA)
  }
  
  ids = c()
  nobs = c()
  
  for (i in id) {
    fileId <- i
    if (fileId < 10) {
      fileId <- paste("00", fileId, sep ="")
    } else if (fileId > 9 & fileId < 100) {
      fileId <- paste("0", fileId, sep ="")
    }
    
    csvFullFileName <- paste(directory, "/", fileId, ".csv", sep = "")
    csvContent <- read.csv(csvFullFileName)
    completeCases <- complete.cases(csvContent)
    
    ids <- c(ids, i)
    nobs <- c(nobs, nrow(csvContent[completeCases,][]))
  }
  
  id <- ids
  data.frame(id, nobs)
}