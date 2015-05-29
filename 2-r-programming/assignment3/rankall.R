rankall <- function(outcome, num = "best") {
  ## Constants
  hospitalName <- 2
  state <- 7
  fileName <- "outcome-of-care-measures.csv"
  ## 11 - heart attack, 17 - Heart failure, 23 Pneumonia
  validOutcome <- list(
    "heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
  validOption <- c("best", "worst")
  
  if (!file.exists(fileName)) {
    stop(paste("'", fileName, "' is missing"))  
  }
  
  ## Read outcome data
  data <- read.csv(fileName, colClasses = "character")
  
  ## Check that state and outcome are valid
  if (class(outcome) != "character") {
    stop("outcome should be a character vector")
  }
  
  if (length(outcome) != 1) {
    stop("outcome should be length of 1")
  }
  
  isOutComeValid <- outcome %in% names(validOutcome)
  if (!isOutComeValid) {
    stop("invalid outcome")
  }
  
  if (length(num) != 1) {
    stop("option should be length of 1")
  }
  
  if (class(num) == "character") {
    isValidOption <- tolower(num) %in% validOption
    if (!isValidOption) {
      stop("invalid option")
    } 
  } else if (class(num) == "numeric" || class(num) == "integer") {
    if (num %%1 !=0) {
      stop("invalid option")
    }
  }
  
  ## Parse data into friendly format before evaluation
  outcomeColIndex <- validOutcome[[outcome]]
  dataProjectRelevant <- data[, c(hospitalName, state, outcomeColIndex)]
  colnames(dataProjectRelevant) <- c("Hospital.Name", "State", "Outcome")
  cleanData <-
    dataProjectRelevant[dataProjectRelevant$Outcome != "Not Available", ]
  cleanData[, "Outcome"] <- as.numeric(cleanData[, "Outcome"])
  sortedData <- cleanData[order(cleanData$Hospital.Name), ] 
  groupedByState <- split(sortedData, sortedData$State) 
  
  ## For each state, find the hospital of the given rank
  intermediateResult <- lapply(groupedByState, function(dataByState) {
    dataByState <-
      dataByState[order(dataByState$Outcome), c("Hospital.Name", "State")]
    
    totalCount <- nrow(dataByState)
    
    ## Parse num
    if (class(num) != "integer" && class(num) != "numeric") { # A character
      if (num == "best") {
        num <- 1
      } else if (num == "worst") {
        num <- totalCount
      } else {
        stop("invalid option") # Rightfully won't reach here
      }
    }
    
    if (num > totalCount) {
      dataByState <- dataByState[1, ]
      dataByState$Hospital.Name <- NA
      return(dataByState)
    } else {
      return(dataByState[num, ])
    }
  })
  
  ## Construct data.frame from list
  result <- data.frame(
    matrix(
      unlist(intermediateResult), nrow = length(intermediateResult), byrow = T),
    stringsAsFactors = FALSE)
  ## Rename column and row names
  colnames(result) <- c("hospital", "state")
  rownames(result) <- result[, "state"]
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  return(result)
}