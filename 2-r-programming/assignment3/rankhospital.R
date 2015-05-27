rankhospital <- function(state, outcome, num = "best") {
  ## Constants
  hospitalName <- 2
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
  uniqueStates <- unique(data$State)
  
  ## Check that state and outcome are valid
  if (class(state) != "character") {
    stop("state should be a character vector")
  }
  
  if (length(state) != 1) {
    stop("state should be length of 1")
  }
  
  isValidState <- state %in% uniqueStates
  if (!isValidState) {
    stop("invalid state")
  }
  
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
  
  if (class(num) != "character" && class(num) != "numeric" && class(num) != "integer") {
    stop("option type is invalid")
  }
  
  if (class(num) == "character") {
    isValidOption <- tolower(num) %in% validOption
    if (!isValidOption) {
      stop("invalid option")
    } 
  } else if (class(num) == "numeric" || class(num) == "integer") {
    if (num%%1!=0) {
      stop("invalid option")
    }
  }

  ## Return hospital name in that state with the given rank
  outcomeColIndex <- validOutcome[[outcome]]
  
  dataProjectRelevant <- 
    data[data$State == state, c(hospitalName, outcomeColIndex)]
  
  orderedByState <- 
    dataProjectRelevant[order(dataProjectRelevant$Hospital.Name),]
  
  ## Rename columns for friendlier access
  colnames(orderedByState) <- c("Hospital.Name", "Outcome")
  
  cleanData <- orderedByState[orderedByState$Outcome != "Not Available", ]
  cleanData[, "Outcome"] <- as.numeric(cleanData[, "Outcome"])
  
  orderByOutome <- cleanData[order(cleanData$Outcome),]
  
  ## 30-day death rate  
  totalCount <- nrow(orderByOutome)

  pointer <- NULL
  if (class(num) == "integer" || class(num) == "numeric") {
    if (num > totalCount) {
      return(NA)
    } else {
      pointer <- num
    }
  } else if (class(num) == "character") {
     if (num == "best") {
       pointer <- 1
     } else if (num == "worst") {
       pointer <- totalCount
     } else {
       return(NA)
     }
  } else {
    return(NA)
  }

  orderByOutome[pointer, "Hospital.Name"]
}