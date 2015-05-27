best <- function(state, outcome) {
  ## Constants
  hospitalName <- 2
  fileName <- "outcome-of-care-measures.csv"
  ## 11 - heart attack, 17 - Heart failure, 23 Pneumonia
  validOutcome <- list(
    "heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
  
  if (!file.exists(fileName)) {
    stop(paste("'", fileName, "' is missing"))  
  }
  
  if (class(state) != "character") {
    stop("state should be a character vector")
  }
  
  if (length(state) != 1) {
    stop("state should be length of 1")
  }
  
  if (class(outcome) != "character") {
    stop("outcome should be a character vector")
  }
  
  if (length(outcome) != 1) {
    stop("outcome should be length of 1")
  }
  
  ## Read outcome data
  data <- read.csv(fileName, colClasses = "character")  
  uniqueStates <- unique(data$State)
  
  ## Check that state and outcome are valid
  isValidState <- state %in% uniqueStates
  if (!isValidState) {
    stop("invalid state")
  }
  
  isOutComeValid <- outcome %in% names(validOutcome)
  if (!isOutComeValid) {
    stop("invalid outcome")
  }
  
  ## Return hospital name in that state with lowest 30-day death
  outcomeColIndex <- validOutcome[[outcome]]
  
  filteredByStateAndProjectRelevant <- 
    data[data$State == state, c(hospitalName, outcomeColIndex)]
  
  orderedByState <- 
    filteredByStateAndProjectRelevant[
      order(filteredByStateAndProjectRelevant$Hospital.Name),]
  
  ## Rename columns for friendlier access
  colnames(orderedByState) <- c("Hospital.Name", "Outcome")
  
  cleanData <- orderedByState[orderedByState$Outcome != "Not Available", ]
  cleanData[, "Outcome"] <- as.numeric(cleanData[, "Outcome"])
  
  filteredByMin <- 
    cleanData[cleanData$Outcome == min(cleanData$Outcome), "Hospital.Name"]
  
  ## rate
  unique(filteredByMin)
}