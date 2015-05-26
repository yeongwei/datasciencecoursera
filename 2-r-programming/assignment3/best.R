best <- function(state, outcome) {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")  
  ## Check that state and outcome are valid
  uniqueStates <- unique(data$State)
  isValidState <- state %in% uniqueStates
  ## 11 - heart attack, 17 - Heart failure, 23 Pneumonia
  validOutcome <- list(
    "heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
  isOutComeValid <- outcome %in% names(validOutcome)
  ## Return hospital name in that state with lowest 30-day death
  hospitalName <- 2 # constants
  outcomeIndex <- validOutcome[[outcome]]
  print(outcomeIndex)
  filteredByState <- 
    data[data$State == state, c(hospitalName, outcomeIndex)]
  orderedByState <- filteredByState[order(filteredByState$Hospital.Name),]
  colnames(orderedByState) <- c("Hospital.Name", "Outcome")
  orderedByState <- orderedByState[orderedByState$Outcome != "Not Available", ]
  orderedByState[, "Outcome"] <- as.numeric(orderedByState[, "Outcome"])
  filteredByMin <- 
    orderedByState[
      orderedByState$Outcome == min(orderedByState$Outcome), "Hospital.Name"]
  ## rate
  unique(filteredByMin)
}