best <- function(state, outcome) {
  ## Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  validateState(state, unique(outcome_data[, 7]))
  validateOutcome(outcome)
  
  outcomeColName <- getOutcomeColumnName(outcome, outcome_data)
  
  ##coerce the mortality data into numbers
  outcome_data[, 11] <- as.numeric(outcome_data[, 11])
  outcome_data[, 17] <- as.numeric(outcome_data[, 17])
  outcome_data[, 23] <- as.numeric(outcome_data[, 23])
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  allStateOutcomes = split(outcome_data, outcome_data$State)
  
  ##grab the particular outcomes for the state
  pa = allStateOutcomes[state]
  
  ##create a logical array selecting the row with the minimum value for the
  ##specified outcome
  t1 <- pa[[1]][outcomeColName] == min(pa[[1]][outcomeColName], na.rm = TRUE)
  
  ##grab the row produced by the filter
  result <- pa[[1]][which(t1),]
  result$Hospital.Name
}

validateState <- function(in_state, allowable_states) {
  if (is.element(in_state, allowable_states) == FALSE) {
    stop("invalid state")
  }
}

validateOutcome <- function(in_outcome) {
  validOutcomes <- c("heart attack", "heart failure", "pneumonia")
  if (is.element(in_outcome, validOutcomes) == FALSE) {
    stop("invalid outcome")
  }
}

getOutcomeColumnName <- function(inOutcome, outcomeData) {

  colNumber <- NaN
  colNames <- names(outcomeData)
  
  if (inOutcome == 'heart attack') {
    colNumber <- 11
  }
  else if (inOutcome == 'heart failure') {
    colNumber <- 17
  }
  else if (inOutcome == 'pneumonia') {
    colNumber <- 23
  }
  
  colNames[colNumber]
}
