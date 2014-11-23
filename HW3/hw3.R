best <- function(state, outcome) {
  ## Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  validateState(state, unique(outcome_data[, 7]))
  validateOutcome(outcome)
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
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
