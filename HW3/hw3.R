best <- function(state, outcome) {
  ## Read outcome data
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  validateState(state, unique(outcome[, 7]))  
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
}

validateState <- function(in_state, allowable_states) {
  if (is.element(in_state, allowable_states) == FALSE) {
    stop("invalid state")
  }
}
