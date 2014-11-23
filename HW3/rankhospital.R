source("best.R")

rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  ## Check that state and outcome are valid
  validateState(state, unique(outcome_data[, 7]))
  validateOutcome(outcome)
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
}