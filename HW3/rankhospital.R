source("best.R")

rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  validateState(state, unique(outcome_data[, 7]))
  validateOutcome(outcome)
  validateRanking(num)
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
  outcomeColName <- getOutcomeColumnName(outcome, outcome_data)
  
  ##coerce the mortality data into numbers
  outcome_data[, 11] <- as.numeric(outcome_data[, 11])
  outcome_data[, 17] <- as.numeric(outcome_data[, 17])
  outcome_data[, 23] <- as.numeric(outcome_data[, 23])
  
  allStateOutcomes <- split(outcome_data, outcome_data$State)
  
  ##grab the particular outcomes for the state
  pa <- allStateOutcomes[state]
  pa_temp = pa[[1]]
  
  ##order by the result, discarding NA values
  order.outcome <- order(pa_temp[outcomeColName], pa_temp$Hospital.Name, decreasing = FALSE, na.last = NA)
  
  nth <- getNthRow(length(order.outcome), num)
  if (is.na(nth)) {
    return(nth)
  }
  
  result <- pa_temp[ order.outcome[nth],]
  result$Hospital.Name
}

getNthRow <- function(len, num) {
  val <- NaN
  if (num == "best") {
    val <- 1
  }
  else if (num == "worst") {
    val <- len
  }
  else if (num > len) {
    val <- NA
  }
  else {
    val <- num
  }
  
  val
}

validateRanking <- function(in_num) {
  validStrNums <- c("best", "worst")
  if (is.character(in_num) && is.element(in_num, validStrNums) == FALSE) {
    stop("invalid ranking")
  }
  
  if (is.character(in_num) == FALSE && is.numeric(in_num) == FALSE ) {
    stop("invalid ranking")
  }
}