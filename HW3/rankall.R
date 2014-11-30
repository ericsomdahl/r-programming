source("best.R")
source("rankhospital.R")


rankall <- function(outcome, num = "best") {
  ## Check that ranking and outcome are valid
  validateOutcome(outcome)
  validateRanking(num)
  
  ## Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  outcomeColName <- getOutcomeColumnName(outcome, outcome_data)
  
  ##coerce the mortality data into numbers
  outcome_data[, 11] <- as.numeric(outcome_data[, 11])
  outcome_data[, 17] <- as.numeric(outcome_data[, 17])
  outcome_data[, 23] <- as.numeric(outcome_data[, 23])
  
  allStateOutcomes <- split(outcome_data, outcome_data$State)
  
  df_val = NA
  
  ## For each state, find the hospital of the given rank
  for (df_state in allStateOutcomes) {
    ##order by the result, discarding NA values
    order.outcome <- order(df_state[outcomeColName], df_state$Hospital.Name, decreasing = FALSE, na.last = NA)
    state = df_state$State[1]
    nth <- getNthRow(length(order.outcome), num)
    if (is.na(nth) == FALSE) {
      temp_result <- df_state[ order.outcome[nth], ]
      df_temp_state = data.frame(state=state, hospital=temp_result$Hospital.Name, stringsAsFactors=FALSE)
    }
    else {
      df_temp_state = data.frame(state=state, hospital=NA, stringsAsFactors=FALSE)
    }
    
    if (is.na(df_val)) {
      df_val = df_temp_state
    }
    else {
      df_val = rbind(df_val, df_temp_state)
    }
  }
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  df_val
}