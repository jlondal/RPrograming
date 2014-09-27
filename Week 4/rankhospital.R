setwd("/Users/jameslondall/Dropbox/DS Course/RPrograming/Week 4")

source('worst.R')
source('best.R')


sub_rankhospital <- function(data, col_nam, state, num) {
  state_subset <- data[data$'State' == state,]  
  outcome_arr <- state_subset[[col_nam]]
  len <- dim(state_subset[!is.na(outcome_arr), ])[1]
  if (num > len) {
    rank <- NA
  } else {
    rank <- state_subset[['Hospital.Name']] [order(outcome_arr, state_subset[['Hospital.Name']])[num]]
  }
  result <- rank
  result
}

rankhospital <- function(state, outcome, num = "best") {

  data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  
  # change data type from character to numeric
  data[, 11] <- as.numeric(data[, 11]) # heart attack
  data[, 17] <- as.numeric(data[, 17]) # heart failure
  data[, 23] <- as.numeric(data[, 23]) # pneumonia
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  if (!state %in% data$State) {
    stop("invalid state")
  } else if(!outcome %in% valid_outcomes) {
    stop("invalid outcome")
  } else {
    if (num == "best") {
      rank <- best(state, outcome)
    } else if (num == "worst") {
      rank <- worst(state, outcome)
    } else {
      if(outcome == "heart attack") {
        data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)         
        rank <- sub_rankhospital(data, 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack', state, num) 
      } else if(outcome == "heart failure") {
        data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure  <- as.numeric(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure) 
        rank <- sub_rankhospital(data, 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure', state, num) 
      } else {
        data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia   <- as.numeric(data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia  )         
        rank <- sub_rankhospital(data,'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia', state, num) 
      }
    }
    result <- rank
    result
  }
}

# tests
#rankhospital("MN", "heart attack", 50)
#rankhospital("MD", "heart attack", "worst")
#rankhospital("TX", "heart failure", 10)