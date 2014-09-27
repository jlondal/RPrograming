setwd("/Users/jameslondall/Dropbox/DS Course/RPrograming/Week 4")

sub_best <- function(data, col_nam, state) {
  state_subset <- data[data$'State' == state,]
  outcome_arr <- state_subset[[col_nam]]
  minN <- min(outcome_arr, na.rm=T)
  min_index <- which(outcome_arr == minN)
  name <- state_subset[min_index, 2]
  name
}

best <- function(state, outcome) {
  data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  valid <- c("heart attack", "heart failure", "pneumonia")
  if (!state %in% data$State) {
      stop("invalid state")
  } else if(!outcome %in% valid) {
      stop("invalid outcome")
  } else {
    if(outcome == "heart attack") {     
      data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack) # heart attack
      hosp_name <- sub_best(data, 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack', state)
    } else if(outcome == "heart failure") {
      data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure  <- as.numeric(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure) # heart failure
      hosp_name <- sub_best(data, 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure', state)
    } else {
      data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia   <- as.numeric(data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia  ) # pneumonia
      hosp_name <- sub_best(data, 'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia', state)
    }
    result <- hosp_name
    result
  }
}

# tests
#best("TX", "heart attack")
#best("TX", "heart failure")
#best("MD", "heart attack")
#best("MD", "pneumonia")
#best("BB", "heart attack")