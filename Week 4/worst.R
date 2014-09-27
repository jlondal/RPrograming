setwd("/Users/jameslondall/Dropbox/DS Course/RPrograming/Week 4")

#Define Worst
sub_worst <- function(data, col_nam, state) {
  state_subset <- data[data$'State' == state,]
  outcome_arr <- state_subset[[col_nam]]
  maxN <- max(outcome_arr, na.rm=T)
  max_index <- which(outcome_arr == maxN)
  name <- state_subset[max_index, 2]
  name
}

worst <- function(state, outcome) {
  
  data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  valid <- c("heart attack", "heart failure", "pneumonia")
  
  if (!state %in% data$State) {
    stop("invalid state")
  } else if(!outcome %in% valid) {
    stop("invalid outcome")
  } else {
    if(outcome == "heart attack") {     
      data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack) # heart attack
      hosp_name <- sub_worst(data, 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack', state)
    } else if(outcome == "heart failure") {
      data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure  <- as.numeric(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure) # heart failure
      hosp_name <- sub_worst(data, 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure', state)
    } else {
      data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia   <- as.numeric(data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia  ) # pneumonia
      hosp_name <- sub_worst(data, 'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia', state)
    }
    result <- hosp_name
    result
  }
}

# tests
#worst("TX", "heart attack")
#worst("TX", "heart failure")
#worst("MD", "heart attack")
#worst("MD", "pneumonia")
#worst("BB", "heart attack")

