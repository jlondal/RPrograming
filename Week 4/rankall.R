setwd("/Users/jameslondall/Dropbox/DS Course/RPrograming/Week 4")

source('worst.R')
source('best.R')
source('rankhospital.R')

rankall <- function(outcome, num = "best") {

  data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  
  valid <- c("heart attack", "heart failure", "pneumonia")
  state_arr <- sort(unique(data$State))
  arr_len <- length(state_arr)
  hospital <- rep("", arr_len)
  
  if (!outcome %in% valid) {
    stop("invalid outcome")
  } else {
    for(i in 1:arr_len) {
      state_subset <- data[data$State==state_arr[i], ]      
      hospital[i] <- rankhospital(state_subset$State[1], outcome, num)
    }
  }

  result <- data.frame(hospital=hospital, state=state_arr)
  result
}

# tests
#head(rankall("heart attack", 20), 10)
#tail(rankall("pneumonia", "worst"), 3)
#tail(rankall("heart failure"), 10)