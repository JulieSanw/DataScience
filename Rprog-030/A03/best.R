best <- function(state, outcome) {
  
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  states <- unique(data$State)
  if(!state %in% states) {
    stop("invalid state")    
  } 
  
  outcomes <- c('heart attack','heart failure','pneumonia')
  if(!outcome %in% outcomes) {
    stop("invalid outcome")    
  } 
  
  ## Return hospital name in that state with lowest 30-day death rate
  data <- data[data$State == state,]
  if(outcome == "heart attack"){
    col <- 11
  }
  if(outcome == "heart failure"){
    col <- 17
  }
  if(outcome == "pneumonia"){
    col <- 23
  }
  num <- as.numeric(data[, col])  
  lna <- is.na(num)
  num[lna] <- Inf
  output <- which.min(num)  
  return((data$Hospital.Name)[output])
}


