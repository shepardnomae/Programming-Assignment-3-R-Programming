## Question 2
rankhospital <- function(state, outcome, num = "best") {
  ## Check if the state is valid
  outcome.data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  if (! state %in% outcome.data$State) {
    stop("invalid state")}
  ## Check if the outcome is valid
  if (! outcome %in% c("heart attack", "heart failure", "pneumonia")) {
    stop("invalid outcome")}
  ## convert initial character data to numeric
  ## "Heart Attack"
  outcome.data[, 11] <- as.numeric(outcome.data[, 11])
  ## "Heart Failure"
  outcome.data[, 17] <- as.numeric(outcome.data[, 17])
  ## "Pneumonia"
  outcome.data[, 23] <- as.numeric(outcome.data[, 23])
  ## my max with na.rm = true by default & and my which max
  my_min <- function(x) {min(x, na.rm = TRUE)}
  my_which_min <- function(x) which(x == my_min(x))
  ## split states
  split_ha <- split(outcome.data[, c(2, 7, 11)], outcome.data$State)
  split_hf <- split(outcome.data[, c(2, 7, 17)], outcome.data$State)
  split_pn <- split(outcome.data[, c(2, 7, 23)], outcome.data$State)
  ## which state
  index_state_ha <- which(state == names(split_ha))
  index_state_hf <- which(state == names(split_hf))
  index_state_pn <- which(state == names(split_pn))
  ## rank hospitals
  order_ha <- split_ha[[index_state_ha]][order(split_ha[[index_state_ha]][3]), 1:3] # Cool guys, looks okay
  order_hf <- split_hf[[index_state_hf]][order(split_hf[[index_state_hf]][3]), 1:3]
  order_pn <- split_pn[[index_state_pn]][order(split_pn[[index_state_pn]][3]), 1:3]
  
  ## out put rank #
  if (outcome %in% c("heart attack")) {
    rkhos.ha <- order_ha[num, 1]
    return(rkhos.ha)}
  
  if (outcome %in% c("heart failure")) {
    return(best_hf)}
  
  if (outcome %in% c("pneumonia")) {
    return(best_pn)}
  ## Read outcome data
}