best <- function(outcome, num1 = 1) {
  outcome.data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
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
  rankhospital <- function(state, outcome, num = 1) {
    
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
      rkhos.ha <- order_ha[num, 1:3]
      return(rkhos.ha)}
    
    if (outcome %in% c("heart failure")) {
      rkhos.hf <- order_hf[num, 1:3]
      return(rkhos.hf)}
    
    if (outcome %in% c("pneumonia")) {
      rkhos.pn <- order_pn[num, 1:3]
      return(rkhos.pn)}
    ## Read outcome data
  }
  my.hospital <- data.frame(hospital= 0)
  ## add my.best funtion here
  my.states <- 1
  my.states.names <- unique(outcome.data$State)
  ## key to success
  for (i in 1:54) {
    my.hospital[my.states, 1:3] <- rankhospital(my.states.names[[my.states]], outcome, num1) #replace with my best function
    my.states = my.states + 1
  }
  print(my.hospital[, 1:2])
}

## experiment 
rankhospital <- function(state, outcome, num = 1) {
  
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
    rkhos.hf <- order_hf[num, 1]
    return(rkhos.hf)}
  
  if (outcome %in% c("pneumonia")) {
    rkhos.pn <- order_pn[num, 1]
    return(rkhos.pn)}
  ## Read outcome data
}


rankall<- function(outcome, num = "best") {
  ## Read outcome data
  outcome.data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check if the outcome is valid
  if (! outcome %in% c("heart attack", "heart failure", "pneumonia")) {
    stop("invalid outcome")}
  ## convert initial character data to numeric
  ## "Heart Attack"
  all.states <- unique(outcome.data$State)
  rank_inner <- function(state, outcome, num = 1) {
    
    all.states <- unique(outcome.data$State)
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
    order_ha <- split_ha[[index_state_ha]][order(split_ha[[index_state_ha]][3], split_ha[[index_state_ha]][1]), 1:3] # break ties Cool guys, looks okay
    order_hf <- split_hf[[index_state_hf]][order(split_hf[[index_state_hf]][3], split_hf[[index_state_hf]][1]), 1:3]
    order_pn <- split_pn[[index_state_pn]][order(split_pn[[index_state_pn]][3], split_pn[[index_state_pn]][1]), 1:3]
    if (outcome %in% c("heart attack") & num %in% c("worst")) {
      order.no.na.ha <- order_ha[!is.na(order_ha[, 3]), 1:3]
      worst.ha <- length(order.no.na.ha[, 3])
      rkhos.ha.worst <- order_ha[worst.ha, 1]
      return(rkhos.ha.worst)}
    if (outcome %in% c("heart failure") & num %in% c("worst")) {
      order.no.na.hf <- order_hf[!is.na(order_hf[, 3]), 1:3]
      worst.hf <- length(order.no.na.hf[, 3])
      rkhos.hf.worst <- order_hf[worst.hf, 1]
      return(rkhos.hf.worst)}
    if (outcome %in% c("pneumonia") & num %in% c("worst")) {
      order.no.na.pn <- order_pn[!is.na(order_pn[, 3]), 1:3]
      worst.pn <- length(order.no.na.pn[, 3])
      rkhos.pn.worst <- order_pn[worst.pn, 1]
      return(rkhos.pn.worst)}
    ## out put rank #
    if (outcome %in% c("heart attack")) {
      rkhos.ha <- order_ha[num, 1]
      return(rkhos.ha)}
    
    if (outcome %in% c("heart failure")) {
      rkhos.hf <- order_hf[num, 1]
      return(rkhos.hf)}
    
    if (outcome %in% c("pneumonia")) {
      rkhos.pn <- order_pn[num, 1]
      return(rkhos.pn)}
    ## Read outcome data
  }
  ranking <- list(hospital = 0)
  for (i in all.states) {
    rank1 <- rank_inner(i, outcome, num)
    rank1
  }
  
}