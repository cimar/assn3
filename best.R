outcome_index<-c(11,17,23)
names(outcome_index)<-c("heart attack", "heart failure", "pneumonia")

best <- function(state, outcome_name) {
  
  ##Read data
  outcome_df<-read.csv("outcome-of-care-measures.csv")
  
  ## Check that state and outcome are valid
  if(!(state %in% outcome_df$State)){
    stop("invalid state")
  }
  if(!(outcome_name %in% names(outcome_index))){
    stop("invalid outcome")
  }
  
  index<-outcome_index[outcome_name]
  ## filter dataset down by state
  state_outcome<-outcome_df[outcome_df$State==state]
  
  ## find mean death rate per hospital
  death_by_hospital<-tapply(as.numeric(outcome_df[, index]),outcome_df$Hospital.Name,mean)

  ## Return hospital name in that state with lowest 30-day death
  ## rate
}