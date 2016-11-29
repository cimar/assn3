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
  filter_state<-outcome_df[outcome_df$State==state,]
  
  ## find death rate per hospital ## don't need mean here
  ## don't need tapply but i originally misunderstood the data-set
  ## and it turns out this works is maybe easier than sorting "filter_state"
  
  ## both my methods fail for MD, pneumonia -- because I'm coercing factors 
  ## incorrectly, I think, as I do this
  death_by_hospital<-tapply(as.numeric(filter_state[, index]),filter_state$Hospital.Name,sum)
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  names(sort(death_by_hospital)[1])
  
  ## can work on alternative path, if you want
  ## sort(filter_state[, index])[1]$Hospital.Name
  
}