outcome_index<-c(11,17,23)
names(outcome_index)<-c("heart attack", "heart failure", "pneumonia")

best <- function(state, outcome_name) {
  
  ##Read data
  outcome_df<-read.csv("outcome-of-care-measures.csv",na.strings="Not Available",stringsAsFactors = FALSE)
  
  ## Check that state and outcome are valid
  if(!(state %in% outcome_df$State)){
    stop("invalid state")
  }
  if(!(outcome_name %in% names(outcome_index))){
    stop("invalid outcome")
  }
  hospi<-2
  statei<-7
  index<-outcome_index[outcome_name]
  ## filter dataset down by state
  filtered_df<-outcome_df[outcome_df$State==state,c(hospi,statei,index)]
  filtered_df<-na.omit(filtered_df)
  
  ## find death rate per hospital ## don't need mean here
  ## don't need tapply but i originally misunderstood the data-set
  ## and it turns out this works is maybe easier than sorting "filter_state"
  
  death_by_hospital<-tapply(filtered_df[, 3],filtered_df$Hospital.Name,sum)
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  names(sort(death_by_hospital)[1])
  
  ## can work on alternative path, if you want
  ## sort(filter_state[, index])[1]$Hospital.Name
  
}