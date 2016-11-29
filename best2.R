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
  deaths<-min(as.numeric(levels(filter_state[,index])))
  
  ## GRR IT'S NOT WORKING something about the factor typing
  best_hospitals<-filter_state[as.numeric(levels(filter_state[,index]))==deaths,]

  ## can work on alternative path, if you want
  min(as.character(best_hospitals$Hospital.Name))
  
}