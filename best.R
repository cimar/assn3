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
  
  deaths<-min(filtered_df[,3])
  
  ## if I don't want the "NAs introduced by coercion
  
  ## they work now
  best_hospitals<-filtered_df[filtered_df[,3]==deaths,]

  ## can work on alternative path, if you want
  min(best_hospitals$Hospital.Name)
  
}