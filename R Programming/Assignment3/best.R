best <- function(state, outcome) {
  
  ##read data from csv
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")  
  
  ## Check that state and outcome are valid
  #matrix with field_position and outcome
  valid_outcomes<-matrix(c(11,17,23,"heart attack","heart failure","pneumonia"),nrow=3,ncol=2)
  
  if(!any(data$State == state))
    stop('invalid state')
  
  if(!any(valid_outcomes[,2] == outcome))
    stop('invalid outcome')
  
  #Get data outcome field position and parse column to numeric
  data_field<-as.numeric(valid_outcomes[valid_outcomes[,2]==outcome,1])
  data[,data_field] <- as.numeric(data[,data_field])
  
  #Subset by state and without NAs
  state_data<-data[data$State == state & !is.na(data[,data_field]),]
  
  #Get best outcome value and subset again by value
  min_outcome<-min(state_data[,data_field], na.rm=FALSE)
  min_data<-state_data[state_data[,data_field] == min_outcome ,]
  
  ## Handling Ties
  #Get best hospital order by name 
  head(sort(min_data$Hospital.Name),1)
}

