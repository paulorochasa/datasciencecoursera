rankall <- function(outcome, num = "best") {
  
  ##read data from csv
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")  
  
  ## Check that state and outcome are valid
  #matrix with field_position and outcome
  valid_outcomes<-matrix(c(11,17,23,"heart attack","heart failure","pneumonia"),nrow=3,ncol=2)
  
  if(!any(valid_outcomes[,2] == outcome))
    stop('invalid outcome')
  
  if(!any(c("best","worst") == num) & !is.numeric(num))
    stop('invalid rank num')
  
  #Get data outcome field position and parse column to numeric
  data_field<-as.numeric(valid_outcomes[valid_outcomes[,2]==outcome,1])
  data[,data_field] <- as.numeric(data[,data_field])
  
  ## For each state, find the hospital of the given rank
  states<-sort(unique(data$State))
  hospitals<-c()
  
  for(state in states){
    #Subset by state and without NAs
    state_data<-data[data$State == state & !is.na(data[,data_field]),c(2,data_field)]
    
    ##Sort state data by outcome and then by name (handling ties included)
    sortnames <- c(2,1)
    state_data_ord<-state_data[do.call("order", state_data[sortnames]), ]
    
    #Get hospital by rank position
    if(num == "best"){
      hospitals<-c(hospitals,head(state_data_ord[,1],1))  
    }
    else if(num=="worst"){
      hospitals<-c(hospitals,tail(state_data_ord[,1],1))
    }
    else{
      hospitals<-c(hospitals,state_data_ord[num,1])
    }
  }
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  data.frame(hospital = hospitals, state = states)

}
