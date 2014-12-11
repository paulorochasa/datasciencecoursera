source("complete.R")
corr <- function(directory, threshold = 0) {
  
  #validate csv's directory
  if(length(list.files(path = directory)) !=332 | list.files(path = directory)[1]!="001.csv"){
    stop("Somethings wrong! Expected files are missing. Restore original folder.")
  }
  
  #validate csv's directory
  if(!class(threshold) =="numeric" | length(threshold)!=1){
    stop("threshold is a numeric vector of length 1")
  }
  
  #get all files in directory
  filesList<-list.files(directory)
  
  #Get all completes 
  completesList<-complete(directory)  
  
  #Filter all completes by nobs greater than threshold 
  nobsFiltered <- completesList[completesList$nobs > threshold, ]

  #For each file calc correlation nitrate and sulfate
  corrsList <- numeric(0)
  for (fileid in nobsFiltered$id) {
    data <- read.csv(file.path(directory,filesList[fileid]))
    corrsList <- c(corrsList, cor(data$sulfate, data$nitrate, use = "pairwise.complete.obs"))
  }
  corrsList
}