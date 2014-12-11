complete <- function(directory, id = 1:332) {
  
  #validate csv's directory
  if(length(list.files(path = directory)) !=332 | list.files(path = directory)[1]!="001.csv"){
    stop("Somethings wrong! Expected files are missing. Restore original folder.")
  }
  
  #validate ids
  if(length(id[id > 0 & id <= 332]) != length(id)){
    stop("ids only support values in the range 1:332")
  }
  
  #get all files in directory
  filesList<-list.files(directory)

  
  #Get complete cases in nobsList
  nobsList <- numeric(0)
  for (fileid in id) {
    data <- read.csv(file.path(directory,filesList[fileid]))
    nobsList <- c(nobsList, nrow(na.omit(data)))

  }

  #return data frame with two columns
  data.frame(id = id, nobs = nobsList)
}