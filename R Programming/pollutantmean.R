
pollutantmean <- function(directory, pollutant, id = 1:332) {
  #validate csv's directory
  if(length(list.files(path = directory)) !=332 | list.files(path = directory)[1]!="001.csv"){
    stop("Somethings wrong! Expected files are missing. Restore original folder.")
  }
  
  #validate pollutant
  valid_pol<- c("sulfate", "nitrate")
  if(!(match(pollutant,valid_pol) > 0 & !is.na(match(pollutant, valid_pol)))){
    stop("Invalid pollutant! Only supports values:{sulfate,nitrate}.")
  }
  
  #validate ids
  if(length(id[id > 0 & id <= 332]) != length(id)){
    stop("ids only support values in the range 1:332")
  }
  
  #select files 
  fileList<-list.files(directory)
  fileNames <- as.numeric(sub("\\.csv$","",fileList))
  selectedFiles = fileList[match(id,fileNames)]
  
  #read all files
  data <- lapply(file.path(directory,selectedFiles),read.csv)
  
  #binding data to a single data frame
  data <- do.call(rbind.data.frame,data)
  
  #calc mean without NA 
  round(mean(data[,pollutant],na.rm=TRUE),3)
}
