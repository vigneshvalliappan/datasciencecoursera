complete <- function(directory, id = 1:332) {
	fullDirectoryName <- paste("/home/vignesh/datasciencecoursera/", directory, sep = '')	
	setwd(fullDirectoryName)
	numberOfFiles = 0
	completeObsMatrix <- data.frame(id, nobs = 0) 			
	for(i in id){
		if(i < 10 && i > 0){
			filename <- paste("00", i, ".csv", sep = '')
		}else if(i >= 10 && i < 100){
			filename <- paste("0", i, ".csv", sep = '')
		}else if(i >= 100 && i < 333){
			filename <- paste(i, ".csv", sep = '')
		}	
		pollutantData <- read.csv(filename)
		numberOfRows <- nrow(pollutantData)
		rowVector <- seq(1, numberOfRows)
		completeObsCount <- 0
		for(j in rowVector){
			if(is.na(pollutantData[j, 2]) || is.na(pollutantData[j, 3])){
				completeObsCount <- completeObsCount + 0
			}else{
				completeObsCount <- completeObsCount + 1
			}
		}
		numberOfFiles = numberOfFiles + 1
		completeObsMatrix[numberOfFiles, 2] <- completeObsCount
	} 
	completeObsMatrix
}



