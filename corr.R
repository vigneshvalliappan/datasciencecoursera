corr = function(directory, threshold = 0) {
	completeDataMatrix = complete("specdata", id = 1:332)
	thresholdCount = 0
	id = 1:332				
	for(k in id){
		if(completeDataMatrix[k, 2] > threshold){
			thresholdCount = thresholdCount + 1
		}
	}
	corrVector = numeric()
	if(thresholdCount == 0){
		corrVector
	}else{
		thresholdVector = seq(1, thresholdCount)
		corrVector = seq(1, thresholdCount)
		thresholdCount2 = 0
		for (m in id){
			if(completeDataMatrix[m, 2] > threshold){
				thresholdCount2 = thresholdCount2 + 1
				thresholdVector[thresholdCount2] = m
			}
		}
		corrVectorCount = 0
		for(n in thresholdVector){
			if(n < 10 && n > 0){
				filename = paste("00", n, ".csv", sep = '')
			}else if(n >= 10 && n < 100){
				filename = paste("0", n, ".csv", sep = '')
			}else if(n >= 100 && n < 333){
				filename = paste(n, ".csv", sep = '')
			}	
			completeData = read.csv(filename)
			corrVectorCount = corrVectorCount + 1
			corrVector[corrVectorCount] = cor(completeData$sulfate, completeData$nitrate, use = "complete.obs")
		}
	}
	corrVector
}



