fixMissing = function(x){
	x[x == "Not Available"] = NA
	x
}

rankall = function(outcome, num = "best"){
	## Read outcome data
	outcomeData = read.csv("outcome-of-care-measures.csv")
	outcomeSubsetData = outcomeData[c(2, 7, 11, 17, 23)]
	outcomeSubsetData[3:5] = lapply(outcomeSubsetData[3:5], fixMissing)
	outcomeSubsetData = outcomeSubsetData[complete.cases(outcomeSubsetData[, 3:5]), ]
	outcomeSubsetData[3:5] = lapply(outcomeSubsetData[3:5], function(x) as.numeric(as.character(x)))
	##outcomeSubsetData[1:2] = lapply(outcomeSubsetData[1:2], function(x) as.character(x))
	outcomeSubsetData[, 1] = as.character(outcomeSubsetData[, 1])

	## Check that state and outcome are valid
	acceptableOutcomeArgs = c("heart attack", "heart failure", "pneumonia")
	if(!(outcome %in% acceptableOutcomeArgs)){
		stop("invalid outcome")
	} 

	## class(outcomeSubsetData)
	## Return hospital name in that state with the given rank 30-day death
	## rate
	outcomeSubsetData2 = outcomeSubsetData[, c(1:2, match(outcome, acceptableOutcomeArgs) + 2)]
	listOFStates = unique(outcomeSubsetData[, 2])
	listOFStates
	##rankData = ave(outcomeSubsetData2[, 2], outcomeSubsetData2[, 3], rank)
	##rankData
	##sortedFinalDataSet = outcomeSubsetData2[order(outcomeSubsetData2[, 3], outcomeSubsetData2[, 1]), ]
	##numberOfRows = nrow(sortedFinalDataSet)
	##rowNumber  = 0	
	##if(num == "best"){
	##	rowNumber = 1
	##}else if(num == "worst"){
	##	rowNumber = numberOfRows
	##}else if(as.numeric(num) > 0 && as.numeric(num) <= numberOfRows){
	##	rowNumber = num
	##}
	##if(rowNumber == 0){
	##	NA
	##}else{
	##	sortedFinalDataSet[rowNumber, 1]
	##}
}

