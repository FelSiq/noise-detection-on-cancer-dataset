# ----------------------------------------------
# INFORMATION (AENN - All-k Edited Nearest Neighbors)
# ----------------------------------------------
# AENN apply ENN for all integer i between 1 and k, and then remove
# all instances considered noise for any i.
# ----------------------------------------------

# ----------------------------------------------
# INFORMATION (ENN - Edited Nearest Neighbors)
# ----------------------------------------------
# Given a dataset, it returns the index of which instances are possible noises, based on
# if the majority of the k-nearest neighbors for each instance have different classes.
# ----------------------------------------------

filter.AENN <- function(dataset, k = 5, classColumn = ncol(dataset), SCALE = FALSE) {
	noises <- vector()

	scaledData <- if (SCALE) scale(dataset[-classColumn]) else dataset[-classColumn]

	n <- min(k, nrow(dataset) - 1)
	for (m in 1:n) {

		# ---------------------------------------------------------
		# START OF ENN 
		# ---------------------------------------------------------
		possibleNoises <- vector()

		for (i in 1:nrow(dataset)) {
			dist <- vector()
			for (j in 1:nrow(dataset)) {
				dist[j] <- sum((scaledData[i, ] - scaledData[j, ])^2.0)^0.5 
			}
			# The first index is not considered because the distance is always 0.0, when i == j.
			# I don't put a 'ifelse' on the 'for' above because this would aggregate unnecessary 
			# extra computational cost. Then, it is simpler and cheaper just ignore the first index. 
			knn <- sort.list(dist)[2:(m + 1)]
			
			equalClasses <- sum(dataset[i, classColumn] == dataset[knn, classColumn])

			if (equalClasses < m/2.0) {
				possibleNoises <- c(possibleNoises, i)
			}
		}
		# ---------------------------------------------------------
		# END OF ENN
		# ---------------------------------------------------------
		noises <- c(noises, possibleNoises)
	}

	newData <- list()
	newData$remIdx <- sort(unique(noises))
	newData$cleanData <- dataset[-newData$remIdx,] 

	return(newData)
}
