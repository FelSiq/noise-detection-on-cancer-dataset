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

filter.AENN <- function(data, k = 5, classColumn = ncol(data)) {
	noises <- vector()
	n <- min(k, nrow(data) - 1)
	for (m in 1:n) {

		# ---------------------------------------------------------
		# START OF ENN 
		# ---------------------------------------------------------
		possibleNoises <- vector()

		for (i in 1:nrow(data)) {
			dist <- vector()
			for (j in 1:nrow(data)) {
				dist[j] <- sum((data[i, -classColumn] - data[j, -classColumn])^2.0)^0.5 
			}
			# The first index is not considered because the distance is always 0.0, when i == j.
			# I don't put a 'ifelse' on the 'for' above because this would aggregate unnecessary 
			# extra computational cost. Then, it is simpler and cheaper just ignore the first index. 
			knn <- sort.list(dist)[2:(m + 1)]
			
			equalClasses <- sum(data[i, classColumn] == data[knn, classColumn])

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
	newData$cleanData <- data[-noises,] 

	return(newData)
}
