# ----------------------------------------------------------
# LOAD SECTION =============================================
# ----------------------------------------------------------
source('./src/LPNoiseInputation.R')
source('./src/config.R')
source('./src/generalFunctions.R')

# ----------------------------------------------------------
# MAIN SECTION =============================================
# ----------------------------------------------------------

# For each dataset...
for (datasetID in 1:nrow(config.DATASET_SEQ)) {
	# 1) Get the Dataset.
	dataset <- read.csv(paste('./datasets', config.DATASET_SEQ[datasetID, 1], sep='/'), sep = ' ')

	# Microarray-type datasets need a 'special' preprocessing
	if (config.DATASET_SEQ[datasetID, 2] == 'Microarray') {
		# 3) Transpose the dataset (?)
		dataset <- as.data.frame(t(dataset))

		# 3.a) Correct the column names
		# The output/target feature will be named 'Class', and the prediction 
		# features will be enumered with a natural number sequence.
		colnames(dataset) <- c('Class', paste('X', seq(1, ncol(dataset) - 1), sep = ''))

		# Move the Class column to the last dataset column
		dataset <- dataset[, c(2:(ncol(dataset)), 1)]
	} else {
		# Noise inputation and SMOTE requeriments
		colnames(dataset)[ncol(dataset)] <- 'Class'
	}

	# Set the target feature binary factor type. 
	# Majority class should be factor '0', and minority class factor '1' (ubSMOTE exigency).
	labels <- unique(dataset$Class)
	majorityLabel <- ifelse(sum(dataset$Class == labels[1]) >= length(dataset$Class)/2, 1, 2)
	dataset$Class <- factor(ifelse(dataset$Class == labels[majorityLabel], 0, 1))

	# 
	bags <- sample(1:config.FOLDS_NUM_CROSS_VALIDATION, size = nrow(dataset), replace = TRUE)

	# 
	for (i in 1:config.FOLDS_NUM_CROSS_VALIDATION) {
		# 
		set.train <- subset(dataset, i != bags)
		set.test <- subset(dataset, i == bags)

		for (smoteEnabled in config.SMOTE_SEQ) {
			# If smote enable, then call ubSMOTE. Otherwise, the 'smotedTrainSet' is 'secretly' just the set.train
			if (smoteEnabled) {
				aux <- ubSMOTE(dataset[-which(colnames(dataset) == 'Class')], dataset$Class)
				smotedTrainSet <- data.frame(aux$X)
				smotedTrainSet$Class <- aux$Y
				# @ Garbage collection (to avoid memory issues) -------
				rm(aux)
				gc()
				# @ ---------------------------------------------------
			} else {
				smotedTrainSet <- set.train
			}
			

			# 5) Input artificial noise
			smotedTrainSet.noise <- rand(smotedTrainSet, config.ERROR_INPUT_RATE)
			
			# For each classifier
			for (classifierID in config.CLASSIFIER_SEQ) {
				# 4) Train a classifier with the original data (before artificial noise inputation)
				predictionsOriginal <- general.fitAndPredict(smotedTrainSet, set.test, classifierID)
				accOriginal <- caret::confusionMatrix(predictionsOriginal, set.test$Class)$overall[1]
				
				# 6) Train a randomForest classifier (from randomForest package) with class noise
				predictionsNoise <- general.fitAndPredict(smotedTrainSet.noise$data, set.test, classifierID)
				accNoise <- caret::confusionMatrix(predictionsNoise, set.test$Class)$overall[1]
				
				# For each noise filter...
				for (noiseFilterID in config.NOISEFILTER_SEQ) {
					# 7) Use a noise filter here
					filterResult <- general.callNoiseFilter(smotedTrainSet.noise$data, noiseFilterID)

					# 8) Train a new classifier, after the noise filtering 
					predictionsFiltered <- general.fitAndPredict(filterResult$cleanData, set.test, classifierID)
					accFiltered <- caret::confusionMatrix(predictionsFiltered, set.test$Class)$overall[1]

					# @ Garbage collection (to avoid memory issues) -------
					rm(filterResult)
					gc()
					# @ ---------------------------------------------------

					# 11) Check accuracy results
					cat(i, config.DATASET_SEQ[datasetID, 1], classifierID, noiseFilterID, 
						smoteEnabled, accOriginal, accNoise, accFiltered, '\n', sep='|')
				}
			}
		}
	}
}