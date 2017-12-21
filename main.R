# ----------------------------------------------------------
# LOAD SECTION =============================================
# ----------------------------------------------------------
source('./src/LPNoiseInputation.R')
source('./src/config.R')
source('./src/generalFunctions.R')

# ----------------------------------------------------------
# MAIN SECTION =============================================
# ----------------------------------------------------------

# prediction <- knn(
	# train = train_set[, -ncol(train_set)], 
	# test = test_set[, -ncol(test_set)],
	# cl = train_set$Purchased,
	# k = 5)

# For each dataset...
for (datasetID in 1:nrow(config.DATASET_SEQ)) {
	# 1) Get the Dataset.
	dataset <- read.csv(paste('./datasets', config.DATASET_SEQ[datasetID, 1], sep='/'), sep = ' ')

	if (config.DATASET_SEQ[datasetID, 2] == 'Microarray') {
			# 3) Transpose the dataset (?)
		dataset <- as.data.frame(t(dataset))

		# 3.a) Correct the column names
		# The output/target feature will be named 'Class', and the prediction 
		# features will be enumered with a natural number sequence.
		colnames(dataset) <- c('Class', seq(1, ncol(dataset) - 1))
	} else {
		# Noise inputation and SMOTE requeriments
		colnames(dataset)[ncol(dataset)] <- 'Class'
	}

	# Set the target feature binary factor type
	dataset$Class <- factor(
		x = dataset$Class, 
		levels = unique(dataset$Class), 
		labels = c(0, 1))

	# 3) Split the dataset in Train and Test sets
	datasplit <- sample.split(dataset$Class, SplitRatio = config.DATASPLIT_RATE)
	set.train <- subset(dataset, datasplit)
	set.test <- subset(dataset, !datasplit)

	# For each classifier...
	for (classifierID in config.CLASSIFIER_SEQ) {
		# For each noise filter...
		for (noiseFilterID in config.NOISEFILTER_SEQ) {
			# On and OFF with SMOTE
			for (smoteEnabled in config.SMOTE_SEQ) {
				# 
				if (smoteEnabled) {
					aux <- ubSMOTE(set.train[-ncol(dataset)], set.train[['Class']])
					smotedTrainSet <- data.frame(aux$X)
					smotedTrainSet['Class'] <- aux$Y
				} else {
					smotedTrainSet <- set.train
				}

				# 4) Train a classifier with the original data (before artificial noise inputation)
				classifierOriginalData <- fitClassifier(smotedTrainSet, classifierID)

				# 5) Input artificial noise
				smotedTrainSet.noise <- rand(smotedTrainSet, config.ERROR_INPUT_RATE)

				# 6) Train a randomForest classifier (from randomForest package) with class noise
				classifierNoiseData <- fitClassifier(smotedTrainSet.noise$data, classifierID)

				# 7) Use a noise filter here
				filterResult <- callNoiseFilter(smotedTrainSet.noise$data, noiseFilterID)

				# 8) Train a new classifier, after the noise filtering 
				classifierNoiseFilteredData <- fitClassifier(filterResult$cleanData, classifierID)

				# 10) Use the original classifier to predict some results
				predictionsOriginal <- predict(
					object = classifierOriginalData,
					newdata = set.test)

				# 9) Use the noise classifier to predict some results
				predictionsNoise <- predict(
					object = classifierNoiseData,
					newdata = set.test)

				# 9) Use the filtered classifier to predict some results
				predictionsFiltered <- predict(
					object = classifierNoiseFilteredData,
					newdata = set.test)

				# 11) Check accuracy results
				accOriginal <- caret::confusionMatrix(predictionsOriginal, set.test$Class)$overall[1]
				accNoise <- caret::confusionMatrix(predictionsNoise, set.test$Class)$overall[1]
				accFiltered <- caret::confusionMatrix(predictionsFiltered, set.test$Class)$overall[1]

				cat(config.DATASET_SEQ[datasetID, 1], classifierID, noiseFilterID, 
					smoteEnabled, accOriginal, accNoise, accFiltered, '\n', sep='+')
			}
		}
	}
}