general.fitClassifier <- function(data.train, whichClassifier = 'RF') {
	model <- NULL
	switch(whichClassifier,
		'RF' = {
			model <- randomForest::randomForest(
				x = data.train[-which(colnames(data.train) == 'Class')],
				y = data.train$Class)
			},
		'SVM' = {
			model <- e1071::svm(
				x = data.train[-which(colnames(data.train) == 'Class')],
				y = data.train$Class,
				type = 'C-classification', 
				kernel = 'linear')
			}
	)
	return (model)
}

general.fitAndPredict <- function(data.train, data.test, whichClassifier = 'RF') {
	prediction <- NULL
	if (whichClassifier == 'KNN') {
		prediction <- class::knn(
			train = data.train[-which(colnames(data.train) == 'Class')], 
			test = data.test[-which(colnames(data.test) == 'Class')],
			cl = data.train$Class,
			k = config.KNN_K)
	} else {
		model <- general.fitClassifier(data.train, whichClassifier)
		prediction <- predict(model, newdata = data.test[-which(colnames(data.test) == 'Class')])
	}
	return (prediction)
}

general.callNoiseFilter <- function(data, whichFilter = 'HARF') {
	cleanData <- NULL
	switch (whichFilter,
		'HARF' = {
				cleanData <- NoiseFiltersR::HARF(data)
			},
		'AENN' = {
				cleanData <- NoiseFiltersR::AENN(data)
			},
		'INFFC' = {
				cleanData <- NoiseFiltersR::INFFC(data)
			},
		'ENG' = {
				cleanData <- NoiseFiltersR::ENG(data)
			}
		)
	return (cleanData)
}

general.getDataset <- function(filepath, dataType = 'Microarray') {
	dataset <- read.csv(filepath, sep = ' ')
	
	# The microarray data need a 'special' treatment, because the dataset is transposed and
	# the class column is the first one (some script parts assume that it's the last one instead).
	if (dataType == 'Microarray') {
		dataset <- as.data.frame(t(dataset))
		colnames(dataset) <- c('Class', paste('X', seq(1, ncol(dataset) - 1), sep = ''))
		dataset <- dataset[, c(2:(ncol(dataset)), 1)]
	} else {
		colnames(dataset)[ncol(dataset)] <- 'Class'
	}
	
	# Factorizing the class label to the a binary factor-type column. The majority class should be
	# always the 0 factor, while the minority, of course, factor 1. This is a requeriment of SMOTE.
	labels <- unique(dataset$Class)
	majorityLabel <- ifelse(sum(dataset$Class == labels[1]) >= length(dataset$Class)/2, 1, 2)
	dataset$Class <- factor(ifelse(dataset$Class == labels[majorityLabel], 0, 1))

	return (dataset)
}