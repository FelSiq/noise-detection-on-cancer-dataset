general.fitClassifier <- function(data, classifierID = 'RF') {
	model <- NULL
	switch(classifierID,
		'RF' = {
			model <- randomForest(
				x = data[-which(colnames(data) == 'Class')],
				y = data$Class)
			},
		'SVM' = {
			model <- svm(
				x = data[-which(colnames(data) == 'Class')],
				y = data$Class,
				type = 'C-classification', 
				kernel = 'linear',
				scale = FALSE)
			}
	)
	return (model)
}

general.fitAndPredict <- function(data.train, data.test, type = 'RF') {
	prediction <- NULL
	if (type == 'KNN') {
		prediction <- knn(
			train = data.train[-which(colnames(data.train) == 'Class')], 
			test = data.test[-which(colnames(data.test) == 'Class')],
			cl = data.train$Class)
	} else {
		model <- general.fitClassifier(data.train, type)
		prediction <- predict(model, newdata = data.test[-which(colnames(data.test) == 'Class')])
	}
	return (prediction)
}

general.callNoiseFilter <- function(data, filterId = 'HARF', dataType = 'RNA-Seq') {
	cleanData <- NULL
	switch (filterId,
		'HARF' = {
				cleanData <- HARF(data)
			},
		'AENN' = {
				# RNA-Seq have too much predictive variables, which often causes stack overflow
				# on R section. This is because on the NoiseFiltersR implementation, a 'formula' is
				# used inside AENN implementation, which consumes too much memory due to the large
				# number of variables. The workaround is to use a personal implementation of AENN
				# with the RNA-Seq dataset type experiments. The results are, in general, very
				# similar, and the accuracies should not differ too much between the two implementations.
				if (dataType != 'RNA-Seq') {
					cleanData <- AENN(data)
				} else {
					cleanData <- filter.AENN(data)
				}
			},
		'INFFC' = {
				cleanData <- INFFC(data)
			},
		'ENG' = {
				cleanData <- ENG(data)
			}
		)
	return (cleanData)
}