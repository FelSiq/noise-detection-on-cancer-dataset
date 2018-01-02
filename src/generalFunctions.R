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

general.callNoiseFilter <- function(data, filterId = 'HARF') {
	cleanData <- NULL
	switch (filterId,
		'HARF' = {
				cleanData <- HARF(data)
			},
		'AENN' = {
				cleanData <- AENN(data)
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