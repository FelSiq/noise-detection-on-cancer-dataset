general.fitClassifier <- function(data, classifierID = 'RF') {
	model <- NULL
	switch(classifierID,
		'RF' = {
			model <- randomForest(
				formula = Class ~ .,
				x = data[-which(colnames(data.test) == 'Class')],
				y = data$Class)
			},
		'SVM' = {
		model <- svm(
			formula = Class ~ ., 
			data = data, 
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
		prediction <- predict(model, newdata = data.test)
	}
	return (prediction)
}

general.callNoiseFilter <- function(data, filterId = 'HARF') {
	cleanData <- NULL
	switch (filterId,
		'HARF' = {
				cleanData <- HARF(Class ~ ., data)
			},
		'AENN' = {
				cleanData <- AENN(Class ~ ., data)
			},
		'INFFC' = {
				cleanData <- INFFC(Class ~ ., data)
			},
		'ENG' = {
				cleanData <- ENG(Class ~ ., data)
			}
		)
	return (cleanData)
}