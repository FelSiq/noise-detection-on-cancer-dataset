general.fitClassifier <- function(data, classifierID = 'RF') {
	model <- NULL
	switch(classifierID,
		'RF' = {
			model <- randomForest(
				formula = Class ~ .,
				x = data[-ncol(data)],
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
	predictions <- NULL
	if (type == 'KNN') {
		predictions <- prediction <- knn(
			train = data.train[-which(colnames(data.train) == 'Class')], 
			test = data.test[-which(colnames(data.test) == 'Class')],
			cl = data.train$Class)
	} else {
		model <- general.fitClassifier(data.train, type)
		predictions <- predict(model, newdata = data.test)
	}
	return (predictions)
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
		'SF' = {
				cleanData <- saturationFilter(Class ~ ., data)
			}
		)
	return (cleanData)
}