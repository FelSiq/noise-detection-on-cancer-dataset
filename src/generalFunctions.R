fitClassifier <- function(data, classifierID = 'RF') {
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

callNoiseFilter <- function(data, filterId = 'HARF') {
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