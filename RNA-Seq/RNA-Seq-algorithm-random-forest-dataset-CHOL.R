# Specifications: ------------------------------------------
# 	Algorithm : 'Random Forest Classfier' from library 'randomForest'
# 	Noise Filtering (1 or 0): 0
# 	Additional Libraries : 'caTools' (for data split)
# 	Dataset type: RNA-Seq
# 	Dataset deeper specifications: 
		# Total Attributes: 19665
		# Total Sample: 45
		# Total TP: 36
		# Total NT: 9
		# About : Expressionless genes have been removed, i.e., features that in all samples have zero value.
		# Total removed genes:  866
# 	Dataset name: 'CHOL.rnaseqv2.txt'
# ----------------------------------------------------------

# 1) Get the Dataset.
dataset <- read.csv('CHOL.rnaseqv2.txt', sep = ' ')

# A custom seed will be used to ensure experiment replication
set.seed(101010)

# 2) Load all the necessary packages for this test
library(caTools) # For data splitting
library(randomForest) # For prediction test

# Set the target feature binary factor type
dataset$class <- factor(
	x = dataset$class, 
	levels = unique(dataset$class), 
	labels = c(0, 1))

# 3) Split the dataset in Train and Test sets
datasplit <- sample.split(dataset$class, SplitRatio = 0.60)
set.train <- subset(dataset, datasplit)
set.test <- subset(dataset, !datasplit)

# 4) Train a randomForest classifier (from randomForest package)
classifier <- randomForest(
	formula = class ~ .,
	ntree = 10,
	x = set.train[-ncol(set.train)],
	y = set.train$class,
	importance = TRUE)

# 5) Use the classifier to predict some results
predictions <- predict(
	object = classifier,
	newdata = set.test)

# 6) Check the confusion matrix
table(set.test$class, predictions)