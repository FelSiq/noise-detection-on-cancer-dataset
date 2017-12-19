# Specifications: ------------------------------------------
# 	Algorithm : 'Random Forest Classfier' from library 'randomForest'
# 	Noise Filtering (1 or 0): 0
# 	Additional Libraries : 'caTools' (for data split)
# 	Dataset type: Microarray
# 	Dataset deeper specifications: Bruno Ferez de Souza thesis @ 
#		http://www.teses.usp.br/teses/disponiveis/55/55134/tde-04012011-142551/pt-br.php
# 	Dataset name: 'dataset_adrenal_dahia.txt'
# ----------------------------------------------------------

# 1) Get the Dataset.
dataset <- read.csv('dataset_adrenal_dahia.txt', sep = ' ')

# A custom seed will be used to ensure experiment replication
set.seed(101010)

# 2) Load all the necessary packages for this test
library(caTools) # For data splitting
library(randomForest) # For prediction test

# 3) Transpose the dataset (?)
dataset.t <- as.data.frame(t(dataset))

# 3.a) Correct the column names
# The output/target feature will be named 'Class', and the prediction 
# features will be enumered with a natural number sequence.
colnames(dataset.t) <- c('Class', seq(1, ncol(dataset.t) - 1))

# Set the target feature to R factor type
dataset.t$Class <- factor(dataset.t$Class, levels = unique(dataset.t$Class))

# 4) Split the dataset in Train and Test sets
datasplit <- sample.split(dataset.t$Class, SplitRatio = 0.75)
set.train <- subset(dataset.t, datasplit)
set.test <- subset(dataset.t, !datasplit)

# 5) Train a randomForest classifier (from randomForest package)
classifier <- randomForest(
	formula = Class ~ .,
	ntree = 10,
	x = set.train[-1],
	y = set.train$Class,
	importance = TRUE)

# 6) Use the classifier to predict some results
predictions <- predict(
	object = classifier,
	newdata = set.test)

# 7) Check the confusion matrix
table(set.test$Class, predictions)
print(classifier)