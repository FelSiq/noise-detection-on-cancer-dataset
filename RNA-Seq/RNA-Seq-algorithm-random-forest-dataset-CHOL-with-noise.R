# Specifications: ------------------------------------------
# 	Algorithm : 'Random Forest Classfier' from library 'randomForest'
# 	Noise Filtering (1 or 0): 1
#	Noise inputation rate used: 0.2
#	Type of noise inputation: Random
# 	Additional Libraries : 
#	'caTools' (for data split), 
#	'NoiseFiltersR' for noise filters
#	'DMwR' for SMOTE technique (for unbalanced data)
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
# Auxiliary functions to wise()
index = function(data, rate) {

	value = trunc(nrow(data)*rate);
	noise = sample(rownames(data), value, replace=FALSE);
	return(noise);
}

# Random: X. Zhu et al. Eliminating class noise in large datasets (& Luis Paulo code)
rand = function(data, rate) {
	aux = list();

	if(rate == 0) {
		aux$data = data;
		return(aux);
	}

	noise = index(data, rate);
	data[noise,]$Class = unlist(lapply(noise, function(i) {
		sample(setdiff(levels(data$Class), data[i,]$Class), 1);
	}));
	
	aux$noise = noise;
	aux$data = data;
	return(aux);
}

# ----------------------------------------------------------
# RandomForest wrapper (n is the number of trees)
callRandomForest <- function(data) {
	return (randomForest(
		formula = Class ~ .,
		x = data[-ncol(data)],
		y = data$Class))
}
# ----------------------------------------------------------


# 1) Get the Dataset.
dataset <- read.csv('/home/felipe/Documentos/IC-related/IC/dataset/RNA-Seq/RNA-Seq\ -\ 2015\ -\ TCGA/CHOL.rnaseqv2.txt', sep = ' ')

# A custom seed will be used to ensure experiment replication
set.seed(101010)

# 2) Load all the necessary packages for this test
library(caTools) # For data splitting
library(randomForest) # For prediction test
library(NoiseFiltersR) # For noise filters
library(unbalanced) # For SMOTE (treatment of unbalanced dataset)
library(caret)

# Set the target feature binary factor type
dataset$class <- factor(
	x = dataset$class, 
	levels = unique(dataset$class), 
	labels = c(0, 1))

# Noise inputation requeriment
colnames(dataset)[ncol(dataset)] <- 'Class'

# 3) Split the dataset in Train and Test sets
datasplit <- sample.split(dataset$Class, SplitRatio = 0.60)
set.train <- subset(dataset, datasplit)
set.test <- subset(dataset, !datasplit)

# 3.1) Use SMOTE on the train set, in order to balance the dataset
# set.train['Class'] <- revalue(dataset[['Class']], c('TP' = 0, 'NT' = 1))
# aux <- ubSMOTE(dataset[-ncol(dataset)], dataset[['Class']])
# smotedTrainSet <- data.frame(aux$X)
# smotedTrainSet['Class'] <- aux$Y

smotedTrainSet <- set.train

# 4) Train a classifier with the original data (before artificial noise inputation)
classifierOriginalData <- callRandomForest(smotedTrainSet)

# 5) Input artificial noise
smotedTrainSet.noise <- rand(smotedTrainSet, 0.2)

# 6) Train a randomForest classifier (from randomForest package) with class noise
classifierNoiseData <- callRandomForest(smotedTrainSet.noise$data)

# 7) Use a noise filter here
filterResult <- HARF(smotedTrainSet.noise$data)

# 8) Train a new classifier, after the noise filtering 
classifierNoiseFilteredData <- callRandomForest(filterResult$cleanData)

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

# 11) Check all results
caret::confusionMatrix(predictionsOriginal, set.test$Class)
caret::confusionMatrix(predictionsNoise, set.test$Class)
caret::confusionMatrix(predictionsFiltered, set.test$Class)

# print(classifierOriginalData)
# print(classifierNoiseData)
# print(classifierNoiseFilteredData)