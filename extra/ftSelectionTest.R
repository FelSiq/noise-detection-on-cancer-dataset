source('./src/generalFunctions.R')

config.DATASET_SEQ <- list()

config.DATASET_SEQ$datasetName <- c(
	'dataset_lymphoma_shipp.txt', 
	'dataset_adrenal_dahia.txt', 
	'dataset_mixed_chowdary.txt', 
	'dataset_colon_alon.txt', 
	'dataset_prostate_singh.txt', 
	'CHOL.rnaseqv2.txt', 
	'LUAD.rnaseqv2.txt', 
	'READ.rnaseqv2.txt', 
	'KICH.rnaseqv2.txt', 
	'THCA.rnaseqv2.txt')
config.DATASET_SEQ$datasetType <- c(
	'Microarray',
	'Microarray',
	'Microarray',
	'Microarray',
	'Microarray',
	'RNA-Seq',
	'RNA-Seq',
	'RNA-Seq',
	'RNA-Seq',
	'RNA-Seq')

n <- min(length(config.DATASET_SEQ$datasetName), length(config.DATASET_SEQ$datasetType))

# RFS from Caret --------------------------------------------------
# sink(file = 'ftSelectionTest_caret.out', append=TRUE)
# library(caret)
# library(randomForest)
# config.FT_SELECTION_KEPT_VARIABLE_NUM <- c(500, 600, 750, 800, 900)

# control <- rfeControl(functions = rfFuncs, method = 'cv', number = 10)

# for (datasetID in 1:n) {
# 	cat('(RFS) processing:', config.DATASET_SEQ$datasetName[datasetID], '...\n', sep = ' ')
# 	dataset <- general.getDataset(
# 		filepath = paste('./datasets', config.DATASET_SEQ$datasetName[datasetID], sep = '/'), 
# 		dataType = config.DATASET_SEQ$datasetType[datasetID])

# 	output <- rfe(x = dataset[-which(colnames(dataset) == 'Class')],
# 		y = dataset$Class,
# 		rfeControl = control,
# 		sizes = config.FT_SELECTION_KEPT_VARIABLE_NUM)
	
# 	print(output)
# }

# BORUTA ---------------------------------------------------------
# library(Boruta)
# library(randomForest)
# sink(file = 'ftSelectionTest_Boruta.out', append=TRUE)

# for (datasetID in 1:n) {
# 	cat('(Boruta) processing:', config.DATASET_SEQ$datasetName[datasetID], '...\n', sep = ' ')

# 	set.train <- general.getDataset(
# 		filepath = paste('./datasets', config.DATASET_SEQ$datasetName[datasetID], sep = '/'), 
# 		dataType = config.DATASET_SEQ$datasetType[datasetID])

# 	partial.result <- Boruta(
# 		x = set.train[-which(colnames(set.train) == 'Class')], 
# 		y = set.train$Class)

# 	final.result <- TentativeRoughFix(partial.result)

# 	print(final.result)
# }

# ----------------------------------------------------------------
# BORUTA X CARET (TO BE TESTED)
# ----------------------------------------------------------------
# https://www.analyticsvidhya.com/blog/2016/03/select-important-variables-boruta-package/
library(Boruta)
library(caret)

library(randomForest)
library(e1071)
library(class)

sink(file = 'ftSelectionTest_Boruta_x_caret.out', append=TRUE)
config.FT_SELECTION_KEPT_VARIABLE_NUM <- c(500, 600, 750, 800, 900)
config.CLASSIFIER_SEQ <- c('RF', 'SVM', 'KNN')

for (datasetID in 1:n) {
	cat('(Boruta x caret) processing:', config.DATASET_SEQ$datasetName[datasetID], '...\n', sep = ' ')

	dataset <- general.getDataset(
		filepath = paste('./datasets', config.DATASET_SEQ$datasetName[datasetID], sep = '/'), 
		dataType = config.DATASET_SEQ$datasetType[datasetID])

	foldsIndex <- sample(c(1:10), nrow(dataset), replace=TRUE)

	for (k in 1:10) {
		set.train <- subset(dataset, foldsIndex != k)
		set.test <- subset(dataset, foldsIndex == k)

		# BORUTA APPROACH
		partial.result <- Boruta(
			x = set.train[-which(colnames(set.train) == 'Class')], 
			y = set.train$Class,
			maxRuns = config.BORUTA_MAX_RUNS)
		final.result <- TentativeRoughFix(partial.result)
		selectedAttBoruta <- getSelectedAttributes(final.result)

		# CARET APPROACH
		control <- rfeControl(functions = rfFuncs, method = 'cv', number = 10)
		results <- rfe.nonCaret(
			x = set.train[-which(colnames(set.train) == 'Class')],
			y = set.train$Class,
			rfeControl = control,
			sizes = config.FT_SELECTION_KEPT_VARIABLE_NUM)
		selectedAttCaret <- predictors(results)

		# Fit predictors
		for (classifierID in config.CLASSIFIER_SEQ) {
			predictionsBoruta <- general.fitAndPredict(
				data.train = set.train[selectedAttBoruta], 
				data.test = set.test[selectedAttBoruta], 
				whichClassifier = classifierID)
			accBoruta <- caret::confusionMatrix(predictionsBoruta, set.test[selectedAttBoruta]$Class)$overall[1]
			pValueBoruta <- caret::confusionMatrix(predictionsBoruta, set.test[selectedAttBoruta]$Class)$overall[6]

			predictionsCaret <- general.fitAndPredict(
				data.train = set.train[selectedAttCaret], 
				data.test = set.test[selectedAttCaret], 
				whichClassifier = classifierID)
			accCaret <- caret::confusionMatrix(predictionsBoruta, set.test[selectedAttCaret]$Class)$overall[1]
			pValueCaret <- caret::confusionMatrix(predictionsBoruta, set.test[selectedAttCaret]$Class)$overall[6]

			cat(date(), i, config.DATASET_SEQ$datasetName[datasetID], classifierID, 
				accBoruta, accCaret, pValueBoruta, pValueCaret, '\n', sep='|')
		}
	}
}
# ----------------------------------------------------------------
sink(NULL)