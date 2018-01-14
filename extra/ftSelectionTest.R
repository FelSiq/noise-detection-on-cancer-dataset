general.getDataset <- function(filepath, dataType = 'Microarray') {
	dataset <- read.csv(filepath, sep = ' ')
	
	if (dataType == 'Microarray') {
		dataset <- as.data.frame(t(dataset))
		colnames(dataset) <- c('Class', paste('X', seq(1, ncol(dataset) - 1), sep = ''))
		dataset <- dataset[, c(2:(ncol(dataset)), 1)]
	} else {
		colnames(dataset)[ncol(dataset)] <- 'Class'
	}
	
	labels <- unique(dataset$Class)
	majorityLabel <- ifelse(sum(dataset$Class == labels[1]) >= length(dataset$Class)/2, 1, 2)
	dataset$Class <- factor(ifelse(dataset$Class == labels[majorityLabel], 0, 1))

	return (dataset)
}
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
library(Boruta)
library(randomForest)
sink(file = 'ftSelectionTest_Boruta.out', append=TRUE)

for (datasetID in 1:n) {
	cat('(Boruta) processing:', config.DATASET_SEQ$datasetName[datasetID], '...\n', sep = ' ')

	set.train <- general.getDataset(
		filepath = paste('./datasets', config.DATASET_SEQ$datasetName[datasetID], sep = '/'), 
		dataType = config.DATASET_SEQ$datasetType[datasetID])

	partial.result <- Boruta(
		x = set.train[-which(colnames(set.train) == 'Class')], 
		y = set.train$Class)

	final.result <- TentativeRoughFix(partial.result)

	print(final.result)
}

# ----------------------------------------------------------------
sink(NULL)