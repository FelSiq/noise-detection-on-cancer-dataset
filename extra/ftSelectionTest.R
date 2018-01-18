produceMetadata <- FALSE
generateBoxplot <- FALSE
printLatexCode <- TRUE

signifPlaces <- function(x) {
	i <- 0
	if (!is.na(x) & x != 0.0) {
		while (x < 1.0) {
			x <- x * 10.0
			i <- i + 1
		}
	}
	return (i)
}

removeZero <- function(x) {
	return(sub('0\\.', '.', as.character(x)))
}

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

set.seed(101010)
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
if (produceMetadata) {
	library(Boruta)
	library(caret)

	library(randomForest)
	library(e1071)
	library(class)

	sink(file = 'ftSelectionTest_Boruta_x_caret.out', append=TRUE)
	config.FT_SELECTION_KEPT_VARIABLE_NUM <- c(500, 600, 750, 800, 900)
	config.BORUTA_MAX_RUNS <- 1000
	config.KNN_K <- 5
	config.CLASSIFIER_SEQ <- c('RF', 'SVM', 'KNN')
	control <- rfeControl(functions = rfFuncs, method = 'cv', number = 10)

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
			selectedAttBoruta <- c(getSelectedAttributes(final.result), 'Class')

			# CARET APPROACH
			results <- rfe(
				x = set.train[-which(colnames(set.train) == 'Class')],
				y = set.train$Class,
				rfeControl = control,
				sizes = config.FT_SELECTION_KEPT_VARIABLE_NUM)
			selectedAttCaret <- c(predictors(results), 'Class')

			# Fit predictors
			for (classifierID in config.CLASSIFIER_SEQ) {
				predictionsBoruta <- general.fitAndPredict(
					data.train = set.train[selectedAttBoruta], 
					data.test = set.test[selectedAttBoruta], 
					whichClassifier = classifierID)
				accBoruta <- caret::confusionMatrix(predictionsBoruta, set.test$Class)$overall[1]
				pValueBoruta <- caret::confusionMatrix(predictionsBoruta, set.test$Class)$overall[6]

				predictionsCaret <- general.fitAndPredict(
					data.train = set.train[selectedAttCaret], 
					data.test = set.test[selectedAttCaret], 
					whichClassifier = classifierID)
				accCaret <- caret::confusionMatrix(predictionsCaret, set.test$Class)$overall[1]
				pValueCaret <- caret::confusionMatrix(predictionsCaret, set.test$Class)$overall[6]

				cat(date(), k, config.DATASET_SEQ$datasetName[datasetID], classifierID, 
					accBoruta, accCaret, pValueBoruta, pValueCaret, '\n', sep='|')
			}
		}
	}
}

if (generateBoxplot | printLatexCode) {
	metadata <- read.csv('ftSelectionTest_Boruta_x_caret.out', sep='|')
	colnames(metadata) <- c('Date', 'Fold', 'Dataset', 'Classifier', 
		'AccBoruta', 'AccCaret', 'PValueBoruta', 'PValueCaret')
}
# ---------------------------------------
# BORUTA X CARET BOXPLOT
# ---------------------------------------
if (generateBoxplot) {
	boxplot(metadata$AccBoruta[1:150], metadata$AccCaret[1:150], 
		metadata$AccBoruta[150:300], metadata$AccCaret[150:300],
		names=c('Boruta (M)', 'RFE (M)', 'Boruta (R)', 'RFE (R)'),
		ylab='Acurácia',
		col=c('steelblue3', 'brown2'),
		pars=list(boxwex = 0.5, staplewex = 0.5, outwex = 0.75), log='y')

	dev.copy(png,'ftSelectionTest_boxplot.png')
	dev.off()
}

# ---------------------------------------
# BORUTA X CARET LATEX CODE (TO BE TESTED)
# ---------------------------------------
if (printLatexCode) {
	config.CLASSIFIER_SEQ <- c('KNN', 'RF', 'SVM')

	dataSeq <- c(
		'CHOL.rnaseqv2.txt', 
		'KICH.rnaseqv2.txt', 
		'LUAD.rnaseqv2.txt', 
		'READ.rnaseqv2.txt', 
		'THCA.rnaseqv2.txt',
		'dataset_adrenal_dahia.txt', 
		'dataset_colon_alon.txt', 
		'dataset_lymphoma_shipp.txt', 
		'dataset_mixed_chowdary.txt', 
		'dataset_prostate_singh.txt') 

	charAsciiIndex <- 65
	for (r in dataSeq) {
		cat('\\colcell\\dados', intToUtf8(charAsciiIndex), 'Nome & ',sep='')
		charAsciiIndex <- if(charAsciiIndex != 69) (charAsciiIndex + 1) else (charAsciiIndex + 6)
		for (c in config.CLASSIFIER_SEQ) {
			curMetadata <- metadata[metadata$Dataset == r & metadata$Classifier == c,]

			if (nrow(curMetadata) > 0) {
				accDiff <- curMetadata$AccBoruta - curMetadata$AccCaret 

				stdDevPredDiff <- signif(sd(accDiff), 1)
				predDiffPlaces <- signifPlaces(stdDevPredDiff)
				cat('$', removeZero(round(mean(accDiff), predDiffPlaces)), '\\pm', removeZero(stdDevPredDiff), '$', sep='')

				if (c != 'SVM')
					cat(' & ')	
			}
		}
		cat(' \\\\\n')
	}
}

# ----------------------------------------------------------------
sink(NULL)