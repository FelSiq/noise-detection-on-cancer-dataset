# ----------------------------------------------
# SCRIPT INFORMATION
# ----------------------------------------------
# This code is related to experiments of Noise Filters on my Scientific Initiation project. The main purpose of
# this script is to get the accuracy of predictive machine learning models on the original datasets, after noise
# inputation (and before noise filter) and after noise filter. Comparing the model accuracies, it's possible to get
# a vague idea of how noise affects the performance of a predictive model. One could use more metadata of the models, 
# given by the confusion matrix of the caret package. In this work, only the accuracy was considered.
#
# It's possible to easily modify the parameters of this script, because all variables, including but not limited to
# noise filters, number of partitions on k-fold cross validation, datasets to be used etc, was stored on a separated 
# file on the "./src/config.R" path.
# 
# Please note that the full experiment originally made with this script take several days to be completed, so run it
# at your own risk. The random seed was fixed (again, on "./src/config.R" file), so you don't need to make the whole
# experiment with a single script run, you just need to modify the "./src/config.R" parameters order (datasets, noise 
# filters etc) file until you get all remaining metadata.
# 
# All the results gotten were pushed into my Github repository, so if you're interested on the raw metadata gotten,
# just check it out: https://github.com/FelSiq/noise-detection-on-cancer-dataset
# ----------------------------------------------

source('./src/LPNoiseInputation.R')
source('./src/config.R')
source('./src/generalFunctions.R')

for (datasetID in 1:nrow(config.DATASET_SEQ)) {
	
	dataset <- read.csv(paste('./datasets', config.DATASET_SEQ[datasetID, 1], sep='/'), sep = ' ')
	
	# The microarray data need a 'special' treatment, because the dataset is transposed and
	# the class column is the first one (some script parts assume that it's the last one instead).
	if (config.DATASET_SEQ[datasetID, 2] == 'Microarray') {
		dataset <- as.data.frame(t(dataset))
		colnames(dataset) <- c('Class', paste('X', seq(1, ncol(dataset) - 1), sep = ''))
		dataset <- dataset[, c(2:(ncol(dataset)), 1)]
	} else {
		colnames(dataset)[ncol(dataset)] <- 'Class'
	}
	
	# Factorizing the class label to the a binary factor-type column. The majority class should be
	# always the 0 factor, while the minority, of course, factor 1. This is a requeriment of SMOTE.
	labels <- unique(dataset$Class)
	majorityLabel <- ifelse(sum(dataset$Class == labels[1]) >= length(dataset$Class)/2, 1, 2)
	dataset$Class <- factor(ifelse(dataset$Class == labels[majorityLabel], 0, 1))
	
	# This is the partitions of the k-fold cross validation. Please note that the size of each
	# partition is not the same, but with approximate size.
	kpartition <- sample(1:config.FOLDS_NUM_CROSS_VALIDATION, size = nrow(dataset), replace = TRUE)
	
	# It is extremely important to SMOTE and insert artificial noise for each fold of cross validation,
	# or the results will be biased at the end.
	for (i in 1:config.FOLDS_NUM_CROSS_VALIDATION) {
		set.train <- subset(dataset, i != kpartition)
		set.test <- subset(dataset, i == kpartition)
		for (smoteEnabled in config.SMOTE_SEQ) {
			# SMOTE is a technique to balance the classes on the dataset. On this binary scenario, it does
			# combine undersampling (i.e some instances are ignored) of the majority class with oversampling 
			# (i.e copies of some instances are made) of minority class. This package has some requeriments to
			# work: the class column must be a binary (0's and 1's) factor-type column, and the majority class
			# is assumed corresponding the '0' factor.
			if (smoteEnabled) {
				aux <- ubSMOTE(dataset[-which(colnames(dataset) == 'Class')], dataset$Class)
				smotedTrainSet <- data.frame(aux$X)
				smotedTrainSet$Class <- aux$Y
				rm(aux)
				gc()
			} else {
				# If SMOTE is not used, the the 'smotedTrainSet' is just a copy of the set.train.
				smotedTrainSet <- set.train
			}
	
			# Here comes the artificial noise input. The 'random' method is used, where all instances have a
			# fixed probability of getting the class label exchanged. On a deeper analysis, different noise
			# input ratio may be used (for example, 0.05, 0.1, 0.2 and 0.4), because some models may perform 
			# much better with less noise, while others are more noise resistent. In this work, as you may see, 
			# only a fixed noise input ration is used, and it is specified @ "./src/config.R".
			smotedTrainSet.noise <- rand(smotedTrainSet, config.ERROR_INPUT_RATE)
	
			for (noiseFilterID in config.NOISEFILTER_SEQ) {
	
				# Call the noise filter here.
				filterResult <- general.callNoiseFilter(smotedTrainSet.noise$data, noiseFilterID)
	
				for (classifierID in config.CLASSIFIER_SEQ) {
					# Here all the three different accuracies are gotten. It is not safe to make any assumptions of
					# the results beforehand, but the expected pattern is predictionsNoise <= predictionsFiltered <= predictionsOriginal.
					# Of course, in some scenarios it will not be true, so the k-fold cross validation comes into the scene,
					# trying to minimize the impact of unusual cases. A big value of k is preferred, but this would cost too much
					# computational power, so k = 5 was originally adopted in this experiment. 

					# Please note that the confusion matrix of the caret package gives much more metadata than the accuracy value,
					# so a deeper analysis could take other statistic values into account.
					predictionsOriginal <- general.fitAndPredict(smotedTrainSet, set.test, classifierID)
					accOriginal <- caret::confusionMatrix(predictionsOriginal, set.test$Class)$overall[1]
	
					predictionsNoise <- general.fitAndPredict(smotedTrainSet.noise$data, set.test, classifierID)
					accNoise <- caret::confusionMatrix(predictionsNoise, set.test$Class)$overall[1]
	
					predictionsFiltered <- general.fitAndPredict(filterResult$cleanData, set.test, classifierID)
					accFiltered <- caret::confusionMatrix(predictionsFiltered, set.test$Class)$overall[1]
	
					cat(date(), i, config.DATASET_SEQ[datasetID, 1], classifierID, noiseFilterID, 
						smoteEnabled, accOriginal, accNoise, accFiltered, '\n', sep='|')
				}
			}
		}
	}
}

# On './src/config.R', a output file is specified to append all results.
# The empty sink() call make the future outputs goes into the 'stdout' again.
sink()
# ----------------------------------------------
# END OF THE SCRIPT.
# ----------------------------------------------