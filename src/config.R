# ===================================================================
# INFORMATION: 
# This is the configuration module. All setup is done here, and affects
# directly the main and other attached modules.
#
# Stuff defined here:
#	1) Random seed
#	2) Datasets
#	3) Data split rate (on x-validation)
#	4) Libraries used
#	5) Algorithms (for noise filtering and classification) used
#	6) Input rate of artificial noise
#	7) Output file to save results
# ===================================================================
# Load all the necessary packages for this test
library(caTools) # For data splitting
library(randomForest) # For Random Forest
library(e1071) # For SVM
library(class) # For knn
library(NoiseFiltersR) # For noise filters
library(unbalanced) # For SMOTE (treatment of unbalanced dataset)
library(caret) # For a good confusion matrix
library(Boruta) # For feature selection
library(parallel)
library(fifer) # For stratified sampling of dataset

# Specify if parallel environment should be used on the script run
config.PARALLEL_SETUP <- FALSE
config.NO_CORES <- detectCores() - 1
# A custom seed will be used to ensure experiment replication
config.RANDOM_SEED <- 101010
# Set default output file
sink(file='noiseResults.out', append=TRUE)
# Display DEBUG information?
config.DEBUG <- TRUE 

config.DATASET_SEQ <- list()
# Initial experiment setup
config.DATASET_SEQ$datasetName <- c(
	'CHOL.mirnaseq.txt', 
	'KICH.mirnaseq.txt', 
	'LUAD.mirnaseq.txt', 
	'THCA.mirnaseq.txt', 
	'BRCA.mirnaseq.txt', 
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
	'Micro-RNA',
	'Micro-RNA',
	'Micro-RNA',
	'Micro-RNA',
	'Micro-RNA',
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
# This is the sequence which the choosen classifiers will be called, for each dataset
config.CLASSIFIER_SEQ <- c('RF', 'SVM', 'KNN')
# Sequence of Noise filters, for each classifier
config.NOISEFILTER_SEQ <- c('HARF', 'AENN', 'INFFC', 'ENG')
# Turn SMOTE OFF and ON, for each noise filter
config.SMOTE_SEQ <- c(FALSE, TRUE)
# Rate of data split between train and test set
config.DATASPLIT_RATE <- 0.6
# Rate of the artificial noise input
config.ERROR_INPUT_RATE <- 0.2
# Number of folds in the cross validation of the experiments
config.FOLDS_NUM_CROSS_VALIDATION <- 5
# Number of variables to be keep on the feature selection (caret approach only)
config.FT_SELECTION_KEPT_VARIABLE_NUM <- c(500, 600, 750, 800, 900)
# k of kNN
config.KNN_K <- 5
# Borute approach only
config.BORUTA_MAX_RUNS <- 1000
# Should the dataset be stratified sampled before processing?
config.SAMPLE_DATA <- FALSE
# If above is TRUE, how many instances should be kept?
config.SAMPLE_SIZE <- 80
# Enable FT Selection for all type of data?
config.ENABLE_FT_SELECTION <- TRUE
