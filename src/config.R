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
# ===================================================================

# A custom seed will be used to ensure experiment replication
set.seed(101010)

# Load all the necessary packages for this test
library(caTools) # For data splitting
library(randomForest) # For Random Forest
library(e1071) # For SVM
library(class) # For knn
library(NoiseFiltersR) # For noise filters
library(unbalanced) # For SMOTE (treatment of unbalanced dataset)
library(caret) # For a good confusion matrix

config.DATASET_SEQ <- t(matrix(c(
		c('BRCA.mirnaseq.txt', 'Micro-RNA'),
		c('KICH.mirnaseq.txt', 'Micro-RNA'),
		c('THCA.mirnaseq.txt', 'Micro-RNA'),
		c('CHOL.mirnaseq.txt', 'Micro-RNA'),
		c('LUAD.mirnaseq.txt', 'Micro-RNA')
	), 2, 5))
config.CLASSIFIER_SEQ <- c('KNN', 'SVM', 'RF')
config.NOISEFILTER_SEQ <- c('HARF')

DEBUG = TRUE 
if (!DEBUG) {
	# Initial experiment setup
	config.DATASET_SEQ <- t(matrix(c(
		c('BRCA.mirnaseq.txt', 'Micro-RNA'),
		c('KICH.mirnaseq.txt', 'Micro-RNA'),
		c('THCA.mirnaseq.txt', 'Micro-RNA'),
		c('CHOL.mirnaseq.txt', 'Micro-RNA'),
		c('LUAD.mirnaseq.txt', 'Micro-RNA'),
		c('dataset_lymphoma_shipp.txt', 'Microarray'),
		c('dataset_adrenal_dahia.txt', 'Microarray'),
		c('dataset_mixed_chowdary.txt', 'Microarray'),
		c('dataset_colon_alon.txt', 'Microarray'),
		c('dataset_prostate_singh.txt', 'Microarray'),
		c('CHOL.rnaseqv2.txt', 'RNA-Seq'),
		c('LUAD.rnaseqv2.txt', 'RNA-Seq'),
		c('READ.rnaseqv2.txt', 'RNA-Seq'),
		c('KICH.rnaseqv2.txt', 'RNA-Seq'),
		c('THCA.rnaseqv2.txt', 'RNA-Seq')),
		2, 15))
	# This is the sequence which the choosen classifiers will be called, for each dataset
	config.CLASSIFIER_SEQ <- c('RF', 'SVM', 'KNN')
	# Sequence of Noise filters, for each classifier
	config.NOISEFILTER_SEQ <- c('HARF', 'AENN', 'INFFC', 'SF')
}

# Turn SMOTE OFF and ON, for each noise filter
config.SMOTE_SEQ <- c(FALSE, TRUE)
#
config.DATASPLIT_RATE <- 0.6
# 
config.ERROR_INPUT_RATE <- 0.2