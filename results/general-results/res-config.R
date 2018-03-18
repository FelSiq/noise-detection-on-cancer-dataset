# ---------------------------------------
# INFORMATION
# ---------------------------------------
# Pre-configuration for all result-related scripts.
# ---------------------------------------
# Get all filepath of results metadata from the experiment.
metadataPathList <- list.files(pattern='.*\\.dat', recursive=TRUE)

# The order of the configuration below follows strictly the pattern on my report and does matter.
config.SMOTE_SEQ <- c(FALSE, TRUE)
config.CLASSIFIER_SEQ <- c('KNN', 'RF', 'SVM')
config.NOISEFILTER_SEQ <- c('HARF', 'AENN', 'INFFC', 'ENG')
# ---------------------------------------
getMetadata <- function(path) {
	metadata <- read.table(path, sep='|')
	metadata <- metadata[-ncol(metadata)]

	colnames(metadata) <- c('Time', 'k-fold', 'Dataset', 'Classifier', 
		'Filter', 'SMOTE', 'predOriginal', 'predCorrupted', 'predFiltered', 
		'PVOriginal', 'PVCorrupted', 'PVFiltered')
		
	return (metadata)
}

# This function counts how many decimal places before the first significative digit of
# a real number.
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
