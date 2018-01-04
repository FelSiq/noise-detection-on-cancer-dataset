# ---------------------------------------
# INFORMATION
# ---------------------------------------
# The purpose of this auxiliary script is to compile all results got on the main experiment,
# and turn it in the latex code specific to my report. The idea is not to made the most optimal script,
# but the script that produce a output which makes possible a copy-pasting from output file direct to the report.
# ---------------------------------------

# ---------------------------------------
metadataPathList <- list.files(pattern = '.*\\.dat', recursive = TRUE)
sink(file = 'metadataProcessed.out', append = TRUE)

# The order of the configuration below follows strictly the pattern on my report and does matter.
config.SMOTE_SEQ <- c(FALSE, TRUE)
config.CLASSIFIER_SEQ <- c('KNN', 'RF', 'SVM')
config.NOISEFILTER_SEQ <- c('HARF', 'AENN', 'INFFC', 'ENG')
config.PRINTSEQ <- c('Original', 'Corrupted', 'Filtered')
# ---------------------------------------
getMetadata <- function(path) {
	metadata <- read.table(path, sep='|')
	metadata <- metadata[-ncol(metadata)]

	if (ncol(metadata) < 9) {
		colnames(metadata) <- c('k-fold', 'Dataset', 'Classifier', 
			'Filter', 'SMOTE', 'predOriginal', 'predCorrupted', 'predFiltered')
		metadata$Time <- rep.int(NA, nrow(metadata))
	} else {
		colnames(metadata) <- c('Time', 'k-fold', 'Dataset', 'Classifier', 
			'Filter', 'SMOTE', 'predOriginal', 'predCorrupted', 'predFiltered')
	}
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

# ---------------------------------------
for (s in config.SMOTE_SEQ) {
	for (f in config.NOISEFILTER_SEQ) {
		charAsciiIndex <- 65
		cat('@', s, f, ':\n')
		for (r in metadataPathList) {
			metadata <- getMetadata(r)
			cat('\\colcell\\dados', intToUtf8(charAsciiIndex), 'Nome & ',sep='')
			charAsciiIndex <- charAsciiIndex + 1
			for (p in config.PRINTSEQ) {
				for (c in config.CLASSIFIER_SEQ) {

				curMetadata <- metadata[metadata$Filter == f & metadata$SMOTE == s & metadata$Classifier == c,]

				if (nrow(curMetadata) > 0) {
						if (p == 'Original') {
							stdDevPredOriginal <- signif(sd(curMetadata$predOriginal), 1)
							predOriginalPlaces <- signifPlaces(stdDevPredOriginal)
							cat('$', round(mean(curMetadata$predOriginal), predOriginalPlaces), '\\pm', stdDevPredOriginal, '$ & ', sep='')
						}
						if (p == 'Corrupted') {
							stdDevPredCorrupted <- signif(sd(curMetadata$predCorrupted), 1)
							predCorruptedPlaces <- signifPlaces(stdDevPredCorrupted)
							cat('$', round(mean(curMetadata$predCorrupted), predCorruptedPlaces), '\\pm', stdDevPredCorrupted, '$ & ', sep='')
						}
						if (p == 'Filtered') {
							stdDevPredFiltered <- signif(sd(curMetadata$predFiltered), 1)
							predFilteredPlaces <- signifPlaces(stdDevPredFiltered)
							cat('$', round(mean(curMetadata$predFiltered), predFilteredPlaces), '\\pm', stdDevPredFiltered, '$', sep='')

							if (c != 'SVM')
								cat(' & ')
						}

					}
				}
			}
			cat(' \\\\\n')
		}
	}
}
sink(NULL)
# ---------------------------------------
# END OF THE SCRIPT
# ---------------------------------------