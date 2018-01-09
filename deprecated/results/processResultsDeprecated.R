# ---------------------------------------
# INFORMATION
# ---------------------------------------
# The purpose of this auxiliary script is to compile all results got on the main experiment,
# and turn it in the latex code specific to my report. The idea is not to made the most optimal script,
# but the script that produce a output which makes possible a copy-pasting from output file direct to the report.
# ---------------------------------------

# ---------------------------------------
metadataPathList <- list.files(pattern = '.*\\.dat', recursive = TRUE)

# The order of the configuration below follows strictly the pattern on my report and does matter.
config.SMOTE_SEQ <- c(FALSE, TRUE)
config.CLASSIFIER_SEQ <- c('KNN', 'RF', 'SVM')
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

sink(file = 'INFFC_DEPRECATED_Acc.out', append = TRUE)
charAsciiIndex <- 65
for (r in metadataPathList) {
	metadata <- getMetadata(r)
	cat('\\colcell\\dados', intToUtf8(charAsciiIndex), 'Nome & ',sep='')
	charAsciiIndex <- charAsciiIndex + 1
	for (s in config.SMOTE_SEQ) {
		for (c in config.CLASSIFIER_SEQ) {

			curMetadata <- metadata[metadata$SMOTE == s & metadata$Classifier == c,]

			if (nrow(curMetadata) > 0) {
				diffPred <- mean(curMetadata$predFiltered) - mean(curMetadata$predCorrupted)
				diffStd <- signif(sd(curMetadata$predFiltered) + sd(curMetadata$predCorrupted), 1)
				diffPlaces <- signifPlaces(diffStd)
							
				diffDisplay <- round(mean(diffPred), diffPlaces)
				color <- if (diffDisplay - diffStd > 0) 'Blue' else if (diffDisplay + diffStd < 0) 'Red' else 'Gray'
				cat('\\cellcolor{', color ,'} ', sep='')

				cat('$', diffDisplay, '\\pm', diffStd, '$', sep='')

				if (!(c == 'SVM' & s == TRUE))
					cat(' & ')
			}
		}
	}	
	cat(' \\\\\n')
}

sink(NULL)
# ---------------------------------------
# END OF THE SCRIPT
# ---------------------------------------