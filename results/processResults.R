# ---------------------------------------
# INFORMATION
# ---------------------------------------
# The purpose of this auxiliary script is to compile all results got on the main experiment,
# and turn it in the latex code specific to my report. The idea is not to made the most optimal script,
# but the script that produce a output which makes possible a copy-pasting from output file direct to the report.
# ---------------------------------------

# ---------------------------------------
metadataPathList <- list.files(pattern = '.*\\.dat', recursive = TRUE)

printOriginalAcc <- TRUE
printFilterAcc <- TRUE
printPValues <- TRUE

# The order of the configuration below follows strictly the pattern on my report and does matter.
config.SMOTE_SEQ <- c(FALSE, TRUE)
config.CLASSIFIER_SEQ <- c('KNN', 'RF', 'SVM')
config.NOISEFILTER_SEQ <- c('HARF', 'AENN', 'INFFC', 'ENG')
# ---------------------------------------
getMetadata <- function(path) {
	metadata <- read.table(path, sep='|')
	metadata <- metadata[-ncol(metadata)]

	colnames(metadata) <- c('Time', 'k-fold', 'Dataset', 
			'Classifier', 'Filter', 'SMOTE', 
			'predOriginal', 'predCorrupted', 'predFiltered',
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

# ---------------------------------------
# PROCESS NON-FILTER BASED ACCURACY
# ---------------------------------------
if (printOriginalAcc){
	sink(file = 'originalAndCorrAcc.out', append = TRUE)
	config.PRINTSEQ <- c('Original', 'Corrupted')
	for (s in config.SMOTE_SEQ) {
		charAsciiIndex <- 65
		cat('@', s, ':\n')
		for (r in metadataPathList) {
			metadata <- getMetadata(r)
			cat('\\colcell\\dados', intToUtf8(charAsciiIndex), 'Nome & ',sep='')
			charAsciiIndex <- charAsciiIndex + 1
			for (p in config.PRINTSEQ) {
				for (c in config.CLASSIFIER_SEQ) {

				curMetadata <- metadata[metadata$SMOTE == s & metadata$Classifier == c,]

				if (nrow(curMetadata) > 0) {
						if (p == 'Original') {
							stdDevPredOriginal <- signif(sd(curMetadata$predOriginal), 1)
							predOriginalPlaces <- signifPlaces(stdDevPredOriginal)
							cat('$', removeZero(round(mean(curMetadata$predOriginal), predOriginalPlaces)), 
								'\\pm', removeZero(stdDevPredOriginal), '$ & ', sep='')
						}
						if (p == 'Corrupted') {
							stdDevPredCorrupted <- signif(sd(curMetadata$predCorrupted), 1)
							predCorruptedPlaces <- signifPlaces(stdDevPredCorrupted)
							cat('$', removeZero(round(mean(curMetadata$predCorrupted), predCorruptedPlaces)), 
								'\\pm', removeZero(stdDevPredCorrupted), '$', sep='')
								
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

# ---------------------------------------
# PROCESS FILTER-BASED ACCURACY
# ---------------------------------------
if (printFilterAcc) {
	sink(file = 'filteredAcc.out', append = TRUE)
	config.PRINTSEQ <- c('Filtered', 'Diff')
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
							if (p == 'Filtered') {
								stdDevPredFiltered <- signif(sd(curMetadata$predFiltered), 1)
								predFilteredPlaces <- signifPlaces(stdDevPredFiltered)

								cat('$', removeZero(round(mean(curMetadata$predFiltered), predFilteredPlaces)), 
									'\\pm', removeZero(stdDevPredFiltered), '$ & ', sep='')
							}
							if (p == 'Diff') {
								diffPred <- mean(curMetadata$predFiltered) - mean(curMetadata$predCorrupted)
								diffStd <- signif(sd(curMetadata$predFiltered) + sd(curMetadata$predCorrupted), 1)
								diffPlaces <- signifPlaces(diffStd)
								
								diffDisplay <- round(mean(diffPred), diffPlaces)
								color <- if (diffDisplay - diffStd > 0) 'Blue' else if (diffDisplay + diffStd < 0) 'Red' else 'Gray'
								cat('\\cellcolor{', color ,'} ', sep='')

								cat('$', removeZero(diffDisplay), '\\pm', removeZero(diffStd), '$', sep='')

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
}

# ---------------------------------------
# PROCESS P-VALUES (TO BE TESTED)
# ---------------------------------------
# http://blog.minitab.com/blog/adventures-in-statistics-2/how-to-correctly-interpret-p-values
if (printPValues) {
	library(metap)
	sink(file = 'processedPValues.out', append = TRUE)
	charAsciiIndex <- 65
	cat('@', s, f, ':\n')
	for (r in metadataPathList) {
		metadata <- getMetadata(r)
		cat('\\colcell\\dados', intToUtf8(charAsciiIndex), 'Nome & ',sep='')
		charAsciiIndex <- charAsciiIndex + 1
		for (s in config.SMOTE_SEQ) {
			for (f in config.NOISEFILTER_SEQ) {
				curMetadata <- metadata[metadata$Filter == f & metadata$SMOTE == s,]

				if (nrow(curMetadata) > 0) {
					SumLogPValue <- sumlog(curMetadata$PVFiltered)
					cat(SumLogPValue, sep='')

					if (!(f == 'ENG' && s == TRUE))
						cat(' & ')				
				}
			cat(' \\\\\n')
			}
		}
	}
}

sink(NULL)
# ---------------------------------------
# END OF THE SCRIPT
# ---------------------------------------