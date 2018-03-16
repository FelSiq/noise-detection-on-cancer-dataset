# ---------------------------------------
# INFORMATION
# ---------------------------------------
# The purpose of this auxiliary script is to compile all results got on the main experiment,
# and turn it in the latex code specific to my report. The idea is not to made the most optimal script,
# but the script that produce a output which makes possible a copy-pasting from output file direct to the report.
# ---------------------------------------

source('res-config.R')

# ---------------------------------------
# PROCESS NON-FILTER BASED ACCURACY
# ---------------------------------------
sink(file = './extra-results/original-corrupted-accuracy.out')

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
sink(NULL)