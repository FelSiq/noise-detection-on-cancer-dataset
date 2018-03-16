# ---------------------------------------
# INFORMATION
# ---------------------------------------
# This script generates a Latex table for the filtered accuracies, ready for copy-paste
# directly into my report.
# ---------------------------------------

source('res-config.R')

# ---------------------------------------
# PROCESS FILTER-BASED ACCURACY
# ---------------------------------------
sink(file = './extra-results/filtered-accuracy.out')

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
sink(NULL)