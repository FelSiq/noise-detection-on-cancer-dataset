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
sink(file = './extra-results/filtered-accbyclass-SBC-positive.out')

classToPred <- TRUE
config.PRINTSEQ <- c('Filtered', 'Diff')
config.SMOTE_SEQ <- c(FALSE)

for (s in config.SMOTE_SEQ) {
	for (f in config.NOISEFILTER_SEQ) {
		cat('\n\n\\hline \\multicolumn{7}{|c|}{\\textbf{Acurácia (filtro: ', f ,')}} \\\\ \\hline\n\n', sep='')	
		charAsciiIndex <- 65

		for (r in metadataPathList) {
			metadata <- getMetadata(r)
			cat('\\colcell\\dados', intToUtf8(charAsciiIndex), 'Nome & ',sep='')
			charAsciiIndex <- charAsciiIndex + 1			

			for (p in config.PRINTSEQ) {
				for (c in config.CLASSIFIER_SEQ) {

					curMetadata <- metadata[metadata$Filter == f & 
						metadata$SMOTE == s & metadata$Classifier == c,]

					if (nrow(curMetadata) > 0) {
						if (classToPred) {
							accFiltered <- curMetadata$FilteredPosPredValue	
							accCorrupted <- curMetadata$CorruptedPosPredValue
						} else {
							accFiltered <- curMetadata$FilteredNegPredValue	
							accCorrupted <- curMetadata$CorruptedNegPredValue	
						}

						accFiltered <- accFiltered[!is.na(accFiltered)]
						accCorrupted <- accCorrupted[!is.na(accCorrupted)]

						if (p == 'Filtered') {	
							stdDevPredFiltered <- signif(sd(accFiltered), 1)
							predFilteredPlaces <- signifPlaces(stdDevPredFiltered)
							cat('$', removeZero(round(mean(accFiltered), predFilteredPlaces)), 
								'\\pm', removeZero(stdDevPredFiltered), ' $ & ', sep='')
						}

						if (p == 'Diff') {
							diffPred <- mean(accFiltered) - mean(accCorrupted)
							diffStd <- signif(sd(accFiltered) + sd(accCorrupted), 1)
							diffPlaces <- signifPlaces(diffStd)
							
							diffDisplay <- round(mean(diffPred), diffPlaces)
							color <- if (diffDisplay - diffStd > 0) 'Blue' else 
								if (diffDisplay + diffStd < 0) 'Red' else 'Gray'
							
							cat('\\cellcolor{', color ,'} ', sep='')

							cat('$', removeZero(diffDisplay), '\\pm', removeZero(diffStd), '$', sep='')

							if (c != 'SVM')
								cat(' & ')
						}
					} # End of IF DATASET NOT EMPTY
				} # End of FOR on DATASET_SEQ
			} # End of FOR on PRINT_SET (Filtered or Diff)
			cat(' \\\\\n')
		} # End of FOR Metadata Pathlist
	} # End of FOR FILTER_SEQ
} # End of FOR SMOTE_SEQ
sink(NULL)
