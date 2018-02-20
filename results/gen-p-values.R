# ---------------------------------------
# INFORMATION
# ---------------------------------------
# Use Fisher's method to combine obtained P-values from the experiment.
# ---------------------------------------

source('res-config.R')

# ---------------------------------------
# PROCESS P-VALUES
# ---------------------------------------
library(metap)

sink(file = './extra-results/processed-p-values.out', append = TRUE)
charAsciiIndex <- 65
for (r in metadataPathList) {
	metadata <- getMetadata(r)
	cat('\\colcell\\dados', intToUtf8(charAsciiIndex), 'Nome & ',sep='')
	charAsciiIndex <- charAsciiIndex + 1
	for (s in config.SMOTE_SEQ) {
		for (f in config.NOISEFILTER_SEQ) {
			curMetadata <- metadata[metadata$Filter == f & metadata$SMOTE == s,]
			if (nrow(curMetadata) > 0) {
				SumLogPValue <- sumlog(curMetadata$PVFiltered)$p
				numlen <- signifPlaces(SumLogPValue)
				if (SumLogPValue > 0.05) {
					cat(r, s, f, '\n\t', curMetadata$PVFiltered, '\nResult:')
					print(sumlog(curMetadata$PVFiltered))
					cat('\n')
				}

				color <- if (SumLogPValue <= 0.05) 'Blue' else 'Red'
				cat('\\cellcolor{', color ,'} ', sep='')

				cat(if (SumLogPValue > 1e-10) removeZero(round(SumLogPValue, numlen)) else 0.0, sep='')

				if (!(f == 'ENG' && s == TRUE))
					cat(' & ')	
			}
		}
	}
	cat(' \\\\\n')
}
sink(NULL)