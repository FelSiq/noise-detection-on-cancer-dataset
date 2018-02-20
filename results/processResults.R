# ---------------------------------------
# INFORMATION
# ---------------------------------------
# The purpose of this auxiliary script is to compile all results got on the main experiment,
# and turn it in the latex code specific to my report. The idea is not to made the most optimal script,
# but the script that produce a output which makes possible a copy-pasting from output file direct to the report.
# ---------------------------------------

# ---------------------------------------
metadataPathList <- list.files(pattern = '.*\\.dat', recursive = TRUE)

printOriginalAcc <- FALSE
printFilterAcc <- FALSE
printPValues <- TRUE
plotDensity <- FALSE
plotNoiseCompare <- FALSE

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
	# sink(file = 'processedPValues.out', append = TRUE)
	charAsciiIndex <- 65
	for (r in metadataPathList) {
		metadata <- getMetadata(r)
		# cat('\\colcell\\dados', intToUtf8(charAsciiIndex), 'Nome & ',sep='')
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
					# cat('\\cellcolor{', color ,'} ', sep='')

					# cat(if (SumLogPValue > 1e-10) removeZero(round(SumLogPValue, numlen)) else 0.0, sep='')

					# if (!(f == 'ENG' && s == TRUE))
						# cat(' & ')				
				}
			}
		}
		cat(' \\\\\n')
	}
}

if (plotDensity) {
	alldata <- list()
	alldata$NSMOTE <- NULL
	alldata$YSMOTE <- NULL
	png('AccuracyGeneralAnalysis.png', width=720, height=720)
	layout(matrix(c(1,2,3,3), 2, 2, byrow = TRUE))
	for (s in config.SMOTE_SEQ) {
		accdata <- list()
		accdata$HARF <- NULL
		accdata$AENN <- NULL
		accdata$INFFC <- NULL
		accdata$ENG <- NULL
		i <- 1
		for (r in metadataPathList) {
			metadata <- getMetadata(r)
			for (f in config.NOISEFILTER_SEQ) {
				if (f == 'HARF')
					accdata$HARF <- c(accdata$HARF, metadata[metadata$Filter == f & metadata$SMOTE == s, 'predFiltered'])
				if (f == 'AENN')
					accdata$AENN <- c(accdata$AENN, metadata[metadata$Filter == f & metadata$SMOTE == s, 'predFiltered'])
				if (f == 'INFFC')
					accdata$INFFC <- c(accdata$INFFC, metadata[metadata$Filter == f & metadata$SMOTE == s, 'predFiltered'])
				if (f == 'ENG')
					accdata$ENG <- c(accdata$ENG, metadata[metadata$Filter == f & metadata$SMOTE == s, 'predFiltered'])
			}
		}

		i <- 1
		for (aux in accdata) {
			if (i == 1) {
				plot(density(aux, to=1.0), type='l', col = i, main=paste('Densidade de acurácia', (if(s) 'com SMOTE' else 'sem SMOTE')), 
					ylim=c(0.0, 7.0), ylab='Densidade', xlab='N = 225')
			} else {
				lines(density(aux, to=1.0), type='l', col = i, lty=i)
			}
			text(x = 0.5, y = 6.5 - 0.5 * i, labels = config.NOISEFILTER_SEQ[i],
				col = 1,pos=4,family='Bookman Old Style')
			rect(0.465, 6.469 - 0.5 * i, 0.4995, 6.55 - 0.5 * i, col=i,border=NA)
			i <- i + 1
		}

		if (s == TRUE) {
			alldata$YSMOTE <- c(accdata$HARF, accdata$AENN, accdata$INFFC, accdata$ENG)
		} else {
			alldata$NSMOTE <- c(accdata$HARF, accdata$AENN, accdata$INFFC, accdata$ENG)
		}
	}
	plot(density(alldata$NSMOTE, to=1.0),col='red', main='Impacto do SMOTE', ylab='Densidade', xlab='N = 900', lty=2)
	lines(density(alldata$YSMOTE, to=1.0), col='blue')
	text(x = 0.35, y = 5.25, labels = 'sem SMOTE',
				col = 'black',pos=4,family='Bookman Old Style')
	text(x = 0.35, y = 5.75, labels = 'com SMOTE',
				col = 'black',pos=4,family='Bookman Old Style')
	rect(0.305, 5.175, 0.35, 5.31, col='red', border=NA)
	rect(0.305, 5.7, 0.35, 5.845, col='blue', border=NA)
	dev.off()
}


if (plotNoiseCompare) {
	png(paste('FilterComparison.png'), width=720, height=720)
	par(mfrow=c(2,2))
	for (f in config.NOISEFILTER_SEQ) {
		accFiltered <- vector()
		accCorrupted <- vector()
		for (d in metadataPathList) {
		metadata <- getMetadata(r)
			accFiltered <- c(accFiltered, metadata[metadata$Filter == f, 'predFiltered'])
			accCorrupted <- c(accCorrupted, metadata[metadata$Filter == f, 'predCorrupted'])
		}

		plot(density(accFiltered, to=1.0), type='l', col = 'firebrick2', 
			main=paste('Densidade de acurácia (filtro: ', f, ')'), ylab='Densidade', xlab='N = 450')
		lines(density(accCorrupted, to=1.0), type='l', col = 'navy', lty=2)

	}
	dev.off()
}

sink(NULL)
# ---------------------------------------
# END OF THE SCRIPT
# ---------------------------------------