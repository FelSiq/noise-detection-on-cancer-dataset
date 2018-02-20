# ---------------------------------------
# INFORMATION
# ---------------------------------------
# Use Fisher's method to combine obtained P-values from the experiment.
# ---------------------------------------

source('res-config.R')
# ---------------------------------------
# GENERATE DENSITY PLOT TO EVALUE NOISE REMOVAL EFFICIENCY
# ---------------------------------------
alldata <- list()
alldata$NSMOTE <- NULL
alldata$YSMOTE <- NULL
png('./figures/accuracy-general-analysis.png', width=720, height=720)
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