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

png(paste('figures/filter-comparison-density-plot.png'), width=720, height=720)
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