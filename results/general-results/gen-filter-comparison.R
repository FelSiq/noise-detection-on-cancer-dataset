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

png('figures/filter-comparison-density-plot.png', width=720, height=720)
par(mfrow=c(2,2))

# SBC PATHLIST
metadataPathList <- c(
	'0-result-RNA-Seq/CHOL.RNA-Seq.dat',
	"0-result-RNA-Seq/KICH.RNA-Seq.dat",
	"0-result-RNA-Seq/LUAD.RNA-Seq.dat",
	"1-result-micro-RNA/BRCA.micro-RNA.dat",
	"1-result-micro-RNA/LUAD.micro-RNA.dat", 
	"1-result-micro-RNA/THCA.micro-RNA.dat",
	"2-result-microarray/adrenal_dahia.microarray.dat",
	"2-result-microarray/lymphoma_shipp.microarray.dat", 
	"2-result-microarray/prostate_singh.microarray.dat")
# END OF SBC CONFIG

for (f in config.NOISEFILTER_SEQ) {
	accFiltered <- vector()
	accCorrupted <- vector()
	for (d in metadataPathList) {
		# DEBUG cat
		# cat(f, length(accFiltered), length(accCorrupted), d, '\n', sep='\t')
		metadata <- getMetadata(d)
		accFiltered <- c(accFiltered, metadata[metadata$Filter == f & metadata$SMOTE == F, 'predFiltered'])
		accCorrupted <- c(accCorrupted, metadata[metadata$Filter == f & metadata$SMOTE == F, 'predCorrupted'])
	}

	# DEBUG cat
	# cat(length(accFiltered), length(accCorrupted), '\n', sep='\t')
	cat(mean(accFiltered), mean(accCorrupted), median(accFiltered), median(accCorrupted), '\n', sep='\t')

	plot(density(accFiltered, from=0.5, to=1.0), type='l', col = 'firebrick2', 
		main=paste('Densidade de acurácia (filtro: ', f, ')', sep=''), ylab='Densidade', xlab='N = 135')
	lines(density(accCorrupted, from=0.5, to=1.0), type='l', col = 'navy', lty=2)

	yMax = par('usr')[4]

	lines(c(0.55, 0.625), c(yMax, yMax) * 0.9, type='l', col='firebrick2')
	lines(c(0.55, 0.625), c(yMax, yMax) * 0.8, type='l', col='navy', lty=2)

	text(labels='Filtrada', x=0.635, y= yMax * 0.9, pos=4) 
	text(labels='Corrompida', x=0.635, y= yMax * 0.8, pos=4) 
}

dev.off()
