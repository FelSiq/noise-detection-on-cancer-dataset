# ---------------------------------------------------
# INFORMATION
# ---------------------------------------------------
# This piece of code should start up all necessary stuff for a
# parallel environment.
# ---------------------------------------------------

if (config.PARALLEL_SETUP) {
	par.cluster <- makeCluster(config.NO_CORES, FORK = TRUE)
	clusterSetRNGStream(cl = par.cluster, config.RANDOM_SEED) 
} else {
	set.seed(config.RANDOM_SEED)
}