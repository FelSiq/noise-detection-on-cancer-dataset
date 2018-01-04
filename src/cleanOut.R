# ---------------------------------------------------
# INFORMATION
# ---------------------------------------------------
# This auxiliary piece of code should be called at the very
# end of the 'main.R' code, where all memory used should be 
# freed and the parallel environment and output sinking both
# stopped.
# ---------------------------------------------------

# On './src/config.R', a output file is specified to append all results.
# The empty sink() call make the future outputs goes into the 'stdout' again.
sink(NULL)

if (config.PARALLEL_SETUP) {
	stopCluster(par.cluster)
}

rm(list = ls())
gc()