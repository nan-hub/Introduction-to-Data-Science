# autoSim.R
rep <- 50
seed <- 203
distTypes = c("gaussian", "t1", "t5")
nVals <- seq(100, 500, by=100)
for (n in nVals) {
  for (dist in distTypes) {
    oFile <- paste("n", n, dist, ".txt", sep="")
    arg <- paste(" n=", n, " dist=", shQuote(shQuote(dist)), 
               " seed=", seed," rep=", rep, sep="")
    sysCall <- paste("nohup Rscript runSim.R", arg, " > ", oFile, sep="")
    system(sysCall, wait = FALSE)
    print(paste("sysCall=", sysCall, sep=""))
  }
}