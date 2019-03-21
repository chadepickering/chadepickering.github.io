nVals <- seq(100, 500, by=100)
dist <- c("gaussian", "t1", "t5")
for (i in 1:3) {
  for (n in nVals) {
    oFile <- paste(dist[i], '_n', n, '.txt', sep='')
    arg <- paste('n=', n, sep='')
    sysCall <- paste('nohup Rscript runSim.R seed=280 ', arg, ' dist="', 
                     dist[i], '" rep=50', " > ", oFile, sep='')
    system(sysCall, ignore.stderr = TRUE)
    print(sysCall)
  }
}