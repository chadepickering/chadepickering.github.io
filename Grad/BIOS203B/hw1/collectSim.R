library(knitr)
nVals <- seq(100, 500, by=100)
dist <- c("gaussian", "t1", "t5")
filenames <- c()
for (i in 1:3) {
  for (n in nVals) {
    oFile <- paste(dist[i], '_n', n, '.txt', sep='')
    filenames <- append(filenames, oFile)
  }
}

means_raw <- lapply(filenames, function(x) read.table(x, header=FALSE))
means_df_raw <- do.call("rbind", means_raw) 
means_df_raw <- means_df_raw[ ,-1]
transpose_means <- t(means_df_raw)
gaussian_col <- c(transpose_means[ ,1:5])
t1_col <- c(transpose_means[ ,6:10])
t5_col <- c(transpose_means[ ,11:15])
final_df <- as.data.frame(cbind(t1_col, t5_col, gaussian_col))
final_df$n <- rep(nVals, each=2)
final_df$Method <- rep(c("PrimeAvg", "SampleAvg"), by=5)
final_df <- final_df[ ,c(4, 5, 1, 2, 3)]
colnames(final_df) <- c("$n$", "Method", "$t_1$", "$t_5$", "Gaussian")
