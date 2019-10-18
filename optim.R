rwd = 2
iti = 2
conditions = c("rich", "poor")
nCondition = length(conditions)
hts_ = list("rich" = c(16, 12, 8, 1.5, 1.5, 1.5, 1.5), "poor" = c(16, 16, 16, 16, 12, 8, 1.5))
unqHts = c(16, 12, 8, 1.5)
nUnqHt = length(unqHts)
chunkSize = 7

# calculate the optimal longRunRate 
# accpet hts <= threshold
optimLongRunRate_ = list()
optimMaxAcpHt_ = list()
for(i in 1 : nCondition){
  condition = conditions[i]
  hts = hts_[[i]]
  longRunRates = sapply(1 : length(unqHts),
                        function(j) {
                          totalReward = sum(hts <= unqHts[j]) * rwd
                          totalTime = iti * chunkSize + sum(hts[hts <= unqHts[j]])
                          totalReward / totalTime
                        })
  optimLongRunRate_[[condition]] = max(longRunRates)
  optimMaxAcpHt_[[condition]] = max(unqHts[rwd / unqHts >= optimLongRunRate_[[condition]]])
}

# block constants 
blockSec = 600
nTrialMax = blockSec / iti
nChunkMax = ceiling(nTrialMax / chunkSize)

# save 
save("rwd", "iti", "conditions",
     "nCondition", "hts_", "unqHts", "chunkSize",
     "unqHts",
     "nUnqHt",
     "optimLongRunRate_", "optimMaxAcpHt_",
     "blockSec",
     "nTrialMax", "nChunkMax",
     file = "expParas.RData")
