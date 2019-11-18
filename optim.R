rwd = 2

iti = 4 # travel time 
conditions = c("rich", "poor")
nCondition = length(conditions)
hts_ = list("rich" = c(18, 13, 10, 2, 2, 2, 2), "poor" = c(18, 18, 18, 18, 13, 10, 2))
unqHts = unique(hts_$rich)
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


tGridGap = 1
# save 
save("rwd", "iti", "conditions",
     "nCondition", "hts_", "unqHts", "chunkSize",
     "unqHts",
     "nUnqHt",
     "optimLongRunRate_", "optimMaxAcpHt_",
     "blockSec",
     "nTrialMax", "nChunkMax",
     "tGridGap",
     file = "expParas.RData")

optimMaxAcpHt_