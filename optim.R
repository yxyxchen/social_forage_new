rwd = 2 * 0.8
highRwd = 2
lowRwd = 0

iti = 11 # travel time 
conditions = c("rich", "poor")
nCondition = length(conditions)
hts_ = list("rich" = c(40, 25, 22, 2, 2, 2, 2), "poor" = c(40, 25, 25, 25, 25, 22, 2))
unqHts = sort(unique(hts_$rich))
nUnqHt = length(unqHts)
chunkSize = length(hts_$rich)

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
optimMaxAcpHt_


# block constants 
blockSec = 600
nTrialMax = blockSec / iti
nChunkMax = ceiling(nTrialMax / chunkSize)


tGridGap = 1
# save 
save("rwd", "iti", "conditions", "highRwd", "lowRwd",
     "nCondition", "hts_", "unqHts", "chunkSize",
     "unqHts",
     "nUnqHt",
     "optimLongRunRate_", "optimMaxAcpHt_",
     "blockSec",
     "nTrialMax", "nChunkMax",
     "tGridGap",
     file = "expParas.RData")

optimMaxAcpHt_