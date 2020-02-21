rwd = 2
highRwd = 3
lowRwd = 1

iti = 11 * 0.70 # travel time 
conditions = c("rich", "poor")
nCondition = length(conditions)
hts_ = list("rich" = c(40, 28, 22, 2.75, 2.75, 2.75, 2.75) * 0.70 , "poor" = c(40, 28, 28, 28, 28, 22, 2.75) * 0.70)
rwds = c(rep(highRwd, chunkSize), rep(lowRwd, chunkSize))
unqHts = sort(unique(hts_$rich))
nUnqHt = length(unqHts)
chunkSize = length(hts_$rich)
nHt = length(hts_[['rich']])

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
  optimMaxAcpHt_[[condition]] = max(unqHts[rwd / unqHts > optimLongRunRate_[[condition]]])
}
optimMaxAcpHt_
rwd / unlist(optimLongRunRate_)

# calculate net rewards
netValuesRich = (highRwd + lowRwd) / 2 - unqHts *  optimLongRunRate_$rich
netValuesPoor = (highRwd + lowRwd) / 2 - unqHts *  optimLongRunRate_$poor

plot(1 / abs(c(netValuesRich, netValuesPoor)))

# block constants 
blockSec = 20 * 60
nTrialMax = blockSec / iti
nChunkMax = ceiling(nTrialMax / chunkSize)


tGridGap = 1
# save 
save("rwd", "iti", "conditions", "highRwd", "lowRwd",
     "rwds",
     "nCondition", "hts_", "unqHts", "chunkSize",
     "unqHts",
     "nUnqHt",
     "optimLongRunRate_", "optimMaxAcpHt_",
     "blockSec",
     "nTrialMax", "nChunkMax",
     "tGridGap",
     "nHt",
     "netValuesRich",
     "netValuesPoor",
     file = "expParas.RData")

optimMaxAcpHt_

