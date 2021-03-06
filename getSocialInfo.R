source("RL.R")
source("RLSocial.R")
library("ggplot2")
library("dplyr")
library("tidyr")
source("subFxs/plotThemes.R")

# load expPara
load("expParas.RData")
blockSec = blockSec
nTrialMax = blockSec / iti
nChunkMax = ceiling(nTrialMax / chunkSize)

# output dir 
dir.create("figures")
dir.create("socialInfo")
# simulation parameters
nSub = 16

# non_social
## average reward rates in different environments
richRwdRates = vector(length = nSub)
poorRwdRates = vector(length = nSub)
## total earnings 
totalEarnings = vector(length = nSub)
## simulation paras
betas = vector(length = nSub)
taus = vector(length = nSub)
iniLongRunRates = vector(length = nSub)
## time series of acceptance and trialEarnings
tGrid = head(seq(0, blockSec * 2, by = tGridGap), -1)
nT = length(tGrid) 
acceptMatrix_ = array(NA, dim = c(nUnqHt, nT, nSub))
trialEarningsOnGrid_ = matrix(NA, nrow = nT, ncol = nSub)
set.seed(123)
for(sIdx in 1 : nSub){
  htSeq_ = lapply(1 : nCondition, function(i) {
    condition = conditions[i]
    tempt = as.vector(replicate(nChunkMax, sample(hts_[[condition]], chunkSize)))
    tempt[1 : nTrialMax]
  })
  
  # create the rwd sequences in two conditions
  rwdSeq_ = lapply(1 : nCondition, function(i) {
    tempt = replicate(ceiling(nChunkMax/2), sample(rwds, chunkSize*2))
    tempt = tempt[1 : nTrialMax]
  })
  beta = runif(1, 0.01, 0.03); betas[sIdx] = beta
  tau = runif(1, 10, 15); taus[sIdx] = tau;
  iniLongRunRate = runif(1, 0.02, 0.04); iniLongRunRates[sIdx] = iniLongRunRate
  RLResults = RL(beta, tau, iniLongRunRate, htSeq_, rwdSeq_, blockSec)
  
  # calculate 
  totalEarnings[sIdx] = sum(RLResults$trialEarnings)
  acceptMatrix_[, ,sIdx] = RLResults$acceptMatrixOnGrid
  
  # examine 
  blockTime = RLResults$blockTime
  blockTime[RLResults$condition == "poor"] = blockTime[RLResults$condition == "poor"] + (blockSec) 
  trialEarningsOnGrid = rep(0, length = nT)
  intervalIdxs = findInterval(tGrid, blockTime) # the end boundary should be 1200
  runLens = rle(intervalIdxs)$lengths
  trialEarningsOnGrid[head(cumsum(runLens), -1)] = RLResults$trialEarnings
  trialEarningsOnGrid_[,sIdx] = trialEarningsOnGrid
}
# plot timeseries to check whether simulated participants adapt their behavior sucessfully 
acceptMatrix = apply(acceptMatrix_, MARGIN = c(1,2), FUN = function(x) mean(x, na.rm = T))
plotData = data.frame(t(acceptMatrix)); colnames(plotData) =  paste(unqHts); plotData$time = tGrid
plotData$condition = rep(conditions, each = nT / 2)
plotData %>% gather(key = ht, value = pAccept, -time, -condition) %>%
  mutate(ht = factor(ht, levels = unqHts)) %>%
  ggplot(aes(time, pAccept)) + geom_line(size = 1) + facet_wrap(~ht) +
  myTheme + xlab("Time (s)") + ylab("Percentage of accepted trials(%)") +
  scale_y_continuous(limits = c(-0.1, 1.1), breaks = c(0, 0.5, 1))

# save example and group-average trialEarning timeseries as social information
trialEarningsOnGrid = apply(trialEarningsOnGrid_[,totalEarnings > 70], MARGIN = 1, FUN = function(x) mean(x, na.rm = T))
write.csv(trialEarningsOnGrid, file = "socialInfo/others.csv")
data.frame(time = tGrid, trialEarnings = trialEarningsOnGrid) %>%
  ggplot(aes(time, trialEarnings)) + geom_line()
singleTrialEarningsOnGrid = trialEarningsOnGrid_[,which.max(totalEarnings)]
save("trialEarningsOnGrid", "singleTrialEarningsOnGrid", 
     file = "socialInfo/others.RData")


