source("RL.R")
source("RLSocial.R")
library("ggplot2")
library("dplyr")
library("tidyr")
source("subFxs/plotThemes.R")

# load expPara
load("expParas.RData")

# create the ht sequences in two conditions
htSeq_ = lapply(1 : nCondition, function(i) {
  condition = conditions[i]
  tempt = as.vector(replicate(nChunkMax, sample(hts_[[condition]], chunkSize)))
  tempt[1 : nTrialMax]
})


# output dir 
dir.create("figures")

# simulation parameters
nSub = 32

# non_social
richRwdRates = vector(length = nSub)
poorRwdRates = vector(length = nSub)
for(sIdx in 1 : nSub){
  beta = runif(1, 0.001, 0.01)
  tau = runif(1, 2, 10)
  iniLongRunRate = runif(1, 0.15, 1)
  RLResults = RL(beta, tau, iniLongRunRate, htSeq_)
  richRwdRates[sIdx] = sum(RLResults$trialEarnings[RLResults$condition == "rich"]) / blockSec
  poorRwdRates[sIdx] = sum(RLResults$trialEarnings[RLResults$condition == "poor"]) / blockSec
}

hist(poorRwdRates)
hist(richRwdRates)

