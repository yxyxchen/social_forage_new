source("social_nonSocial.R")
library("dplyr")
library("tidyr")

# load expPara
load("expParas.RData")

# create the ht sequences in two conditions
htSeq_ = lapply(1 : nCondition, function(i) {
  condition = conditions[i]
  tempt = as.vector(replicate(nChunkMax, sample(hts_[[condition]], chunkSize)))
  tempt[1 : nTrialMax]
})
  

# shuffle all possive rewards to generate the reward sequence
# call the function
social_nonSocial(htSeq_)


