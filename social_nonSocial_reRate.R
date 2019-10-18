social_nonSocial = function(beta, beta_self, beta_other, tau, ctxRwd, rwd_){
  source("RL.R")
  source("RLSocial.R")
  library("ggplot2")
  library("dplyr")
  library("tidyr")
  source("subFxs/plotThemes.R")
  
  # output dir 
  dir.create("figures")
  
  # read in parameters 
  para = read.csv("para.csv")
  
  # constants 
  # time constants 
  iti = 2 # iti duration
  ht = 2 # handle duration
  probRwds = seq(3, 18, by = 3) # possible rewards in prob trials 
  nProb = length(probRwds) # number of prob trials in a sample cycle 
  
  # rewards constant
  probRwds = seq(3, 18, by = 3) # possible rewards in prob trials 
  probRwdRates =  probRwds / ht # possible local reward rates in prob trials
  nProb = length(probRwds) # number of prob trials in a sample cycle 
  nCtx = 6 # number of context modulation trials in a sample cycle 
  nStim = nProb + nCtx # number of trials in a sample cycle 
  nChunk = 10 # number of sample cycles 
  nTrial = nChunk * (nStim) # toatl num of trials in this block
  tIdxInBlock_ = 1 : nTrial # trial indexs within this block
  tIdxInChunk_ = rep(1 : nStim, nChunk) # trial indexs within each chunk
  
  
  # simulation parameters
  nBlock = 20
  
 # non-social
  reRateNonSocial = rep(0, 120)
  for(bkIdx in 1 : nBlock){
    beta = sample(para$phi, 1)
    tau = sample(para$tau, 1)
    RLResults = RL(beta, tau, ctxRwd, rwd_)
    reRateNonSocial = reRateNonSocial + RLResults$reRate
  }
  reRateNonSocial = reRateNonSocial / nBlock
  plot(reRateNonSocial)
  
  
  # social RL
  reRateSocial = rep(0, 120)
  for(bkIdx in 1 : nBlock){
    beta_self = sample(para$phi, 1)
    beta_other = beta_self
    tau = sample(para$tau, 1)
    RLResults = RLSocial(beta_self, beta_other, tau, ctxRwd, rwd_ )
    reRateSocial = reRateSocial  + RLResults$reRate
  }
  # reRate
  reRateSocial = reRateSocial / nBlock
  plot(reRateSocial) 
  
  
  # 
  probColors = c(
    "#034e7b",
    "#3690c0",
    "#fee090",
    "#fe9929",
    "#e31a1c",
    "#b10026"
  )
  aggData = data.frame(reRate = c(reRateNonSocial, reRateSocial), trialNum = rep(1 : 120,  2))
  aggData$condition = rep(c("no_social", "social"), each = 120)
  
  aggData %>%
    ggplot(aes(trialNum, reRate,
               linetype = condition)) + geom_line(size = 1.5) +
    myTheme 
  # plot differences in reward rates 
}


