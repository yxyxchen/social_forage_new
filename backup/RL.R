# this function simulates choices of an agent 
# who ignores social information in the prey selection task 

# inputs:
# beta : learning rate, 0-1
# tau :  temperature parameter in the soft-max function, 0, +inf
# ctxRwd : reward for context modualtion trials 

# outputs:
# tIdxInBlock [nTrialx1 int]: trial index within the block
# tIdxInChunk [nTrialx1 int]: trial index within the chunk
# rwd [nTrialx1 int]: trial-wise rewards
# trialEarnings [nTrialx1 int]: trial-wise payments
# timespent [nTrialx1 int]: time spent on each trial
# reRate  [nTrialx1 real]: trial-wise estimates for the long-run reward rate
# delta [nTrialx1 real]: trial-wise prediction errors
# optimLongRunRate [1x1 real]: optimal long-run reward rate 
# optimThreshold [1x1 real]: optimal threshold for accepting options
# iniThreshold [1x1 real]: initial threshold for accepting options
# iniNLeaveProb [1x1 real]: number of forgone prob rewards initially
# optimNLeaveProb [1x1 real]: number of forgone prob rewards optimally 

RL = function(beta, tau, ctxRwd, rwd_){
  # time constants 
  iti = 2 # iti duration
  ht = 2 # handle duration
 
  # rewards constant
  probRwds = seq(3, 18, by = 3) # possible rewards in prob trials 
  probRwdRates =  probRwds / ht # possible local reward rates in prob trials
  ctxRwdRate = ctxRwd / ht # local reward rate in context modualtion trials
  
  # ctxRwd should not equal any possible reward in prob trials 
  if(any(ctxRwd == probRwds)){
    print("ctxRwd should not equal rewards in prob trials!")
    break
  }
  
  # other constants 
  nProb = length(probRwds) # number of prob trials in a sample cycle 
  nCtx = 6 # number of context modulation trials in a sample cycle 
  nStim = nProb + nCtx # number of trials in a sample cycle 
  nChunk = 10 # number of sample cycles 
  nTrial = nChunk * (nStim) # toatl num of trials in this block
  
################### optim analysis ################
  # calculate the optim long-run reward rates and the optim threshold 
  
  # possible decision thresholds: engage only when the reward rate >= the threshold
  thresholds = sort(c(ctxRwdRate, probRwdRates)) 
  # calculate the long-term reward rate for every possible decision threshold 
  # noticably, long-term reward rates are different from local reward rates
  # whereas local reward rate = reward / ht
  # and long-term reward rates = E[reward] / E[iti + ht]
  longRunRates = sapply(1 : length(thresholds),
                       function(j) {
                         totalReward = sum(probRwds[probRwdRates >=  thresholds[j]]) + 
                           ifelse(ctxRwdRate >= thresholds[j], ctxRwd * nCtx, 0) 
                         totalTime = iti * (nCtx + length(probRwdRates)) + 
                           ifelse(ctxRwdRate >= thresholds[j], ht * nCtx, 0) + 
                           sum(probRwdRates >= thresholds[j]) * ht
                         totalReward / totalTime
                       })
  optimLongRunRate = max(longRunRates)
  optimThreshold = thresholds[which.max(longRunRates)] 
  optimNLeaveProb = sum(round(probRwdRates, 5) < round(optimThreshold, 5))
  ############################# model ################################
  
  # generate the reward sequence
  rwds = c(probRwds, rep(ctxRwd, nCtx)) # all possible rewards 
  chunkNum = rep(1 : nChunk, each = nStim)
  tIdxInBlock_ = 1 : nTrial # trial indexs within this block
  tIdxInChunk_ = rep(1 : nStim, nChunk) # trial indexs within each chunk
  
  # initialize variables 
  reRate = 5.25 # initial estimate of the long-run reward rate
  iniThreshold = min(thresholds[thresholds >= reRate]) # initial threshold given the initial long-run reward rate
  iniNLeaveProb = sum(probRwdRates >= reRate)
    
  timeSpent_ = vector(length = nTrial) # variables to record spent time, if engage = ht otherwise = 0
  reRate_ = vector(length = nTrial) # variable to record reRate
  trialEarnings_ = vector(length = nTrial) # variable to record trialEarnings
  target_ = vector(length = nTrial) # variable to record update targets 
  delta_ = vector(length = nTrial) # variable to record prediction errors
  
  # loop over trials
  for(i in 1 : nTrial){
    rwd = rwd_[i] # current rwd
    
    # make the action
    pAccept = 1 / (1 + exp( ((ht) * reRate - rwd) * tau)) 
    action = ifelse(runif(1) <= pAccept, "accept", "forgo") 
    trialEarnings = ifelse(action == "accept", rwd, 0)
    timeSpent = ifelse(action == "accept", ht, 0)
    
    # update reRate given the self-generated outcome
    # in formal R-learning, we should minus Q here. However, they should cancel out since E(Q) = 0
    # also, we use timeSpent + iti here
    delta  = trialEarnings - (timeSpent + iti) * reRate # we should one sec stop of the task
    reRate = reRate + beta * delta
    
    
    # save
    timeSpent_[i] = timeSpent
    delta_[i] = delta
    reRate_[i] = reRate
    trialEarnings_[i] = trialEarnings
  }
  
  # calculate how responses to prob stims change
  acceptMatrix = matrix(NA, nProb, nChunk)
  for(i in 1 : nProb){
    for(j in 1 : nChunk){
      acceptMatrix[i, j] = timeSpent_[rwd_ == probRwds[i] & chunkNum == j] == ht
    }
  }

  # return outputs
  outputs = list(
    'tIdxInBlock' = tIdxInBlock_,
    'tIdxInChunk' = tIdxInChunk_,
    "rwd" = rwd_,
    "trialEarnings" = trialEarnings_,
    "timeSpent" = timeSpent_,
    "reRate" = reRate_,
    "delta" = delta_,
    "optimLongRunRate" = optimLongRunRate,
    "optimThreshold" = optimThreshold,
    "acceptMatrix" = acceptMatrix,
    "iniThreshold" =   iniThreshold,
    "optimNLeaveProb" = optimNLeaveProb,
    "iniNLeaveProb" = iniNLeaveProb
  )
  
}