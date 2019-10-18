# this function simulates choices of an agent 
# who has no social information in the prey selection task 

# inputs:
# beta : learning rate, 0-1
# tau :  temperature parameter in the soft-max function, 0, +inf
# ctxRwd : reward for the context modualtion trials 
# rwd_ : reward sequences 

# outputs:
# tIdxInBlock [nTrialx1 int]: trial index within the block
# tIdxInChunk [nTrialx1 int]: trial index within the chunk
# rwd [nTrialx1 int]: trial-wise rewards

# trialEarnings [nTrialx1 int]: trial-wise payments
# requiredHt [nTrialx1 int]: time spent on each trial
# blockTime [nTrialx1 int]: elapsed time at the end of each trial 
# reRate  [nTrialx1 real]: trial-wise estimates for the long-run reward rate
# delta [nTrialx1 real]: trial-wise prediction errors

# iniLongRunRate [1x1 real]: initial estimate for the long-run reward rate
# iniMinAcpRwd [1x1 real]: minimal reward accepted initially 
# iniNLeaveProb [1x1 real]: number of forgone prob rewards initially
# optimLongRunRate [1x1 real]: the optimal long-run reward rate 
# optimNLeaveProb [1x1 real]: number of forgone prob rewards optimally 
# optimMinAcpRwd [1x1 real]: minimal reward accepted under the optimal policy

RL = function(beta, tau, iniLongRunRate, htSeq_){
  # load expParas
  load("expParas.RData")
  
  ############################# model ################################
  # initialize the estimate for the long-run reward rate
  reRate = iniLongRunRate 
  
  # loop over conditions
  for(c in 1 : nCondition){
    condition = conditions[c]
    htSeq = htSeq_[[c]]
    
    # initialize recording variables 
    condition_ = rep(NA, length = nCondition * nTrialMax)
    tIdxInChunk_ = rep(NA, length = nCondition * nTrialMax)
    cIdxInBlock_ = rep(NA, length = nCondition * nTrialMax)
    scheduledHt_ = rep(NA, length = nTrialMax * nCondition) # scheduled ht
    spentHt_ = rep(NA, length = nTrialMax * nCondition) # variables to record spent time, if engage = ht otherwise = 0
    blockTime_ = rep(NA, length = nTrialMax * nCondition)
    trialEarnings_ = rep(NA, length = nTrialMax * nCondition) # variable to record trialEarnings
    reRate_ = rep(NA, length = nTrialMax * nCondition) # variable to record reRate
    delta_ = rep(NA, length = nTrialMax * nCondition) # diagnosis variable to record prediction errors
    
    # loop over trials
    blockTime = 0 # elapsedTime since the beginning of the block
    tIdx = 1
    while(blockTime < blockSec){
      # current ht
      scheduledHt = htSeq[[tIdx]]
      
      # make the action
      pAccept = 1 / (1 + exp( ((scheduledHt) * reRate - rwd) * tau)) 
      action = ifelse(runif(1) <= pAccept, "accept", "forgo") 
      
      # record trialEarnings and spentHt
      trialEarnings = ifelse(action == "accept", rwd, 0)
      spentHt = ifelse(action == "accept", scheduledHt, 0)
      
      # update reRate given the self-generated outcome
      # in formal R-learning, we should minus Q here. However, they should cancel out since E(Q) = 0
      # also, we use requiredHt + iti here
      delta  = trialEarnings - (spentHt + iti) * reRate # we should one sec stop of the task
      reRate = reRate + beta * delta
      
      #update blockTime and trialIndex
      blockTime = blockTime + spentHt + iti
      
      # save variables 
      if(blockTime <= blockSec){
        tIdxInChunk_[tIdx] = ifelse(tIdx %% chunkSize == 0, chunkSize, tIdx %% chunkSize)
        cIdxInBlock_[tIdx] = ceiling(tIdx / chunkSize)
        condition_[tIdx] = condition
        scheduledHt_[tIdx] = scheduledHt
        spentHt_[tIdx] = spentHt
        blockTime_[tIdx] = blockTime
        trialEarnings_[tIdx] = trialEarnings
        reRate_[tIdx] = reRate
        delta_[tIdx] = delta 
      }
      # update trial index
      tIdx = tIdx + 1
    }
    # truncate and save
    tempt = data.frame(
      "condition" = condition_,
      'tIdxInChunk' = tIdxInChunk_,
      'cIdxInBlock' = cIdxInBlock_,
      'scheduledHt' = scheduledHt_,
      'spentHt' = spentHt_,
      'blockTime' = blockTime_,
      'trialEarnings' = trialEarnings_,
      'reRate' = reRate_,
      'delta' = delta_
    )
    nTrial = sum(!is.na(condition_), na.rm = T)
    tempt = tempt[1 : nTrial,]
    if(condition == "rich"){
      richOutputs = tempt
    }else{
      poorOutputs = tempt
    }
 }
  
 # combine data from the two conditions
 junk = rbind(richOutputs, poorOutputs)
 junk$ckIdxInTask = junk$cIdxInBlock
 junk$ckIdxInTask[junk$condition == conditions[2]] =  junk$ckIdxInTask[junk$condition == conditions[2]] + 
   tail(junk$ckIdxInTask[junk$condition == conditions[1]], 1)
  
 # count chunks and trials
 nChunk = length(unique(junk$ckIdxInTask))
 nTrial = nrow(junk)
   
  # calculate how responses to prob stims change
  acceptMatrix = matrix(NA, nUnqHt, nChunk)
  for(i in 1 : nUnqHt){                                                                                                                                                                                                                                                                                                                                                                                                                                              
    for(j in 1 : nChunk){
      if(sum(junk$scheduledHt == unqHts[i] & junk$ckIdxInTask == j) != 0){
        acceptMatrix[i, j] = mean(junk$trialEarnings[junk$scheduledHt == unqHts[i] & junk$ckIdxInTask == j] == rwd)
      }else{
        acceptMatrix[i, j] = NA
      }
    }                                               
  }
                                                                                                                                 
  # map reRate and acceptMatrix to a standard time grid                                    
  tGrid = seq(0, blockSec, by = 2)  
  nT = length(tGrid)
  reRateOnGrid = vector(length = nT)
  for(i in 1 : nT){
    t = tGrid[i]
    if(t >= min(blockTime_[tIdxInBlock_ <= nTrial])){
      reRateOnGrid[i] = reRate_[max(which(blockTime_ <= t & tIdxInBlock_ <= nTrial))]
    }else{
      reRateOnGrid[i] = iniLongRunRate
    }
  }
  acceptMatrixOnGrid = matrix(NA, nrow = nProb, ncol = nT)
  endOfBlockTimes = blockTime_[tIdxInChunk_ == chunkSize & tIdxInBlock_ <= nTrial]
  for(i in 1 : nT){
    t = tGrid[i]
    if(t <= max(endOfBlockTimes)){
      acceptMatrixOnGrid[,i] = acceptMatrix[,min(which(endOfBlockTimes >= t))]
    }else{
      acceptMatrixOnGrid[,i] = acceptMatrix[,nChunk]
    }
  }
  
  # map reRate and acceptMatrix to a standard time grid                                    
  tGrid = seq(0, blockSec, by = 2)  
  nT = length(tGrid)
  reRateOnGrid = vector(length = nT)
  for(i in 1 : nT){
    t = tGrid[i]
    if(t >= min(blockTime_[tIdxInBlock_ <= nTrial])){
      reRateOnGrid[i] = reRate_[max(which(blockTime_ <= t & tIdxInBlock_ <= nTrial))]
    }else{
      reRateOnGrid[i] = iniLongRunRate
    }
  }
  acceptMatrixOnGrid = matrix(NA, nrow = nProb, ncol = nT)
  endOfBlockTimes = blockTime_[tIdxInChunk_ == nStim & tIdxInBlock_ <= nTrial]
  for(i in 1 : nT){
    t = tGrid[i]
    if(t <= max(endOfBlockTimes)){
      acceptMatrixOnGrid[,i] = acceptMatrix[,min(which(endOfBlockTimes >= t))]
    }else{
      acceptMatrixOnGrid[,i] = acceptMatrix[,nChunk]
    }
  }
  
  # map reRate and acceptMatrix to a standard time grid                                    
  tGrid = seq(0, blockSec * nCondition, by = 2)  
  nT = length(tGrid)
  reRateOnGrid = vector(length = nT)
  for(i in 1 : nT){
    t = tGrid[i]
    if(t >= min(blockTime_[tIdxInBlock_ <= nTrial])){
      reRateOnGrid[i] = reRate_[max(which(blockTime_ <= t & tIdxInBlock_ <= nTrial))]
    }else{
      reRateOnGrid[i] = iniLongRunRate
    }
  }
  acceptMatrixOnGrid = matrix(NA, nrow = nProb, ncol = nT)
  endOfBlockTimes = blockTime_[tIdxInChunk_ == nStim & tIdxInBlock_ <= nTrial]
  for(i in 1 : nT){
    t = tGrid[i]
    if(t <= max(endOfBlockTimes)){
      acceptMatrixOnGrid[,i] = acceptMatrix[,min(which(endOfBlockTimes >= t))]
    }else{
      acceptMatrixOnGrid[,i] = acceptMatrix[,nChunk]
    }
  }
  
  # return outputs
  outputs = list(
    "iniLongRunRate" =   iniLongRunRate,
    "iniMinAcpRwd" = iniMinAcpRwd,
    "iniNLeaveProb" = iniNLeaveProb,
    "acceptMatrix" = acceptMatrix,
    "acceptMatrixOnGrid" = acceptMatrixOnGrid,
    "reRateOnGrid" = reRateOnGrid 
  )
  outputs = c(junk, outputs)
  return(outputs)
}
