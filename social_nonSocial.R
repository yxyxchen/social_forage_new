social_nonSocial = function(htSeq_){
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
  
  # block constants 
  tIdxInBlock_ = 1 : nTrialMax # trial indexs within this block
  tIdxInChunk_ = rep(1 : nStim, nChunkMax) # trial indexs within each chunk
  tIdxInChunk_ = tIdxInChunk_[1 : nTrialMax]
  
  
  # simulation parameters
  nBlock = 32
  
  # non_social
  tGrid = seq(0, blockSec, by = 2)  
  nT = length(tGrid)
  acceptMatrix_ = array(NA, dim = c(nProb,  nT, nBlock))
  for(bkIdx in 1 : nBlock){
    beta = sample(para$phi, 1)
    tau = runif(1, 10, 15)
    iniLongRunRate = runif(1, 3 / ht, 18 / ht)
    RLResults = RL(beta, tau, iniLongRunRate, ctxRwd, rwd_)
    acceptMatrix_[, ,bkIdx] = RLResults$acceptMatrixOnGrid
  }
  acceptMatrix = apply(acceptMatrix_, MARGIN = c(1,2), FUN = function(x) mean(x, rm.na = T))
  plotData = data.frame(t(acceptMatrix)); colnames(plotData) =  paste(probRwds); plotData$time = tGrid
  nonSocialData = plotData
  
  
  # social
  tGrid = seq(0, blockSec, by = 2)  
  nT = length(tGrid)
  acceptMatrix_ = array(NA, dim = c(nProb,  nT, nBlock))
  for(bkIdx in 1 : nBlock){
    beta_self = sample(para$phi, 1)
    beta_other = beta_self
    tau = runif(1, 10, 15)
    iniLongRunRate = runif(1, 3 / ht, 18 / ht)
    RLResults = RLSocial(beta_self, beta_other, tau, iniLongRunRate, ctxRwd, rwd_)
    acceptMatrix_[, ,bkIdx] = RLResults$acceptMatrixOnGrid
  }
  acceptMatrix = apply(acceptMatrix_, MARGIN = c(1,2), FUN = function(x) mean(x, rm.na = T))
  plotData = data.frame(t(acceptMatrix)); colnames(plotData) =  paste(probRwds); plotData$time = tGrid
  socialData = plotData
  
  # 
  aggData = rbind(gather(socialData, key = rwd, value = pAccept, -time),
                  gather(nonSocialData, key = rwd, value = pAccept, -time))
  aggData$condition = factor(rep(c("social", "non_social"), each = nT * nProb),
                             levels = c("social", "non_social"))
  # calculate the ideal behavior, assuming there is noise in actions 
  medianTau = median(para$tau)
  aggData$optimPAccept = rep(rep(1 / (1 + exp(medianTau * (RLResults$optimLongRunRate * ht - probRwds))),
                             each = nT),2)
  
  aggData %>% mutate(rwd = factor(rwd, levels = probRwds)) %>%
    ggplot(aes(time, pAccept, color = condition)) + geom_line(size = 1) +
    scale_color_manual(values = c("red", "black")) + facet_wrap(~rwd) + 
    geom_line(aes(time, optimPAccept), color = "#353535", linetype = "dashed") + 
    myTheme + xlab("Time (s)") + ylab("Percentage of accepted trials(%)") +
    scale_y_continuous(limits = c(-0.1, 1.1), breaks = c(0, 0.5, 1))
 # plot differences in reward rates 
}


