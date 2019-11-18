social_nonSocial = function(){
  source("RL.R")
  source("RLSocial.R")
  library("ggplot2")
  library("dplyr")
  library("tidyr")
  source("subFxs/plotThemes.R")
  
  # output dir 
  dir.create("figures")
  
  # simulation parameters
  nSub = 32
  
  # non_social
  tGrid = c(seq(0, blockSec, by = tGridGap), seq(0, blockSec, by = tGridGap) + blockSec)
  nT = length(tGrid) 
  acceptMatrix_ = array(NA, dim = c(nUnqHt,  nT, nSub))
  for(sIdx in 1 : nSub){
    htSeq_ = lapply(1 : nCondition, function(i) {
      condition = conditions[i]
      tempt = as.vector(replicate(nChunkMax, sample(hts_[[condition]], chunkSize)))
      tempt[1 : nTrialMax]
    })
    rwds_ = c(rep(0.5, chunkSize), rep(3.5, chunkSize))
    rwdSeq_ = lapply(1 : nCondition, function(i) {
      condition = conditions[i]
      tempt = as.vector(replicate(ceiling(nChunkMax / 2), sample(rwds_, chunkSize * 2)))
      tempt[1 : nTrialMax]
    })
    beta = runif(1, 0.001, 0.01)
    tau = runif(1, 10, 15)
    iniLongRunRate = runif(1, 0.12, 0.2)
    RLResults = RL(beta, tau, iniLongRunRate, htSeq_, rwdSeq_)
    acceptMatrix_[, ,sIdx] = RLResults$acceptMatrixOnGrid
  }
  acceptMatrix = apply(acceptMatrix_, MARGIN = c(1,2), FUN = function(x) mean(x, na.rm = T))
  plotData = data.frame(t(acceptMatrix)); colnames(plotData) =  paste(unqHts); plotData$time = tGrid
  plotData$condition = rep(conditions, each = nT / 2)
  nonSocialData = plotData
  
  
  # social
  tGrid = c(seq(0, blockSec, by = tGridGap), seq(0, blockSec, by = tGridGap) + blockSec)
  nT = length(tGrid)
  acceptMatrix_ = array(NA, dim = c(nUnqHt,  nT, nSub))
  for(sIdx in 1 : nSub){
    htSeq_ = lapply(1 : nCondition, function(i) {
      condition = conditions[i]
      tempt = as.vector(replicate(nChunkMax, sample(hts_[[condition]], chunkSize)))
      tempt[1 : nTrialMax]
    })
    rwds_ = c(rep(0.5, chunkSize), rep(3.5, chunkSize))
    rwdSeq_ = lapply(1 : nCondition, function(i) {
      condition = conditions[i]
      tempt = as.vector(replicate(ceiling(nChunkMax / 2), sample(rwds_, chunkSize * 2)))
      tempt[1 : nTrialMax]
    })
    beta_self = runif(1, 0.001, 0.01)
    beta_other = beta_self * 2
    tau = runif(1, 10, 15)
    iniLongRunRate = runif(1, 0.12, 0.2)
    RLResults = RLSocial(beta_self, beta_other, tau, iniLongRunRate, htSeq_, rwdSeq_)
    acceptMatrix_[, ,sIdx] = RLResults$acceptMatrixOnGrid
  }
  acceptMatrix = apply(acceptMatrix_, MARGIN = c(1,2), FUN = function(x) mean(x, na.rm = T))
  plotData = data.frame(t(acceptMatrix)); colnames(plotData) =  paste(unqHts); plotData$time = tGrid
  plotData$condition = rep(conditions, each = nT / 2)
  socialData = plotData
  
  # combine data from two conditions 
  aggData = rbind(gather(socialData, key = ht, value = pAccept, -time, -condition),
                  gather(nonSocialData, key = ht, value = pAccept, -time, -condition))
  aggData$social = factor(rep(c("social", "non_social"), each = nT * nUnqHt),
                             levels = c("social", "non_social"))
  
  # calculate the ideal behavior, assuming there is noise in actions 
  meanTau = 12.5
  # aggData$optimPAccept = 1 / (1 + exp(meanTau * (as.numeric(aggData$ht) * as.numeric(optimLongRunRate_[aggData$condition]) - rwd)))
  aggData$optimPAccept = ifelse(as.numeric(aggData$ht) <= as.numeric(optimMaxAcpHt_[aggData$condition]), 1, 0)
  
  
  aggData %>% mutate(ht = factor(ht, levels = unqHts)) %>%
    ggplot(aes(time, pAccept, color = social)) + geom_line(size = 1) +
    scale_color_manual(values = c("red", "black")) + facet_wrap(~ht) + 
    geom_line(aes(time, optimPAccept), color = "#353535", linetype = "dashed") + 
    myTheme + xlab("Time (s)") + ylab("Percentage of accepted trials(%)") +
    scale_y_continuous(limits = c(-0.1, 1.1), breaks = c(0, 0.5, 1))
 
  # plot differences in reward rates 
}


