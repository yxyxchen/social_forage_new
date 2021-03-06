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
  tGrid = seq(0, blockSec * 2, by = tGridGap)
  nT = length(tGrid) 
  acceptMatrix_ = array(NA, dim = c(nUnqHt,  nT, nSub))
  for(sIdx in 1 : nSub){
    htSeq_ = lapply(1 : nCondition, function(i) {
      condition = conditions[i]
      tempt = as.vector(replicate(nChunkMax, sample(hts_[[condition]], chunkSize)))
      tempt[1 : nTrialMax]
    })
    rwdSeq_ = lapply(1 : nCondition, function(i) {
      tempt = replicate(nTrialMax, ifelse((rnorm(1) < 0 & rnorm(1) < 0), lowRwd, highRwd))
    })
    beta = runif(1, 0.001, 0.01)
    tau = runif(1, 10, 15)
    iniLongRunRate = runif(1, 0.12, 0.2)
    RLResults = RL(beta, tau, iniLongRunRate, htSeq_, rwdSeq_)
    acceptMatrix_[, ,sIdx] = RLResults$acceptMatrixOnGrid
  }
  acceptMatrix = apply(acceptMatrix_, MARGIN = c(1,2), FUN = function(x) mean(x, na.rm = T))
  plotData = data.frame(t(acceptMatrix)); colnames(plotData) =  paste(unqHts); plotData$time = tGrid
  plotData$condition = rep(conditions, c(floor(nT / 2), ceiling(nT / 2)))
  nonSocialData = plotData
  
  
  # social
  tGrid = seq(0, blockSec * 2, by = tGridGap)
  nT = length(tGrid)
  acceptMatrix_ = array(NA, dim = c(nUnqHt,  nT, nSub))
  for(sIdx in 1 : nSub){
    htSeq_ = lapply(1 : nCondition, function(i) {
      condition = conditions[i]
      tempt = as.vector(replicate(nChunkMax, sample(hts_[[condition]], chunkSize)))
      tempt[1 : nTrialMax]
    })
    rwdSeq_ = lapply(1 : nCondition, function(i) {
      tempt = replicate(nTrialMax, ifelse((rnorm(1) < 0 & rnorm(1) < 0), lowRwd, highRwd))
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
  plotData$condition = rep(conditions, c(floor(nT / 2), ceiling(nT / 2)))
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
    ggplot(aes(time, pAccept, color = social)) + 
    geom_rect(mapping=aes(xmin=0, xmax=blockSec, ymin=0, ymax=1), color=NA, fill= "#9ecae1", alpha=0.05) +
    geom_rect(mapping=aes(xmin=blockSec, xmax=blockSec * 2, ymin=0, ymax=1), color=NA, fill= "#ffeda0", alpha=0.05) +
    geom_line(size = 1) +
    scale_color_manual(values = c("red", "black")) + facet_grid(~ht) + 
    geom_line(aes(time, optimPAccept), color = "#353535", linetype = "dashed") + 
    myTheme + xlab("Task time (s)") + ylab("Acceptance (%)") + 
    scale_y_continuous(limits = c(-0.1, 1.1), breaks = c(0, 0.5, 1)) +
    scale_x_continuous(breaks = c(0, blockSec, 2 * blockSec)) +
    theme(legend.title=element_blank())
 
  # plot differences in reward rates 
}


