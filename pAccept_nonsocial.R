source("RL.R")
source("RLSocial.R")
library("ggplot2")
library("dplyr")
library("tidyr")
library("lme4")
source("subFxs/plotThemes.R")
library("data.table")
# load expParas
load("expParas.RData")

# create the ht sequences in two conditions
# create the rwd sequences in two conditions
rwds_ = c(rep(3.5, chunkSize), rep(0.5, chunkSize))


# simulation
dir.create("figures")
# simulate non_social data
nSub = 32
RLResults_ = list(length = nSub)
for(sIdx in 1 : nSub){
  htSeq_ = lapply(1 : nCondition, function(i) {
    condition = conditions[i]
    tempt = as.vector(replicate(nChunkMax, sample(hts_[[condition]], chunkSize)))
    tempt[1 : nTrialMax]
  })
  rwdSeq_ = lapply(1 : nCondition, function(i) {
    condition = conditions[i]
    tempt = as.vector(replicate(ceiling(nChunkMax / 2), sample(rwds_, chunkSize * 2)))
    tempt[1 : nTrialMax]
  })
  beta = runif(1, 0.005, 0.01)
  tau = runif(1, 3, 15)
  iniLongRunRate = runif(1, 0.15, 1)
  RLResults = RL(beta, tau, iniLongRunRate, htSeq_, rwdSeq_)
  RLResults_[[sIdx]] =  RLResults 
}


# concatenate 
dfList = lapply(1 : nSub, function(i){
  RLResults = RLResults_[[i]]
  action = ifelse(RLResults$spentHt > 0, 1, 0)
  pastEarnings1 = c(NA, head(RLResults$trialEarnings, -1))
  pastHt1 = c(NA, head(RLResults$spentHt, -1) + iti)
  
  # 
  spentTime = RLResults$spentHt + iti # assume reward occurs at the end of the feedback period
  spentTime4LastRwd = c(NA, head(ave(spentTime, cumsum(spentTime != 4), FUN = cumsum), -1))
  preSpentTime = c(NA, head(spentTime, -1))
  
  # local reward fluctuation 
  localTime = 20 # after 10s, the previous belief will be multipled by (1 - beta) ^ 10, and the new reward sequences will be added 
  rewardUpdates = sapply(1 : length(action), function(i){
    if(sum(spentTime[1:i]) < localTime){
      return(NA)
    }else{
      timeTrace = c(tail(rev(cumsum(rev(spentTime[1:i]))), -1), 0) 
      stopIdx = min(which(timeTrace <= localTime)) 
      sum(RLResults$trialEarnings[stopIdx : i] * ((1 - beta) ^ timeTrace[stopIdx : i]))
    }
  })
  
  data = data.frame(
    subId = rep(i, length = length(action)),
    ht = RLResults$scheduledHt,
    condition = RLResults$condition,
    trialEarnings = RLResults$trialEarnings,
    pastEarninings = pastEarnings1,
    action = action,
    preAction  = c(NA, head(action, -1)),
    pastRwdRate = pastEarnings1 / pastHt1,
    rewardUpdate = rewardUpdates,
    preSpentTime = preSpentTime,
    spentTime4LastRwd = spentTime4LastRwd
  )

})
data = bind_rows(dfList)
data = data[apply(data, MARGIN = 1, FUN = function(x) all(!is.na(x))),]
data$rewardUpdateBin = cut(data$rewardUpdate, breaks = seq(-0.5, 8.5, by = 0.5), labels = seq(0:17))

# plot the effect of reward sizes and environments 
# make sure to average within each participant first. And use se across participants 
data %>% group_by(ht, subId, condition) %>% summarise(pAccept = sum(action) / length(action)) %>%
  group_by(ht, condition) %>% summarise(mu = mean(pAccept),
                             se = sd(pAccept) / sqrt(length(pAccept)),
                             min = mu - se,
                             max = mu + se) %>%
  ggplot(aes(as.factor(ht), mu, fill = condition)) + geom_bar(stat = "identity", position = "dodge") + myTheme +
  geom_errorbar(aes(ymin = min, ymax = max, width = 0.3), position = position_dodge(width=0.9)) +
  xlab("Handling time (s)") + ylab("Acceptance (%)")

# plot the effect of trial earnings 
data %>% dplyr::filter((data$ht > 2) & (data$preAction == 1)) %>% 
  group_by(pastEarninings, subId) %>%
  summarise(pAccept = sum(action) / length(action)) %>% 
  group_by(pastEarninings) %>% summarise(mu = mean(pAccept),
                                                   se = sd(pAccept) / sqrt(length(pAccept)),
                                                   min = mu - se,
                                                   max = mu + se) %>%
  ggplot(aes(as.factor(pastEarninings), mu)) +
  geom_bar(stat = "identity", fill = "#767676") +
  geom_errorbar(aes(ymin = min, ymax = max), width = 0.3 ) +
  myTheme + xlab("Previous reward") + ylab("Acceptance (%)")


