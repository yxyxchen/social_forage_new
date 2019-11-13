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

# output dir 
dir.create("figures")

# create the ht sequences in two conditions
htSeq_ = lapply(1 : nCondition, function(i) {
  condition = conditions[i]
  tempt = as.vector(replicate(nChunkMax, sample(hts_[[condition]], chunkSize)))
  tempt[1 : nTrialMax]
})


# simulate non_social data
nSub = 64
RLResults_ = list(length = nSub)
for(sIdx in 1 : nSub){
  beta = runif(1, 0.001, 0.01)
  tau = runif(1, 10, 15)
  iniLongRunRate = runif(1, 0.15, 1)
  RLResults = RL(beta, tau, iniLongRunRate, htSeq_)
  RLResults_[[sIdx]] =  RLResults 
}


# concatenate 
dfList = lapply(1 : nSub, function(i){
  RLResults = RLResults_[[i]]
  action = ifelse(RLResults$trialEarnings > 0, 1, 0)
  pastEarnings1 = c(NA, head(RLResults$trialEarnings, -1))
  pastHt1 = c(NA, head(RLResults$spentHt, -1) + iti)
  
  # 
  spentTime = RLResults$spentHt + iti # assume reward occurs at the end of the feedback period
  spentTime4LastRwd = c(NA, head(ave(spentTime, cumsum(spentTime != 4), FUN = cumsum), -1))
  
  data = data.frame(
    action = action,
    pastEarninings = pastEarnings1,
    pastRwdRate = pastEarnings1 / pastHt1,
    ht = RLResults$scheduledHt,
    spentTime4LastRwd = spentTime4LastRwd,
    subId  = rep(i, length = length(action))
  )
})


data = bind_rows(dfList)
data = data[apply(data, MARGIN = 1, FUN = function(x) all(!is.na(x))),]


# plot the effect of reward sizes 
# make sure to average within each participant first. And use se across participants 
data %>% group_by(ht, subId) %>% summarise(pAccept = sum(action) / length(action)) %>%
  group_by(ht) %>% summarise(mu = mean(pAccept),
                             se = sd(pAccept) / sqrt(length(pAccept)),
                             min = mu - se,
                             max = mu + se) %>%
  ggplot(aes(ht, mu)) + geom_bar(stat = "identity") + myTheme +
  geom_errorbar(aes(ymin = min, ymax = max, width = 1))

# plot the effect of past earnings 
data %>% group_by(spentTime4LastRwd, subId) %>% summarise(pAccept = sum(action) / length(action)) %>%
  group_by(spentTime4LastRwd) %>% summarise(mu = mean(pAccept),
                             se = sd(pAccept) / sqrt(length(pAccept)),
                             min = mu - se,
                             max = mu + se) %>%
  ggplot(aes(spentTime4LastRwd, mu)) + geom_bar(stat = "identity") + myTheme +
  geom_errorbar(aes(ymin = min, ymax = max, width = 0.01))


# plot the effect of reward sizes 
sumData %>% ggplot(aes(factor(pastEarnings1), pAccept)) +
  geom_bar(stat = "identity") + facet_wrap(~ht) + myTheme +
  xlab("Last trial earnings")


# plot the effect of reward sizes 
x = seq(0, max(unqHts), by = 1)
data %>% group_by(ht) %>% summarise(pAccept = sum(action) / length(action)) %>%
  ggplot(aes(ht, pAccept)) + geom_bar(stat = "identity") + myTheme                    