source("RL.R")
source("RLSocial.R")
library("ggplot2")
library("dplyr")
library("tidyr")
library("lme4")
library("logistf")
library(afex)
source("subFxs/plotThemes.R")
library("data.table")
# load expParas
load("expParas.RData")

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
  # create the rwd sequences in two conditions
  rwdSeq_ = lapply(1 : nCondition, function(i) {
    tempt = replicate(nChunkMax, sample(rwds, chunkSize))
    tempt = tempt[1 : nTrialMax]
  })
  beta = runif(1, 0.005, 0.01)
  tau = runif(1, 5, 15)
  iniLongRunRate = runif(1, 0.02, 0.04)
  RLResults = RL(beta, tau, iniLongRunRate, htSeq_, rwdSeq_, blockSec)
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
    spentTime4LastRwd = spentTime4LastRwd,
    blockTime =  cut(RLResults$blockTime, breaks = seq(0, blockSec, length.out = 4 + 1), labels = 1:4)
  )

})
data = bind_rows(dfList)
data = data[apply(data, MARGIN = 1, FUN = function(x) all(!is.na(x))),]
data$rewardUpdateBin = cut(data$rewardUpdate,breaks = quantile(c(0, data$rewardUpdate, ceiling(data$rewardUpdate)),
                                                               c(0, 1/3, 2/3, 1)), labels = 1:3)

# plot the effect of reward sizes and environments 
# make sure to average within each participant first. And use se across participants 
data %>% mutate(ht = as.factor(ht)) %>% group_by(ht, subId, condition) %>% summarise(pAccept = sum(action) / length(action)) %>%
  group_by(ht, condition) %>% summarise(mu = mean(pAccept),
                                        se = sd(pAccept) / sqrt(length(pAccept)),
                                        min = mu - se,
                                        max = mu + se) %>%  as.data.frame() %>%
  mutate(ht = as.factor(ht)) %>%
  ggplot(aes(ht, mu, fill = condition)) +
  geom_bar(stat = "identity", position = 'dodge') +
  geom_errorbar(aes(ymin = min, ymax = max), position = position_dodge(0.9), width = 0.5) +
  xlab("Handling time (s)") + ylab("Acceptance (%)") + myTheme +
  scale_fill_manual(values = c("#9ecae1", "#ffeda0"))

# I can also do a demean version of it if anyone wants 

# fix a regression here. Definetly individual can be different in their effect to environments and the handling time
# yet since since it is not nuisance, we don't have to take them as random effects 
# regression is less straightforward and against pair-wise comparison
regData = data %>% group_by(ht, subId, condition) %>% summarise(pAccept = sum(action) / length(action)) 
fit1 = lmer(pAccept ~ ht + condition + (1 | subId), regData)
summary(fit1)

# fit a single participant 
fit1 = logistf(action ~ ht + condition, data[data$subId == 1, ])
summary(fit1)

# for a single participant 
# use the adjusted se here, otherwise it can be 0 sometimes 
data %>% filter(subId == 1) %>%  mutate(ht = as.factor(ht)) %>%
  group_by(ht, condition)%>% summarise(mu = sum(action) / length(action),
                                       n = length(action),
                                       muAdj = (sum(action) + 0.5) / (n + 1),
                                       se = sqrt((1 - muAdj) * muAdj) / (n + 1),
                                       min = mu - se,
                                       max = mu + se) %>%  as.data.frame() %>% ggplot(aes(ht, mu, fill = condition)) +
  geom_bar(stat = "identity", position = 'dodge') +
  geom_errorbar(aes(ymin = min, ymax = max), position = position_dodge(0.9), width = 0.5) +
  xlab("Handling time (s)") + ylab("Acceptance (%)") + myTheme +
  scale_fill_manual(values = c("#9ecae1", "#ffeda0"))



####  the effect of the rewards 
# given the design here, we balaced out the condition and the HT already, since the reward is 
# independent of these two, and they are colsely to be perfectly balanced. 
# the only factor we need to consider is the individual differences, in doing the t test and in
# calculating the se

# the one without controlling for the individual differences 
# in the simulation data, the individual differences are very small
data %>% filter(data$preAction == 1) %>% 
  group_by(pastEarninings, subId) %>% 
  summarise(pAccept = mean(action)) %>%
  group_by(pastEarninings) %>% 
  summarise(
    mu = mean(pAccept),
    se = sd(pAccept) / sqrt(nSub),
    min = mu - se,
    max = mu + se
  )%>%
  ggplot(aes(as.factor(pastEarninings), mu)) +
  geom_bar(stat = "identity", fill = "#767676") +
  geom_errorbar(aes(ymin = min, ymax = max), width = 0.3) +
  myTheme + xlab("Previous reward") + ylab("Acceptance (%)")


# we can run a mixed effect regression 
fit2 = glmer(action ~ pastEarninings + condition + ht + (1 | subId), data[data$preAction == 1,],
             family = "binomial")
summary(fit2)

fit2 = logistf(action ~ pastEarninings + condition + ht, data[data$subId == 1 & data$preAction == 1,])
summary(fit2) # 

