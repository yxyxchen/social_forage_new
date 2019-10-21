source("RL.R")
source("RLSocial.R")
library("ggplot2")
library("dplyr")
library("tidyr")
library("lme4")
source("subFxs/plotThemes.R")

# load expParas
load("expParas.RData")

# output dir 
dir.create("figures")

# create the ht sequence
htSeq_ = lapply(1 : nCondition, function(i) {
  condition = conditions[i]
  tempt = as.vector(replicate(nChunkMax, sample(hts_[[condition]], chunkSize)))
  tempt[1 : nTrialMax]
})


# simulate non_social data
beta = 0.002
tau = 2
iniLongRunRate = runif(1, 0.15, 1)
RLResults = RL(beta, tau, iniLongRunRate, htSeq_)

# prepare data
action = ifelse(RLResults$trialEarnings == rwd, 1, 0)
pastRwdRate = RLResults$trialEarnings / RLResults$spentHt
pastRwdRate[RLResults$spentHt == 0] = 0
pastRwdRate = c(NA, head(pastRwdRate, -1))
data = data.frame(
  action,
  pastRwdRate,
  ht = RLResults$scheduledHt,
  condition = RLResults$condition
)
data = data[apply(data, MARGIN = 1, FUN = function(x) all(!is.na(x))),]

# summarise pAccept for different reward sizes and conditions
sumData = data %>% 
  group_by(condition, ht) %>% summarise(sum(action) / length(action))
colnames(sumData) = c("condition", "ht", "pAccept")
sumData %>% ggplot(aes(ht, pAccept, fill= condition)) +
  geom_bar(stat = "identity",position = 'dodge' ) 


# 
sumData = data %>% 
  group_by(condition, pastRwdRate) %>% summarise(sum(action) / length(action))
colnames(sumData) = c("condition", "pastRwdRate", "pAccept")
sumData %>% ggplot(aes(pastRwdRate, pAccept, fill= condition)) +
  geom_bar(stat = "identity",position = 'dodge' ) 



