source("RL.R")
source("RLSocial.R")
library("ggplot2")
library("dplyr")
library("tidyr")
library("logistf")
library("lme4")
source("subFxs/plotThemes.R")
library("data.table")
# load expParas
load("expParas.RData")

# simulation
set.seed(123)
dir.create("figures")
dir.create("figures/analysis")

# read in data
thisTrialData = read.csv("data/102.csv", header = T)
thisTrialData$condition = factor(ifelse(thisTrialData$blockIdx == 1, "rich", "poor"), levels = c("rich", "poor"))

# plot the effect of the environment and the ht
thisTrialData %>% mutate(ht = as.factor(scheduledHt)) %>% group_by(condition, ht) %>%
  summarise(mu = sum(trialEarnings > 0) / length(trialEarnings)) %>% 
  ggplot(aes(ht, mu, fill = condition)) +
  geom_bar(stat = "identity", position = 'dodge') +
  xlab("Handling time (s)") + ylab("Acceptance (%)") + myTheme +
  scale_fill_manual(values = c("#9ecae1", "#ffeda0"))
ggsave("figures/analysis/htEnv.png", width = 4, height = 3)

# the effect of the reward
thisTrialData$preTrialEarnings = c(NA, head(thisTrialData$trialEarnings, -1))
thisTrialData %>% filter(thisTrialData$preTrialEarnings > 0) %>%
  mutate(ht = as.factor(scheduledHt)) %>% group_by(condition, ht, preTrialEarnings) %>%
  summarise(mu = sum(trialEarnings > 0) / length(trialEarnings),
            n = length(trialEarnings),
            muAdj =  (sum(trialEarnings > 0) + 0.5) / (n + 1),
            se =  sqrt((1 - muAdj) * muAdj) / (n + 1),
            min = mu - se,
            max = mu + se
  ) %>% 
  ggplot(aes(as.factor(preTrialEarnings), mu)) +
  geom_bar(stat = "identity", fill = "#767676") +
  geom_errorbar(aes(ymin = min, ymax = max), width = 0.3 ) +
  myTheme + xlab("Previous reward") + ylab("Acceptance (%)") + facet_grid(condition~ht)
ggsave("figures/analysis/reward.png", width = 6, height = 3)

# fit a model 
thisTrialData$action = thisTrialData$trialEarnings > 0
thisTrialData$timeSpent = ifelse(thisTrialData$action, thisTrialData$scheduledHt + iti, 0 + iti)
thisTrialData$preTimeSpent = c(NA, head(thisTrialData$timeSpent, -1))
fitData = thisTrialData[thisTrialData$scheduledHt == 22 | thisTrialData$scheduledHt == 28,]
fit = logistf(action ~ condition + preTimeSpent + scheduledHt + preTrialEarnings, family = "binomial",fitData)   
summary(fit)

# check the effect of learning
thisTrialData$taskTime = thisTrialData$blockTime 
thisTrialData$taskTime[thisTrialData$blockIdx == 2] = thisTrialData$taskTime[thisTrialData$blockIdx == 2] + blockSec
thisTrialData$chunkIdx = ceiling(thisTrialData$trialIdx / nHt)
thisTrialData$chunkIdx[thisTrialData$blockIdx == 2] = with(thisTrialData, chunkIdx[blockIdx == 2] + max(chunkIdx[blockIdx == 1]))
tGrid = head(seq(0, blockSec * 2, by = 5), -1)  
nT = length(tGrid) 
thisAcceptMatrixOnGrid = matrix(NA, nrow = nUnqHt, ncol = nT)
nChunk = max(thisTrialData$chunkIdx)
endOfChunkTimes = sapply(1 : nChunk, function(i) max(thisTrialData$taskTime[thisTrialData$chunkIdx == i]))
# NA on the end, assuming not observed actions the same as the one on the last chunk
for(i in 1 : nT){
  t = tGrid[i]
  if(t <= max(endOfChunkTimes)){
    thisChunkIdx = min(which(endOfChunkTimes > t))
  }else{
    thisChunkIdx = nChunk
  }
  for(j in 1 : nUnqHt){
    tempt = thisTrialData %>% filter(thisTrialData$chunkIdx == thisChunkIdx & thisTrialData$scheduledHt == unqHts[j])
    thisAcceptMatrixOnGrid[j,i] = mean(tempt$action)
  }
}

df = data.frame(cbind(t(thisAcceptMatrixOnGrid), tGrid)) 
names(df) = c(paste0("ht", unqHts), "time")
df %>% gather(key = "ht", value = "mu", -time) %>%
  ggplot(aes(time, mu)) + geom_line(size = 1) + geom_vline(xintercept = blockSec, color = "grey") +
  facet_grid(~ht) + xlab("Time (min)") + ylab("Accept %")  + myTheme +
  scale_x_continuous(breaks = c(0, 1200, 2400), labels = c(0, 20, 40))
ggsave("figures/analysis/learningCurve.png", width = 6, height = 3)

