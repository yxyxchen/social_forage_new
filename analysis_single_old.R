source("RL.R")
source("RLSocial.R")
library("ggplot2")
library("dplyr")
library("tidyr")
library("logistf")
library("lme4")
library("lmerTest")
source("subFxs/plotThemes.R")
library("data.table")
# load expParas
load("expParasOld.RData")

# simulation
set.seed(123)
dir.create("figures")
dir.create("figures/analysis")

# read in data
thisTrialData = read.csv("data/103.csv", header = T)
thisTrialData$condition = factor(ifelse(thisTrialData$blockIdx == 1, "rich", "poor"), levels = c("rich", "poor"))
thisTrialData$ht = as.factor(thisTrialData$scheduledHt)
thisTrialData$taskTime = thisTrialData$blockTime 
thisTrialData$taskTime[thisTrialData$blockIdx == 2] = thisTrialData$taskTime[thisTrialData$blockIdx == 2] + blockSec
thisTrialData$chunkIdx = ceiling(thisTrialData$trialIdx / nHt)
thisTrialData$chunkIdx[thisTrialData$blockIdx == 2] = with(thisTrialData, chunkIdx[blockIdx == 2] + max(chunkIdx[blockIdx == 1]))
thisTrialData$action = ifelse(thisTrialData$trialEarnings == -2, NA, ifelse(thisTrialData$trialEarnings == 0, 0, 1))
thisTrialData$trialEarningsLag1 = unlist(thisTrialData %>% group_by(condition) %>% group_map(~c(NA, head(.x$trialEarnings, -1))))
thisTrialData$timeSpent = ifelse(thisTrialData$action, thisTrialData$scheduledHt + iti, 0 + iti)
thisTrialData$timeSpentLag1 = unlist(thisTrialData %>% group_by(condition) %>% group_map(~c(NA, head(.x$timeSpent, -1))))
  
# plot the effect of the environment and the ht
# use adjusted pAccept and se 
thisTrialData %>% group_by(condition, ht) %>%
  dplyr::summarise(mu = sum(trialEarnings > 0) / length(trialEarnings),
                   n = length(action),
                   muAdj = (sum(action) + 0.5) / (length(action) + 1),
                   seAdj =  sqrt((1 - muAdj) * muAdj) / (length(action) + 1)) %>% 
  ggplot(aes(ht, muAdj, fill = condition)) +
  geom_bar(stat = "identity", position = 'dodge') +
  xlab("Handling time (s)") + ylab("Acceptance (%)") + myTheme +
  scale_fill_manual(values = c("#9ecae1", "#ffeda0")) + 
  geom_errorbar(aes(x = ht, ymin = muAdj - seAdj, ymax = muAdj + seAdj),  position=position_dodge(width=1))

# the effect of the previous trial
# I think when we only have few samples it is better to use not adjusted percentages
thisTrialData %>%  filter(trialIdx > 1 & trialEarningsLag1 > 0 & condition == "poor") %>%
  mutate(trialEarningsLag1 = as.factor(trialEarningsLag1), timeSpentLag1 = as.factor(timeSpentLag1)) %>%
  group_by(trialEarningsLag1, ht, timeSpentLag1) %>%
  summarise(mu = sum(action, na.rm = T) / sum(!is.na(action)),
            muAdj = (sum(action, na.rm = T) + 0.5) / (sum(!is.na(action))+1),
            se = sqrt((1 - mu) * mu) /sum(!is.na(action)),
            seAdj = sqrt((1 - muAdj) * muAdj) /(sum(!is.na(action))+1),
            n = sum(!is.na(action))) %>%
  ggplot(aes(trialEarningsLag1,  mu)) + 
  geom_bar(stat = "identity") + 
  geom_text(aes(trialEarningsLag1, label = n, y = 1.3)) +
  facet_grid(timeSpentLag1~ht) +
  geom_errorbar(aes(trialEarningsLag1, ymin = mu - se, ymax = mu + se)) + myTheme +
  xlab("Previous payoff") + ylab("Acceptance (%)") + ylim(c(0, 1.5))


# fit a model 
thisTrialData$action = ifelse(thisTrialData$trialEarnings > 0, 1, 0)
thisTrialData$timeSpent = ifelse(thisTrialData$action, thisTrialData$scheduledHt + iti, 0 + iti)
thisTrialData$preTimeSpent = c(NA, head(thisTrialData$timeSpent, -1))
fitData = thisTrialData[thisTrialData$scheduledHt == 22 * 0.7 | thisTrialData$scheduledHt == 28 * 0.7,]
fit = logistf(action ~ condition + preTimeSpent + scheduledHt + preTrialEarnings, family = "binomial",fitData)   
summary(fit)

# check the effect of learning
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

######################## reaction time analysis #######################
df = data.frame(cbind(t(thisAcceptMatrixOnGrid), tGrid)) 
names(df) = c(paste0("ht", unqHts), "time")
df %>% gather(key = "ht", value = "mu", -time) %>%
  ggplot(aes(time, mu)) + geom_line(size = 1) + geom_vline(xintercept = blockSec, color = "grey") +
  facet_grid(~ht) + xlab("Time (min)") + ylab("Accept %")  + myTheme +
  scale_x_continuous(breaks = c(0, 1200, 2400), labels = c(0, 20, 40))
ggsave("figures/analysis/learningCurve.png", width = 6, height = 3)


# calculate the reaction time 
# error bars and outliers 
thisTrialData  %>%
  group_by(ht, condition) %>%
  dplyr::summarise(mu = mean(responseRT)) %>% 
  ggplot(aes(ht, mu, fill = condition)) + 
  geom_bar(stat = "identity", position = 'dodge') +
  xlab("Ht (s)") + ylab("RT (s)") + myTheme 


thisTrialData  %>% 
  ggplot(aes(ht, responseRT)) + 
  geom_boxplot() +
  xlab("RT (s)") + ylab("Count") + myTheme +
  facet_grid( ~ condition)


thisTrialData  %>% 
  ggplot(aes(responseRT, color = factor(action))) + 
  geom_density() +
  xlab("RT (s)") + ylab("Count") + myTheme +
  facet_grid(ht ~ condition)

selectData = thisTrialData[thisTrialData$condition == "poor" & thisTrialData$scheduledHt == unqHts[3],]
t.test(selectData$responseRT[selectData$action == 0],
            selectData$responseRT[selectData$action == 1])

# check the effect of taskTime
thisTrialData %>% 
  ggplot(aes(chunkIdx, responseRT)) +
  geom_line() + facet_grid(~ ht)

