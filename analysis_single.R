source("RL.R")
source("RLSocial.R")
library("ggplot2")
library("tidyr")
library("logistf")
library("lme4")
library("lmerTest")
source("subFxs/plotThemes.R")
library("data.table")
library("dplyr")
# load expParas
load("expParas.RData")

# simulation
set.seed(123)
dir.create("figures")
dir.create("figures/analysis")

# read in data
id = "202"
thisTrialData = read.csv(sprintf("data/%s.csv", id), header = T)
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
thisTrialData$Action = ifelse(thisTrialData$action == 1, "Accept", "Reject")
# plot the effect of the environment and the ht
# use adjusted pAccept and se 
thisTrialData %>% filter(!is.na(action)) %>% group_by(condition, ht) %>%
  summarise(mu = sum(action) / length(action),
                   se = sqrt(mu * (1 -mu)) / sqrt(length(action)),
                   n = length(action)) %>% 
  ggplot(aes(ht, mu, fill = condition)) +
  geom_bar(stat = "identity", position = 'dodge') +
  xlab("Handling time (s)") + ylab("Acceptance (%)") + myTheme +
  scale_fill_manual(values = c("#9ecae1", "#ffeda0")) + 
  geom_errorbar(aes(x = ht, ymin = mu - se, ymax = mu + se),  position=position_dodge(width=1))


# the effect of the previous trial
thisTrialData %>%  filter(trialIdx > 1 & trialEarningsLag1 > 0 & condition == "poor" & !is.na(action)) %>%
  mutate(trialEarningsLag1 = as.factor(trialEarningsLag1), timeSpentLag1 = as.factor(timeSpentLag1)) %>%
  group_by(trialEarningsLag1, ht, timeSpentLag1) %>%
  summarise(mu = mean(action),
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

# learning curve 
moreFreqDf = thisTrialData %>%
  filter((condition == "rich" & scheduledHt == unqHts[1]) |
           (condition == "poor" & scheduledHt == unqHts[3]))
lessFreqDf = thisTrialData %>%
  filter((condition == "rich" & scheduledHt != unqHts[1]) |
           (condition == "poor" & scheduledHt != unqHts[3]))
tempt1 = moreFreqDf %>% filter(!is.na(action)) %>%
  group_by(condition, ht, chunkIdx) %>% summarise(pAccept = mean(action))

tempt2 = lessFreqDf %>%
  group_by(condition, ht) %>% mutate(groupIdx = ceiling(rank(trialIdx) / 4)) %>%
  group_by(condition, ht, groupIdx) %>%
  summarise(pAccept = mean(action, na.rm = T),
                   chunkIdx = mean(chunkIdx, na.rm = T))
learnCurveDf = rbind(tempt1, tempt2)
junk = thisTrialData %>% group_by(ht) %>% summarise(max(chunkIdx[condition == 'rich']))
condDf = data.frame(
  ht = factor(unqHts),
  seg = junk[[2]] 
)
learnCurveDf %>% ggplot(aes(chunkIdx, pAccept)) + geom_line() +
  geom_point(aes(chunkIdx, pAccept)) + facet_grid(~ ht) + myTheme +
  geom_vline(aes(xintercept = seg),data = condDf, color = "grey", linetype = "dashed") +
  xlab("Run") + ylab("Acceptance (%)")
ggsave(sprintf("figures/learnCurve_%s.png", id), width = 6, height = 4)

# plot the reaction time 
## create stimIdx 
thisTrialData$stimIdx = thisTrialData$chunkIdx
select = (thisTrialData$condition == "rich" & thisTrialData$scheduledHt == unqHts[1]) |
  (thisTrialData$condition == "poor" & thisTrialData$scheduledHt == unqHts[3])
thisTrialData$stimIdx[select] = thisTrialData$chunkIdx[select] +
  unlist(thisTrialData %>% filter(select) %>% group_by(condition, ht, chunkIdx) %>% group_map(~rank(.x$trialIdx) / 4 - 0.25))
## detect RT outliers
thisTrialData =  thisTrialData %>%
  group_by(condition, ht) %>%
  mutate(isRTOutlier = responseRT > (median(responseRT) + 1.5 * IQR(responseRT)) |
           responseRT < (median(responseRT) - 1.5 * IQR(responseRT)))
  
thisTrialData %>% filter(!isRTOutlier) %>%
  ggplot(aes(stimIdx, responseRT)) +
  geom_line(aes(color = Action), alpha = 0.8) +
  facet_grid(~ ht) + 
  scale_color_manual(values = c("#4575b4", "#d73027")) +
  xlab("Run") + ylab("RT (s)") + myTheme +
  geom_vline(aes(xintercept = seg),data = condDf, color = "grey", linetype = "dashed") 
ggsave(sprintf("figures/RT_%s.png", id), width = 6, height = 4)


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
  summarise(mu = mean(responseRT)) %>% 
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

