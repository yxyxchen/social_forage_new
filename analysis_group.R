source("RL.R")
source("RLSocial.R")
library("ggplot2")
library("plyr")
library("dplyr")
library("tidyr")
library("logistf")
library("lme4")
library("lmerTest")
source("subFxs/plotThemes.R")
library("data.table")
source("subFxs/loadFxs.R")

# load para
load("expParas.RData")

################## load data #################
allData = loadAllData()
ids = allData[['ids']]
trialData = allData$'trialData'
data = ldply(trialData, rbind)
nSub = length(ids)

################# variable effects ###############
# without a prior yet with random effects
data$condition = ifelse(data$blockIdx == 1, "rich", "poor")
data$preTrialEarnings = c(NA, head(data$trialEarnings, -1))
data$action = data$trialEarnings > 0
data$timeSpent = ifelse(data$action, data$scheduledHt + iti, 0 + iti)
data$preTimeSpent = c(NA, head(data$timeSpent, -1))
data$ht = as.factor(data$scheduledHt)
data$chunkIdx = ceiling(data$trialIdx / nHt)
  
#################### pAccept #################
# use between-participant se
data %>% group_by(ht, condition, id) %>% summarise(pAccept = mean(action)) %>%
  group_by(ht, condition) %>% 
  summarise(mu = mean(pAccept), se = sd(pAccept) / sqrt(length(pAccept) - 1)) %>%
  ggplot(aes(ht, mu, fill = condition)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#9ecae1", "#ffeda0")) +
  geom_errorbar(aes(x = ht, ymin = mu - se, ymax = mu + se), position = position_dodge(0.9), width = 0.5) 
  
  

# 204 maynot meet the requirement 
fitData = data %>% filter(id != "204")
fit = glmer(action ~ condition + preTimeSpent + scheduledHt + preTrialEarnings + (1 | id), family = "binomial",fitData)   
summary(fit)


########################## reaction time ###################
# what kind of figure I should show?? normalization ??
# maybe skip errorbars here. 
uncertaintyData = data.frame(
  condition = rep(conditions, each = 4),
  uncertainty = pmin(0.2 / abs(c(netValuesRich, netValuesPoor)), 3),
  ht = factor(rep(unqHts, 2))
)
data %>% group_by(ht, condition, id) %>% summarise(RT = mean(responseRT, na.rm = T)) %>%
  group_by(ht, condition) %>% 
  summarise(mu = mean(RT, na.rm = T), se = sd(RT, na.rm = T) / sqrt(sum(!is.na(RT)) - 1)) %>%
  ggplot(aes(ht, mu, fill = condition)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#9ecae1", "#ffeda0")) + 
  geom_point(data = uncertaintyData, aes(x = ht, y = uncertainty)) +
  facet_grid(~condition) + xlab("Handling time (s)") + ylab("RT (s)")


# generally summarise it by anova
fit = lmer(responseRT ~ condition * ht + (1 | id), data)
summary(fit)
anova(fit)

# explor the time course of RT
# detect outliers 
tempt = data %>% group_by(id, condition, ht) %>% summarise(median = median(responseRT, na.rm = T),
                                                           iqr = IQR(responseRT, na.rm = T),
                                                           upper = median + iqr * 1.5,
                                                           lower = median - iqr * 1.5)

isRTOutlier = unlist(sapply(1 : nrow(data), function(i)
  ifelse(data$responseRT[i] > tempt$upper[tempt$condition == data$condition[i] & tempt$ht == data$ht[i] & tempt$id == data$id[i]] |
    data$responseRT[i] < tempt$lower[tempt$condition == data$condition[i] & tempt$ht == data$ht[i] & tempt$id == data$id[i]], 1, 0)))
data$isRTOutlier = isRTOutlier
data %>% group_by(condition, ht) %>% summarise(sum(isRTOutlier, na.rm = T))

# only use commonly available data 
maxCommonChunk_ = c(
  min(sapply(1 : nSub, function(i) max(data$chunkIdx[data$id == ids[i] & data$condition == "rich"]))),
  min(sapply(1 : nSub, function(i) max(data$chunkIdx[data$id == ids[i] & data$condition == "poor"])))
)
medianRTdf = data %>% filter(!isRTOutlier) %>% 
  filter((condition == "rich" & chunkIdx <= 19) | (condition == "poor" & chunkIdx <= 15)) %>%
  group_by(condition, ht, chunkIdx) %>%
  summarise(median = median(responseRT)) 

# plot reaction time changes 
data %>% filter(!isRTOutlier) %>% ggplot(aes(chunkIdx, responseRT)) +
  geom_line(aes(group = factor(id)), color = "grey") +
  facet_grid(condition ~ ht) + 
  geom_line(aes(chunkIdx, median), medianRTdf) + facet_grid(condition ~ ht) +
  xlab("Run num") + ylab("RT (s)")

## plot pAccept as a function of chunk
data %>%  filter((condition == "rich" & chunkIdx <= 19) | (condition == "poor" & chunkIdx <= 15)) %>%
  group_by(condition, ht, chunkIdx)  %>%
  summarise(pAccept = sum(action) / length(action)) %>%
  ggplot(aes(chunkIdx, pAccept)) + geom_line() +
  facet_grid(condition ~ ht)
