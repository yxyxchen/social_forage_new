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
allData = loadAllDataOld()
ids = allData[['ids']]
trialData = allData$'trialData'
data = ldply(trialData, rbind, .id = NULL)
nSub = length(ids)

################# variable effects ###############
# without a prior yet with random effects
data$condition = ifelse(data$blockIdx == 1, "rich", "poor")
data$responseTaskTime = data$responseBlockTime
data$responseTaskTime[data$blockIdx == 2] = data$responseTaskTime[data$blockIdx == 2] + blockSec
data$preTrialEarnings = c(NA, head(data$trialEarnings, -1)) # oo I am wrong
data$action = data$trialEarnings > 0
data$timeSpent = ifelse(data$action, data$scheduledHt + iti, 0 + iti)
data$preTimeSpent = c(NA, head(data$timeSpent, -1))
data$ht = as.factor(data$scheduledHt)
data$chunkIdx = ceiling(data$trialIdx / nHt)
data$cumEarnings = unlist(data %>%
  group_by(id) %>%
  group_map(~ cumsum(.x$trialEarnings )))
data$preTiralUpdate = data$preTrialEarnings -
  data$preTimeSpent * ifelse(data$condition == "rich", optimLongRunRate_[['rich']], optimLongRunRate_[['poor']])
data$preTrialUpdate2 =  data$preTrialEarnings2 -
  data$preTimeSpent2 * ifelse(data$condition == "rich", optimLongRunRate_[['rich']], optimLongRunRate_[['poor']])
#################### pAccept #################
# use between-participant se
data %>% group_by(ht, condition, id) %>% summarise(pAccept = mean(action)) %>%
  group_by(ht, condition) %>% 
  summarise(mu = mean(pAccept), se = sd(pAccept) / sqrt(length(pAccept) - 1)) %>%
  ggplot(aes(ht, mu, fill = condition)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#9ecae1", "#ffeda0")) +
  geom_errorbar(aes(x = ht, ymin = mu - se, ymax = mu + se), position = position_dodge(0.9), width = 0.5) +
  myTheme + xlab("Handling time (s)") + ylab("Acceptance (%)")
ggsave("figures/pAccept_group_old.png", width = 5, height = 5)


data$actionAdj =
  unlist(data %>%
  group_by(id) %>%
  group_map(~ .x$action - mean(.x$action, na.rm = T) + mean(data$action)))

sumData = data %>% group_by(id, condition, scheduledHt) %>%
  summarise(pAccept = mean(actionAdj), n = length(action))

# effects of environments and handling time
fit = glmer(action ~ condition +  scheduledHt + (1 | id), data, family = "binomial")
summary(fit)


# effects of preTrialEarnings
fitData = data %>% filter(trialIdx > 4 & scheduledHt > 2 & scheduledHt < 40 & preTrialEarnings > 0)
fit = glm(action ~ condition + scheduledHt + preTimeSpent + preTimeSpent2 + preTimeSpent3 + preTimeSpent4 + preTrialEarnings, family = "binomial",fitData)   
summary(fit)

# the effect 
fit = glm(action ~ condition + scheduledHt + preTimeSpent + preTrialEarnings, family = "binomial",fitData)   
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

#### plot the earnings so far
data %>% ggplot(aes(responseTaskTime, cumEarnings)) +
  geom_line(aes(group = id, color = id))

# if we choose one participant as the reference
feedbacks = 0 : 10
countDf = list()
for(c in 1 : nCondition){
  condition = conditions[c]
  counts = list()
  freqs = list()
  for(k in 1 : (nUnqHt+1)){
    # calcualte the interval 
    if(k == 1){
      interval = iti
    }else{
      interval = iti + unqHts[k - 1]
    }
    # calculate the feedback
    junk = vector(length =  floor(2 * blockSec / interval))
    for(i in 1 : floor(2 * blockSec / interval)){
      junk[i] = sum(data$trialEarnings[data$id == "201" & data$condition == condition &
                                         data$responseTaskTime >= (i-1) * interval &
                                         data$responseTaskTime < i * interval])
      }
    #
    count = as.numeric(table(junk))
    count = c(count, rep(0, length(feedbacks) - length(count)))
    counts[[k]] = count
    freqs[[k]] = count / sum(count)
  }
  countDf[[c]] = data.frame(
    feedback = rep(feedbacks, nUnqHt + 1),
    condition = rep(condition, (nUnqHt + 1) * length(feedbacks)),
    interval = rep(c(iti, unqHts + iti), each = length(feedbacks)),
    count = unlist(counts, rbind),
    freq =  unlist(freqs, rbind) 
  )
}
ldply(countDf, rbind, .id = NULL) %>%  ggplot(aes(factor(feedback), freq)) + geom_bar(stat = "identity") +
  facet_grid(condition~interval) + myTheme + xlab("Social Feedback") + ylab("Frequency (%)") 
ggsave("figures/feedback_single.png", width = 6, height = 3)

# if we choose the average over participants 
feedbacks = list()
index = 1
for(c in 1 : nCondition){
  condition = conditions[c]
  for(k in 1 : (nUnqHt+1)){
    # calcualte the interval 
    if(k == 1){
      interval = iti
    }else{
      interval = iti + unqHts[k - 1]
    }
    # calculate the feedback
    junk = vector(length =  floor(2 * blockSec / interval))
    for(i in 1 : floor(2 * blockSec / interval)){
      junk[i] = sum(data$trialEarnings[data$condition == condition &
                                         data$responseTaskTime >= (i-1) * interval &
                                         data$responseTaskTime < i * interval]) / nSub
    }
    feedbacks[[index]] = junk
    index = index + 1
  }
}
feedbackDf = data.frame(
  feedback = unlist(feedbacks),
  ht = rep(rep(c(0, unqHts), 2), sapply(feedbacks, length)),
  condition = rep(rep(conditions, each = (nUnqHt + 1)), sapply(feedbacks, length))
)
feedbackDf %>% ggplot(aes(feedback)) + geom_histogram(bins = 10) +
  facet_grid(condition ~ ht) + myTheme + xlab("Social Feedback") + ylab("Count") 
feedbackDf %>% group_by(ht, condition) %>% summarise(sum(feedback == 0) / length(feedback))
ggsave("figures/feedback_average.png", width = 6, height = 3)
