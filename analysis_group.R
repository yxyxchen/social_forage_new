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
source("subFxs/loadFxs.R")

# load para
load("expParas.RData")

################## load data #################
allData = loadAllData()
ids = allData[['ids']]
trialData = allData$'trialData'
data = ldply(trialData, rbind, .id = NULL)
nSub = length(ids)

################# variable effects ###############
# without a prior yet with random effects
data$ht = factor(data$scheduledHt)
data$condition = ifelse(data$blockIdx == 1, "rich", "poor")
data$responseTaskTime = data$responseBlockTime
data$action = ifelse(data$trialEarnings == -2, NA, ifelse(data$trialEarnings == 0, 0, 1))
data$responseTaskTime[data$blockIdx == 2] = data$responseTaskTime[data$blockIdx == 2] + blockSec
data$trialEarningsLag1 = unlist(data %>% group_by(id, blockIdx) %>% group_map(~c(NA, head(.x$trialEarnings, -1))))
data$trialEarningsLag2 = unlist(data %>% group_by(id, blockIdx) %>% group_map(~c(NA, NA, head(.x$trialEarnings, -2))))
data$trialEarningsLag3 = unlist(data %>% group_by(id, blockIdx) %>% group_map(~c(NA, NA, NA, head(.x$trialEarnings, -3))))
data$timeSpent = ifelse(data$action, data$scheduledHt + iti, 0 + iti)
data$timeSpentLag1 = unlist(data %>% group_by(id, blockIdx) %>% group_map(~c(NA, head(.x$timeSpent, -1))))
data$timeSpentLag2 = unlist(data %>% group_by(id, blockIdx) %>% group_map(~c(NA, NA, head(.x$timeSpent, -2))))
data$timeSpentLag3 = unlist(data %>% group_by(id, blockIdx) %>% group_map(~c(NA, NA, NA, head(.x$timeSpent, -3))))
data$ht = as.factor(data$scheduledHt)
data$chunkIdx = ceiling(data$trialIdx / nHt)
data$cumEarnings = unlist(data %>%
  group_by(id) %>%
  group_map(~ cumsum(.x$trialEarnings )))

#################### pAccept #################
# use between-participant se
data %>% group_by(ht, condition, id) %>% dplyr::summarise(pAccept = mean(action, na.rm = T)) %>%
  group_by(ht, condition) %>% 
  dplyr::summarise(mu = mean(pAccept), se = sd(pAccept) / sqrt(length(pAccept) - 1)) %>%
  ggplot(aes(ht, mu, fill = condition)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#9ecae1", "#ffeda0")) +
  geom_errorbar(aes(x = ht, ymin = mu - se, ymax = mu + se), position = position_dodge(0.9), width = 0.5) +
  myTheme + xlab("Handling time (s)") + ylab("Acceptance (%)")
ggsave("figures/pAccept_group.png", width = 5, height = 5)

fit = glmer(action ~ condition +  scheduledHt + (1 | id), data, family = "binomial")
summary(fit)


# detect the effect of previous spent time
data %>%  filter(trialIdx > 1 & trialEarningsLag1 > 0 & id != "201") %>%
  mutate(timeSpentLag1  = as.factor(timeSpentLag1)) %>%
  group_by(id, timeSpentLag1, condition, ht) %>% dplyr::summarise(pAccept = mean(action, na.rm = T)) %>%
  group_by(timeSpentLag1, condition, ht) %>% dplyr::summarise(mu = mean(pAccept), se = sd(pAccept) / sqrt(sum(!is.na(pAccept)) - 1)) %>%
  ggplot(aes(timeSpentLag1,  mu)) + geom_bar(stat = "identity") + facet_grid(condition~ht) +
  geom_errorbar(aes(timeSpentLag1, ymin = mu - se, ymax = mu + se)) + myTheme + 
  xlab("Previous spent time (s)") + ylab("Acceptance (%)")
ggsave("figures/pAccept_previous_time.png", width = 9, height = 4)


# detect the effect of previous payoff
data %>%  filter(trialIdx > 1 & trialEarningsLag1 > 0 & id != "201") %>%
  mutate(trialEarningsLag1 = as.factor(trialEarningsLag1)) %>%
  group_by(id, trialEarningsLag1, condition, ht) %>%
  dplyr::summarise(pAccept = mean(action, na.rm = T)) %>%
  group_by(trialEarningsLag1, condition, ht) %>%
  dplyr::summarise(mu = mean(pAccept), se = sd(pAccept) / sqrt(sum(!is.na(pAccept)) - 1)) %>%
  ggplot(aes(trialEarningsLag1,  mu)) + geom_bar(stat = "identity") + facet_grid(condition~ht) +
  geom_errorbar(aes(trialEarningsLag1, ymin = mu - se, ymax = mu + se)) + myTheme + 
  xlab("Previous payoff") + ylab("Acceptance (%)")
ggsave("figures/pAccept_previous_payoff.png", width = 6, height = 4)

# effect of both 
data %>%  filter(trialIdx > 1 & trialEarningsLag1 > 0 & id != "201" & condition == "poor") %>%
  mutate(trialEarningsLag1 = as.factor(trialEarningsLag1), timeSpentLag1 = as.factor(timeSpentLag1)) %>%
  group_by(id, trialEarningsLag1, ht, timeSpentLag1) %>%
  dplyr::summarise(pAccept = mean(action, na.rm = T)) %>%
  group_by(trialEarningsLag1, timeSpentLag1, ht) %>%
  dplyr::summarise(mu = mean(pAccept, na.rm = T),
            se = sd(pAccept) / sqrt(sum(!is.na(pAccept)) - 1),
            n = sum(!is.na(pAccept)))%>%
  ggplot(aes(trialEarningsLag1,  mu)) +
  geom_bar(stat = "identity") + facet_grid(timeSpentLag1~ht) +
  geom_errorbar(aes(trialEarningsLag1, ymin = mu - se, ymax = mu + se)) + myTheme + 
  geom_text(aes(trialEarningsLag1, label = n, y = 1.3)) +
  xlab("Previous payoff") + ylab("Acceptance (%)") + ylim(c(0, 1.5))
ggsave("figures/pAccept_previous_poor1.png", width = 9, height = 4)

data %>%  filter(trialIdx > 1 & trialEarningsLag1 > 0 & id != "201" & condition == "poor") %>%
  mutate(trialEarningsLag1 = as.factor(trialEarningsLag1), timeSpentLag1 = as.factor(timeSpentLag1)) %>%
  group_by(id, trialEarningsLag1, ht, timeSpentLag1) %>% summarise(pAccept = mean(action, na.rm = T)) %>%
  group_by(trialEarningsLag1, timeSpentLag1, ht) %>%
  summarise(mu = mean(pAccept), se = sd(pAccept) / sqrt(sum(!is.na(pAccept)) - 1), n = sum(!is.na(pAccept))) %>%
  ggplot(aes(timeSpentLag1,  mu)) + geom_bar(stat = "identity") +
  facet_grid(trialEarningsLag1 ~ ht) +
  geom_errorbar(aes(timeSpentLag1, ymin = mu - se, ymax = mu + se)) +
  geom_text(aes(timeSpentLag1, label = n, y = 1.3)) +
  myTheme + xlab("Previous spent time (s)") + ylab("Acceptance (%)")
ggsave("figures/pAccept_previous_poor2.png", width = 9, height = 4)

fitData = data %>% filter(trialIdx > 1 & scheduledHt > 2 & scheduledHt < 20 & trialEarningsLag1 > 0 
                          & id != "201" & condition == "poor")
fit = glm(action ~ scheduledHt + timeSpentLag1 + trialEarningsLag1,  family = "binomial",fitData)   
summary(fit)


########################## reaction time ###################
# what kind of figure I should show?? normalization ??
# maybe skip errorbars here. 
uncertaintyData = data.frame(
  condition = rep(conditions, each = 4),
  uncertainty = pmin(0.2 / abs(c(netValuesRich, netValuesPoor)), 3),
  ht = factor(rep(unqHts, 2))
)
data %>% filter(!is.na(responseRT)) %>%
  group_by(ht, condition, id) %>%
  dplyr::summarise(RT = mean(responseRT),
                   se = sd(responseRT) / sqrt(nSub - 1)) %>%
  group_by(ht, condition) %>% 
  dplyr::summarise(mu = mean(RT, na.rm = T), se = sd(RT, na.rm = T) / sqrt(sum(!is.na(RT)) - 1)) %>%
  ggplot(aes(ht, mu, fill = condition)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mu - se, ymax = mu + se), position = position_dodge(1)) + 
  scale_fill_manual(values = c("#9ecae1", "#ffeda0")) +
  xlab("Handling time (s)") + ylab("RT (s)") + myTheme 



# generally summarise it by anova
fit = lmer(responseRT ~ condition * ht + (1 | id), data)
summary(fit)
anova(fit)

# explor the time course of RT
# detect outliers 
tempt = data %>% filter(!is.na(action)) %>%
  group_by(id, condition, ht) %>% dplyr::summarise(median = median(responseRT),
                                                           iqr = IQR(responseRT),
                                                           upper = median + iqr * 1.5,
                                                           lower = median - iqr * 1.5)

isRTOutlier = unlist(sapply(1 : nrow(data), function(i)
  ifelse(data$responseRT[i] > tempt$upper[tempt$condition == data$condition[i] & tempt$ht == data$ht[i] & tempt$id == data$id[i]] |
    data$responseRT[i] < tempt$lower[tempt$condition == data$condition[i] & tempt$ht == data$ht[i] & tempt$id == data$id[i]], 1, 0)))
data$isRTOutlier = isRTOutlier

# only use commonly available data 
maxCommonChunk_ = c(
  min(sapply(1 : nSub, function(i) max(data$chunkIdx[data$id == ids[i] & data$condition == "rich"]))),
  min(sapply(1 : nSub, function(i) max(data$chunkIdx[data$id == ids[i] & data$condition == "poor"])))
)
medianRTdf = data %>% filter(!isRTOutlier & !is.na(action)) %>% 
  mutate(Action = ifelse(action == 1, "Accept", "Reject")) %>%
  filter((condition == "rich" & chunkIdx <= 19) | (condition == "poor" & chunkIdx <= 15)) %>%
  group_by(condition, ht, chunkIdx, Action) %>%
  dplyr::summarise(median = median(responseRT)) 


# plot reaction time as a function of run
data$Action = ifelse(data$action == 1, "Accept", "Reject")
data %>% filter(!isRTOutlier & condition == "rich")  %>%
  ggplot(aes(chunkIdx, responseRT)) +
  geom_line(aes(group = factor(id), color = id)) +
  facet_grid(Action ~ ht) 
ggsave("figures/RT_run_rich.png", width = 6, height = 4) 

data %>% filter(!isRTOutlier & condition == "poor")  %>%
  ggplot(aes(chunkIdx, responseRT)) +
  geom_point(aes(group = factor(id), color = id))+
  geom_line(aes(group = factor(id), color = id)) +
  facet_grid(Action ~ ht) 
ggsave("figures/RT_run_poor.png", width = 6, height = 4) 

medianRTdf %>% ggplot(aes(chunkIdx, median, color = Action)) +
  geom_line() +
  xlab("Run num") + ylab("RT (s)") + facet_grid(condition ~ ht) +
  scale_color_manual(values = c("#4575b4", "#d73027")) + myTheme
ggsave("figures/RT_run_group.png", width = 6, height = 4)

# plot reaction time as a function of trial
data %>% filter(!isRTOutlier & condition == "poor" & scheduledHt == unqHts[2])  %>%
  mutate(stimIdx = unlist(sapply(1 : nSub, function(i) 1 : sum(id == ids[i]))),
         Action = ifelse(action == 1, "Accept", "Reject"))%>% 
  ggplot(aes(stimIdx, responseRT)) +
  geom_point(aes(color = Action))+
  geom_line(aes(color = Action))+
  facet_grid(~id) + myTheme + scale_color_manual(values = c("#4575b4", "#d73027"))
ggsave("figures/RT_stim_poor.png", width = 6, height = 4) 



################  learning curve ######################
# can I have learning curve for an individual 
data


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
