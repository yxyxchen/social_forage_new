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
dir.create("figures")
# load para
load("expParas.RData")

################## load data #################
allData = loadAllData()
ids = allData[['ids']]
trialData = allData$'trialData'
data = plyr::ldply(trialData, rbind, .id = NULL)
nSub = length(ids)

################# variable effects ###############
# I can't delete the na.trials 
data$ht = factor(data$scheduledHt)
data$condition = ifelse(data$blockIdx == 1, "rich", "poor")
data$responseTaskTime = data$responseBlockTime
data$action = ifelse(data$trialEarnings == -2, NA, ifelse(data$trialEarnings == 0, 0, 1))
data$Action = ifelse(data$trialEarnings == -2, NA, ifelse(data$trialEarnings == 0, "Reject", "Accept"))
data$responseTaskTime[data$blockIdx == 2] = data$responseTaskTime[data$blockIdx == 2] + blockSec
data = data %>% arrange(id, blockIdx) # somehow tricky to use group_map if it is not arranged
data$trialEarningsLag1 = unlist(data %>% group_by(id, blockIdx) %>% group_map(~c(NA, head(.x$trialEarnings, -1))))
data$trialEarningsLag2 = unlist(data %>% group_by(id, blockIdx) %>% group_map(~c(NA, NA, head(.x$trialEarnings, -2))))
data$trialEarningsLag3 = unlist(data %>% group_by(id, blockIdx) %>% group_map(~c(NA, NA, NA, head(.x$trialEarnings, -3))))
data$timeSpent = ifelse(data$action, data$scheduledHt + iti, 0 + iti)
data$timeSpentLag1 = unlist(data %>% group_by(id, blockIdx) %>% group_map(~c(NA, head(.x$timeSpent, -1))))
data$timeSpentLag2 = unlist(data %>% group_by(id, blockIdx) %>% group_map(~c(NA, NA, head(.x$timeSpent, -2))))
data$timeSpentLag3 = unlist(data %>% group_by(id, blockIdx) %>% group_map(~c(NA, NA, NA, head(.x$timeSpent, -3))))
data$ht = as.factor(data$scheduledHt)
data$chunkIdx = ceiling(data$trialIdx / nHt) # I don't think it is a good idea to make chunkIdx accumulative 
data$cumEarnings = unlist(data %>%
  group_by(id) %>%
  group_map(~ cumsum(.x$trialEarnings )))


#################### pAccept #################
# use between-participant se
data %>% filter(!is.na(action)) %>%
  group_by(ht, condition, id) %>% summarise(pAccept = mean(action)) %>%
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

################### learn curve ###############
moreFreqSelect = (data$condition == "rich" & data$scheduledHt == unqHts[1]) |
  (data$condition == "poor" & data$scheduledHt == unqHts[3])
tempt1 = data[moreFreqSelect,] %>% filter(!is.na(action)) %>%
  group_by(id, condition, ht, chunkIdx) %>%
  summarise(pAccept = mean(action))
tempt2 = data[!moreFreqSelect,] %>% filter(!is.na(action)) %>%
  group_by(id, condition, ht) %>%
  mutate(chunkIdx = ceiling(chunkIdx / 4) * 4 - 1.5) %>%
  ungroup() %>%
  group_by(id, condition, ht, chunkIdx) %>% 
  summarise(pAccept = mean(action))
learnCurveDf = rbind(tempt1, tempt2)
learnCurveDf$chunkIdx[learnCurveDf$condition == "poor"] =
  learnCurveDf$chunkIdx[learnCurveDf$condition == "poor"] + 
  20
learnCurveDf %>% ggplot(aes(chunkIdx, pAccept)) + geom_line() +
  geom_point() +
  facet_grid(id ~ ht) + myTheme + xlab("Run") +
  ylab("Acceptance") +
  geom_vline(xintercept = max(learnCurveDf$chunkIdx[learnCurveDf$condition == "rich"]), color = "grey", linetype = "dashed") +
  scale_x_continuous(breaks = c(10, 20, 30, 40), labels = c(10, 20, 10, 20))
ggsave("figures/learnCurve_single.png", width = 6, height = 9)


tempt1 = data[moreFreqSelect,] %>% filter(!is.na(action)) %>%
  filter((condition == "rich" & chunkIdx <= 19) | (condition == "poor" & chunkIdx <= 15)) %>%
  group_by(condition, ht, chunkIdx, id) %>%
  summarise(pAccept = mean(action)) %>%
  group_by(condition, ht, chunkIdx) %>%
  summarise(mu = mean(pAccept, na.rm = T), se = sd(pAccept) / sqrt(sum(!is.na(pAccept))))
tempt2 = data[!moreFreqSelect,] %>% filter(!is.na(action)) %>%
  filter((condition == "rich" & chunkIdx <= 19) | (condition == "poor" & chunkIdx <= 15)) %>%
  group_by(condition, ht, id) %>%
  mutate(chunkIdx = ceiling(chunkIdx / 4) * 4 - 1.5) %>%
  ungroup() %>%
  group_by(condition, ht, chunkIdx, id) %>% 
  summarise(pAccept = mean(action)) %>%
  group_by(condition, ht, chunkIdx) %>%
  summarise(mu = mean(pAccept, na.rm = T), se = sd(pAccept) / sqrt(sum(!is.na(pAccept))))

learnCurveDf = rbind(tempt1, tempt2)
learnCurveDf$chunkIdx[learnCurveDf$condition == "poor"] =
  learnCurveDf$chunkIdx[learnCurveDf$condition == "poor"] + 20
learnCurveDf %>% ggplot(aes(chunkIdx, mu)) + 
  geom_ribbon(aes(ymin = mu - se, ymax = mu + se), fill = "grey") +
  geom_line() +
  facet_grid( ~ ht) + myTheme + xlab("Run") +
  ylab("Acceptance") +
  geom_vline(xintercept = max(learnCurveDf$chunkIdx[learnCurveDf$condition == "rich"]), color = "grey", linetype = "dashed") +
  scale_x_continuous(breaks = c(10, 20, 30, 40), labels = c(10, 20, 10, 20),
                     limits = c(0, 42)) 
ggsave("figures/learnCurve_group.png", width = 6, height = 4)

# detect the effect of previous spent time
data %>%  filter(trialIdx > 1 & trialEarningsLag1 > 0) %>% 
  filter(!is.na(action)) %>%
  mutate(timeSpentLag1  = as.factor(timeSpentLag1)) %>%
  group_by(id, timeSpentLag1, condition, ht) %>%
  summarise(pAccept = mean(action)) %>%
  group_by(timeSpentLag1, condition, ht) %>% 
  summarise(mu = mean(pAccept, na.rm = T),
            n = sum(!is.na(pAccept)),
            se = sd(pAccept) / sqrt(sum(!is.na(pAccept)) - 1)) %>%
  ggplot(aes(timeSpentLag1,  mu)) + geom_bar(stat = "identity") + facet_grid(condition~ht) +
  geom_errorbar(aes(timeSpentLag1, ymin = mu - se, ymax = mu + se)) + myTheme + 
  xlab("Previous spent time (s)") + ylab("Acceptance (%)")
ggsave("figures/pAccept_previous_time.png", width = 6, height = 4)

# detect the effect of previous spent time
tempt1 = data[moreFreqSelect,] %>% filter(!is.na(action)) %>%
  filter(trialIdx > 1 & trialEarningsLag1 > 0) %>%
  mutate(TimeSpentLag1 = ifelse(timeSpentLag1 <= 10, "Short", "Long")) %>%
  group_by(condition, ht, chunkIdx, id, TimeSpentLag1) %>%
  summarise(pAccept = mean(action)) %>%
  group_by(condition, ht, chunkIdx, TimeSpentLag1) %>%
  summarise(mu = mean(pAccept, na.rm = T), se = sd(pAccept) / sqrt(sum(!is.na(pAccept))))
tempt2 = data[!moreFreqSelect,]  %>% filter(!is.na(action)) %>%
  filter(trialIdx > 1 & trialEarningsLag1 > 0) %>%
  mutate(TimeSpentLag1 = ifelse(timeSpentLag1 <= 10, "Short", "Long")) %>%
  group_by(condition, ht, id, TimeSpentLag1) %>%
  mutate(chunkIdx = ceiling(chunkIdx / 4) * 4 - 1.5) %>%
  ungroup() %>%
  group_by(condition, ht, chunkIdx, id, TimeSpentLag1) %>% 
  summarise(pAccept = mean(action)) %>%
  group_by(condition, ht, chunkIdx, TimeSpentLag1) %>%
  summarise(mu = mean(pAccept, na.rm = T), se = sd(pAccept) / sqrt(sum(!is.na(pAccept))))

learnCurveDf = rbind(tempt1, tempt2)
learnCurveDf$chunkIdx[learnCurveDf$condition == "poor"] =
  learnCurveDf$chunkIdx[learnCurveDf$condition == "poor"] + 20
learnCurveDf %>% ggplot(aes(chunkIdx, mu)) +
  geom_line(aes(color = TimeSpentLag1)) +
  geom_ribbon(aes(ymin = mu - se, ymax = mu + se, fill = TimeSpentLag1), alpha = 0.5) + 
  facet_grid(~ ht) + myTheme + xlab("Run") +
  ylab("Acceptance") +
  geom_vline(xintercept = max(learnCurveDf$chunkIdx[learnCurveDf$condition == "rich"]), color = "grey", linetype = "dashed") +
  scale_x_continuous(breaks = c(10, 20, 30, 40), labels = c(10, 20, 10, 20),
                     limits = c(0, 42)) 


# detect the effect of previous payoff
data %>%  filter(trialIdx > 1 & trialEarningsLag1 > 0) %>% filter(!is.na(action)) %>%
  mutate(trialEarningsLag1 = as.factor(trialEarningsLag1)) %>%
  group_by(id, trialEarningsLag1, condition, ht) %>%
  summarise(pAccept = mean(action)) %>%
  group_by(trialEarningsLag1, condition, ht) %>%
  summarise(mu = mean(pAccept,  na.rm = T), se = sd(pAccept) / sqrt(sum(!is.na(pAccept)) - 1)) %>%
  ggplot(aes(trialEarningsLag1,  mu)) + geom_bar(stat = "identity") + facet_grid(condition~ht) +
  geom_errorbar(aes(trialEarningsLag1, ymin = mu - se, ymax = mu + se)) + myTheme + 
  xlab("Previous payoff") + ylab("Acceptance (%)")
ggsave("figures/pAccept_previous_payoff.png", width = 6, height = 4)

# effect of both 
data %>%  filter(trialIdx > 1 & trialEarningsLag1 > 0 & condition == "poor") %>%
  filter(!is.na(action)) %>%
  mutate(trialEarningsLag1 = as.factor(trialEarningsLag1), timeSpentLag1 = as.factor(timeSpentLag1)) %>%
  group_by(id, trialEarningsLag1, ht, timeSpentLag1) %>%
  summarise(pAccept = mean(action)) %>%
  group_by(trialEarningsLag1, timeSpentLag1, ht) %>%
  summarise(mu = mean(pAccept, na.rm = T),
            se = sd(pAccept) / sqrt(sum(!is.na(pAccept)) - 1),
            n = sum(!is.na(pAccept)))%>%
  ggplot(aes(trialEarningsLag1,  mu)) +
  geom_bar(stat = "identity") + facet_grid(timeSpentLag1~ht) +
  geom_errorbar(aes(trialEarningsLag1, ymin = mu - se, ymax = mu + se)) + myTheme + 
  geom_text(aes(trialEarningsLag1, label = n, y = 1.3)) +
  xlab("Previous payoff") + ylab("Acceptance (%)") + ylim(c(0, 1.5))
ggsave("figures/pAccept_previous_poor.png", width = 9, height = 4)


########################## reaction time ###################
# explor the time course of RT
# detect outliers 
RTdf = data  %>% group_by(condition, ht, id) %>% 
  mutate(isRTOutlier = responseRT > (median(responseRT, na.rm = T) + 1.5 * IQR(responseRT, na.rm = T)) |
           responseRT < (median(responseRT, na.rm = T) - 1.5 * IQR(responseRT, na.rm = T)))
RTdf1 = RTdf[moreFreqSelect,] %>% 
  filter(!is.na(action)) %>%
  group_by(id, ht, condition) %>%
  mutate(chunkIdx = chunkIdx + (seq_along(chunkIdx) - 1) %% 4 / 4)  %>%
  arrange(id, condition, ht)
RTdf2 =  RTdf[!moreFreqSelect, ] %>% filter(!is.na(action)) %>%
  arrange(id, condition, ht)
RTdf = rbind(RTdf1, RTdf2)

# only use commonly available data 
maxCommonChunk_ = c(
  min(sapply(1 : nSub, function(i) max(data$chunkIdx[data$id == ids[i] & data$condition == "rich"]))),
  min(sapply(1 : nSub, function(i) max(data$chunkIdx[data$id == ids[i] & data$condition == "poor"])))
)
meanRTdf = RTdf %>%
  filter(!isRTOutlier & !is.na(action)) %>% 
  mutate(Action = ifelse(action == 1, "Accept", "Reject")) %>%
  filter((condition == "rich" & chunkIdx <= 19) | (condition == "poor" & chunkIdx <= 15)) %>%
  group_by(condition, ht, chunkIdx, Action, id) %>%
  summarise(RT= mean(responseRT, na.rm = T)) %>%
  group_by(condition, ht, chunkIdx, Action) %>%
  summarise(RT= mean(RT,na.rm = T))
  


# plot reaction time as a function of run
RTdf %>% filter(!isRTOutlier & condition == "rich")  %>%
  ggplot(aes(chunkIdx, responseRT)) +
  geom_line(aes(color = Action)) +
  facet_grid(id ~ ht, scales = "free") +
  scale_color_manual(values = c("#4575b4", "#d73027")) +
  myTheme
ggsave("figures/RT_run_rich.png", width = 6, height = 4) 

RTdf %>% filter(!isRTOutlier & condition == "poor")  %>%
  ggplot(aes(chunkIdx, responseRT)) +
  geom_line(aes(color = Action)) +
  facet_grid(id ~ ht, scales = "free") +
  scale_color_manual(values = c("#4575b4", "#d73027")) +
  myTheme
ggsave("figures/RT_run_poor.png", width = 6, height = 4) 

meanRTdf %>% ggplot(aes(chunkIdx, RT, color = Action)) +
  geom_line() +
  xlab("Run num") + ylab("RT (s)") + facet_grid(condition ~ ht) +
  scale_color_manual(values = c("#4575b4", "#d73027")) + myTheme
ggsave("figures/RT_run_group.png", width = 6, height = 4)


############## payoff curve ############
data %>% ggplot(aes(responseTaskTime, cumEarnings)) +
  geom_line(aes(group = id, color = id))
timeStep = 0.15
tGrid = seq(0, blockSec * 2 - 0.15, by = timeStep)
payoffDf = data[,c("responseBlockTime", "trialEarnings", "condition", "id", "blockIdx")]
write.csv(payoffDf, file = "reference_group.csv")
