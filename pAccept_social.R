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
load("others.RData")

# simulation
set.seed(123)
dir.create("figures")


# simulate non_social data
nSub = 16
RLResults_ = list(length = nSub)
for(sIdx in 1 : nSub){
  # 
  htSeq_ = lapply(1 : nCondition, function(i) {
    condition = conditions[i]
    tempt = as.vector(replicate(nChunkMax, sample(hts_[[condition]], chunkSize)))
    tempt[1 : nTrialMax]
  })
  # create the rwd sequences in two conditions
  rwdSeq_ = lapply(1 : nCondition, function(i) {
    tempt = replicate(ceiling(nChunkMax/2), sample(rwds, chunkSize * 2))
    tempt = tempt[1 : nTrialMax]
  })
  # simulate 
  beta = runif(1, 0.005, 0.01)
  tau = runif(1, 3, 15)
  iniLongRunRate = runif(1, 0.02, 0.04)
  RLResults = RLSocial(beta, beta, tau, iniLongRunRate, htSeq_, rwdSeq_, blockSec)
  RLResults_[[sIdx]] =  RLResults 
}

# prepare a data.frame
dfList = lapply(1 : nSub, function(i){
  RLResults = RLResults_[[i]]
  action = ifelse(RLResults$spentHt > 0, 1, 0) # dependent variable 
  
  data = data.frame(
    subId = rep(i, length = length(action)),
    action = action, 
    ht = RLResults$scheduledHt,
    condition = RLResults$condition,
    trialEarnings = RLResults$trialEarnings,
    pastEarnings = c(NA, head(RLResults$trialEarnings, -1)),
    preAction  = c(NA, head(action, -1)),
    preSpentTime = c(NA, head(RLResults$spentHt + iti, -1)),
    preTrialEarningsOther = c(NA, head(RLResults$trialEarningsOther, -1)),
    blockTime = RLResults$blockTime
  )
})
data = bind_rows(dfList)
data = data[apply(data, MARGIN = 1, FUN = function(x) all(!is.na(x))),]


# plot the effect of hts and environments 
# generally our results look quit clearly, so we don't need to normalize our data across participants 
# and plot our data separately for different conditions.
# group level: make sure to average within each participant first. And use se across participants 
data %>% mutate(ht = as.factor(ht)) %>% group_by(ht, subId, condition) %>% summarise(pAccept = sum(action) / length(action)) %>%
  group_by(ht, condition) %>%
  summarise(mu = mean(pAccept),
            se = sd(pAccept) / sqrt(length(pAccept)),
            min = mu - se,
            max = mu + se) %>%
  as.data.frame() %>%
  mutate(ht = as.factor(ht)) %>%
  ggplot(aes(ht, mu, fill = condition)) +
  geom_bar(stat = "identity", position = 'dodge') +
  geom_errorbar(aes(ymin = min, ymax = max), position = position_dodge(0.9), width = 0.5) +
  xlab("Handling time (s)") + ylab("Acceptance (%)") + myTheme +
  scale_fill_manual(values = c("#9ecae1", "#ffeda0"))
# participant level, use adjusted S.E.s since S.E = 0 sometimes for binary data 
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

# Examine the effect of hts and environments formally in logistic regression 
# We don't control more nusiance factors since these two effects are quit robust.
# Additionally, overall our experiments are well designed and counterbalanced 
# For example, reward outcomes are counterbalanced for accepted trials. Also, even though
# decision-related and sequence-related variables, like the actions, rewards and outcomes of previous trials,
# could not be perfectly counterbalanced, we have quite a lot of data so they are nearly counterbalanced.
## group level
fit1 = glmer(action ~ ht + condition + (1 | subId), family = binomial,
             data)
summary(fit1)
## participant level 
## since we have less data on the participant level and have quasi completely segment issues, 
## we use logistf which adds priors to the logistic regression
## we use logistf to h which adds a piror to the logistic regression. 
fit1 = logistf(action ~ ht + condition, data[data$subId == 1, ])
summary(fit1)


# the effect of rewards 
# again, the effect looks quite clear here so we don't do any across-participant normalization 
# and conditional plots. 
data %>% filter(data$preAction == 1) %>% 
  group_by(pastEarnings, subId) %>% 
  summarise(pAccept = mean(action)) %>%
  group_by(pastEarnings) %>% 
  summarise(
    mu = mean(pAccept),
    se = sd(pAccept) / sqrt(nSub),
    min = mu - se,
    max = mu + se
  )%>%
  ggplot(aes(as.factor(pastEarnings), mu)) +
  geom_bar(stat = "identity", fill = "#767676") +
  geom_errorbar(aes(ymin = min, ymax = max), width = 0.3 ) +
  myTheme + xlab("Previous reward") + ylab("Acceptance (%)")
# participant level 
# here we need to plot conditional figures.Compare the difference below
data %>% dplyr::filter(subId == 1 & (data$preAction == 1)) %>% 
  group_by(pastEarnings) %>%
  summarise(mu = sum(action) / length(action),
            n = length(action),
            muAdj = (sum(action) + 0.5) / (n + 1),
            se = sqrt((1 - muAdj) * muAdj) / (n + 1),
            min = mu - se,
            max = mu + se) %>%
  ggplot(aes(as.factor(pastEarnings), mu)) +
  geom_bar(stat = "identity", fill = "#767676") +
  geom_errorbar(aes(ymin = min, ymax = max), width = 0.3 ) +
  myTheme + xlab("Previous reward") + ylab("Acceptance (%)") 
data %>% dplyr::filter(subId == 1 & (data$preAction == 1)) %>% 
  group_by(pastEarnings, ht) %>%
  summarise(mu = sum(action) / length(action),
            n = length(action),
            muAdj = (sum(action) + 0.5) / (n + 1),
            se = sqrt((1 - muAdj) * muAdj) / (n + 1),
            min = mu - se,
            max = mu + se) %>%
  ggplot(aes(as.factor(pastEarnings), mu)) +
  geom_bar(stat = "identity", fill = "#767676") +
  geom_errorbar(aes(ymin = min, ymax = max), width = 0.3 ) +
  myTheme + xlab("Previous reward") + ylab("Acceptance (%)") + facet_grid(~ht)

# examine the effect of rewards in regression
fit2 = glmer(action ~ pastEarnings + condition + ht + (1 | subId), data[data$preAction == 1,],
             family = "binomial")
summary(fit2)
fit2 = logistf(action ~ pastEarnings + condition + ht, data[data$subId == 1 & data$preAction == 1,])
summary(fit2) # 

# 
data %>% filter((preSpentTime < 50) & (blockTime > 120)) %>%
  group_by(condition, preSpentTime, subId, preTrialEarningsOther) %>%
  summarise(pAccept = sum(action) / length(action)) %>% 
  group_by(preTrialEarningsOther, condition, preSpentTime) %>%
  summarise(mu = mean(pAccept),
  n = length(pAccept),
  se = sd(pAccept) / sqrt(length(pAccept)),
  min = mu - se,
  max = mu + se) %>% filter(n > 1) %>%
  ggplot(aes(preTrialEarningsOther, mu)) +
  geom_bar(stat = "identity", fill = "#767676") +
  geom_errorbar(aes(ymin = min, ymax = max), width = 0.3 ) +
  myTheme + ylab("Acceptance (%)") + facet_grid(preSpentTime~condition) +
  xlab("Others'reward previously")
ggsave("figures/others_payment_13.png", width = 4, height = 3)



## probably regression is still the best way, just controlling everything. If it is well balanced
## and we assume no interactions, sometimes we don't meed to control it
## if it is not significant, then probably do some grouping. like make continous measurement binary 
## and maybe look at interactions more carefully. 
# no need for grouping

fit3 = logistf(action ~ condition + preSpentTime + preTrialEarningsOther + ht + pastEarnings + (1 | subId), family = "binomial", data)   
summary(fit3)

# can we use for a single participant 
# we always need to control the condition, since different condition have different preSpentTime
# we need to control ht, since even for the other earnins, ht are balanced, yet it is not
# for condition
# the only concern is that corlinearity, right. 

# here, as I add the preSpentTime, the effect of the condition was washed away
# since there is definitly quite high convariance 
# ht also change with condition a little bit ,but the convariance is not that high
# there is some convariance so it might wash something away, and it is ok 
fit3 = logistf(action ~  condition + ht + pastEarnings + preTrialEarningsOther + preSpentTime,
               data =  data[data$subId  == 1,]) 
summary(fit3)




