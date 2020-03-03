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
# without a prior yet with random effects
data$ht = factor(data$scheduledHt)
data$condition = ifelse(data$blockIdx == 1, "rich", "poor")
data$responseTaskTime = data$responseBlockTime
data$action = ifelse(data$trialEarnings == 0, 0, 1)
data$Action = ifelse(data$trialEarnings == 0, "Reject", "Accept")
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
                            group_map(~ cumsum(.x$trialEarnings)))


################  payoff curve ######################



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
