source("RL.R")
source("RLSocial.R")
library("ggplot2")
library("plyr")
library("dplyr")
library("tidyr")
library("logistf")
library("lme4")
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


################# variable effects ###############
# without a prior yet with random effects
data$condition = ifelse(data$blockIdx == 1, "rich", "poor")
data$preTrialEarnings = c(NA, head(data$trialEarnings, -1))
data$action = data$trialEarnings > 0
data$timeSpent = ifelse(data$action, data$scheduledHt + iti, 0 + iti)
data$preTimeSpent = c(NA, head(data$timeSpent, -1))
fitData = data %>% filter(id != "204" & scheduledHt > min(unqHts) & scheduledHt < max(unqHts))
fit = glmer(action ~ condition + preTimeSpent + scheduledHt + preTrialEarnings + (1 | id), family = "binomial",fitData)   
summary(fit)

# with a prior yet without random effects 
fit = logistf(action ~ condition + preTimeSpent + scheduledHt + preTrialEarnings, family = "binomial",fitData)   
summary(fit)

# with a prior and random effects 
fitDataShort = fitData %>% group_by(id, condition, preTimeSpent, scheduledHt, preTrialEarnings) %>%
  dplyr::summarise(pAccept = mean(action),
            n = length(action),
            pAcceptAdj = (sum(action) + 0.5) / (length(action) + 1),
            nAdj = length(action) +1)
fit = glmer(pAcceptAdj ~ condition + preTimeSpent + scheduledHt + preTrialEarnings + (1 | id), family = "binomial",fitDataShort, weights = nAdj)   
summary(fit)

########################## 
