ids = c("101", "102", "103", "104")

data = read.table(sprintf("data/101.csv", id), header = T)
data$id = "101"
for(i in 2:4){
  id = ids[i]
  thisTrialData = read.csv(sprintf("data/%s.csv", id), header = T)
  thisTrialData = thisTrialData[,1:9]
  thisTrialData$id = id
  data = rbind(data, thisTrialData)
}


fitData = data %>% filter(scheduledHt == 22 | scheduledHt == 28) 
fitData$condition = ifelse(fitData$blockIdx == 1, "rich", "poor")
fitData$preTrialEarnings = c(NA, head(fitData$trialEarnings, -1))
fitData$action = fitData$trialEarnings > 0
fitData$timeSpent = ifelse(fitData$action, fitData$scheduledHt + iti, 0 + iti)
fitData$preTimeSpent = c(NA, head(fitData$timeSpent, -1))
fit = glmer(action ~ condition + preTimeSpent + scheduledHt + preTrialEarnings + (1 | id), family = "binomial",fitData)   
summary(fit)


fit = logistf(action ~ condition + preTimeSpent + scheduledHt + preTrialEarnings, family = "binomial",fitData)   
summary(fit)


