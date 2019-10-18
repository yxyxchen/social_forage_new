source("RL.R")
source("RLSocial.R")
library("ggplot2")
library("dplyr")
library("tidyr")
library("lme4")
source("subFxs/plotThemes.R")

# load expParas
load("expParas.RData")

# output dir 
dir.create("figures")

# read in parameters 
para = read.csv("para.csv")

# reward constants 
ctxRwd = 40.1
nCtx = 6
rwds = c(probRwds, rep(ctxRwd, nCtx)) # all possible rewards 
unqRwds = unique(rwds) # all unique rewards
set.seed(123)
rwd_ = as.vector( replicate(nChunkMax, sample(rwds)) )
rwd_ = rwd_[1 : nTrialMax]

# simulate non_social data
nSub = 64
RLResults_ = list(length = nSub)
for(sIdx in 1 : nSub){
  beta = runif(1, 0.01, 0.2)
  tau = runif(1, 0.1, 2)
  iniLongRunRate = runif(1, 3 / ht, 18 / ht)
  thisResults = RL(beta, tau, iniLongRunRate, ctxRwd, rwd_)
  RLResults_[[sIdx]] = thisResults
}


# prepare data
dfList = lapply(1 : nSub, function(i) {
  RLResults = RLResults_[[i]]
  action = ifelse(RLResults$requiredHt == ht, 1, 0)
  pastEarnings1 = c(NA, head(RLResults$rwd, -1))
  data = data.frame(
    action,
    pastEarnings1,
    rwd = RLResults$rwd
  )
}
)
data = bind_rows(dfList)
data = data[apply(data, MARGIN = 1, FUN = function(x) all(!is.na(x))),]

# summarise pAccept for different reward sizes and different past earnings
sumData = data %>% 
  group_by(rwd, pastEarnings1) %>% summarise(sum(action) / length(action))
colnames(sumData) = c("rwd", "pastEarnings1", "pAccept")

# regression with reward sizes 
fit1 = glm(pAccept~rwd,
           data = sumData, family = binomial)
summary(fit1)

# plot the effect of reward sizes 
x = seq(0, max(rwds), by = 1)
fitData = data.frame(
  x,
  y = exp(fit1$coefficients[1] + fit1$coefficients[2] * x) / 
    (1 + exp(fit1$coefficients[1] + fit1$coefficients[2] * x))
)
data %>% group_by(rwd) %>% summarise(pAccept = sum(action) / length(action)) %>%
  ggplot(aes(rwd, pAccept)) + geom_point() + myTheme + 
  geom_line(data = fitData, aes(x, y), linetype = "dashed")

# plot the effect of reward sizes 
sumData %>% ggplot(aes(factor(pastEarnings1), pAccept)) +
  geom_bar(stat = "identity") + facet_wrap(~rwd) + myTheme +
  xlab("Last trial earnings")
