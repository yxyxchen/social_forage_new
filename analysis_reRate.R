source("social_nonSocial_reRate.R")

# learning parameters
beta = 0.01
beta_self = 0.01
beta_other = 0.01
tau = 20
ctxRwd = 40

# reward sequences 
nChunk = 10
nCtx = 6
probRwds = seq(3, 18, by = 3) # possible rewards in prob trials 
rwds = c(probRwds, rep(ctxRwd, nCtx)) # all possible rewards 
set.seed(123)
# rwd_ = pmax(as.vector( replicate(nChunk, sample(rwds + sapply(1 : length(rwds), function(i) rnorm(1, 0, rwds[i] * 0.1))))), 0) # shuffle all possive rewards to generate the reward sequence
rwd_ = as.vector( replicate(nChunk, sample(rwds, length(rwds))))

# the effect of beta 
set.seed(231)
social_nonSocial(0.001, 0.001, 0.01, tau, ctxRwd, rwd_)

# the effect of beta
social_nonSocial(0.01, 0.01, beta_other, tau, ctxRwd, rwd_)
