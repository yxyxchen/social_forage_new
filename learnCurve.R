source("social_nonSocial.R")
library("dplyr")
library("tidyr")

# load expPara
load("expParas.RData")

# create the ht sequences in two conditions


# shuffle all possive rewards to generate the reward sequence
# call the function
social_nonSocial()
ggsave("learn_curve.png", width = 10, height = 3)

