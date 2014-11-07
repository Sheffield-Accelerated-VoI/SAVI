# this file holds functions that help generate reactive text

# 1) Incremental costs
incCost <- function (costs){
  incCost <- (costs[, 2] - costs[, 1])
  incCost
}

# 2) Incremental effects
incBen <- function (effects){
  incBen <- (effects[, 2] - effects[, 1])  
  incBen
}

# 3) Mean Incremental Cost/Benefit
incValue <- function(data) {
  incValue <- format(mean(data), digits = 2, nsmall = 2)
  incValue
}

# 4) More/Less Detector (for costs and benefits)
moreLess <- function (data) {
  moreLess <- if (mean(data) > 0) {"more"}
              else {"less"}
  moreLess
}

# 5) Confidence intervals for cost effectiveness (value = e.g. 0.025 for 2.5th CI, 0.975 for 97.5th CI)
confIntCE <- function(data, value) {
  confIntCE <- format(quantile(data, value), digits = 4,  nsmall = 2)
  confIntCE
}

# 6) Probability cost saving
pCostsaving <- function(data) {
  pCostsaving <- format(length(which(data < 0)) / length(data), digits = 2, nsmall = 3)
  pCostsaving
}

# 7) Probability provides more benefits
pMoreben <- function(data) {
  pMoreben <- format(length(which(data > 0)) / length(data), digits = 2, nsmall = 3)
  pMoreben
}

# 8) ICER
iCER <- function(costs, bens) {
  iCER <- format(mean(costs[,2] - costs[,1]) / mean(bens[,2] - bens[,1]), digits = 2, nsmall = 2)
  iCER
}

# 9) Above/Below Detector (for ICER)
aboveBelow <- function(costs, bens, lambda) {
  aboveBelow <- if ((mean(costs[,2] - costs[,1]) / mean(bens[,2] - bens[,1])) > lambda) {"above"}
  else {"below"}
  aboveBelow
}

# 10) Would/Would not Detector (for ICER)
wouldNot <- function(costs, bens, lambda) {
  wouldNot <- if ((mean(costs[,2] - costs[,1]) / mean(bens[,2] - bens[,1])) > lambda) {"would not"}
  else {"would"}
  wouldNot
}

# 11) Probability cost-effective
  pCE <- function (costs, bens, lambda, nInt) {
  for (i in 1:nInt)
  pCEAC <- format(max(as.matrix(length(which((bens[,i] - bens[,1]) * lambda - (costs[,i] - costs[,1]) > 0)) / length(costs[,1]))), digits = 2, nsmall = 3)    
  pCEAC
}

# 12) Which most cost effective?
bestCE <- function(costs, bens, lambda, nInt) {
  for (i in 1:nInt)
  bestCE <- which.max(as.matrix(length(which((bens[,i] - bens[,1]) * lambda - (costs[,i] - costs[,1]) > 0)) / length(costs[,1]))) + 1 # 1 added on to account for lack of baseline row in calculation
  bestCE <- colnames(costs[bestCE])
  bestCE
}

# 13) Net Benefit costs
netBencosts <- function(costs, bens, lambda, nInt) {
  for (i in 1:nInt)
  netBencosts <- format(max(as.matrix(mean(bens[,i] * lambda - costs[,i]))), digits = 2, nsmall = 2)
  netBencosts
}

# 14) Net Benefit effects
netBeneffects <- function(costs, bens, lambda, nInt) {
  for (i in 1:nInt)
  netBeneffects <- format(max(as.matrix(mean(bens[,i] - (costs[,i] / lambda)))), digits = 2, nsmall = 2)
  netBeneffects
}

# 15) Which best strategy?
bestnetBen <- function(costs, bens, lambda, nInt) {
  for (i in 1:nInt)
  bestnetBen <- which.max(as.matrix(mean(bens[,i] * lambda - costs[,i]))) + 1 # 1 added on to account for lack of baseline row in calculation
  bestnetBen <- colnames(costs[bestnetBen])
  bestnetBen
}