# this file holds functions that help generate reactive text

# 1) Incremental Cost/Benefit
incValue <- function(data) {
  incValue <- format(mean(data[,2] - data[,1]), digits = 2, nsmall = 2)
  incValue
}

# 2) More/Less Detector (for costs and benefits)
moreLess <- function(data) {
  moreLess <- if (mean(data[,2]) > mean(data[,1])) {"more"}
              else {"less"}
  moreLess
}

# 3) Confidence intervals for cost effectiveness (value = e.g. 0.025 for 2.5th CI, 0.975 for 97.5th CI)
confIntCE <- function(data, value) {
  confIntCE <- format(quantile((data[,2] - data[,1]), value), digits = 4,  nsmall = 2)
  confIntCE
}

# 4) Probability cost saving
pCostsaving <- function(data) {
  pCostsaving <- format(length(which((data[,2] - data[,1])<0)) / length(data[,1]), digits = 2, nsmall = 3)
  pCostsaving
}

# 5) Probability provides more benefits
pMoreben <- function(data) {
  pMoreben <- format(length(which((data[,2] - data[,1])>0)) / length(data[,1]), digits = 2, nsmall = 3)
  pMoreben
}

# 6) ICER
iCER <- function(costs, bens) {
  iCER <- format(mean(costs[,2] - costs[,1]) / mean(bens[,2] - bens[,1]), digits = 2, nsmall = 2)
  iCER
}

# 7) Above/Below Detector (for ICER)
aboveBelow <- function(costs, bens, lambda) {
  aboveBelow <- if ((mean(costs[,2] - costs[,1]) / mean(bens[,2] - bens[,1])) > lambda) {"above"}
  else {"below"}
  aboveBelow
}

# 8) Would/Would not Detector (for ICER)
wouldNot <- function(costs, bens, lambda) {
  wouldNot <- if ((mean(costs[,2] - costs[,1]) / mean(bens[,2] - bens[,1])) > lambda) {"would not"}
  else {"would"}
  wouldNot
}

# 9) Probability cost-effective
pCE <- function(costs, bens, lambda) {
  pCE <- format((length(which((bens[,2] - bens[,1]) * lambda - (costs[,2] - costs[,1]) > 0)) / length(costs[,1])), digits = 2, nsmall = 3)
  pCE
}

# 10) Net Benefit costs
netBencosts <- function(costs, bens, lambda, nInt) {
  for (i in 1:nInt)
  netBencosts <- format(mean(bens[,i] * lambda - costs[,i]), digits = 2, nsmall = 4)
  netBencosts
}

# 11) Net Benefit effects
netBeneffects <- function(costs, bens, lambda, nInt) {
  for (i in 1:nInt)
    netBeneffects <- format(mean(bens[,i] - (costs[,i] / lambda)), digits = 2, nsmall = 4)
  netBeneffects
}

# 12) Confidence intervals for net benefits costs (value = e.g. 0.025 for 2.5th CI, 0.975 for 97.5th CI)
confIntNBC <- function(costs, bens, lambda, nInt, value) {
  for (i in 1:nInt)
  confIntNBC <- format(quantile((bens[,i] * lambda - costs[,i]), value), digits = 4,  nsmall = 2)
  confIntNBC
}

# 13) Confidence intervals for net benefits effects (value = e.g. 0.025 for 2.5th CI, 0.975 for 97.5th CI)
confIntNBE <- function(costs, bens, lambda, nInt, value) {
  for (i in 1:nInt)
  confIntNBE <- format(quantile((bens[,i] - (costs[,i] / lambda)), value), digits = 4,  nsmall = 2)
  confIntNBE
}
