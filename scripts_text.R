# this file holds functions that help generate reactive text

# 1) Incremental Cost
iCost <- function(costs.int) {
  iCost <- format(mean(costs.int[,2] - costs.int[,1]), digits = 2, nsmall = 2)
  iCost
}

# 2) More/Less