# this file holds the table generating functions

makeTableCePlane <- function(costs.int, effects.int, lambda) {
  incCost <- costs.int[, 2] - costs.int[, 1]
  incBen <- effects.int[, 2] - effects.int[, 1]
  npsa <- length(costs.int[, 1])
  tableCePlane <- matrix(NA, ncol=ncol(costs.int)-1, nrow = 13)
  tableCePlane[1] <- format(lambda, digits=4, nsmall = 0)
  tableCePlane[2] <- colnames(costs.int)[1]
  tableCePlane[3] <- format(npsa)
  tableCePlane[4] <- format(mean(incBen), digits=2, nsmall=4)
  tableCePlane[5] <- format(mean(incCost), digits=2, nsmall=2)
  tableCePlane[6] <- format(mean(incCost) /  mean(incBen), digits=2, nsmall=2)
  tableCePlane[7] <- format(quantile((incBen), 0.025), digits=4, nsmall=4)
  tableCePlane[8] <- format(quantile((incBen), 0.975), digits=4, nsmall=4)
  tableCePlane[9] <- format(quantile((incCost), 0.025), digits=4,  nsmall=2)
  tableCePlane[10] <- format(quantile((incCost), 0.975), digits=4, nsmall=2)
  tableCePlane[11] <- format(length(which((incCost) < 0)) / npsa, digits=2, nsmall=3)
  tableCePlane[12] <- format(length(which((incBen) > 0)) / npsa, digits=2, nsmall=3)
  tableCePlane[13] <- format(length(which((incBen) * lambda - (incCost)>0)) / npsa, digits=2,
                             nsmall=3)
  colnames(tableCePlane) <- colnames(costs.int)[2]
  tableCePlane
}

makeTableNetBenefit <- function(costs.int, effects.int, lambda, nInt) {
  
  tableNetBenefit <- matrix(NA, ncol= nInt, nrow = 8) 
  for (i in 1:nInt) {
    tableNetBenefit[1,i] <- format(mean(effects.int[,i]), digits=2, nsmall=4)
    tableNetBenefit[2,i] <- format(mean(costs.int[,i]), digits=2, nsmall=2)
    tableNetBenefit[3,i] <- format(mean(effects.int[,i] * lambda - costs.int[,i]), digits=2, nsmall=2)
    tableNetBenefit[4,i] <- format(quantile(effects.int[,i] * lambda - costs.int[,i], 0.025), digits=2, nsmall=2)
    tableNetBenefit[5,i] <- format(quantile(effects.int[,i] * lambda - costs.int[,i], 0.975), digits=2, nsmall=2) 
    tableNetBenefit[6,i] <- format(mean(effects.int[,i] - (costs.int[,i] / lambda)), digits=2, nsmall=4)
    tableNetBenefit[7,i] <- format(quantile(effects.int[,i] - (costs.int[,i] / lambda), 0.025), digits=2, nsmall=4)
    tableNetBenefit[8,i] <- format(quantile(effects.int[,i] - (costs.int[,i] / lambda), 0.975), digits=2, nsmall=4)
  }
  colnames(tableNetBenefit)<-colnames(costs.int)
  tableNetBenefit
}



# function for building up table of parameter sets for partial EVPI
buildSetStoreTable <- function(store) {
  maxRows <- max(unlist(lapply(store, length)))
  tableRows <- lapply(store, function(x) c(x, rep("", maxRows - length(x))))
  df <- t(data.frame(tableRows))
  rownames(df) <- paste("Set", 1:length(store))
  colnames(df) <- NULL
  names(df) <- NULL
  df
}

bold.allrows <- function(x) {
  h <- paste('<strong>',x,'</strong>', sep ='')
  h
}
