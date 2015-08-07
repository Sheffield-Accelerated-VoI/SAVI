# Copyright (c) 2014, 2015 the SAVI authors (see AUTHORS.txt).
# Licensed under the BSD 3-clause license (see LICENSE.txt)

# this file holds the table generating functions

###################
# PSA Results tab #
###################

# Table of Key Cost-Effectiveness Statistics
makeTableCePlane <- function(lambda, comparator, cache) {
  
  costs <- cache$costs
  effects <- cache$effects
  comp <- which(cache$namesDecisions%in%comparator)
  incCost <- (costs - costs[, comp])[, -comp, drop=FALSE]
  incBen <- (effects - effects[, comp])[, -comp, drop=FALSE]
  inb <- incBen * lambda - incCost
  npsa <- NROW(costs)
  
  tabCePlane <- matrix(NA, ncol=ncol(costs) - 1, nrow = 13) # incremental, no zero column
  tabCePlane[1, ]  <- format(lambda, digits=4, nsmall = 0)
  tabCePlane[2, ]  <- colnames(cache$uploadedCosts)[comp]
  tabCePlane[3, ]  <- format(npsa)
  tabCePlane[4, ]  <- format(colMeans(incBen), digits=2, nsmall=2)
  tabCePlane[5, ]  <- format(colMeans(incCost), digits=2, nsmall=2)
  tabCePlane[6, ]  <- format(colMeans(incCost) /  colMeans(incBen), digits=2, nsmall=2)
  tabCePlane[7, ]  <- format(apply(incBen, 2, quantile, 0.025), digits=2, nsmall=2)
  tabCePlane[8, ]  <- format(apply(incBen, 2, quantile, 0.975), digits=2, nsmall=2)
  tabCePlane[9, ]  <- format(apply(incCost, 2, quantile, 0.025), digits=2,  nsmall=2)
  tabCePlane[10, ] <- format(apply(incCost, 2, quantile, 0.975), digits=2, nsmall=2)
  tabCePlane[11, ] <- format(apply(incCost, 2, function(x) sum(x < 0)) / npsa, digits=2, nsmall=2)
  tabCePlane[12, ] <- format(apply(incBen, 2, function(x) sum(x > 0)) / npsa, digits=2, nsmall=2)
  tabCePlane[13, ] <- format(apply(inb, 2, function(x) sum(x > 0)) / npsa, digits=2, nsmall=2)
  
  colnames(tabCePlane) <- colnames(cache$uploadedCosts)[-comp]
  tabCePlane
}


# Summary of Absolute Net Benefit Statistics
makeTableNetBenefit <- function(costs.int, effects.int, lambda, nInt) {
  
  tabNetBenefit <- matrix(NA, ncol= nInt, nrow = 8) 
  
  for (i in 1:nInt) {
    tabNetBenefit[1,i] <- format(mean(effects.int[,i]), digits=2, nsmall=4)
    tabNetBenefit[2,i] <- format(mean(costs.int[,i]), digits=2, nsmall=2)
    tabNetBenefit[3,i] <- format(mean(effects.int[,i] * lambda - costs.int[,i]), digits=2, nsmall=2)
    tabNetBenefit[4,i] <- format(quantile(effects.int[,i] * lambda - costs.int[,i], 0.025), digits=2, nsmall=2)
    tabNetBenefit[5,i] <- format(quantile(effects.int[,i] * lambda - costs.int[,i], 0.975), digits=2, nsmall=2) 
    tabNetBenefit[6,i] <- format(mean(effects.int[,i] - (costs.int[,i] / lambda)), digits=2, nsmall=4)
    tabNetBenefit[7,i] <- format(quantile(effects.int[,i] - (costs.int[,i] / lambda), 0.025), digits=2, nsmall=4)
    tabNetBenefit[8,i] <- format(quantile(effects.int[,i] - (costs.int[,i] / lambda), 0.975), digits=2, nsmall=4)
  }
  
  colnames(tabNetBenefit) <- colnames(costs.int)
  tabNetBenefit
}








####################
# EVPPI groups tab #
####################


# function for building up table of parameter sets for partial EVPI
buildSetStoreTable <- function(store, groupPartialEvpi, cache) {
  
  if (is.null(cache$overallEvpi)) {
    cache$overallEvpi <- calcEvpi(cache$costs, cache$effects, 
                          lambda=cache$lambdaOverall)
  }
  groups <- sapply(store, function(x) {
    output <- paste(x, "+", sep="", collapse="")
    output <- substr(output, 1, nchar(output) - 1)
    output})
  df <- data.frame(groups, groupPartialEvpi)
  df$indexed <- as.numeric(groupPartialEvpi[, 1]) / cache$overallEvpi
  df$annprev <- as.numeric(groupPartialEvpi[, 1]) * cache$annualPrev
  df$horizon <- as.numeric(groupPartialEvpi[, 1]) * cache$annualPrev * cache$horizon
  rownames(df) <- c(paste("Set", 1:(length(store))))
  colnames(df) <- c("Parameters", paste("Per Person EVPPI (", cache$currency, ")", sep=""), 
    "Approx Standard Error","Indexed to Overall EVPI", 
    paste("EVPPI for ", cache$jurisdiction, " Per Year (", cache$currency, ")", sep=""), 
    paste("EVPPI for ", cache$jurisdiction, " over ", cache$horizon, " years (", cache$currency, ")", sep=""))
  as.matrix(df)
}

bold.allrows <- function(x) {
  h <- paste('<strong>', x ,'</strong>', sep ='')
  h
}
