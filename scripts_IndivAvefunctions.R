# Copyright (c) 2015 the SAVI authors (see AUTHORS.txt).
# Licensed under the BSD 3-clause license (see LICENSE.txt)

# this file holds the MARS functions to average out patient level variability
# UNDER TESTING

getModelledCostsAndEffects <- function(cache, session) {
  print("Computing modelled costs and effects")
  p <- NCOL(cache$uploadedCosts)
  # cache$modelledCosts <- fitFullModelMARS(cache$uploadedCosts, cache, session)
  # cache$modelledEffects <- fitFullModelMARS(cache$uploadedEffects, cache, session)  
  fits <- fitFullModelMARS(data.frame(cache$uploadedCosts, cache$uploadedEffects), cache, session)
  cache$modelledCosts <- fits[, 1:p]
  cache$modelledEffects <- fits[, (p+1):(2*p)]
  rm(fits)
}

applyEarth <- function(y, inputs) { 
  if(var(y) == 0 & sum(y) == 0) return(rep(0, length(y)))
  earth(inputs, y, degree = 5, fast.k=5)$fitted
}


fitFullModelMARS <- function(outcomeVar, cache, session) {
  input.parameters <- as.matrix(cache$params[, ,drop=FALSE])
  
  paramSet <- cbind(input.parameters)
  sets <- colnames(input.parameters)
  
  constantParams <- (apply(paramSet, 2, var) == 0)
  
  if (sum(constantParams) == NCOL(paramSet)) { # if all regressors are constant
    modelFitted <- matrix(rep(apply(outcomeVar, 2, mean), each = N), ncol = D)
    return(modelFitted)
  } 
  
  if (sum(constantParams) > 0) sets <- sets[-which(constantParams)] # remove constants
  
  # check for linear dependence and remove 
  paramSet <- cbind(cbind(input.parameters)[, sets, drop=FALSE]) # now with constants removed
  rankifremoved <- sapply(1:NCOL(paramSet), function (x) qr(paramSet[,-x])$rank)
  while(length(unique(rankifremoved)) > 1) {
    linearCombs <- which(rankifremoved == max(rankifremoved))
    print(linearCombs)
    print(paste("Linear dependence: removing column", colnames(paramSet)[max(linearCombs)]))
    paramSet <- cbind(paramSet[, -max(linearCombs), drop=FALSE])
    # sets <- sets[-max(linearCombs)] # don't need this
    rankifremoved <- sapply(1:NCOL(paramSet), function (x) qr(paramSet[,-x])$rank)
  }
  
  while(qr(paramSet)$rank == rankifremoved[1]) {
    print(paste("Linear dependence... removing column", colnames(paramSet)[1]))
    paramSet <- cbind(paramSet[, -1, drop=FALSE]) # special case only lincomb left
    rankifremoved <- sapply(1:NCOL(paramSet), function (x) qr(paramSet[,-x])$rank)
  }
  
  
  print(head(outcomeVar))
  p <- NCOL(outcomeVar)
  names(outcomeVar) <- 1:p
  print(head(outcomeVar))
  print(head(paramSet))
  
  fits <- lapply(as.data.frame(outcomeVar), applyEarth, paramSet)
  fits <- as.data.frame(fits)
  names(fits) <- rep(cache$namesDecisions, 2)
  print(head(fits))
  fits
}
