# Copyright (c) 2015 the SAVI authors (see AUTHORS.txt).
# Licensed under the BSD 3-clause license (see LICENSE.txt)

# this file holds the functions to average out patient level variability



###################
# PSA Results Tab #
# EVPI Tab        #
###################

# THESE ARE THE GAM-BASED INDIV LEVEL AVERAGING FUNCTIONS

# This function builds a GAM model of all the parameters and returns the fitted values - 
# to get the CEAC and overall EVPI when the indiv sim box in tixed.


getModelledCostsAndEffects <- function(cache, session) {
  print("Computing modelled costs and effects")
  cache$modelledCosts <- fitFullModel(cache$uploadedCosts, cache, session)
  cache$modelledEffects <- fitFullModel(cache$uploadedEffects, cache, session)  
}


fitFullModel <- function(outcomeVar, cache, session) {
  
  D <- ncol(outcomeVar)
  N <- nrow(outcomeVar)
  modelFitted <- matrix(0, ncol=D, nrow=N)
  
  input.parameters <- cache$params[, ,drop=FALSE]
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
  
  
  regression.model <- formulaGeneratorAdditive(colnames(paramSet))
  
  progress <- shiny::Progress$new(session, min=1, max=D-1)
  on.exit(progress$close())
  progress$set(message = 'Averaging over patients',
               detail = 'Please wait...')
  
  for(d in 1:D) {
    
    progress$set(value = d)
    print(paste("estimating fitted for option", d))
    dependent <- outcomeVar[, d]
    f <- update(formula(dependent~.), formula(paste(".~", regression.model)))
    model <- gam(f, data=data.frame(input.parameters)) 
    modelFitted[, d] <- model$fitted
  }  
  colnames(modelFitted) <- colnames(outcomeVar)
  modelFitted 
  
}


formulaGeneratorAdditive <- function(namesList) {
  regModel <- NULL
  for (i in namesList) {
    regModel <- paste("te(", i, ")+", regModel, sep="")
  }
  strtrim(regModel, nchar(regModel)-1)
}
