# Copyright (c) 2014, the SAVI authors (see AUTHORS.txt).
# Licensed under the BSD 3-clause license (see LICENSE.txt)

##############################################################################
## This function estimates EVPI and SE via GAM
## S is the simulation size for the Monte Carlo computation of SE
##############################################################################

gamFunc <- function(NB, sets, s=1000, cache, session) {
  
  if(!is.null(dim(NB))) {
    NB <- NB - NB[, 1]
  } else {
    NB <- cbind(0, NB)
  }
  
  D <- ncol(NB)
  N <- nrow(NB)
  g.hat <- beta.hat <- Xstar <- V <- tilde.g <- vector("list", D)
  g.hat[[1]] <- 0
  
  input.parameters <- cache$params
  paramSet <- cbind(cbind(input.parameters)[, sets, drop=FALSE])
  
  constantParams <- (apply(paramSet, 2, var) == 0)

  if (sum(constantParams) == length(sets)) return(list(EVPI=0, SE=0)) # if all regressors are constant
  if (sum(constantParams) > 0) sets <- sets[-which(constantParams)] # remove constants
  
  # check for linear dependence and remove 
  paramSet <- cbind(cbind(input.parameters)[, sets, drop=FALSE]) # now with constants removed
  rankifremoved <- sapply(1:NCOL(paramSet), function (x) qr(paramSet[,-x])$rank)
  while(length(unique(rankifremoved)) > 1) {
    linearCombs <- which(rankifremoved == max(rankifremoved))
    # print(linearCombs)
    print(paste("Linear dependence: removing column", colnames(paramSet)[max(linearCombs)]))
    paramSet <- cbind(paramSet[, -max(linearCombs), drop=FALSE])
    sets <- sets[-max(linearCombs)]
    rankifremoved <- sapply(1:NCOL(paramSet), function (x) qr(paramSet[,-x])$rank)
  }
  while(qr(paramSet)$rank == rankifremoved[1]) { # special case only lincomb left
    print(paste("Linear dependence: removing column", colnames(paramSet)[1]))
    paramSet <- cbind(paramSet[, -1, drop=FALSE]) 
    sets <- sets[-1]
    rankifremoved <- sapply(1:NCOL(paramSet), function (x) qr(paramSet[,-x])$rank)
  }
  
  regression.model <- formulaGenerator(colnames(input.parameters)[sets])
  
  
  progress <- shiny::Progress$new(session, min=1, max=D-1)
  on.exit(progress$close())
  progress$set(message = 'Calculating conditional expected net benefits',
               detail = 'Please wait...')
  
  for(d in 2:D) {
    progress$set(value = d-1)
    print(paste("estimating g.hat for incremental NB for option", d ,"versus 1"))
    dependent <- NB[, d]
    f <- update(formula(dependent~.), formula(paste(".~", regression.model)))
    model <- gam(f, data=data.frame(input.parameters)) 
    g.hat[[d]] <- model$fitted
    beta.hat[[d]] <- model$coef
    Xstar[[d]] <- predict(model,type="lpmatrix")
    V[[d]] <- model$Vp
  }
  
  perfect.info <- mean(do.call(pmax, g.hat)) 
  baseline <- max(unlist(lapply(g.hat, mean)))
  partial.evpi <- perfect.info - baseline ## estimate EVPI
  rm(g.hat); gc()
  
  print("computing standard error via Monte Carlo")
  for(d in 2:D) {
    sampled.coef <- mvrnorm(s, beta.hat[[d]], V[[d]])
    tilde.g[[d]] <- sampled.coef%*%t(Xstar[[d]])  
  }
  
  tilde.g[[1]] <- matrix(0, nrow=s, ncol=N)
  rm(V, beta.hat, Xstar, sampled.coef);gc()
  
  sampled.perfect.info <- rowMeans(do.call(pmax, tilde.g))
  sampled.baseline <- do.call(pmax, lapply(tilde.g, rowMeans)) 
  rm(tilde.g); gc()
  sampled.partial.evpi <- sampled.perfect.info - sampled.baseline
  SE <- sd(sampled.partial.evpi)
  #upward.bias <- mean(sampled.partial.evpi) - partial.evpi
  
  return(list(EVPI=partial.evpi, SE=SE)) #,upward.bias=upward.bias))
  
}

# This function generates the GAM model formulas from the list of parameter names

formulaGenerator <- function(namesList) {
  form <- paste(namesList, ",", sep="", collapse="")
  form <- substr(form, 1, nchar(form) - 1)
  if (length(namesList) == 4) {
    form <- paste("te(", form, ",k=4)", sep="") # restrict to 4 knots if 4 params
  } else {
    form <- paste("te(", form, ")", sep="")    
  }
  form
}

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

