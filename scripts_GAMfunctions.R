# Copyright (c) 2014, 2015 the SAVI authors (see AUTHORS.txt).
# Licensed under the BSD 3-clause license (see LICENSE.txt)


###############################
# EVPPI single parameters Tab # 
# EVPPI groups Tab            #
###############################


# This function estimates EVPI and SE via GAM
# S is the simulation size for the Monte Carlo computation of SE
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


# This function for getting SE for a GAM fit. NOT USED AT PRESENT
# works for a single decision option scenario with incremental net benefits

calculateSEforGAM <- function(gam.obj, N=1000) {
  #######################################################################
  ## calculates SE of evsi / evpi obtained from GAM method
  ## this works for two decision problem, where the inb has been modelled
  ## 
  ## arguments:
  ## 	gam.obj: is GAM object
  ## 	N: is number of samples from MVN for empirical SE calculation
  ##
  ## returns:
  ##	SE: the standard error
  ########################################################################
  
  Xp <- predict(gam.obj, type="lpmatrix", unconditional = TRUE)
  mu <- coef(gam.obj)
  V <- gam.obj$Vp
  
  n <- nrow(Xp)
  
  samp <- mvrnorm(N, mu, V)
  
  random.mu <- samp%*%t(Xp)
  
  sample.set <- matrix(pmax(0, random.mu), nrow=N)
  evppi.samples <- rowMeans(sample.set)
  
  rm(sample.set); gc()
  
  baselines <- pmax(0, rowMeans(random.mu))
  evppi <- evppi.samples - baselines
  
  SE <- sd(evppi)
  
  return(SE=SE)
}





