##############################################################################
## This function estimates EVPI and SE via GAM
## S is the simulation size for the Monte Carlo computation of SE
##############################################################################

gamFunc <- function(NB, sets, s=1000, cache) {
  
  if(!is.null(dim(NB))) 
  {
    NB <- NB-NB[, 1]
  }
  else
  {
    NB <- cbind(0, NB)
  }
  
  D <- ncol(NB)
  N <- nrow(NB)
  g.hat <- beta.hat <- Xstar <- V <- tilde.g <- vector("list", D)
  g.hat[[1]] <- 0
  
  input.parameters <- get("params", envir=cache)
  regression.model <- formulaGenerator(sets)
  
  for(d in 2:D)
  {
    print(paste("estimating g.hat for incremental NB for option",d,"versus 1"))
    f <- update(formula(NB[,d]~.), formula(paste(".~", regression.model)))
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
  
  print("computing standard error and upward bias via Monte Carlo")
  for(d in 2:D)
  {
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

formulaGenerator <- function(namesList) {
  form <- paste(namesList, ",", sep="", collapse="")
  form <- substr(form, 1, nchar(form) - 1)
  form <- paste("te(", form, ")", sep="")
  form
}
