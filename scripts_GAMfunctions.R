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
  paramSet <- cbind(input.parameters[, sets])
  constantParams <- (apply(paramSet, 2, var) == 0)
  # to sort
#   if (sum(constantParams) == length(sets)) return(0) # if all regressors are constant
#   
#   if (sum(constantParams) > 0) sets <- sets[-which(constantParams)] # remove constants
#   
#   # check for linear dependence and remove 
#   paramSet <- cbind(params[, sets])
#   rankifremoved <- sapply(1:NCOL(paramSet), function (x) qr(paramSet[,-x])$rank)
#   while(length(unique(rankifremoved)) > 1) {
#     linearCombs <- which(rankifremoved == max(rankifremoved))
#     # print(linearCombs)
#     print(paste("Linear dependence: removing column", colnames(params)[max(linearCombs)]))
#     params <- params[, -max(linearCombs)]
#     rankifremoved <- sapply(1:ncol(params), function (x) qr(params[,-x])$rank)
#   }
#   
  regression.model <- formulaGenerator(colnames(input.parameters)[sets])
  
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
  form <- paste("te(", form, ", k=4)", sep="")
  form
}
