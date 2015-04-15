#ppr function

# call is (NB, sets, s=1000, cache, session)

library(parallel)

pprFunction <- function(op, inp, cache) {
  model <- ppr(inp, op, nterms = 25, bass = 10)
  fitted(model)
}

pprFunc <- function(NB, sets, s=1000, cache, session) {
  
  paramSet <- cbind(cache$params[, sets])
  constantParams <- (apply(paramSet, 2, var) == 0)
  
  #remove constants
  if (sum(constantParams) == length(sets)) return(list(EVPI=0, SE=0)) # if all regressors are constant
  if (sum(constantParams) > 0) sets <- sets[-which(constantParams)] # remove constants
  paramSet <- cbind(cbind(cache$params)[, sets]) # now with constants removed
  
  # check for linear dependence and remove 
  
  rankifremoved <- sapply(1:NCOL(paramSet), function (x) qr(paramSet[, -x])$rank)
  while(length(unique(rankifremoved)) > 1) {
    linearCombs <- which(rankifremoved == max(rankifremoved))
    # print(linearCombs)
    print(paste("Linear dependence: removing column", colnames(paramSet)[max(linearCombs)]))
    paramSet <- cbind(paramSet[, -max(linearCombs)])
    sets <- sets[-max(linearCombs)]
    rankifremoved <- sapply(1:NCOL(paramSet), function(x) qr(paramSet[, -x])$rank)
  }  
  
  if(qr(paramSet)$rank == rankifremoved[1]) {
    paramSet <- cbind(paramSet[, -1]) # special case only lincomb left
    sets <- sets[-1]
    print(paste("Linear dependence: removing column", colnames(paramSet)[1]))
  }
  
  
  if(!is.null(dim(NB))) {
    NB <- NB - NB[, 1]
  } else {
    NB <- cbind(0, NB)
  }
  
  input.matrix <- as.matrix(paramSet)
  colmin <- apply(input.matrix, 2, min)
  colmax <- apply(input.matrix, 2, max)
  colrange <- colmax - colmin
  input.matrix <- sweep(input.matrix, 2, colmin, "-")
  input.matrix <- sweep(input.matrix, 2, colrange, "/")
  
  
  print(class(NB))
  print(dim(NB))
  fits <- mclapply(data.frame(NB[, -1]), pprFunction, input.matrix, mc.cores = 3)
  fits <- data.frame(0, fits)
  partial.evpi <- mean(do.call(pmax, fits)) - max(colMeans(fits))
  return(list(EVPI=partial.evpi, SE=999))
}
