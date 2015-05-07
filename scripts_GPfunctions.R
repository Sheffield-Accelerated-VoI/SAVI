# Copyright (c) 2014, 2015 the SAVI authors (see AUTHORS.txt).
# Licensed under the BSD 3-clause license (see LICENSE.txt)

# this file holds the functions to calculate the GP

dinvgamma <- function(x, alpha, beta) {
  (beta ^ alpha) / gamma(alpha) * x ^ (-alpha - 1) * exp(-beta / x)
}

cor.Gaussian <- function(X, phi, m) { #  needs sorting...... probably with dist()
  txbuild1 <- function(h) exp(-rowSums(t((t(X) - h) / phi) ^ 2))
  apply(as.matrix(as.matrix(X)[1:m, ]), 1, txbuild1)
}

makeA.Gaussian <- function(X, phi) { # function to make A matrix with the Gaussian correlation function
  n <- NROW(X)
  if(length(phi) > 1) {
    b <- diag(phi ^ (-2))
  } else {
    b <- phi ^ (-2) }
  R <- X %*% as.matrix(b) %*% t(X)
  
  S <- matrix(diag(R), nrow = n, ncol = n)
  exp(2 * R - S - t(S))
}

# function to calculate posterior density


post.density <- function(hyperparams, NB, input.m) {
  
  input.m <- as.matrix(input.m, drop = FALSE)
  
  N <- nrow(input.m)
  p <- NCOL(input.m)
  H <- cbind(1, input.m)
  q <- ncol(H)
  
  a.sigma <- 0.001; b.sigma <- 0.001  ##  hyperparameters for IG prior for sigma^2
  a.nu <- 0.001; b.nu <- 1            ##  hyperparameters for IG prior for nu
  delta <- exp(hyperparams)[1:p]
  nu <- exp(hyperparams)[p + 1]
  
  A <- makeA.Gaussian(input.m, delta)
  Astar <- A + nu * diag(N)
  T <- chol(Astar)
  y <- backsolve(t(T), NB, upper.tri = FALSE)
  x <- backsolve(t(T), H, upper.tri = FALSE)
  tHAstarinvH <- t(x) %*% (x) + 1e-7* diag(q)
  betahat <- solve(tHAstarinvH) %*% t(x) %*% y
  residSS <- y %*% y -t(y) %*% x %*% betahat - t(betahat) %*% t(x) %*% y +
    t(betahat) %*% tHAstarinvH %*% betahat
  prior <- prod(dnorm(log(delta), 0, sqrt(1e5))) * dinvgamma(nu, a.nu, b.nu)
  l <- -sum(log(diag(T))) - 1 / 2 * log(det(tHAstarinvH)) -
    (N - q + 2 * a.sigma) / 2 * log(residSS / 2 + b.sigma) + log(prior)
  return(l)
}


estimate.hyperparameters <- function(NB, inputs, session) {
  
  p <- NCOL(inputs)
  D <- ncol(NB)
  
  hyperparameters <- vector("list", D)
  hyperparameters[[1]] <- NA

  progress1 <- shiny::Progress$new(session, min=1, max=D)
  on.exit(progress1$close())
  progress1$set(message = 'Estimating GP hyperparameters',
                detail = 'Please wait...')
  progress1$set(value = 1)
  
  for(d in 2:D) {
    progress1$set(value = d)
    initial.values <- rep(0, p + 1)
    repeat {
      print(paste("calling optim function for net benefit", d))
      log.hyperparameters <- optim(initial.values, fn=post.density, 
                                   NB=NB[, d], input.m=inputs,
                                   method="Nelder-Mead",
                                   control=list(fnscale=-1, maxit=10000, trace=0))$par
      if (sum(abs(initial.values - log.hyperparameters)) < 0.05) {
        hyperparameters[[d]] <- exp(log.hyperparameters)
        break
      }	
      initial.values <- log.hyperparameters
    }
  }
  
  return(hyperparameters)
  
}

gpFunc <- function(NB, sets, s=1000, cache, session) {
  
  input.parameters <- cache$params
  paramSet <- cbind(input.parameters[, sets])
  constantParams <- (apply(paramSet, 2, var) == 0)

  #remove constants
  if (sum(constantParams) == length(sets)) return(list(EVPI=0, SE=0)) # if all regressors are constant
  if (sum(constantParams) > 0) sets <- sets[-which(constantParams)] # remove constants
  
  # check for linear dependence and remove 
  paramSet <- cbind(cbind(input.parameters)[, sets]) # now with constants removed
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
  
  inputs.of.interest <- sets
  p <- length(inputs.of.interest)
  
  if(!is.null(dim(NB))) {
    NB <- NB - NB[, 1]
  } else {
    NB <- cbind(0, NB)
  }
  
  maxSample <- min(5000, nrow(NB)) # to avoid trying to invert huge matrix
  NB <- as.matrix(NB[1:maxSample, ])
  D <- ncol(NB)
  
  input.matrix <- as.matrix(input.parameters[1:maxSample, inputs.of.interest, drop=FALSE])
  colmin <- apply(input.matrix, 2, min)
  colmax <- apply(input.matrix, 2, max)
  colrange <- colmax - colmin
  input.matrix <- sweep(input.matrix, 2, colmin, "-")
  input.matrix <- sweep(input.matrix, 2, colrange, "/")
  N <- nrow(input.matrix)
  p <- ncol(input.matrix)
  H <- cbind(1, input.matrix)
  q <- ncol(H)
  

  
  m <- min(30 * p, 250)
  m <- min(nrow(NB), m)
  setForHyperparamEst <- 1:m # sample(1:N, m, replace=FALSE)
  hyperparameters <- estimate.hyperparameters(NB[setForHyperparamEst, ], 
                                              input.matrix[setForHyperparamEst, ], session)
    
  V <- g.hat <- vector("list", D)
  g.hat[[1]] <- rep(0, N)
  

  progress1 <- shiny::Progress$new(session, min=1, max=D)
  #on.exit(progress1$close())
  progress1$set(message = 'Calculating conditional expected net benefits',
                detail = 'Please wait...')
  progress1$set(value = 1)
  
  for(d in 2:D)
  {
    progress1$set(value = d)
    print(paste("estimating g.hat for incremental NB for option", d, "versus 1"))
    delta.hat <- hyperparameters[[d]][1:p]
    nu.hat <- hyperparameters[[d]][p+1]
    A <- makeA.Gaussian(input.matrix, delta.hat)
    Astar <- A + nu.hat * diag(N)
    Astarinv <- chol2inv(chol(Astar))
    rm(Astar); gc()
    AstarinvY <- Astarinv %*% NB[, d]
    tHAstarinv <- t(H) %*% Astarinv
    tHAHinv <- solve(tHAstarinv %*% H + 1e-7* diag(q))
    betahat <- tHAHinv %*% (tHAstarinv %*% NB[, d])
    Hbetahat <- H %*% betahat
    resid <- NB[, d] - Hbetahat
    g.hat[[d]] <- Hbetahat+A %*% (Astarinv %*% resid)
    AAstarinvH <- A %*% t(tHAstarinv)
    sigmasqhat <- as.numeric(t(resid) %*% Astarinv %*% resid)/(N - q - 2)
    V[[d]] <- sigmasqhat*(nu.hat * diag(N) - nu.hat ^ 2 * Astarinv +
                            (H - AAstarinvH) %*% (tHAHinv %*% t(H - AAstarinvH)))
    rm(A, Astarinv, AstarinvY, tHAstarinv, tHAHinv, betahat, Hbetahat, resid, sigmasqhat);gc()
  }
  progress1$close()
  perfect.info <- mean(do.call(pmax, g.hat)) 
  baseline <- max(unlist(lapply(g.hat, mean)))
  
  partial.evpi <- perfect.info - baseline
  
  print("Computing standard error via Monte Carlo")
  tilde.g <- vector("list", D)
  tilde.g[[1]] <- matrix(0, nrow=s, ncol=N)     
  
  progress2 <- shiny::Progress$new(session, min=1, max=D)
  #on.exit(progress2$close())
  progress2$set(message = 'Calculating Standard Error',
                detail = 'Please wait...')
  progress2$set(value = 1)

  for(d in 2:D) {
    progress2$set(value = d)
    tilde.g[[d]] <- mvrnorm(s, g.hat[[d]][1:(min(N, 1000))], V[[d]][1:(min(N, 1000)), 1:(min(N, 1000))])
  }
  progress2$close()
  
  sampled.perfect.info <- rowMeans(do.call(pmax, tilde.g))
  sampled.baseline <- do.call(pmax, lapply(tilde.g, rowMeans)) 
  rm(tilde.g);gc()
  sampled.partial.evpi <- sampled.perfect.info - sampled.baseline
  SE <- sd(sampled.partial.evpi)
  # g.hat.short <- lapply(g.hat,function(x) x[1:(min(N, 1000))])
  # mean.partial.evpi <- mean(do.call(pmax, g.hat.short)) - max(unlist(lapply(g.hat.short,mean)))
  # upward.bias <- mean(sampled.partial.evpi) - mean.partial.evpi 
  rm(V, g.hat);gc()
  return(list(EVPI=partial.evpi, SE=SE))

}

