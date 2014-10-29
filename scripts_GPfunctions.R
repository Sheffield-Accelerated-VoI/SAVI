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


post.density <- function(hyperparams, NB, input.matrix) {
  
  input.matrix <- as.matrix(input.matrix, drop = FALSE)
  
  N <- nrow(input.matrix)
  p <- NCOL(input.matrix)
  H <- cbind(1, input.matrix)
  q <- ncol(H)
  
  a.sigma <- 0.001; b.sigma <- 0.001  ##  hyperparameters for IG prior for sigma^2
  a.nu <- 0.001; b.nu <- 1            ##  hyperparameters for IG prior for nu
  delta <- exp(hyperparams)[1:p]
  nu <- exp(hyperparams)[p + 1]
  
  A <- makeA.Gaussian(input.matrix, delta)
  Astar <- A + nu * diag(N)
  T <- chol(Astar)
  y <- backsolve(t(T), NB, upper.tri = FALSE)
  x <- backsolve(t(T), H, upper.tri = FALSE)
  tHAstarinvH <- t(x) %*% (x)
  betahat <- solve(tHAstarinvH) %*% t(x) %*% y
  residSS <- y %*% y -t(y) %*% x %*% betahat - t(betahat) %*% t(x) %*% y +
    t(betahat) %*% tHAstarinvH %*% betahat
  prior <- prod(dnorm(log(delta), 0, sqrt(1e5))) * dinvgamma(nu, a.nu, b.nu)
  l <- -sum(log(diag(T))) - 1 / 2 * log(det(tHAstarinvH)) -
    (N - q + 2 * a.sigma) / 2 * log(residSS / 2 + b.sigma) + log(prior)
  return(l)
}

gpFunc <- function(sets, y, s=1000, cache) {list(EVPI=0, SE=0)} # for now
