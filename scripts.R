## calculator functions

## more comments https://github.com/Sheffield-Accelerated-VoI/SAVI.git

## make a change we want to roll back 
## want to get rid of this line

createINB <- function(costs, effects, lambda = 20000, incremental = FALSE) {
  ## this function creates the INB matrix
  repository <<- "CRAN"
  inb <- as.matrix(effects) * lambda - as.matrix(costs)
  if(incremental) {
    inb <- cbind(0, inb)
  } else {
    inb <- inb - inb[, 1]
  }
  inb
}

singleParamGAM <- function(inputParam, inb) {
  ## this function calculates EVPI for a single parameter using GAM
  D <- ncol(inb)
  N <- nrow(inb)
  g.hat <- vector("list", D)
  g.hat[[1]] <- rep(0, N)   
  
  for(d in 2:D) {
    #print(paste("estimating g.hat for incremental NB for option", d, "versus 1"))
    f <- formula(inb[, d] ~ te(inputParam))
    model <- gam(f) 
    g.hat[[d]] <- model$fitted
  }
  
  perfect.info <- mean(do.call(pmax,g.hat)) 
  baseline <- max(unlist(lapply(g.hat,mean)))
  
  partial.evpi <- perfect.info - baseline
  partial.evpi
}

apply.singleParamGam <- function(df, inb) {
  ## this function applies singleParamGAM over the parameters
  df <- as.matrix(df)
  numVar <- sapply(1:ncol(df),function(x){is.numeric(df[, x])})
  if (sum(numVar)==0) {
    return(NULL)
    stop("PSA parameters are non-numeric!")
  } else res <- apply(df[, numVar], 2, singleParamGAM, inb)
  res
}

# plot generator functions

make.CEPlaneplot <- function(costs, effects, lambda, main, xlab, ylab, col) {
  ## makes the CE plane
  
  inc_costs <<- costs[, 2] - costs[, 1]
  inc_effects <<- effects[, 2] - effects[, 1]
  
  m.costs <<- max(abs(inc_costs))
  m.effects <<- max(abs(inc_effects))
  m2.effects <<- m.costs / lambda
  m2.costs <<- m.effects * lambda
  m3.costs <<- max(m.costs, m2.costs)
  m3.effects <<- max(m.effects, m2.effects)
  
  main <- paste("CE plane\nlambda =", lambda)
  plot(inc_effects, inc_costs, main=main, xlab=xlab, ylab=ylab, col=col, pty="s",
       ylim=c(-m3.costs, m3.costs), xlim=c(-m3.effects, m3.effects))
  abline(1, lambda, lty=2)
  abline(h=0)
  abline(v=0)
}

make.CEACplot <<- function(ceac, lambda, main, xlab, ylab, col) {
  ## makes the CEAC plot
  plot(ceac$l.seq, ceac$p[, 1], type="l", main="CEAC", xlab=xlab, ylab=ylab, col=col, ylim=c(0,1))
  for (i in 2:ceac$d){
    lines(ceac$l.seq, ceac$p[, i], col = i)
  }
  abline(v=lambda, lty=2)
}


make.CEAC <- function(costs, effects, incremental) {
  ## generates the CEAC values
  l.seq <- seq(0, 60000, 1000)
  d <- ncol(costs) + ifelse(incremental, 1, 0)
  p <- c()
  p.ce <- matrix(ncol = d, nrow = length(l.seq))
  for (i in 1:length(l.seq)) {
    lambda <- l.seq[i]
    inb <- as.matrix(effects) * lambda - as.matrix(costs)
    if(incremental) {
      inb <- cbind(0, inb)
    } else {
      inb <- inb - inb[, 1]
    }
    inb
    for(j in 1:d) {
      p.ce[i, j] <- sum(apply(inb, 1, which.max) == j) / nrow(inb)
    }
  }	
  list(p=p.ce, l.seq=l.seq, d=d)
}

make.EVPIplot <- function(costs, effects, main2, xlab2, ylab2, col2, incremental = FALSE, costscale = TRUE) {
  ## makes the overall EVPI plot
  l.seq <- seq(0, 60000, 1000)
  p <- c()
  for (lambda in l.seq) {
    inb <- as.matrix(effects) * lambda - as.matrix(costs)
    if(incremental) {
      inb <- cbind(0, inb)
    } else {
      inb <- inb - inb[, 1]
    }
    inb
    evpi <- mean(pmax(inb[, 2], inb[, 1])) - max(colMeans(inb))
    if(!costscale) evpi <- evpi / lambda
    p <- c(p, evpi)
  }	
  plot(l.seq, p, type="l", main=main2, xlab=xlab2, ylab=ylab2, col=col2)
}

make.4way.plot <<- function(costs, effects, ceac, lambda, main, xlab, ylab, col, 
                              main2, xlab2, ylab2, col2) {
  ## makes a four way plot of CE plane, CEAC and EVPI
  opar <- par(mfrow = c(2,2))
  make.CEPlaneplot(costs, effects, lambda, main, xlab, ylab, col)
  make.CEACplot(ceac, lambda, main, xlab, ylab, col)
  make.EVPIplot(costs, effects, main2, xlab2, ylab2, col2, 
                incremental = FALSE, costscale = TRUE)
  make.EVPIplot(costs, effects, main2, xlab2, ylab2, col2, 
                incremental = FALSE, costscale = FALSE)  
  on.exit(par(opar))
}



## Partial EVPI functions
## Author: Mark Strong
## Dec 2012
## Requires packages MASS and mgcv 

## We assume that the PSA sample is held in two objects.
## The input parameter samples are held in a N x r matrix (or data frame), where N is the number of PSA runs
## and r is the number of parameters. 

## The corresponding net benefits are held in a N x D matrix (or data frame),
## where D is the number of decision options.  



##########################################################################################################
## This is a generic function for estimating partial EVPI via GAM
## NB is the matrix (or data frame) of net benefits
## input.parameters is the matrix (or data frame) of input parameters

## regression.model is the specification of the regression model (must be in quotes)
## For parameters that are expected to interact use e.g. for two parameters "te(x1,x2)"  
## For parameters that are not expected to interact use e.g. "s(x1)+s(x2)"  
## If there are more than three parameters that are expected to interact set a maximum basis dimension of 4
## This will avoid the model trying to estimate too many coefficients "te(x1,x2,x3,x4,k=4)"  
## If there are more than six parameters that are expected to interact, the GP approach may be better.
## In some scenarios it may be possible to group parameters together that will interact into separate sets
## e.g. regression.model<-"te(x1,x2,x3)+te(x4,x5,x6)".
###########################################################################################################





evpi.gam<-function(NB, input.parameters, regression.model)
{
  require(mgcv)
  
  if(!is.null(dim(NB))) 
  {
    NB <- NB-NB[,1]
  }
  else
  {
    NB <- cbind(0,NB)
  }
  
  D <- ncol(NB)
  N <- nrow(NB)
  g.hat <- vector("list",D)
  g.hat[[1]] <- rep(0,N)   
  
  for(d in 2:D)
  {
    print(paste("estimating g.hat for incremental NB for option",d,"versus 1"))
    f <- update(formula(NB[,d]~.),formula(paste(".~",regression.model)))
    model <- gam(f,data=data.frame(input.parameters)) 
    g.hat[[d]] <- model$fitted
  }
  
  
  perfect.info <- mean(do.call(pmax,g.hat)) 
  baseline <- max(unlist(lapply(g.hat,mean)))
  rm(g.hat);gc()
  partial.evpi <- perfect.info - baseline ## estimate EVPI
  
  return(partial.evpi)
}


##############################################################################
## This is function includes estimation of SE and upward bias for EVPI via GAM
## S is the simulation size for the Monte Carlo computation of SE and bias 
##############################################################################

evpi.gam.SE.bias <- function(NB, input.parameters, regression.model, S=1000)
{
  require(mgcv);require(MASS)
  
  if(!is.null(dim(NB))) 
  {
    NB <- NB-NB[,1]
  }
  else
  {
    NB <- cbind(0,NB)
  }
  
  D <- ncol(NB)
  N <- nrow(NB)
  g.hat <- beta.hat <- Xstar <- V <- tilde.g <- vector("list",D)
  g.hat[[1]] <- rep(0,N)
  
  for(d in 2:D)
  {
    print(paste("estimating g.hat for incremental NB for option",d,"versus 1"))
    f <- update(formula(NB[,d]~.),formula(paste(".~",regression.model)))
    model <- gam(f,data=data.frame(input.parameters)) 
    g.hat[[d]] <- model$fitted
    beta.hat[[d]] <- model$coef
    Xstar[[d]] <- predict(model,type="lpmatrix")
    V[[d]] <- model$Vp
  }
  
  
  perfect.info <- mean(do.call(pmax,g.hat)) 
  baseline <- max(unlist(lapply(g.hat,mean)))
  partial.evpi <- perfect.info - baseline ## estimate EVPI
  rm(g.hat);gc()
  
  print("computing standard error and upward bias via Monte Carlo")
  for(d in 2:D)
  {
    sampled.coef <- mvrnorm(S,beta.hat[[d]],V[[d]])
    tilde.g[[d]] <- sampled.coef%*%t(Xstar[[d]])	
  }
  
  tilde.g[[1]] <- matrix(0,nrow=S,ncol=N)
  rm(V,beta.hat,Xstar,sampled.coef);gc()
  
  sampled.perfect.info <- rowMeans(do.call(pmax,tilde.g))
  sampled.baseline <- do.call(pmax,lapply(tilde.g,rowMeans)) 
  rm(tilde.g);gc()
  sampled.partial.evpi <- sampled.perfect.info - sampled.baseline
  SE <- sd(sampled.partial.evpi)
  upward.bias <- mean(sampled.partial.evpi) - partial.evpi
  
  return(list(partial.evpi=partial.evpi,SE=SE,upward.bias=upward.bias))
  
}

word_document_local <- function (fig_width = 5, fig_height = 4, fig_caption = FALSE, 
                                 highlight = "default", reference_docx = "default", pandoc_args = NULL) {
  knitr <- knitr_options(opts_chunk = list(dev = "postscript", dpi = 600, 
                                           fig.width = fig_width, fig.height = fig_height))
  args <- c()
  if (!is.null(highlight)) 
    highlight <- match.arg(highlight, rmarkdown:::highlighters())
  args <- c(args, rmarkdown::pandoc_highlight_args(highlight))
  if (!is.null(reference_docx) && !identical(reference_docx, 
                                             "default")) {
    args <- c(args, "--reference-docx", rmarkdown::pandoc_path_arg(reference_docx))
  }
  args <- c(args, pandoc_args)
  output_format(knitr = knitr, pandoc = rmarkdown::pandoc_options(to = "docx", 
                                                       from = rmarkdown:::from_rmarkdown(fig_caption), 
                                                       args = args))
}
