## calculator functions
## more comments https://github.com/Sheffield-Accelerated-VoI/SAVI.git


# valuesImportedFLAG <- function(input){
#  # if (!is.null(input$loadSession)) return(TRUE) else {
#       is.null(input$parameterFile) | 
#       is.null(input$costsFile) | 
#       is.null(input$effectsFile)
#       )   
#   {return(FALSE)} else {return (TRUE)}#}
# }

valuesImportedFLAG <- function(cache, input){  
  
  dummy1 <- input$parameterFile 
  dummy2 <- input$costsFile 
  dummy3 <- input$effectsFile 
  dummy4 <- input$loadSession
  
  if (
      is.null(get("params", envir=cache)) | 
      is.null(get("costs", envir=cache)) | 
      is.null(get("effects", envir=cache))
  )   
  {return(FALSE)} else {return (TRUE)}
}


createInb <- function(costs.int, effects.int, lambda = 20000, incremental = FALSE) {
  ## this function creates the INB matrix
#  repository <<- "CRAN"
  inb <- as.matrix(effects.int) * lambda - as.matrix(costs.int)
  if(incremental) {
    inb <- cbind(0, inb)
  } else {
    inb <- inb - inb[, 1]
  }
  # assign("inb", inb, envir=cache)
  return(inb)
}

createNb <- function(costs.int, effects.int, lambda = 20000, incremental = FALSE) {
   ## this function creates the NB matrix
     nb <- as.matrix(effects.int) * lambda - as.matrix(costs.int)
     return(nb)
}

calcEvpi <- function(costs.int, effects.int, lambda = 20000) {
  ## this function creates the NB matrix
  nb <- data.frame(as.matrix(effects.int) * lambda - as.matrix(costs.int))
  evpi <- mean(do.call(pmax, nb)) - max(colMeans(nb))
  return(evpi)
}

calcSingleParamGAM <- function(inputParam, inb) {
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
  
  perfect.info <- mean(do.call(pmax, g.hat)) 
  baseline <- max(unlist(lapply(g.hat, mean)))
  
  partial.evpi <- perfect.info - baseline
  partial.evpi
}

applyCalcSingleParamGam <- function(df, inb) {
  ## this function applies singleParamGAM over the parameters
  df <- as.matrix(df)
  numVar <- sapply(1:ncol(df),function(x){is.numeric(df[, x])})
  if (sum(numVar)==0) {
    return(NULL)
    stop("PSA parameters are non-numeric!")
  } else res <- apply(df[, numVar], 2, calcSingleParamGAM, inb)
  res
}

# plot generator functions


makeCeac <- function(costs.int, effects.int, incremental.int) {
  ## generates the CEAC values
  l.seq <- seq(0, 60000, 1000)
  d <- ncol(costs.int) + ifelse(incremental.int, 1, 0)
  p <- c()
  p.ce <- matrix(ncol = d, nrow = length(l.seq))
  for (i in 1:length(l.seq)) {
    lambda.int <- l.seq[i]
    inb.int <- as.matrix(effects.int) * lambda.int - as.matrix(costs.int)
    if(incremental.int) {
      inb.int <- cbind(0, inb.int)
    } else {
      inb.int <- inb.int - inb.int[, 1]
    }
    inb.int
    for(j in 1:d) {
      p.ce[i, j] <- sum(apply(inb.int, 1, which.max) == j) / nrow(inb.int)
    }
  }	
  list(p=p.ce, l.seq=l.seq, d=d)
}


makeCeacPlot <- function(ceac.int, lambda.int, names, ...) {
  ## makes the CEAC plot
  plot(ceac.int$l.seq, ceac.int$p[, 1], type="l", ylim=c(0,1), ...)
  for (i in 2:ceac.int$d){
    lines(ceac.int$l.seq, ceac.int$p[, i], col = i)
  }
  abline(v=lambda.int, lty=2)
  for (i in 1:ceac.int$d){  
    points(lambda.int, ceac.int$p[which(ceac.int$l.seq == lambda.int), i], pch=20, col="black")
    text(lambda.int, ceac.int$p[which(ceac.int$l.seq == lambda.int), i], ceac.int$p[which(ceac.int$l.seq == lambda.int), i], pos=1, offset=0.1, cex=0.7)
  }
  legend("topright", names, col = c(1:i), lty = 1)
}

makeCEPlanePlot <- function(costs.int, effects.int, lambda, ...) {
  ## makes the CE plane
  
  inc_costs <- costs.int[, 2] - costs.int[, 1]
  inc_effects <- effects.int[, 2] - effects.int[, 1]
  
  m.costs <- max(abs(inc_costs))
  m.effects <- max(abs(inc_effects))
  m2.effects <- m.costs / lambda
  m2.costs <- m.effects * lambda
  m3.costs <- max(m.costs, m2.costs)
  m3.effects <- max(m.effects, m2.effects)
  
  main <- paste("Standardised Cost-effectiveness Plane per Person\nlambda =", lambda)
  plot(inc_effects, inc_costs, pty="s", cex=0.4,
       ylim=c(-m3.costs, m3.costs), xlim=c(-m3.effects, m3.effects), col="orangered", ...)
  abline(1, lambda, lty=2)
  abline(h=0)
  abline(v=0)
  points(mean(inc_effects),mean(inc_costs), pch=20, col="black", cex=1.5)
  text(mean(inc_effects),mean(inc_costs),"mean", pos=1, offset=0.1, cex=0.7)
}

makeEvpiPlot <- function(costs.int, effects.int, 
                          incremental.int = FALSE, costscale = TRUE, ...) {
  ## makes the overall EVPI plot
  l.seq <- seq(0, 60000, 1000)
  p <- c()
  for (lambda.int in l.seq) {
    inb.int <- as.matrix(effects.int) * lambda.int - as.matrix(costs.int)
    if(incremental.int) {
      inb.int <- cbind(0, inb.int)
    } else {
      inb.int <- inb.int - inb.int[, 1]
    }
    #inb.int
    evpi <- mean(pmax(inb.int[, 2], inb.int[, 1])) - max(colMeans(inb.int))
    if(!costscale) evpi <- evpi / lambda.int
    p <- c(p, evpi)
  }	
  plot(l.seq, p, type="l", ...)
}

make4wayPlot <- function(costs.int, effects.int, incremental.int, ceac.int, lambda.int, 
                          main1, xlab1, ylab1, col1, 
                          main2, xlab2, ylab2, col2) {
  ## makes a four way plot of CE plane, CEAC and EVPI
  opar <- par(mfrow = c(2,2))
  makeCEPlanePlot(costs.int, effects.int, lambda.int, main = main1, xlab = xlab1, ylab = ylab1, col = col1)
  makeCeacPlot(ceac.int, lambda.int, main = main1, xlab = xlab1, ylab = ylab1, col = col1)
  makeEvpiPlot(costs.int, effects.int, incremental.int, costscale = TRUE,
                main = main2,  xlab = xlab2, ylab = ylab2, col = col2)
  makeEvpiPlot(costs.int, effects.int, incremental.int, costscale = FALSE,
                main = main2,  xlab = xlab2, ylab = ylab2, col = col2)
  on.exit(par(opar))
}


makeNbDensity <- function (costs.int, effects.int, lambda, ...) {
  nb <- createNb(costs.int, effects.int, lambda, FALSE)
  d <- ncol(costs.int) + ifelse(FALSE, 1, 0)
  xmax<-max(nb)
  xmin<-min(nb)
  ymax<-c(1:d)
  for (i in 1:d){
    den<-density(nb[, i])
    ymax[i]<-max(den$y)
  }
  ymax<-max(ymax)
  plot(density(nb[, 1]), type = "l", col = 1, xlim = c(xmin, xmax), ylim = c(0, ymax),xlab="Net Benefit",main="Net Benefit Densities")
  for (i in 2:d){
    lines(density(nb[, 2]), col = i)
  }
  # Need strategy names adding
  legend("topright",colnames(nb),col=c(1:d), lty = 1)
}

makeInbOptDens <- function (costs.int, effects.int, lambda) {
  nb <- createNb(costs.int, effects.int, lambda, FALSE)
  c <- which.max(as.matrix(colMeans(nb)))
  inbOpt <- nb-nb[,c]
  inbOpt <- as.matrix(inbOpt[,-c])
  colnames(inbOpt) <- colnames(nb)[-c]
  d <- ncol(inbOpt) + ifelse(FALSE, 1, 0)
  xmax<-max(inbOpt)
  xmin<-min(inbOpt)
  ymax<-c(1:d)
  for (i in 1:d){
    den<-density(nb[, i])
    ymax[i]<-max(den$y)
  }
  ymax<-max(ymax)
  plot(density(inbOpt[, 1]), type = "l", col = 1, xlim = c(xmin, xmax), 
       ylim = c(0, ymax),xlab="INB vs. Optimal Strategy",
       main="Incremental Net Benefit Density")
  if (d>1) {
  for (i in 2:d){
    lines(density(inbOpt[, i]), col = i)
  }    
  }
  # Need strategy names adding
  legend("topright",colnames(inbOpt),col=c(1:d), lty = 1)
  abline(v=0, lty=2)
}

make2wayDensity <- function(costs.int, effects.int, lambda) {
  ## makes a two way plot of Net Benefit and incremental density
  opar <- par(mfrow = c(1,2))
  makeNbDensity(costs.int, effects.int, lambda)
  makeInbOptDens(costs.int, effects.int, lambda)
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

# function for building up table of parameter sets for partial EVPI
buildSetStoreTable <- function(store) {
  maxRows <- max(unlist(lapply(store, length)))
  tableRows <- lapply(store, function(x) c(x, rep("", maxRows - length(x))))
  df <- t(data.frame(tableRows))
  rownames(df) <- paste("Selection", 1:length(store))
  colnames(df) <- NULL
  names(df) <- NULL
  df
}

bold.allrows <- function(x) {
  h <- paste('<strong>',x,'</strong>', sep ='')
  h
}

calSubsetEvpi <- function(sets, lambda, cache) {
  f <- formulaGenerator(sets)
  # lambda <- input$lambda
  costs <- get("costs", envir = cache)
  effects <- get("effects", envir = cache)
  params <- get("params", envir = cache)
  nb <- effects * lambda - costs
  inb <- nb - nb[ ,1]
  modelFitted <- c()
  for (i in 2:ncol(inb)) {
    y <- inb[, i]
    modelFitted[[i]] <- fitted(gam(formula(f), data = params))
  }
  modelFitted[[1]] <- 0
  dfModelFitted <- data.frame(modelFitted)
  evpi <- mean(do.call(pmax, dfModelFitted)) - max(colMeans(dfModelFitted))
  evpi
 # 0
}

formulaGenerator <- function(namesList) {
    form <- paste("te(", namesList, ")+", sep="", collapse="")
    form <- substr(form, 1, nchar(form) - 1)
    form <- paste("y~", form, sep="")
    form
}

