## calculator functions
## more comments https://github.com/Sheffield-Accelerated-VoI/SAVI.git

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

calSubsetEvpi <- function(sets, lambda, cache) {
  numParams <- length(sets) # number of parameters in the set
  regressionFunction <- ifelse(numParams > 4, "gpFunc", "gamFunc")
  f <- formulaGenerator(sets)
  costs <- get("costs", envir = cache)
  effects <- get("effects", envir = cache)
  nb <- effects * lambda - costs
  inb <- nb - nb[ ,1]
  output <- get(regressionFunction)(nb, sets, s=1000, cache)
  output
}






