# Copyright (c) 2014, 2015 the SAVI authors (see AUTHORS.txt).
# Licensed under the BSD 3-clause license (see LICENSE.txt)

## calculator functions

valuesImportedFLAG <- function(cache, input){  
  
  dummy1 <- input$parameterFile 
  dummy2 <- input$costsFile 
  dummy3 <- input$effectsFile 
  dummy4 <- input$loadSession
  
  if (
      is.null(cache$params) | 
      is.null(cache$uploadedCosts) | 
      is.null(cache$uploadedEffects)) {return(FALSE)} else {return (TRUE)}
}


createInb <- function(costs.int, effects.int, lambda) {
  ## this function creates the INB matrix
  inb <- as.matrix(effects.int) * lambda - as.matrix(costs.int)
    inb <- inb - inb[, 1]
  return(inb)
}

createNb <- function(costs.int, effects.int, lambda) {
   ## this function creates the NB matrix
     nb <- as.matrix(effects.int) * lambda - as.matrix(costs.int)
     return(nb)
}

calcEvpi <- function(costs.int, effects.int, lambda) {
  ## this function calculates EVPI
  nb <- data.frame(as.matrix(effects.int) * lambda - as.matrix(costs.int))
  evpi <- mean(do.call(pmax, nb)) - max(colMeans(nb))
  return(evpi)
}


applyCalcSingleParamGam <- function(parameterDf, nb, session, cache) {
  ## this function applies singleParamGAM over the parameters

  parameterDf <- as.matrix(parameterDf)
    
  numVar <- NCOL(parameterDf)
  progress <- shiny::Progress$new(session, min=1, max=sum(numVar))
  on.exit(progress$close())
  progress$set(message = 'Calculation in progress',
               detail = 'Please wait...')
  
  res <- matrix(ncol = 2, nrow = NCOL(parameterDf))
  
  for (i in 1:NCOL(parameterDf)) {
    progress$set(i)
    result <- gamFunc(nb, i, s=1000, cache, session)
    res[i, ] <- unlist(result) 
  }
  res
}

makeCeac <- function(costs.int, effects.int, lambda, session) {
  ## generates the CEAC values
  l.seq <- seq(0, lambda * 10, lambda / 5)
  d <- ncol(costs.int)
  p <- c()
  
  progress <- shiny::Progress$new(session, min=0, max=length(l.seq))
  on.exit(progress$close())
  progress$set(message = 'Calculation in progress',
               detail = 'Please wait...')
  
  p.ce <- matrix(ncol = d, nrow = length(l.seq))
  for (i in 1:length(l.seq)) {
    progress$set(value = i)
    
    lambda.int <- l.seq[i]
    inb.int <- as.matrix(effects.int) * lambda.int - as.matrix(costs.int)

    for(j in 1:d) {
      p.ce[i, j] <- sum(apply(inb.int, 1, which.max) == j) / nrow(inb.int)
    }
  }	
  list(p=p.ce, l.seq=l.seq, d=d)
}


calSubsetEvpi <- function(sets, lambda, cache, session) {
  numParams <- length(sets) # number of parameters in the set
  regressionFunction <- ifelse(numParams > 4, "gpFunc", "gamFunc") # change gp to ppr
  f <- formulaGenerator(sets)
  costs <- cache$costs
  effects <- cache$effects
  nb <- effects * lambda - costs
  inb <- nb - nb[ ,1]
  output <- get(regressionFunction)(nb, sets, s=1000, cache, session)
  output
}






word_document_local <- function (fig_width = 5, fig_height = 4, fig_caption = FALSE, 
                                 highlight = "default", reference_docx = "default", pandoc_args = NULL) {
  
  knitr <- knitr_options(opts_chunk = list(dev = "png", dpi = 96, fig.width = fig_width,
                                           fig.height = fig_height)
  )
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



