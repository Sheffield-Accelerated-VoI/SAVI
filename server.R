# source all the functions we need

source("scripts.R")

# load the libraries we need

library(mgcv)
library(knitr)
library(rmarkdown)

print("server.R called")

shinyServer(
  
  function(input, output, session) {
    # `cache' is the environment unique to each user visit
    # This is where we will save values that need to persist, 
    # and that can be picked up and incldued in the report
    
    print("cache is")
    print(cache <- new.env())
    
    
    print("shinyServer called")
    print("session is")
    print(session)
    
    print("objects in the session environment")
    print(ls(envir=session))
    
    print("current environment")
    print(environment())
    
    # initialise cached variable values
    
    assign("savedSession", 0, envir=cache)
    assign("nIterate", 0, envir = cache)
    assign("pEVPI", NULL, envir = cache)
    assign("params", NULL, envir = cache)
    assign("costs", NULL, envir = cache)  
    assign("effects", NULL, envir = cache) 
    assign("counterAdd", 1, envir = cache)     
    assign("setStore", vector("list", 100), envir = cache) # up to 100 sets for the group inputs
    
    # these three rows autoload values for testing purposes - to avoid having to load them manually. MS
    # ###########
    #   load.parameters <- function() read.csv("parameters.csv")                                   
    #   load.costs <- function() read.csv("costs.csv")
    #   load.effects <- function() read.csv("effects.csv")
    # ########### 
    
  load("SAVISession.Rdata", envir=cache)
    #  Function that loads saved session
    observe({
      inFile = input$loadSession
      if (is.null(inFile))
        return(NULL)
      assign("savedSession", 1, envir=TRUE)
      load(inFile$datapath, envir=cache)
      #print(lapply(ls(envir=cache), function(x) object.size(get(x, envir=cache))))
      #print(ls(envir=cache))
    })   
    
    #  Function that imports parameters - NEED TO DO _ SANITY CHECK
    #loadParameters <- 
      observe({
      inFile = input$parameterFile
      if (is.null(inFile))
        return(NULL)
#       
#       if (input$rownames1) {
#         dat <- read.csv(inFile$datapath, header=input$header1, #sep=input$sep,
#                         row.names=1)#, dec=input$dec)
#         assign("params", dat, envir = cache)
#         assign("nParams", ncol(dat), envir=cache)
#         assign("nIterate", nrow(dat), envir=cache)
#         
#       } else {
        dat <- read.csv(inFile$datapath)#, header=input$header1)#, sep=input$sep, dec=input$dec)
        assign("params", dat, envir = cache)
        assign("nParams", ncol(dat), envir=cache)
        assign("nIterate", nrow(dat), envir=cache)
#    }
    })
    
    #  Function that imports costs    
    #loadCosts <- 
      observe({
      inFile = input$costsFile
      if (is.null(inFile))
        return(NULL)
#       
#       if (input$rownames2) {
#         dat <- read.csv(inFile$datapath, header=input$header2, #sep=input$sep,
#                         row.names=1)#, dec=input$dec)
#         assign("costs", dat, envir = cache)
#         assign("nInt", ncol(dat), envir=cache)
#       } else {
        dat <- read.csv(inFile$datapath)#, header=input$header2)#, sep=input$sep, dec=input$dec)
        assign("costs", dat, envir = cache)
        assign("nInt", ncol(dat), envir=cache)
#      }
    })
    
    # Function that imports effects
    #loadEffects <- 
      observe({
      inFile = input$effectsFile      
      if (is.null(inFile))
        return(NULL)
#       
#       if (input$rownames3) {
#         dat <- read.csv(inFile$datapath, header=input$header3, #sep=input$sep,
#                         row.names=1)#, dec=input$dec)
#         assign("effects", dat, envir = cache)
#       } else {
        dat <- read.csv(inFile$datapath)#, header=input$header3)#, sep=input$sep, dec=input$dec)
        assign("effects", dat, envir = cache)
 #     }
    })
    
    # Functions that render the data files and pass them to ui.R
    
    output$checktable1 <- renderTable({
      x <- input$parameterFile 
      y <- input$loadSession
      tableValues <- get("params", envir=cache)
      assign("nParamSamples", nrow(tableValues), envir=cache)
      if (is.null(tableValues)) return(NULL)
      if (ncol(tableValues) > 10) tableValues = tableValues[, 1:10]
      head(tableValues, n=5)
    })
    
    output$checktable2 <- renderTable({
      x <- input$costsFile 
      y <- input$loadSession
      tableValues <- get("costs", envir=cache)

      if (is.null(tableValues)) return(NULL)
      if (ncol(tableValues) > 10) tableValues = tableValues[, 1:10]
      head(tableValues, n=5)  
    })
    
    output$checktable3 <- renderTable({
      x <- input$effectsFile 
      y <- input$loadSession
      tableValues <- get("effects", envir=cache)
      if (is.null(tableValues)) return(NULL)
      if (ncol(tableValues) > 10) tableValues = tableValues[, 1:10]
      head(tableValues, n=5)
    })
    
    
    # Function that calculates the single partial EVPI outputs to be sent to the main panel in ui.R
    # Stores pEVPI object in cache
    # also stores the inb object
    
    calcPartialEvpi <- reactive({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      inb <- createInb(get("costs", envir=cache), get("effects", envir=cache), 
                       input$lambdaOverall, input$incremental)
      assign("inb", inb, envir=cache)
      pEVPI <- cbind(applyCalcSingleParamGam(get("params", envir=cache), inb))
      assign("pEVPI", pEVPI, envir = cache)
      pEVPI
    })
    
    output$summary <- renderTable({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      calcPartialEvpi()
    })
      
    # function that calculates ceac
    ceac <- reactive({ 
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      makeCeac(get("costs", envir=cache), get("effects", envir=cache), input$incremental)
    })
        
  
    # Functions that make reactive text to accompany plots
    output$textCEplane1 <- renderText({
      paste("This graph shows the standardised cost-effectiveness plane per person based on",input$nIterate,"model runs,
            in which uncertain model parameters are varied simultaneously in a probabilistic sensitivity analysis.  
            The mean incremental cost of ", input$t3, " versus ", input$current," is ",input$currency,"X. This suggests that
            ",input$t3,"is more/less costly. There is some uncertainty due to model 
            parameters, with the 95% CI for the incremental cost ranging from (lower CI, upper CI).  
            The probability that", input$t3, "is cost saving compared 
            to",input$current,"is XX%.", sep="")
    })                       ###THIS FUNCTION STILL NEEDS TO BE MADE REACTIVE TO RESULTS
    
    output$textCEplane2 <- renderText({

      paste("The mean incremental benefit of", input$t3, "versus", input$current, "is", input$t6, "X.  This suggests that",input$t3,"
            is more/or less beneficial over the",input$n7,"year time horizon.  Again, there is some uncertainty due to 
            model parameters, with the 95% CI for the incremental benefit ranging from (lower credible interval, upper CI).
            The probability that",input$t3,"is more beneficial than",input$current,"is XX%.")
    })                        ###THIS FUNCTION STILL NEEDS TO BE MADE REACTIVE TO RESULTS
    
    output$textCEplane3 <- renderText({
      paste("The incremental expected cost per unit of benefit is estimated at ",
            input$currency, "XX_ICER", input$unitBens, ". This 
            is above/below the threshold of ", input$currency, input$lambdaOverall,
            " per ", input$unitBens, " indicating that ", input$t3,
            "would (not) be considered cost-effective at this threshold. There is uncertainty with a XX% probability that", 
            input$t3,"is more cost-effective (XX% of the probabilistic model run ‘dots’ are below and to the right of the diagonal threshold line).", sep="")
    })                         ###THIS FUNCTION STILL NEEDS TO BE MADE REACTIVE TO RESULTS
    
    output$textCEplane4 <- renderText({
      paste(input$t3,"vs.",input$current)
    })
    
    output$textCEplane5 <- renderText({
      paste("$XX%$ probability that",input$t3,"is more cost-effective than",input$current,"at a threshold 
            of",input$currency,input$lambdaOverall,"per",input$unitBens)
    })                       ###THIS FUNCTION STILL NEEDS TO BE MADE REACTIVE TO RESULTS
    
    output$textCEAC1 <- renderText({
      paste("This graph shows the cost-effectiveness acceptability curve for the comparison of strategies. The results show that at a threshold 
            value for cost-effectiveness of",input$currency,input$lambdaOverall,"per",input$unitBens,"the strategy with the highest probability of being most cost-effective 
            is X, with a probability of XX%. More details on how to interpret CEACs are available from the literature")
    })                       ###THIS FUNCTION STILL NEEDS TO BE MADE REACTIVE TO RESULTS
    
    output$textCEAC2 <- renderText({
      paste(input$current)
    })                       
    
    output$textCEAC3 <- renderText({
      paste(input$t3)
    })                       
    
    
    
    # Functions that make tables  
    output$tableCEplane <- renderTable({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      tableCEplane <- makeTableCePlane(get("costs", envir=cache), get("effects", 
                            envir=cache), lambda=input$lambdaOverall)
      rownames(tableCEplane) <- c(paste("Threshold (", input$currency, ")"), "Comparator", 
                            "Number of PSA runs", 
                            paste("Mean inc. Effect per Person (", input$unitBens, ")"), 
                            paste("Mean inc. Cost per Person (", input$currency, ")"),
                            paste("ICER Estimate (", input$currency, "per", input$unitBens, ")"),
                            paste("2.5th CI for inc. Effects (", input$unitBens, ")"), 
                            paste("97.5th CI for inc. Effects (", input$unitBens, ")"),
                            paste("2.5th CI for inc. Costs (", input$currency, ")"),
                            paste("97.5th CI for inc. Costs (", input$currency, ")"),
                            "Probability intervention is cost saving", 
                            "Probability intervention provides more benefit", 
                            "Probability that intervention is cost-effective")
            tableCEplane
    })  

    output$tableNetBenefit <- renderTable({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
     tableNetBenefit <- makeTableNetBenefit(get("costs", envir=cache), get("effects", 
                            envir=cache), lambda=input$lambdaOverall, get("nInt", envir=cache))
     rownames(tableNetBenefit) <- c(paste("Mean", input$effectDef), 
                                    paste("Mean", input$costDef), 
                                    paste("Expected Net Benefit at", 
                                          input$currency,input$lambdaOverall, "per",input$unitBens), 
                                    "95% Lower CI (on Costs Scale)", 
                                    "95% Upper CI (on Costs Scale)", 
                                    "Expected Net Benefit on Effects Scale", 
                                    "95% Lower CI (on Effects Scale)", 
                                    "95% Upper CI (on Effects Scale)")
     tableNetBenefit
   })  
   
   output$tableEVPI <- renderTable({
     if (!valuesImportedFLAG(cache, input)) return(NULL)
     tableEVPI <- matrix(NA, nrow = 7, ncol = 2)
     colnames(tableEVPI) <- c(paste("Overall EVPI Financial Valuation (", input$currency, ")"), paste("Overall EVPI", input$unitBens, "Valuation"))
     rownames(tableEVPI) <- c("Per Person Affected by the Decision", 
                              paste("Per Year in", input$jurisdiction, "Assuming", input$annualPrev, "Persons Affected per Year"), 
                              "Over 5 Years", 
                              "Over 10 Years", 
                              "Over 15 Years", 
                              "Over 20 years", 
                              paste("Over Specified Decision Relevance Horizon (", input$horizon, "years)"))
     
     overallEvpi <- calcEvpi(get("costs", envir=cache), get("effects", envir=cache), 
              lambda=input$lambdaOverall)
     evpiVector <- c(overallEvpi, overallEvpi * input$annualPrev, overallEvpi * input$annualPrev * 5, 
                     overallEvpi * input$annualPrev * 10, overallEvpi * input$annualPrev * 15,
                     overallEvpi * input$annualPrev * 20,
                     overallEvpi * input$annualPrev * input$horizon)     
     tableEVPI[, 1] <- signif(evpiVector, 4)          
     tableEVPI[, 2] <- signif(evpiVector / input$lambdaOverall, 4)   
     tableEVPI
   }, digits=cbind(rep(0, 7), rep(0, 7), rep(2, 7))) 
   
   output$tableEVPPI <- renderTable({
     if (!valuesImportedFLAG(cache, input)) return(NULL)
     tableEVPPI <- matrix(NA, nrow = ncol(get("params", envir=cache)), ncol = 4)
     tableEVPPI[, 1] <- get("pEVPI", envir=cache)
     colnames(tableEVPPI) <- c(paste("Per Person EVPPI (", input$currency, ")"), "Indexed Overall EVPI = 1.00", paste("EVPPI for", input$jurisdiction, "Per Year"), paste("EVPPI for", 
                               input$jurisdiction, "over", input$horizon, "years"))
     rownames(tableEVPPI) <- colnames(get("params", envir=cache))
     tableEVPPI
   }) 
   
    
    # Functions that make plots
    output$plots1 <- renderPlot({
    
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      makeCEPlanePlot(get("costs", envir=cache), get("effects", envir=cache), 
                      lambda=input$lambdaOverall, xlab=input$effectDef, 
                      ylab=input$costDef)
    })  ###NEED TO ADD POINT FOR MEAN ON PLOT - SHOULD BE LARGER AND BRIGHTER (E.G.DARK RED, STANDARD SIZE AND SOLID WOULD WORK WELL)
    
    output$plots2 <- renderPlot({
    
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      ceac.obj <- assign("ceac.obj", ceac(), envir=cache)
      makeCeacPlot(ceac.obj, lambda=input$lambdaOverall, 
                   main="Cost-effectiveness Acceptability Curve", 
                   xlab="Threshold willingness to pay", 
                   ylab="Probability strategy is cost-effective",
                   names=colnames(get("costs", envir=cache)))
    })  ###NEED TO ADD % COST-EFFECTIVENESS AT LINE AS A LABEL 
    
    output$plots3 <- renderPlot({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      makeEvpiPlot(get("costs", envir=cache), get("effects", envir=cache), lambda=input$lambdaOverall,
                   main=input$main3, 
                   xlab="Threshold willingness to pay", 
                   ylab="Overall EVPI per person affected (on costs scale)",
                   col="red", input$incremental, costscale = TRUE)
    })
    
    output$plots4 <- renderPlot({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      makeEvpiPlot(get("costs", envir=cache), get("effects", envir=cache), lambda=input$lambdaOverall,
                   main=input$main4, 
                   xlab="Threshold willingness to pay", 
                   ylab="Overall EVPI per person affected (on effects scale)",
                   col="red", input$incremental, costscale = FALSE)
    })
    
    output$plots4way <- renderPlot({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      make4wayPlot(get("costs", envir=cache), get("effects", envir=cache), 
                   get("ceac.obj", envir=cache), lambda=input$lambdaOverall, main=input$main1, 
                   xlab=input$xlab2, ylab=input$ylab2, col=input$color2, 
                                       main2=input$main4, xlab2=input$xlab4, 
                                       ylab2=input$ylab4,
                                       col2=input$color4)
    })
    
    output$plots5 <- renderPlot({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      make2wayDensity(get("costs", envir=cache), get("effects", envir=cache), 
                      lambda=input$lambdaOverall)
    })
  
    output$plots6 <- renderPlot({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      make4wayEvpiPlot(get("costs", envir=cache), get("effects", envir=cache), lambda=input$lambdaOverall, 
                       prevalence=input$annualPrev, horizon=input$horizon, measure1 = input$currency, 
                       measure2 = input$unitBens)
    })
    
    output$plot7 <- renderPlot({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      makeEvppiBar(get("pEVPI", envir=cache), get("params", envir=cache))
    })
      
    observe({
      x <- input$parameterFile
      y <- input$loadSession
      if (is.null(get("params", envir=cache))) return(NULL)
      updateCheckboxGroupInput(session, "pevpiParameters", 
                               choices = colnames(get("params", envir=cache)))
    })
      

    # get the selection and assign it to cache
    observe({
      currentSelection <- input$pevpiParameters
      assign("currentSelection", currentSelection, envir = cache)
    })
    
    # save the current selection and then increase counter
    observe({
      # counterAdd <- get("counterAdd", envir=cache)
      counterAdd <- input$addSelection
      if(counterAdd==0) return(NULL)
      setStore <- get("setStore", envir=cache)
      currentSelection <- get("currentSelection", envir=cache)
      #nCurrentSelection <- length(currentSelection)
      #nParams <- get("nParams", envir = cache)
      #completedCurrentSelection <- c(currentSelection, rep("", nParams - nCurrentSelection))
      setStore[[counterAdd]] <-currentSelection
      assign("setStore", setStore, envir = cache)
      assign("counterAdd", counterAdd, envir=cache)   
    })

    # output the selection table when add button pressed

    output$selectedTable <- renderTable({
      x <- input$addSelection
      if(x==0) return(NULL)
      setStore <- get("setStore", envir=cache)
      buildSetStoreTable(setStore[1:x])
    }, include.colnames = FALSE, sanitize.rownames.function =  bold.allrows)

    # Output the subset EVPI table
    output$selectedEvpiTable <- renderTable({
      x <- input$calculateSubsetsEvpi
      if(x==0 & get("savedSession", envir=cache)==0) return(NULL)
      counterAdd <- get("counterAdd", envir = cache)
      setStore <- get("setStore", envir=cache)

      #subsetEvpiValues <- calSubsetEvpi(setStore[1:counterAdd])
      subsetEvpiValues <- unlist(lapply(setStore[1:counterAdd], calSubsetEvpi, input$lambdaOverall, cache))
      assign("subsetEvpiValues", subsetEvpiValues, envir = cache)
      assign("setStoreMatchEvpiValues", setStore, envir = cache) # cache these for the report in case they change
      
      #sets <- buildSetStoreTable(setStore[1:counterAdd])
      #df <- data.frame(EVPI = subsetEvpiValues, sets)
      #names(df) <- c("EVPI", rep("", ncol(sets)))
      
      df <- data.frame(EVPI = subsetEvpiValues)      
      rownames(df) <- paste("Set", 1:counterAdd)
      df
    }, sanitize.rownames.function =  bold.allrows)  
    
#     observe({ # clear the selections
#       x <- input$clearSubsetsEvpi
#       if(x==0) return(NULL)
#       counterAdd <- 0
#       setStore <- vector("list", 100)
#       assign("setStore", setStore, envir = cache)
#       assign("counterAdd", counterAdd, envir = cache)
#     })

    
    # Functions that make the reports
    
    # Download csv file
    output$downloadSummary <- downloadHandler(
      filename = "evpi\ values.csv",
      content = function(file) {
        write.csv(calcPartialEvpi(), file)
      })
    
    # Download pdf / html / docx report - NEED TO FIX THE HTML AND DOCX
    output$downloadReport <- downloadHandler(
      filename = function() {#"my-report.pdf"
        paste('my-report', sep = '.', switch(
          input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
        ))
      },
      
      content = function(file) {
        #src <- normalizePath('report.Rmd')
        
        # temporarily switch to the temp dir, in case you do not have write
        # permission to the current working directory
        #owd <- setwd(tempdir())
        #on.exit(setwd(owd))
        #file.copy(src, 'report.Rmd')
        
        library(rmarkdown)
        out <- render('report.Rmd', #pdf_document()
                      switch(
                        input$format,
                        PDF = pdf_document(), HTML = html_document(), Word = word_document_local()),
                      envir = cache
        )
        file.copy(out, file)
      }
    )
    
    
    #output: rmarkdown::default
    #tables: true
    #   output$report <- downloadHandler(
    #     filename = 'myreport.pdf',
    #     content = function(file) {
    #       knit('report.Rnw')
    #       system("pdflatex -synctex=1 -interaction=nonstopmode report.tex")
    #       #out = knit2pdf('report.Rnw', clean = TRUE)
    #       out <- "report.pdf"
    #       file.copy(out, file) # move pdf to file for downloading
    #     },
    #     contentType = 'application/pdf'
    #   )
    
    
    # output$text1 <- renderText({
    #   # if (is.null(dInput())) {paste("No parameters available for selection")} else                         
    #   {checkboxGroupInput("parameter", NULL, colnames_inputs, selected = NULL)}
    #   # need to set this up in a loop so will print over and over again
    # })
    
    # output$text1 <- checkboxGroupInput("parameter", "Parameters", c("a", "b", "c"), selected = NULL)
    
    # Download .Rdata file
    output$saveSession <- downloadHandler(
      filename =  function() paste(input$RdataFileName),
      content = function(file) {
        save(list = ls(envir=cache), file = file, envir=cache)
      },
      contentType = "text/plain")
    
    
    })
