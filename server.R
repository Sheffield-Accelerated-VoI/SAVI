source("scripts.R")
library(mgcv)
library(knitr)
library(rmarkdown)
#print(cacheEnv <- new.env()) # define an environment in which to cache values of costs, etc

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
    
  
    #  Function that loads saved session
    observe({
      inFile = input$loadSession
      if (is.null(inFile))
        return(NULL)
      
      load(inFile$datapath, envir=cache)
      #print(lapply(ls(envir=cache), function(x) object.size(get(x, envir=cache))))
      #print(ls(envir=cache))
    })   
    
    #  Function that imports parameters
    #loadParameters <- 
      observe({
      inFile = input$parameterFile
      if (is.null(inFile))
        return(NULL)
      
      if (input$rownames1) {
        dat <- read.csv(inFile$datapath, header=input$header1, #sep=input$sep,
                        row.names=1)#, dec=input$dec)
        assign("params", dat, envir = cache)
      } else {
        dat <- read.csv(inFile$datapath, header=input$header1)#, sep=input$sep, dec=input$dec)
        assign("params", dat, envir = cache)
      }
    })
    
    #  Function that imports costs    
    #loadCosts <- 
      observe({
      inFile = input$costsFile
      if (is.null(inFile))
        return(NULL)
      
      if (input$rownames2) {
        dat <- read.csv(inFile$datapath, header=input$header2, #sep=input$sep,
                        row.names=1)#, dec=input$dec)
        assign("costs", dat, envir = cache)
      } else {
        dat <- read.csv(inFile$datapath, header=input$header2)#, sep=input$sep, dec=input$dec)
        assign("costs", dat, envir = cache)
      }
    })
    
    # Function that imports effects
    #loadEffects <- 
      observe({
      inFile = input$effectsFile      
      if (is.null(inFile))
        return(NULL)
      
      if (input$rownames3) {
        dat <- read.csv(inFile$datapath, header=input$header3, #sep=input$sep,
                        row.names=1)#, dec=input$dec)
        assign("effects", dat, envir = cache)
      } else {
        dat <- read.csv(inFile$datapath, header=input$header3)#, sep=input$sep, dec=input$dec)
        print(assign("effects", dat, envir = cache))
      }
    })
    
    # Functions that render the data files and pass them to ui.R
    
    output$checktable1 <- renderTable({
      x <- input$parameterFile 
      y <- input$loadSession
      tableValues <- get("params", envir=cache)
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
    
    calcPartialEvpi <- reactive({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      inb <- createInb(get("costs", envir=cache), get("effects", envir=cache), 
                       input$lambda, input$incremental)
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
            The mean incremental cost of", input$t3, "versus", input$current,"is",input$currency,"X. This suggests that
            ",input$t3,"is more/less costly over the",input$horizon,"year time horizon. There is some uncertainty due to model 
            parameters, with the 95% CI for the incremental cost ranging from (lower CI, upper CI).  
            The probability that",input$t3,"is cost saving (i.e. cheaper over the",input$horizon,"year time horizon) compared 
            to",input$current,"is XX%.")
    })                       ###THIS FUNCTION STILL NEEDS TO BE MADE REACTIVE TO RESULTS
    
    output$textCEplane2 <- renderText({

      paste("The mean incremental benefit of", input$t3, "versus", input$current, "is", input$t6, "X.  This suggests that",input$t3,"
            is more/or less beneficial over the",input$n7,"year time horizon.  Again, there is some uncertainty due to 
            model parameters, with the 95% CI for the incremental benefit ranging from (lower credible interval, upper CI).
            The probability that",input$t3,"is more beneficial than",input$current,"is XX%.")
    })                        ###THIS FUNCTION STILL NEEDS TO BE MADE REACTIVE TO RESULTS
    
    output$textCEplane3 <- renderText({
      paste("The incremental expected cost per unit of benefit is estimated at",input$currency,"XX per",input$unitBens,". This 
            is above/below the threshold of",input$currency,input$lambda2,"per",input$unitBens,"indicating that",input$t3,"would (not) be considered cost-effective
            at this threshold.  There is uncertainty with a XX% probability that",input$t3,"is more cost-effective (XX% of the 
            probabilistic model run ‘dots’ are below and to the right of the diagonal threshold line).")
    })                         ###THIS FUNCTION STILL NEEDS TO BE MADE REACTIVE TO RESULTS
    
    output$textCEplane4 <- renderText({
      paste(input$t3,"vs.",input$current)
    })
    
    output$textCEplane5 <- renderText({
      paste("$XX%$ probability that",input$t3,"is more cost-effective than",input$current,"at a threshold 
            of",input$currency,input$lambda2,"per",input$unitBens)
    })                       ###THIS FUNCTION STILL NEEDS TO BE MADE REACTIVE TO RESULTS
    
    output$textCEAC1 <- renderText({
      paste("This graph shows the cost-effectiveness acceptability curve for the comparison of strategies. The results show that at a threshold 
            value for cost-effectiveness of",input$currency,input$lambda2,"per",input$unitBens,"the strategy with the highest probability of being most cost-effective 
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
      tableCEplane <- matrix(c(input$lambda2, input$current, input$nIterate, NA, NA, NA, NA, NA, NA, NA, NA, NA), nrow = 12, ncol = ncol(get("costs", envir=cache))-1)
      colnames(tableCEplane) <- colnames(get("costs", envir=cache)[,-1])
      rownames(tableCEplane) <- c(paste("Threshold (", input$currency, ")"), "Comparator", "Number of PSA runs", paste("Mean inc. Effect per Person (", input$unitBens, ")"), paste("Mean inc. Cost per Person (", input$currency, ")"),
                                  paste("ICER Estimate (", input$currency, "per", input$unitBens, ")"), "PSA RESULTS", paste("95% CI for inc. Costs (", input$currency, ")"), 
                                  paste("95% CI for inc. Effects (", input$unitBens, ")"), "Probability intervention is cost saving", "Probability intervention provides more benefit", 
                                  "Probability that intervention is cost-effective")
      tableCEplane
    })  

   output$tableNetBenefit <- renderTable({
     if (!valuesImportedFLAG(cache, input)) return(NULL)
     tableNetBenefit <- matrix(NA, nrow = 8, ncol = ncol(get("costs", envir=cache)))
     colnames(tableNetBenefit) <- colnames(get("costs", envir=cache))
     rownames(tableNetBenefit) <- c(paste("Mean", input$effectDef), paste("Mean", input$costDef), paste("Expected Net Benefit at",input$currency,input$lambda2,"per",input$unitBens), 
                                  "95% Lower CI (on Costs Scale)", "95% Upper CI (on Costs Scale)", "Expected Net Benefit on Effects Scale", "95% Lower CI (on Effects Scale)", "95% Upper CI (on Effects Scale)")
     tableNetBenefit
   })  
   
   output$tableEVPI <- renderTable({
     if (!valuesImportedFLAG(cache, input)) return(NULL)
     tableEVPI <- matrix(NA, nrow = 7, ncol = 2)
     colnames(tableEVPI) <- c(paste("Overall EVPI Financial Valuation (", input$currency, ")"), paste("Overall EVPI", input$unitBens, "Valuation"))
     rownames(tableEVPI) <- c("Per Person Affected by the Decision", paste("Per Year in", input$jurisdiction, "Assuming", input$annualPrev, "Persons Affected per Year"), 
                              "Over 5 Years", "Over 10 Years", "Over 15 Years", "Over 20 years", paste("Over Specified Decision Relevance Horizon (", input$horizon, "years)"))
     tableEVPI
   }) 
   
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
                      lambda=input$lambda2, xlab=input$effectDef, 
                      ylab=input$costDef, col="orangered")
    })  ###NEED TO ADD POINT FOR MEAN ON PLOT - SHOULD BE LARGER AND BRIGHTER (E.G.DARK RED, STANDARD SIZE AND SOLID WOULD WORK WELL)
    
    output$plots2 <- renderPlot({
    
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      ceac.obj <- assign("ceac.obj", ceac(), envir=cache)
      makeCeacPlot(ceac.obj, lambda=input$lambda2, 
                   main="Cost-effectiveness Acceptability Curve", 
                   xlab="Threshold willingness to pay", 
                   ylab="Probability strategy is cost-effective",
                   names=colnames(get("costs", envir=cache)))
    })  ###NEED TO ADD % COST-EFFECTIVENESS AT LINE AS A LABEL 
    
    output$plots3 <- renderPlot({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      makeEvpiPlot(get("costs", envir=cache), get("effects", envir=cache), 
                   main=input$main3, 
                   xlab="Threshold willingness to pay", 
                   ylab="Overall EVPI per person affected (on costs scale)",
                   col="red", input$incremental, costscale = TRUE)
    })
    
    output$plots4 <- renderPlot({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      makeEvpiPlot(get("costs", envir=cache), get("effects", envir=cache), 
                   main=input$main4, 
                   xlab="Threshold willingness to pay", 
                   ylab="Overall EVPI per person affected (on effects scale)",
                   col="red", input$incremental, costscale = FALSE)
    })
    
    output$plots4way <- renderPlot({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      make4wayPlot(get("costs", envir=cache), get("effects", envir=cache), 
                   get("ceac.obj", envir=cache), lambda=input$lambda2, main=input$main1, 
                   xlab=input$xlab2, ylab=input$ylab2, col=input$color2, 
                                       main2=input$main4, xlab2=input$xlab4, 
                                       ylab2=input$ylab4,
                                       col2=input$color4)
    })
    
    output$plots5 <- renderPlot({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      makeNbDensity(get("costs", envir=cache), get("effects", envir=cache), 
                      lambda=input$lambda2)
    })
    
    output$plots6 <- renderPlot({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      ### Need to replace 1 with usual care/base column number
      makeInbBaseDens(get("costs", envir=cache), get("effects", envir=cache), 1,
                      lambda=input$lambda2)
    })

    output$plots7 <- renderPlot({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      makeInbOptDens(get("costs", envir=cache), get("effects", envir=cache), 
                      lambda=input$lambda2)
    })
    
    observe({
      x <- input$parameterFile
      y <- input$loadSession
      if (is.null(get("params", envir=cache))) return(NULL)
      updateCheckboxGroupInput(session, "pevpiParameters", 
                               choices = colnames(get("params", envir=cache)))
    })
      
#     output$selection <- reactive({
#       if(input$addSelection==0) return(NULL)
#       print(counterAdd <- input$addSelection)
#       assign("counterAdd", counterAdd, envir=cache)
# 
#       setStore <- get("setStore", envir=cache)
#       setStore[[counterAdd]] <- input$pevpiParameters
#       assign("setStore", setStore, envir = cache)
#       
#       setStore[1:counterAdd]
#       })
 
    observe({
      setStore <- get("setStore", envir=cache)
      counterAdd <- get("counterAdd", envir = cache)
      setStore[[counterAdd]] <- input$pevpiParameters
      assign("setStore", setStore, envir = cache)
    })
    
    #fix the selection
    output$selection <- reactive({
      if(input$addSelection==0) return(NULL)
      print(counterAdd <- input$addSelection)
      assign("counterAdd", counterAdd, envir=cache)
      setStore <- get("setStore", envir=cache)
      setStore[1:counterAdd]
    })
    
    
    # output$selectionParameters <-observe({
    #   params <- input$parameter.file
    #   colnames(params)
    # })
    #   
    #  updater <- observe({params <- input$parameter.file
    #     colnamesParams <- colnames(input$parameter.file)
    #     updateCheckboxGroupInput(session, "pevpiParameters", label = colnamesParams)
    #   })
    
    
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
