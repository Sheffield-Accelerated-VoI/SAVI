# source all the functions we need

source("scripts.R")
source("scripts_GPfunctions.R") # separate file to hold the GPfunctions
source("scripts_GAMfunctions.R")
source("scripts_plots.R")
source("scripts_tables.R")
source("scripts_text.R")

# load the libraries we need

library(MASS)
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
    assign("subsetEvpiValues", NULL, envir=cache)
    
    # these three rows autoload values for testing purposes - to avoid having to load them manually. MS
    # ###########
    #   load.parameters <- function() read.csv("parameters.csv")                                   
    #   load.costs <- function() read.csv("costs.csv")
    #   load.effects <- function() read.csv("effects.csv")
    # ########### 
    
 # load("SAVISession.Rdata", envir=cache)
    #  Function that loads saved session
    observe({
      inFile = input$loadSession
      if (is.null(inFile))
        return(NULL)
      assign("savedSession", 1, envir=cache)
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
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      paste("This graph shows the standardised cost-effectiveness plane per person based on ", nrow(get("params", envir=cache)), 
            " model runs, in which uncertain model parameters are varied simultaneously in a probabilistic sensitivity analysis.  
            The mean incremental cost of ", input$t3, " versus ", input$current," is ", input$currency, incValue(get("costs", envir=cache)), 
            ". This suggests that ",input$t3," is ", moreLess(get("costs", envir=cache)), " costly. There is some uncertainty due to model 
            parameters, with the 97.5% confidence interval for the incremental cost ranging from ", cI(get("costs", envir=cache), 0.025), 
            " to ", cI(get("costs", envir=cache), 0.975),". The probability that ", input$t3, " is cost saving compared to ",input$current,
            " is ", pCostsaving(get("costs", envir=cache)), ".", sep="")
    })                       
    
    output$textCEplane2 <- renderText({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      paste("The mean incremental benefit of ", input$t3, " versus ", input$current, " is ", incValue(get("effects", envir=cache)), " ", 
            input$unitBens, "s.  This suggests that ",input$t3," is ", moreLess(get("effects", envir=cache)), " beneficial.  
            Again, there is some uncertainty due to model parameters, with the 97.5% confidence interval for the incremental benefit ranging 
            from ", confIntCE(get("effects", envir=cache), 0.025), " to ", confIntCE(get("effects", envir=cache), 0.975),". The probability that ",input$t3,
            " is more beneficial than ",input$current," is ", pMoreben(get("effects", envir=cache)), ".", sep="")
    })                        
    
    output$textCEplane3 <- renderText({
      if (!valuesImportedFLAG(cache, input)) return(NULL)      
      paste("The expected incremental cost per ", input$unitBens," (ICER) is estimated at ", input$currency, iCER(get("costs", envir=cache), 
            get("effects", envir=cache)), ". This is ", aboveBelow(get("costs", envir=cache), get("effects", envir=cache), input$lambdaOverall),  
            " the threshold of ", input$currency, input$lambdaOverall, " per ", input$unitBens, " indicating that ", input$t3, " ",
            wouldNot(get("costs", envir=cache), get("effects", envir=cache), input$lambdaOverall), " be considered cost-effective 
            at this threshold. There is uncertainty with a ", pCE(get("costs", envir=cache), get("effects", envir=cache), input$lambdaOverall), 
            " probability that ", input$t3, " is more cost-effective (", pCE(get("costs", envir=cache), get("effects", envir=cache), input$lambdaOverall), 
            " of the probabilistic model run dots are below and to the right of the diagonal threshold line).", sep="")
    })                         
    
    output$textCEplane4 <- renderText({
      if (!valuesImportedFLAG(cache, input)) return(NULL) 
      paste(input$t3,"vs.",input$current)
    })
    
    output$textCEplane5 <- renderText({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      paste(pCE(get("costs", envir=cache), get("effects", envir=cache), input$lambdaOverall), " probability that ",input$t3," is more cost-effective 
      than ",input$current," at a threshold of ",input$currency, input$lambdaOverall," per ",input$unitBens, sep="")
    })                       
    
    output$textCEAC1 <- renderText({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      paste("This graph shows the cost-effectiveness acceptability curve for the comparison of strategies. The results show that at a threshold 
            value for cost-effectiveness of ",input$currency, input$lambdaOverall," per ",input$unitBens," the strategy with the highest 
            probability of being most cost-effective is X, with a probability of ", pCE(get("costs", envir=cache), get("effects", envir=cache), 
            input$lambdaOverall), ". More details on how to interpret CEACs are available from the literature.", sep="")
    })                       
                       
    output$textNB1 <- renderText({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      paste("Net benefit is a calculation that puts ", input$costDef, " and ", input$effectDef, " onto the same scale.  This is done by calculating 
            the monetary value of ", input$effectDef, " using a simple multiplication i.e. ", input$unitBens, "s * lambda, where:", sep="")
    })  

    output$textNB2 <- renderText({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      paste("Net benefit for a strategy = ", input$unitBens, "s * ", input$lambdaOverall, " - Cost (" ,input$currency, ").", sep="")
    }) 

    output$textNB3 <- renderText({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      paste("The plot below shows the expected net benefit of the ", get("nInt", envir=cache), " strategies, together with the 95% credible 
            interval for each one.  The strategy with highest expected net benefit is estimated to be $SmaxexpNB$, with an expected net benefit of 
            ", input$currency, netBencosts(get("costs", envir=cache), get("effects", envir=cache), input$lambdaOverall, get("nInt", envir=cache)),
            " (equivalent to a net benefit on the effectiveness scale of ", netBeneffects(get("costs", envir=cache), get("effects", envir=cache), 
            input$lambdaOverall, get("nInt", envir=cache)), input$unitBens, "s. The 95% credible interval suggests that the net benefit of $SmaxexpNB$ 
            could range from ", input$currency, confIntNBC(get("costs", envir=cache), get("effects", envir=cache), input$lambdaOverall, get("nInt", envir=cache), 0.025),
            " to ", input$currency, confIntNBC(get("costs", envir=cache), get("effects", envir=cache), input$lambdaOverall, get("nInt", envir=cache), 0.975), " (",
            confIntNBE(get("costs", envir=cache), get("effects", envir=cache), input$lambdaOverall, get("nInt", envir=cache), 0.025), " ", input$unitBens, "s to ",
            confIntNBE(get("costs", envir=cache), get("effects", envir=cache), input$lambdaOverall, get("nInt", envir=cache), 0.975), " ", input$unitBens, "s on effect scale).", sep="")
    }) 

    output$textEVPI1 <- renderText({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      paste("The overall EVPI per person affected by the decision is estimated at ", input$currency, ". $overall EVPIperperson$.  This is equivalent to 
            $overallEVPIusingeffectneasure$ per person’s worth of decision uncertainty on the ", input$unitBens, " scale.", sep="")
    })     

    output$textEVPI2 <- renderText({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      paste("Assuming an annual number of people affected by the decision of " , input$annualPrev, ", the overall EVPI per year is ", input$currency, "
            $overall EVPIper jusrisdictionperyear$ for ", input$jurisdiction, ".", sep="")
    }) 

    output$textEVPI3 <- renderText({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      paste("When thinking about the overall expected value of removing decision uncertainty, one needs to consider how long the current comparison 
            will remain relevant e.g. if new treatments of options or even cures are anticipated to become available for a disease.  For the specified 
            decision relevance horizon of ", input$horizon, " years, the overall expected value of removing decision uncertainty for ", 
            input$jurisdiction, " would in total be ", input$currency, " $overallevpidecisionrelevance$.", sep="")
    }) 

    output$textEVPI4 <- renderText({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      paste("Research or data collection exercises costing more than this amount would not be considered cost-effective use of resources. This is because 
            the return on investment from the research – as measured by the health gain and cost savings of enabling decision makers ability to switch and 
            select other strategies when evidence obtained reduces decision uncertainty – is expected to be no higher than the figure of ", input$currency, 
            " $overallevpidecisionrelevance$.", sep="")
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
     assign("overallEvpi", overallEvpi, envir = cache)
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
     lambda <- input$lambdaOverall # re-run if labmda changes
     params <- get("params", envir=cache)
     costs <- get("costs", envir=cache)
     effects <- get("effects", envir=cache)

     overallEvpi <- calcEvpi(costs, effects, lambda)
     assign("overallEvpi", overallEvpi, envir = cache)
     
     inb <- createInb(costs, effects, lambda)
     pEVPI <- applyCalcSingleParamGam(params, inb)
     assign("pEVPI", pEVPI, envir=cache)
     
     tableEVPPI <- matrix(NA, nrow = ncol(params), ncol = 4)
     tableEVPPI[, 1] <- round(pEVPI, 2)
     tableEVPPI[, 2] <- round(pEVPI / overallEvpi , 2)
     tableEVPPI[, 3] <- signif(pEVPI * input$annualPrev, 4)
     tableEVPPI[, 4] <- signif(pEVPI * input$annualPrev * input$horizon, 4)
     colnames(tableEVPPI) <- c(paste("Per Person EVPPI (", input$currency, ")"), "Indexed Overall EVPI = 1.00", 
                               paste("EVPPI for ", input$jurisdiction, " Per Year"), 
                               paste("EVPPI for ", input$jurisdiction, " over ", input$horizon, " years", sep=""))
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
    
    output$plots5a <- renderPlot({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      makeInbOptBar(get("costs", envir=cache), get("effects", envir=cache), 
                      lambda=input$lambdaOverall)
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
      if(x==0) return(NULL)
      counterAdd <- get("counterAdd", envir = cache)
      setStore <- get("setStore", envir=cache)

      #first pull down the existing values
      print(subsetEvpiValues <- get("subsetEvpiValues", envir = cache))
      if (is.null(subsetEvpiValues)) {
        subsetEvpiValues <- t(sapply(setStore[1:counterAdd], calSubsetEvpi, input$lambdaOverall, cache))
      } else {
        newEvpiValue <- t(sapply(setStore[(NROW(subsetEvpiValues)+1):counterAdd], calSubsetEvpi, input$lambdaOverall, cache))
        subsetEvpiValues <- rbind(subsetEvpiValues, newEvpiValue)
      }
      #subsetEvpiValues <- unlist(lapply(resultsList, function(x) x$evpi)
      #subsetEvpiValues <- unlist(lapply(setStore[1:counterAdd], calSubsetEvpi, input$lambdaOverall, cache))
      assign("subsetEvpiValues", subsetEvpiValues, envir = cache)
      assign("setStoreMatchEvpiValues", setStore, envir = cache) # cache these for the report in case they change
      
      #sets <- buildSetStoreTable(setStore[1:counterAdd])
      #df <- data.frame(EVPI = subsetEvpiValues, sets)
      #names(df) <- c("EVPI", rep("", ncol(sets)))
      
      df <- data.frame(subsetEvpiValues)   
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
