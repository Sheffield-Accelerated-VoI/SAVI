#######################################
# START SHINY SERVER
#######################################

print("server.R called") # this is called when we start the shiny server on SAVI via $ sudo start shiny-server

# max upload for files
options(shiny.maxRequestSize=100*1024^2) # increase max upload to 100Mb

# debugging option
options(shiny.reactlog=FALSE)
# options(shiny.reactlog=TRUE) # only set to true for debugging. MUST BE FALSE FOR LIVE USE

# load the libraries we need
library(MASS)
library(mgcv)
library(knitr)
library(rmarkdown)
library(xtable)

# source all the functions we need
source("scripts.R")
source("scripts_GPfunctions.R")
source("scripts_GAMfunctions.R")
source("scripts_plots.R")
source("scripts_tables.R")
source("scripts_text.R")


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
    
    print(ls())
    
    assign("savedSession", 0, envir=cache)
    assign("nIterate", 0, envir = cache)
    assign("nInt", 0, envir = cache)
    assign("pEVPI", NULL, envir = cache)
    assign("params", NULL, envir = cache)
    assign("costs", NULL, envir = cache)  
    assign("effects", NULL, envir = cache) 
    assign("counterAdd", 0, envir = cache)     
    assign("setStore", vector("list", 100), envir = cache) # up to 100 sets for the group inputs
    assign("subsetEvpiValues", NULL, envir=cache)
    assign("setStoreMatchEvpiValues", NULL, envir=cache)
    assign("currentSelection", NULL, envir=cache)
    assign("ceac.obj", NULL, envir=cache)

    # assign null values to the about the model variables in the cache
    assign("modelName", NULL, envir = cache)
    assign("current", NULL, envir = cache)    
    assign("t3", NULL, envir = cache)       
    assign("lambdaOverall", 0, envir = cache)  
    assign("effectDef", NULL, envir = cache)
    assign("costDef", NULL, envir = cache)
    assign("annualPrev", 0, envir = cache)
    assign("horizon", 0, envir = cache)
    assign("currency", NULL, envir = cache)
    assign("unitBens", NULL, envir = cache)
    assign("jurisdiction", NULL, envir = cache)

    # these three rows autoload values for testing purposes - to avoid having to load them manually. MS
    # ###########
    #   load.parameters <- function() read.csv("parameters.csv")                                   
    #   load.costs <- function() read.csv("costs.csv")
    #   load.effects <- function() read.csv("effects.csv")
    # ########### 
    
    load("adenoma.Rdata", envir=cache) # auto load for testing purposes

    # Function that loads saved session
    # is evaluated if a new session is loaded
    
    observe({
      inFile = input$loadSession
      if (is.null(inFile)) return(NULL)
      load(inFile$datapath, envir=cache)
      
      # update "about the model" variables
      updateTextInput(session, "modelName", value = get("modelName", envir=cache))
      updateTextInput(session, "current",  value = get("current", envir=cache))
      updateTextInput(session, "t3",  value = get("t3", envir=cache))
      updateNumericInput(session, "lambdaOverall",  value = get("lambdaOverall", envir=cache))
      updateTextInput(session, "effectDef",  value = get("effectDef", envir=cache))
      updateTextInput(session, "costDef",  value = get("costDef", envir=cache))
      updateNumericInput(session, "annualPrev",  value = get("annualPrev", envir=cache))
      updateNumericInput(session, "horizon",  value = get("horizon", envir=cache))
      updateTextInput(session, "currency",  value = get("currency", envir=cache))
      updateTextInput(session, "unitBens",  value = get("unitBens", envir=cache))
      updateTextInput(session, "jurisdiction",  value = get("jurisdiction", envir=cache))
      
      # set the group EVPI objects to NULL / 0
      assign("counterAdd", 0, envir = cache)     
      assign("setStore", vector("list", 100), envir = cache) # up to 100 sets for the group inputs
      assign("subsetEvpiValues", NULL, envir=cache)
      assign("setStoreMatchEvpiValues", NULL, envir=cache)
      assign("currentSelection", NULL, envir=cache)
      
      assign("savedSession", 1, envir=cache)    # not used
      
    })
    
    #  Function that imports parameters
      observe({
      inFile = input$parameterFile
      if (is.null(inFile))
        return(NULL)
        dat <- read.csv(inFile$datapath)
        assign("params", dat, envir = cache)
        assign("nParams", ncol(dat), envir=cache)
        assign("nIterate", nrow(dat), envir=cache) # size of PSA
    })

      #  Function that imports costs    
      observe({
      inFile = input$costsFile
      if (is.null(inFile))
        return(NULL)
        dat <- read.csv(inFile$datapath)
        assign("costs", dat, envir = cache)
        assign("nInt", ncol(dat), envir=cache) # number of interventions
    })
    
     # Function that imports effect
      observe({
      inFile = input$effectsFile      
      if (is.null(inFile))
        return(NULL)
        dat <- read.csv(inFile$datapath)
        assign("effects", dat, envir = cache)
    })
    


    # Function that saves "about the model" variables to the cache if they are changed in the input.

    observe({
      assign("modelName", input$modelName, envir = cache)
      assign("current", input$current, envir = cache)    
      assign("t3", input$t3, envir = cache)       
      assign("lambdaOverall", input$lambdaOverall, envir = cache)  
      assign("effectDef", input$effectDef, envir = cache)
      assign("costDef", input$costDef, envir = cache)
      assign("annualPrev", input$annualPrev, envir = cache)
      assign("horizon", input$horizon, envir = cache)
      assign("currency", input$currency, envir = cache)
      assign("unitBens", input$unitBens, envir = cache)
      assign("jurisdiction", input$jurisdiction, envir = cache)
    })



    # Functions that render the data files and pass them to ui.R
    
    output$checktable1 <- renderTable({
      x <- input$parameterFile 
      y <- input$loadSession
      tableValues <- get("params", envir=cache)
      # assign("nParamSamples", nrow(tableValues), envir=cache)
      if (is.null(tableValues)) return(NULL)
      head(tableValues, n=5)
    })
    
    output$checktable2 <- renderTable({
      x <- input$costsFile 
      y <- input$loadSession
      tableValues <- get("costs", envir=cache)

      if (is.null(tableValues)) return(NULL)
      head(tableValues, n=5)  
    })
    
    output$checktable3 <- renderTable({
      x <- input$effectsFile 
      y <- input$loadSession
      tableValues <- get("effects", envir=cache)
      if (is.null(tableValues)) return(NULL)
      head(tableValues, n=5)
    })
      
    # function that calculates ceac
    ceac <- reactive({ 
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      makeCeac(get("costs", envir=cache), get("effects", envir=cache), input$lambdaOverall, session)
    })
        
  
    # Functions that make reactive text to accompany plots

    output$textCEplane1 <- renderText({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      paste("The figure above shows the (standardised) cost-effectiveness plane based on the ", get("nIterate", envir=cache), 
            " model runs in the probabilistic sensitivity analysis. The willingness-to-pay threshold is shown as a 45 degree line. 
            The mean incremental cost of ", input$decisionOptionCE1, " versus ",  input$decisionOptionCE0," is ",
            input$currency, incValue(get("costs", envir=cache), input$decisionOptionCE1, input$decisionOptionCE0), ". This suggests that ", input$decisionOptionCE1, " is ", 
            moreLess(get("costs", envir=cache), input$decisionOptionCE1, input$decisionOptionCE0), " costly. The incremental cost is uncertain because the model parameters are uncertain. 
            The 97.5% credible interval for the incremental cost ranges from ", input$currency, confIntCE(get("costs", envir=cache), input$decisionOptionCE1, input$decisionOptionCE0, 0.025)," to ", 
            input$currency, confIntCE(get("costs", envir=cache), input$decisionOptionCE1, input$decisionOptionCE0, 0.975),". The probability that ", input$decisionOptionCE1, " is cost 
            saving compared to ", input$decisionOptionCE0," is ", pCostsaving(get("costs", envir=cache), input$decisionOptionCE1, input$decisionOptionCE0), ".", sep="")
    })                       
    
    output$textCEplane2 <- renderText({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      paste("The mean incremental benefit of ", input$decisionOptionCE1, " versus ", input$decisionOptionCE0, " is ", 
            incValue(get("effects", envir=cache), input$decisionOptionCE1, input$decisionOptionCE0), " ",input$unitBens, "s.  This suggests that ", input$decisionOptionCE1," is ", 
            moreLess(get("effects", envir=cache), input$decisionOptionCE1, input$decisionOptionCE0), " beneficial. Again, there is uncertainty in the incremental benefit 
            due to uncertainty in the model parameters. The 97.5% 
            credible interval for the incremental benefit ranges from ", confIntCE(get("effects", envir=cache), input$decisionOptionCE0, input$decisionOptionCE1, 0.025), " ", input$unitBens, "s to ", 
            confIntCE(get("effects", envir=cache), input$decisionOptionCE0, input$decisionOptionCE1, 0.975), " ", input$unitBens,"s. The probability that ", input$decisionOptionCE1, 
            " is more beneficial than ", input$decisionOptionCE0, " is ", pMoreben(get("effects", envir=cache), input$decisionOptionCE1, input$decisionOptionCE0), ".", sep="")
    })                        
    
    output$textCEplane3 <- renderText({
      if (!valuesImportedFLAG(cache, input)) return(NULL)      
      paste("The expected incremental cost per ", input$unitBens," (ICER) is estimated at ", input$currency, iCER(get("costs", envir=cache), 
            get("effects", envir=cache), input$decisionOptionCE1, input$decisionOptionCE0), ". There is a probability of ", pCE(input$decisionOptionCE1, input$decisionOptionCE0, input$lambdaOverall, cache), 
            " that ", input$decisionOptionCE1, " is more cost-effective than ", input$decisionOptionCE0, ".", sep="")
    })                         
    
    
#     This is ", 
#             aboveBelow(get("costs", envir=cache), get("effects", envir=cache), input$decisionOptionCE1, input$decisionOptionCE0, input$lambdaOverall),  
#             " the threshold of ", input$currency, input$lambdaOverall, " per ", input$unitBens, " indicating that ", 
#     input$decisionOptionCE1,
#     " ", wouldNot(get("costs", envir=cache), get("effects", envir=cache), input$decisionOptionCE1, input$decisionOptionCE0, input$lambdaOverall), " be considered cost-effective 
#     relative to ", input$decisionOptionCE0, " at this threshold. 
    
    output$textCEplane4 <- renderText({
      if (!valuesImportedFLAG(cache, input)) return(NULL) 
      paste(input$decisionOptionCE1, "versus", input$decisionOptionCE0)
    })
    
    output$textCEplane5 <- renderText({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      paste("There is a ", pCE(input$decisionOptionCE1, input$decisionOptionCE0, input$lambdaOverall, cache), " probability that ", input$decisionOptionCE1, " is more cost-effective 
      than ", input$decisionOptionCE0, " at a threshold of ",input$currency, input$lambdaOverall," per ",input$unitBens, sep="")
    })                       
    
    output$textCEAC1 <- renderText({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      paste("This graph shows the cost-effectiveness acceptability curve for the comparison of strategies. The results show that at a threshold 
            value for cost-effectiveness of ",input$currency, input$lambdaOverall," per ",input$unitBens," the strategy with the highest 
            probability of being most cost-effective is ", bestCE(get("costs", envir=cache), get("effects", envir=cache), input$lambdaOverall, get("nInt", envir=cache)), 
            ", with a probability of ", pCE(input$decisionOptionCE1, input$decisionOptionCE0, input$lambdaOverall, cache),
            ". More details on how to interpret CEACs are available from the literature.", sep="")
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
     paste("The plot below shows the expected net benefit of the ", get("nInt", envir=cache), " strategies, together with the 97.5% credible 
           interval for each one.  The strategy with highest expected net benefit is ", bestnetBen(get("costs", envir=cache), 
           get("effects", envir=cache), input$lambdaOverall, get("nInt", envir=cache)), ", with an expected net benefit of 
           ", input$currency, netBencosts(get("costs", envir=cache), get("effects", envir=cache), input$lambdaOverall, get("nInt", envir=cache)),
           " (equivalent to a net benefit on the effectiveness scale of ", netBeneffects(get("costs", envir=cache), get("effects", envir=cache), 
           input$lambdaOverall, get("nInt", envir=cache)), " ", input$unitBens, "s). Net benefit and 97.5% credible intervals for all strategies 
           are presented in the above table. ", sep="")
   }) 

   output$textEVPI1 <- renderText({
     if (!valuesImportedFLAG(cache, input)) return(NULL)
     paste("The overall EVPI per person affected by the decision is estimated to be ", input$currency, format(calcEvpi(get("costs", envir=cache), 
          get("effects", envir=cache), input$lambdaOverall), digits = 4, nsmall=2), ".  This is equivalent to ", 
          format(calcEvpi(get("costs", envir=cache), get("effects", envir=cache), input$lambdaOverall)/input$lambdaOverall, digits = 4, nsmall=1), " ", input$unitBens,
          "s per person on the health effects scale.", sep="")
   })     

    output$textEVPI2 <- renderText({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      paste("If the number of people affected by the decision per year is " , input$annualPrev, ", then the overall EVPI per year is ", input$currency,
            format(calcEvpi(get("costs", envir=cache), get("effects", envir=cache), input$lambdaOverall)*input$annualPrev, digits = 4, nsmall=2), " for ", input$jurisdiction, ".", sep="")
    }) 

    output$textEVPI3 <- renderText({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      paste("When thinking about the overall expected value of removing decision uncertainty, one needs to consider how long the current comparison 
            will remain relevant. If the decision relevance horizon is ", input$horizon, " years, then the overall expected value of removing 
            decision uncertainty for ", 
            input$jurisdiction, " would be ", input$currency, format(calcEvpi(get("costs", envir=cache), get("effects", envir=cache), 
            input$lambdaOverall)*input$annualPrev*input$horizon, digits = 4, nsmall=2),".", sep="")
    }) 

    output$textEVPI4 <- renderText({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      paste("Research or data collection exercises costing more than this amount would not be considered an efficient use of resources. This is because 
            the return on investment from the research – as measured by the health gain and cost savings resulting from enabling the decision maker to better 
            identify the best decision  option – is expected to be no higher than ", input$currency, 
            format(calcEvpi(get("costs", envir=cache), get("effects", envir=cache), input$lambdaOverall)*input$annualPrev*input$horizon, digits = 4, nsmall=2),".", sep="")  
      }) 

    output$textEVPI5 <- renderText({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      paste("The EVPI estimates in the table below quantify the expected value to decision makers within ", input$jurisdiction, " of removing all current 
            decision uncertainty at a threshold of ", input$currency, input$lambdaOverall, " per ", input$unitBens, ".  This will enable comparison against 
            previous analyses to provide an idea of the scale of decision uncertainty in this topic compared with other previous decisions. The EVPI estimate 
            for a range of willingness-to-pay thresholds are illustrated in the figures below the table.", sep="")
    })


    # Functions that make tables 

    output$tableCEplane <- renderTable({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      tableCEplane <- makeTableCePlane(lambda=input$lambdaOverall, input$decisionOptionCE0, cache)
      assign("lambdaOverall", input$lambdaOverall, envir = cache)
      rownames(tableCEplane) <- c(paste("Threshold (", input$currency, ")", sep=""), 
                            "Comparator", 
                            "Number of PSA runs", 
                            paste("Mean inc. Effect per Person (", input$unitBens, ")", sep=""), 
                            paste("Mean inc. Cost per Person (", input$currency, ")", sep=""),
                            paste("ICER Estimate (", input$currency, " per ", input$unitBens, ")", sep=""),
                            paste("2.5th CI for inc. Effects (", input$unitBens, ")", sep=""), 
                            paste("97.5th CI for inc. Effects (", input$unitBens, ")", sep=""),
                            paste("2.5th CI for inc. Costs (", input$currency, ")", sep=""),
                            paste("97.5th CI for inc. Costs (", input$currency, ")", sep=""),
                            "Probability intervention is cost saving", 
                            "Probability intervention provides more benefit", 
                            "Probability that intervention is cost-effective against comparator")
      tableCEplane
    })  

    output$tableNetBenefit <- renderTable({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
     tableNetBenefit <- makeTableNetBenefit(get("costs", envir=cache), get("effects", 
                            envir=cache), lambda=input$lambdaOverall, get("nInt", envir=cache))
     assign("lambdaOverall", input$lambdaOverall, envir = cache)
     rownames(tableNetBenefit) <- c(paste("Mean", input$effectDef), 
                                    paste("Mean", input$costDef), 
                                    paste("Expected Net Benefit at", 
                                          input$currency, input$lambdaOverall, "per", input$unitBens), 
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
     colnames(tableEVPI) <- c(paste("Overall EVPI monetary scale (", input$currency, ")", sep=""), paste("Overall EVPI", input$unitBens, "scale"))
     rownames(tableEVPI) <- c("Per Person Affected by the Decision", 
                              paste("Per Year in", input$jurisdiction, "Assuming", input$annualPrev, "Persons Affected per Year"), 
                              "Over 5 Years", 
                              "Over 10 Years", 
                              "Over 15 Years", 
                              "Over 20 years", 
                              paste("Over Specified Decision Relevance Horizon (", input$horizon, "years)"))
     
#      overallEvpi <- ifelse(input$indSim, calcEvpiSingle(get("costs", envir=cache), get("effects", envir=cache), 
#                                                   lambda=input$lambdaOverall, cache, session),
#                            calcEvpi(get("costs", envir=cache), get("effects", envir=cache), 
#              lambda=input$lambdaOverall, cache, session))
     overallEvpi <- calcEvpi(get("costs", envir=cache), get("effects", envir=cache), 
                                           lambda=input$lambdaOverall, cache, session)
     assign("overallEvpi", overallEvpi, envir = cache)
     assign("lambdaOverall", input$lambdaOverall, envir = cache)
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
     assign("lambdaOverall", input$lambdaOverall, envir = cache)
     params <- get("params", envir=cache)
     costs <- get("costs", envir=cache)
     effects <- get("effects", envir=cache)

     overallEvpi <- calcEvpi(costs, effects, lambda)
     assign("overallEvpi", overallEvpi, envir = cache)
     
     inb <- createInb(costs, effects, lambda)
     pEVPI <- applyCalcSingleParamGam(params, inb, session)
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
   

    # This function gets the parameter names
    # The output is the checkbox list for the intervention for the CE plane
    observe({
      x <- input$costsFile
      y <- input$loadSession
      costs <- get("costs", envir=cache)
      if (is.null(costs)) return(NULL)
      namesOptions <- colnames(costs)
      updateRadioButtons(session, "decisionOptionCE1", 
                               choices = namesOptions, selected = colnames(costs)[2])
    })    

    # The output is the checkbox list for the comparator for the CE plane
    observe({
      x <- input$costsFile
      y <- input$loadSession
      costs <- get("costs", envir=cache)
      if (is.null(costs)) return(NULL)
      namesOptions <- colnames(costs)
      updateRadioButtons(session, "decisionOptionCE0", 
                               choices = namesOptions, selected = colnames(costs)[1])
    })    


    # Functions that make plots
    # CE plane
    output$plots1 <- renderPlot({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      assign("lambdaOverall", input$lambdaOverall, envir = cache)
      costs <- get("costs", envir=cache)
      effects <- get("effects", envir=cache)
      print(dim(costs))
      makeCEPlanePlot(costs, effects, 
                      lambda=input$lambdaOverall, input$decisionOptionCE1, input$decisionOptionCE0, cache)
    })  
 
    # CEAC
    output$plots2 <- renderPlot({
       if (!valuesImportedFLAG(cache, input)) return(NULL)
      ceac.obj <- assign("ceac.obj", ceac(), envir=cache)
      assign("lambdaOverall", input$lambdaOverall, envir = cache)
      makeCeacPlot(ceac.obj, lambda=input$lambdaOverall,
                   names=colnames(get("costs", envir=cache)))
    })  

    # EVPI versus lambda (costs)
    output$plots3 <- renderPlot({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      assign("lambdaOverall", input$lambdaOverall, envir = cache)
      makeEvpiPlot(get("costs", envir=cache), get("effects", envir=cache), lambda=input$lambdaOverall,
                   main=input$main3, 
                   xlab="Threshold willingness to pay", 
                   ylab="Overall EVPI per person affected (on costs scale)",
                   col="blue",  costscale = TRUE, session)
    })
   
    # EVPI versus lambda (effects)
    output$plots4 <- renderPlot({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      makeEvpiPlot(get("costs", envir=cache), get("effects", envir=cache), lambda=input$lambdaOverall,
                   main=input$main4, 
                   xlab="Threshold willingness to pay", 
                   ylab="Overall EVPI per person affected (on effects scale)",
                   col="blue",  costscale = FALSE, session)
    })
    
    # EVPI INB bar plot
    output$plots5a <- renderPlot({ # NEED TO DISCUSS THIS - MS
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      makeInbOptBar(get("costs", envir=cache), get("effects", envir=cache), 
                      lambda=input$lambdaOverall)
    })

    # Absolute net benefit densities
    output$plots5 <- renderPlot({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      make2wayDensity(get("costs", envir=cache), get("effects", envir=cache), 
                      lambda=input$lambdaOverall)
    })
    
    # EVPI plots
    output$plots6 <- renderPlot({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      make4wayEvpiPlot(get("costs", envir=cache), get("effects", envir=cache), lambda=input$lambdaOverall, 
                       prevalence=input$annualPrev, horizon=input$horizon, measure1 = input$currency, 
                       measure2 = input$unitBens, session)
    })
 
    # EVPPi horizontal bar chart
    output$plot7 <- renderPlot({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      makeEvppiBar(get("pEVPI", envir=cache), get("params", envir=cache))
    })
  

    # This function gets the parameter names
    # The output is the checkbox list
    observe({
      x <- input$parameterFile
      y <- input$loadSession
      params <- get("params", envir=cache)
      if (is.null(params)) return(NULL)
      namesParams <- colnames(params)
      namesParams <- paste(1:ncol(params), ") ", namesParams, sep="")
      updateCheckboxGroupInput(session, "pevpiParameters", 
                               choices = namesParams)
    })
      

    # These functions take the user input groups, call the partial EVPI (for groups) functions
    # and then output the results.

    # This function gets the selection and assigns it to cache

    observe({
      currentSelectionNames <- input$pevpiParameters
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      params <- get("params", envir = cache)
      if (is.null(params)) return(NULL)
      paramNames <- paste(1:ncol(params), ") ", colnames(params), sep="")
      currentSelection <- which(paramNames%in%currentSelectionNames)
      assign("currentSelection", currentSelection, envir = cache)
    })

    # This function responds to the add button being pressed
    # This function saves the current selection and then increase counter
    # It does the calculation and then outputs the selection table 
    
    output$selectedTable <- renderTable({
      dummy <- input$calculateSubsetsEvpi
      if (dummy == 0) return(NULL)
      if (!isolate(valuesImportedFLAG(cache, input))) return(NULL)
      if (dummy == 0) return(NULL)
      
      counterAdd <- get("counterAdd", envir=cache)
      print(counterAdd <- counterAdd + 1)
      assign("counterAdd", counterAdd, envir=cache) 
      
      setStore <- get("setStore", envir=cache)
      currentSelection <- get("currentSelection", envir=cache)
      setStore[[counterAdd]] <- currentSelection
      assign("setStore", setStore, envir = cache)
      
      calc <- function(x, inp, cache, session) { # pass session so the progress bar will work
        calSubsetEvpi(x, inp, cache, session)
      }
      
      #first pull down the existing values
      subsetEvpiValues <- get("subsetEvpiValues", envir = cache)
      if (is.null(subsetEvpiValues)) {
        subsetEvpiValues <- t(sapply(setStore[1:counterAdd], calc, input$lambdaOverall, cache, session))
      } else {
        newEvpiValue <- t(sapply(setStore[(NROW(subsetEvpiValues)+1):counterAdd], calc, input$lambdaOverall, cache, session))
        subsetEvpiValues <- rbind(subsetEvpiValues, newEvpiValue)
      }
      
      assign("subsetEvpiValues", subsetEvpiValues, envir = cache)
      assign("setStoreMatchEvpiValues", setStore, envir = cache) # cache these for the report in case they change

      buildSetStoreTable(setStore[1:counterAdd], subsetEvpiValues)
    }, sanitize.rownames.function = bold.allrows)

     # This clears everything, either on pressing the clear all button, or on loading new data.
     observe({ # clear the selections
       dummy <- input$clearSubsetsEvpi
       dummy1 <- valuesImportedFLAG(cache, input)
       # if (!valuesImportedFLAG(cache, input)) return(NULL)
       print("clearing")
       setStore <- vector("list", 100)
       assign("setStore", setStore, envir = cache)
       assign("counterAdd", 0, envir = cache)
       assign("subsetEvpiValues", NULL, envir = cache)
       assign("setStoreMatchEvpiValues", NULL, envir = cache) # cache these for the report in case they change
     })

    
    # Functions that make the reports
    
    # Download csv file
    output$downloadSummary <- downloadHandler(
      filename = "evppi\ values.csv",
      content = function(file) {
        write.csv(get("pEVPI", envir=cache), file)
      },
      contentType = "text/plain"
    )


    # thanks to yijui for this code
    # https://github.com/rstudio/shiny-examples/blob/master/016-knitr-pdf/server.R
    # Download pdf / html / docx report - NEED TO FIX THE HTML AND DOCX 
    output$downloadReport <- downloadHandler(
      filename = function() {#"my-report.pdf"
        paste('my-report', sep = '.', switch(
          input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
        ))
      },
      
      content = function(file) {
        src <- normalizePath('report.Rmd')
        
        # temporarily switch to the temp dir, in case you do not have write
        # permission to the current working directory
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        file.copy(src, 'report.Rmd')
        
        library(rmarkdown)
        out <- render('report.Rmd', #pdf_document()
                      switch(
                        input$format,
                        PDF = pdf_document(), HTML = html_document(), Word = word_document()),
                      envir = cache
        )
        file.copy(out, file)
      },
      contentType = "text/plain"
    )
    

    # Download .Rdata file
    output$saveSession <- downloadHandler(
      filename =  function() paste(input$RdataFileName),
      content = function(file) {
        save(list = ls(envir=cache), file = file, envir=cache)
      },
      contentType = "text/plain")
    
    
    })
