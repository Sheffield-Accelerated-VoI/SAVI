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
    

    #  load("SAVISession.Rdata", envir=cache) # auto load for testing purposes

    #  Function that loads saved session
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
        assign("nIterate", nrow(dat), envir=cache) # size of PSA
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
        assign("nInt", ncol(dat), envir=cache) # number of interventions

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
    


# function that saves "about the model" variables to the cache if they are changed in the input.
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



#    Functions that render the data files and pass them to ui.R
    
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
      paste("This graph shows the standardised cost-effectiveness plane per person based on ", nrow(get("params", envir=cache)), 
            " model runs, in which uncertain model parameters are varied simultaneously in a probabilistic sensitivity analysis.  
            The mean incremental cost of ", colnames(get("costs", envir=cache))[2], " versus ", colnames(get("costs", envir=cache))[1]," is ",
            input$currency, incValue(incCost(get("costs", envir=cache))), ". This suggests that ",colnames(get("costs", envir=cache))[2]," is ", 
            moreLess(incCost(get("costs", envir=cache))), " costly. There is some uncertainty due to model parameters, with the 97.5% credible 
            interval for the incremental cost ranging from ", confIntCE(incCost(get("costs", envir=cache)), 0.025)," to ", 
            confIntCE(incCost(get("costs", envir=cache)), 0.975),". The probability that ", colnames(get("costs", envir=cache))[2], " is cost 
            saving compared to ",colnames(get("costs", envir=cache))[1]," is ", pCostsaving(incCost(get("costs", envir=cache))), ".", sep="")
    })                       
    
    output$textCEplane2 <- renderText({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      paste("The mean incremental benefit of ", colnames(get("costs", envir=cache))[2], " versus ", colnames(get("costs", envir=cache))[1], " is ", 
            incValue(incBen(get("effects", envir=cache))), " ",input$unitBens, "s.  This suggests that ",colnames(get("costs", envir=cache))[2]," is ", 
            moreLess(incBen(get("effects", envir=cache))), " beneficial. Again, there is some uncertainty due to model parameters, with the 97.5% 
            credible interval for the incremental benefit ranging from ", confIntCE(incBen(get("effects", envir=cache)), 0.025), " to ", 
            confIntCE(incBen(get("effects", envir=cache)), 0.975),". The probability that ",colnames(get("costs", envir=cache))[2],
            " is more beneficial than ",colnames(get("costs", envir=cache))[1]," is ", pMoreben(incBen(get("effects", envir=cache))), ".", sep="")
    })                        
    
    output$textCEplane3 <- renderText({
      if (!valuesImportedFLAG(cache, input)) return(NULL)      
      paste("The expected incremental cost per ", input$unitBens," (ICER) is estimated at ", input$currency, iCER(get("costs", envir=cache), 
            get("effects", envir=cache)), ". This is ", aboveBelow(get("costs", envir=cache), get("effects", envir=cache), input$lambdaOverall),  
            " the threshold of ", input$currency, input$lambdaOverall, " per ", input$unitBens, " indicating that ", colnames(get("costs", envir=cache))[2],
            " ", wouldNot(get("costs", envir=cache), get("effects", envir=cache), input$lambdaOverall), " be considered cost-effective 
            at this threshold. There is uncertainty with a ", pCE(get("costs", envir=cache), get("effects", envir=cache), input$lambdaOverall, get("nInt", envir=cache)), 
            " probability that ", colnames(get("costs", envir=cache))[2], " is more cost-effective (", pCE(get("costs", envir=cache), get("effects", envir=cache), 
            input$lambdaOverall, get("nInt", envir=cache)), " of the probabilistic model run dots are below and to the right of the diagonal threshold line).", sep="")
    })                         
    
    output$textCEplane4 <- renderText({
      if (!valuesImportedFLAG(cache, input)) return(NULL) 
      paste(colnames(get("costs", envir=cache))[2],"vs.",colnames(get("costs", envir=cache))[1])
    })
    
    output$textCEplane5 <- renderText({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      paste(pCE(get("costs", envir=cache), get("effects", envir=cache), input$lambdaOverall, get("nInt", envir=cache)), " probability that ",colnames(get("costs", envir=cache))[2]," is more cost-effective 
      than ",colnames(get("costs", envir=cache))[1]," at a threshold of ",input$currency, input$lambdaOverall," per ",input$unitBens, sep="")
    })                       
    
    output$textCEAC1 <- renderText({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      paste("This graph shows the cost-effectiveness acceptability curve for the comparison of strategies. The results show that at a threshold 
            value for cost-effectiveness of ",input$currency, input$lambdaOverall," per ",input$unitBens," the strategy with the highest 
            probability of being most cost-effective is ", bestCE(get("costs", envir=cache), get("effects", envir=cache), input$lambdaOverall, get("nInt", envir=cache)), 
            ", with a probability of ", pCE(get("costs", envir=cache), get("effects", envir=cache), input$lambdaOverall, get("nInt", envir=cache)),
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
           interval for each one.  The strategy with highest expected net benefit is estimated to be ", bestnetBen(get("costs", envir=cache), 
           get("effects", envir=cache), input$lambdaOverall, get("nInt", envir=cache)), ", with an expected net benefit of 
           ", input$currency, netBencosts(get("costs", envir=cache), get("effects", envir=cache), input$lambdaOverall, get("nInt", envir=cache)),
           " (equivalent to a net benefit on the effectiveness scale of ", netBeneffects(get("costs", envir=cache), get("effects", envir=cache), 
           input$lambdaOverall, get("nInt", envir=cache)), " ", input$unitBens, "s). Net benefit and 97.5% credible intervals for all strategies 
           are presented in the above table. ", sep="")
   }) 

   output$textEVPI1 <- renderText({
     if (!valuesImportedFLAG(cache, input)) return(NULL)
     paste("The overall EVPI per person affected by the decision is estimated at ", input$currency, format(calcEvpi(get("costs", envir=cache), 
          get("effects", envir=cache), input$lambdaOverall), digits = 4, nsmall=2), ".  This is equivalent to ", 
          format(calcEvpi(get("costs", envir=cache), get("effects", envir=cache), input$lambdaOverall)/input$lambdaOverall, digits = 4, nsmall=1), 
          " per person’s worth of decision uncertainty on the ", input$unitBens, " scale.", sep="")
   })     

    output$textEVPI2 <- renderText({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      paste("Assuming an annual number of people affected by the decision of " , input$annualPrev, ", the overall EVPI per year is ", input$currency,
            format(calcEvpi(get("costs", envir=cache), get("effects", envir=cache), input$lambdaOverall)*input$annualPrev, digits = 4, nsmall=2), " for ", input$jurisdiction, ".", sep="")
    }) 

    output$textEVPI3 <- renderText({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      paste("When thinking about the overall expected value of removing decision uncertainty, one needs to consider how long the current comparison 
            will remain relevant e.g. if new treatments of options or even cures are anticipated to become available for a disease.  For the specified 
            decision relevance horizon of ", input$horizon, " years, the overall expected value of removing decision uncertainty for ", 
            input$jurisdiction, " would in total be ", input$currency, format(calcEvpi(get("costs", envir=cache), get("effects", envir=cache), 
            input$lambdaOverall)*input$annualPrev*input$horizon, digits = 4, nsmall=2),".", sep="")
    }) 

    output$textEVPI4 <- renderText({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      paste("Research or data collection exercises costing more than this amount would not be considered cost-effective use of resources. This is because 
            the return on investment from the research – as measured by the health gain and cost savings of enabling decision makers ability to switch and 
            select other strategies when evidence obtained reduces decision uncertainty – is expected to be no higher than the figure of ", input$currency, 
            format(calcEvpi(get("costs", envir=cache), get("effects", envir=cache), input$lambdaOverall)*input$annualPrev*input$horizon, digits = 4, nsmall=2),".", sep="")  
      }) 

    output$textEVPI5 <- renderText({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      paste("The EVPI estimates in the table below quantify the expected value to decision makers within ", input$jurisdiction, " of removing all current 
            decision uncertainty at a threshold of ", input$currency, input$lambdaOverall, " per ", input$unitBens, ".  This will enable comparison against 
            previous analyses to provide an idea of the scale of decision uncertainty in this topic compared with other previous decisions. The EVPI estimate 
            for varying willingness to pay thresholds are illustrated in the figures below.", sep="")
    })


    # Functions that make tables  
    output$tableCEplane <- renderTable({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      tableCEplane <- makeTableCePlane(get("costs", envir=cache), get("effects", 
                            envir=cache), lambda=input$lambdaOverall)
      assign("lambdaOverall", input$lambdaOverall, envir = cache)
      rownames(tableCEplane) <- c(paste("Threshold (", input$currency, ")"), 
                            "Comparator", 
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
     colnames(tableEVPI) <- c(paste("Overall EVPI Financial Valuation (", input$currency, ")"), paste("Overall EVPI", input$unitBens, "Valuation"))
     rownames(tableEVPI) <- c("Per Person Affected by the Decision", 
                              paste("Per Year in", input$jurisdiction, "Assuming", input$annualPrev, "Persons Affected per Year"), 
                              "Over 5 Years", 
                              "Over 10 Years", 
                              "Over 15 Years", 
                              "Over 20 years", 
                              paste("Over Specified Decision Relevance Horizon (", input$horizon, "years)"))
     
     #calcEvpiFunc <- ifelse(input$indSim, get("calcEvpiSingle"), get("calcEvpi"))
     #print(input$indSim)
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
   
    
    # Functions that make plots
    # CE plane
    output$plots1 <- renderPlot({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      assign("lambdaOverall", input$lambdaOverall, envir = cache)
      makeCEPlanePlot(get("costs", envir=cache), get("effects", envir=cache), 
                      lambda=input$lambdaOverall, xlab=input$effectDef, 
                      ylab=input$costDef)
    })  
 
    # CEAC
    output$plots2 <- renderPlot({
       if (!valuesImportedFLAG(cache, input)) return(NULL)
      ceac.obj <- assign("ceac.obj", ceac(), envir=cache)
      assign("lambdaOverall", input$lambdaOverall, envir = cache)
      makeCeacPlot(ceac.obj, lambda=input$lambdaOverall,
                   names=colnames(get("costs", envir=cache)))
    })  ###NEED TO ADD % COST-EFFECTIVENESS AT LINE AS A LABEL  

    # EVPI versus lambda (costs)
    output$plots3 <- renderPlot({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      assign("lambdaOverall", input$lambdaOverall, envir = cache)
      makeEvpiPlot(get("costs", envir=cache), get("effects", envir=cache), lambda=input$lambdaOverall,
                   main=input$main3, 
                   xlab="Threshold willingness to pay", 
                   ylab="Overall EVPI per person affected (on costs scale)",
                   col="red",  costscale = TRUE, session)
    })
   
    # EVPI versus lambda (effects)
    output$plots4 <- renderPlot({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      makeEvpiPlot(get("costs", envir=cache), get("effects", envir=cache), lambda=input$lambdaOverall,
                   main=input$main4, 
                   xlab="Threshold willingness to pay", 
                   ylab="Overall EVPI per person affected (on effects scale)",
                   col="red",  costscale = FALSE, session)
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
    #
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

    #  Need a function that clears everything, either on pressing the clear all button, or on loading new data.
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
      filename = "evpi\ values.csv",
      content = function(file) {
        write.csv(get("pEVPI", envir=cache), file)
      },
      contentType = "text/plain"
    )

### SWITCH THIS OFF UNTIL FIXED IN SAVI SERVER    



    # Download pdf / html / docx report - NEED TO FIX THE HTML AND DOCX 
#     output$downloadReport <- downloadHandler(
#       filename = function() {#"my-report.pdf"
#         paste('my-report', sep = '.', switch(
#           input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
#         ))
#       },
#       
#       content = function(file) {
#         #src <- normalizePath('report.Rmd')
#         
#         # temporarily switch to the temp dir, in case you do not have write
#         # permission to the current working directory
#         #owd <- setwd(tempdir())
#         #on.exit(setwd(owd))
#         #file.copy(src, 'report.Rmd')
#         
#         library(rmarkdown)
#         out <- render('report.Rmd', #pdf_document()
#                       switch(
#                         input$format,
#                         PDF = pdf_document(), HTML = html_document(), Word = word_document_local()),
#                       envir = cache
#         )
#         file.copy(out, file)
#       },
#       contentType = "text/plain"
#     )
#     
    
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
