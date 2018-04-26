# Copyright (c) 2014, 2015, 2018 the SAVI authors (see AUTHORS.txt).
# Licensed under the BSD 3-clause license (see LICENSE.txt)

######################
# START SHINY SERVER #
######################

print("server.R called") # this is called when we start the shiny server on SAVI via $ sudo start shiny-server

rm(list=ls())

##################
# SET OPTIONS    #
# LOAD LIBRARIES #
# SOURCE SCRIPTS #
##################



# max upload for files
options(shiny.maxRequestSize=1024*1024^2) # Max upload 1Gb

# debugging option. Only set to true for debugging. MUST BE FALSE FOR LIVE USE
options(shiny.reactlog=FALSE)

# select old style progress bar (needed since Shiny upversion to 0.14)
shinyOptions(progress.style = "old") 

# load the libraries we need
library(MASS)
library(mgcv)
library(knitr)
library(rmarkdown)
library(xtable)
# library(earth)
library(parallel)

# source all the functions we need
source("scripts.R")
source("scripts_GPfunctions.R")
source("scripts_GAMfunctions.R")
source("scripts_plots.R")
source("scripts_tables.R")
source("scripts_text.R")
source("scripts_GAMBasedIndivAvefunctions.R")
# source("scripts_IndivAvefunctions.R")


###########################
# TEST DATA               #
# users can download this # 
# to try out the app      #
###########################

testParams <- as.matrix(read.csv("test_data/brennan10000/parameters.csv"))
testCosts <- as.matrix(read.csv("test_data/brennan10000/costs_2d.csv"))
testEffects <- as.matrix(read.csv("test_data/brennan10000/effects_2d.csv"))


###################
# SERVER FUNCTION #
###################


shinyServer(
  
  function(input, output, session) {

    ##################################################################################################

    #####################################
    # CREATE NEW ENVIRONMENT 'cache'    #
    # Initialise cached variable values #
    #####################################

    # `cache' is the environment unique to each user visit
    # This is where we will save values that need to persist, 
    # and that can be picked up and included in the report
    
    if(exists("cache")) rm(cache, inherits = TRUE) # we shouldn't need this
    cache <- new.env()
    
    cache$savedSession <- 0
    cache$nIterate <- 0
    cache$nInt <- 0
    cache$pEVPI <- NULL
    cache$params <- NULL
    cache$uploadedCosts <- NULL   # these are the costs that are uploaded
    cache$uploadedEffects <- NULL # these are the effects that are uploaded
    cache$modelledCosts <- NULL   # these are the costs that are modelled in the ind sim case
    cache$modelledEffects <- NULL # these are the effects that are uploaded in the ind sim case
    cache$costs <- NULL
    cache$effects <- NULL
    
    cache$tableCEplane <- NULL
    cache$tableNetBenefit <- NULL
    cache$groupTable <- NULL
    cache$tableEVPI <- NULL
    cache$tableEVPPI <- NULL
    cache$ceac.obj <- NULL
    
    cache$textCEplane1 <- NULL
    cache$calcEvpiVal <- NULL
    cache$incValueCosts <- NULL
    cache$incValueEffects <- NULL
    cache$namesDecisions <- NULL
    cache$moreLessCosts <- NULL
    cache$confIntCE025costs <- NULL
    cache$confIntCE975costs <- NULL
    cache$confIntCE025effects <- NULL
    cache$confIntCE975effects <- NULL
    cache$pCostsavingVal <- NULL
    cache$pMorebenVal <- NULL
    cache$iCERVal <- NULL
    cache$pCEVal <- NULL
    
    cache$counterAdd <- 0
    cache$setStore <- vector("list", 100) # up to 100 sets for the group inputs
    cache$subsetEvpiValues <- NULL
    cache$setStoreMatchEvpiValues <- NULL
    cache$currentSelection <- NULL
    cache$ceac.obj <- NULL
    
    # assign null values to the about the model variables in the cache
    cache$modelName <- NULL
    cache$current <- NULL  
    cache$t3 <- NULL       
    cache$lambdaOverall <- 0
    cache$effectDef <- NULL
    cache$costDef <- NULL
    cache$annualPrev <- 0
    cache$horizon <- 0
    cache$currency <- NULL
    cache$unitBens <- NULL
    cache$jurisdiction <- NULL
    
    cache$indSim <- FALSE

    ########################
    # AUTOLOAD FOR TESTING #
    ########################
    
    # these three rows autoload values for testing purposes - to avoid having to load them manually. MS
    # ###########
    #     load.parameters <- function() read.csv("../test/parameters.csv")                                   
    #     load.costs <- function() read.csv("../test/costs.csv")
    #     load.effects <- function() read.csv("../test/effects.csv")
    # ########### 
    # Or load an Rdata file
    # load("adenoma.Rdata", envir=cache) # auto load for testing purposes

    
    ######################################################################################
        
    
    
    
    
    
    
    
    
    
    
    ################################## TABS BELOW #########################################
    
    
    ############
    # HOME TAB #
    ############
    
    # Function that LOADS SAVED SESSION
    # is evaluated if a new session is loaded 
    
    # CURRENTLY OUT OF ACTION
    
#     observe({
#       inFile = input$loadSession
#       if (is.null(inFile)) return(NULL)
#       load(inFile$datapath, envir=cache)
#       
#       # update "about the model" variables  
#       updateTextInput(session, "modelName", value = cache$modelName)
#       updateTextInput(session, "current",  value = cache$current)
#       updateTextInput(session, "t3",  value = cache$t3)
#       updateNumericInput(session, "lambdaOverall",  value = cache$lambdaOverall)
#       updateTextInput(session, "effectDef",  value = cache$effectDef)
#       updateTextInput(session, "costDef",  value = cache$costDef)
#       updateNumericInput(session, "annualPrev",  value = cache$annualPrev)
#       updateNumericInput(session, "horizon",  value = cache$horizon)
#       updateTextInput(session, "currency",  value = cache$currency)
#       updateTextInput(session, "unitBens",  value = cache$unitBens)
#       updateTextInput(session, "jurisdiction",  value = cache$jurisdiction)
#       updateTextInput(session, "indSim",  value = cache$indSim)
#       
#       # set the group EVPI objects to NULL / 0
#       cache$counterAdd <- 0
#       cache$setStore <- vector("list", 100) # up to 100 sets for the group inputs
#       cache$subsetEvpiValues <- NULL
#       cache$setStoreMatchEvpiValues <- NULL
#       cache$currentSelection <- NULL
#       
#       cache$namesDecisions <- paste(1:ncol(cache$costs), ") ", 
#                                     colnames(cache$costs), sep="") # defines the decision option names   
#       
#       if(is.null(cache$uploadedCosts)) {
#         cache$uploadedCosts <- cache$costs
#         cache$uploadedEffects <- cache$effects
#       }
#       
# 
#     })
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    ########################
    # ABOUT YOUR MODEL TAB #
    ########################  
    
    # Function that saves "about the model" variables to the cache if they are changed in the input.
    
    observe({
      cache$modelName <- input$modelName
      cache$current <- input$current
      cache$t3 <- input$t3
      cache$lambdaOverall <- input$lambdaOverall
      cache$effectDef <- input$effectDef
      cache$costDef <- input$costDef
      cache$annualPrev <- input$annualPrev
      cache$horizon <- input$horizon
      cache$currency <- input$currency
      cache$unitBens <- input$unitBens
      cache$jurisdiction <- input$jurisdiction
      # cache$indSim <- input$indSim
    })
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    ####################
    # IMPORT FILES TAB #
    ####################
    
    ### Parameters
    
    #  Function that imports parameters
    observe({
      inFile <- input$parameterFile
      if (is.null(inFile)) return(NULL)
      dat <- read.csv(inFile$datapath, sep=input$sep, dec=input$dec, encoding = 'UTF-8')
      cache$params <- dat
      cache$nParams <- NCOL(dat)
      cache$nIterate <- NROW(dat) # size of PSA
    })
    
    # Function that checks sanity of parameter file
    output$textCheckTabParams <- renderText({
      x1 <- input$parameterFile     
      params <- cache$params
      if (is.null(params)) return(NULL)    
      
      if (sum(is.na(params)) > 0) {
        return("There are missing values - please check data and reload")
      }
      
      if (!prod(unlist(c(lapply(params, function(x) {class(x) == "numeric" | class(x) == "integer"}))))) {
        return("Not all columns are numeric - please check data and reload")
      }
      
      if (sum(unlist(lapply(params, function(x) length(unique(x)) > 1 & length(unique(x)) < 5))) > 0) {
        return("One or more columns contains too few (<5) unique values for EVPPI analysis")
      }
      return(NULL)
    })
    
      
    ### Costs
       
    #  Function that imports costs    
    observe({
      inFile <- input$costsFile
      if (is.null(inFile)) return(NULL)
      dat <- read.csv(inFile$datapath, sep=input$sep2, dec=input$dec2, encoding = 'UTF-8')
      cache$uploadedCosts <- cache$costs <- dat
      cache$namesDecisions <- paste(1:ncol(dat), ") ", colnames(dat), sep="") # defines the decision option names      
      cache$nInt <- NCOL(dat) # number of interventions

    })

    # Function that checks sanity of costs file
    output$textCheckTabCosts <- renderText({
      x2 <- input$costsFile 
    
      costs <- cache$uploadedCosts
      if (is.null(costs)) return(NULL)      
      
      if (sum(is.na(costs)) > 0) return("There are missing values - please check data and reload")
      
      if (NCOL(costs) == 1) return("There must be at least two decision options. 
                                      If you have a single set of incremental 
                                   costs for a two-decision option problem, 
                                    either upload the absolute costs, or include a column of zeroes.")

      if (!prod(unlist(c(lapply(costs, function(x) {class(x) == "numeric" | class(x) == "integer"}))))) {
        return("Not all columns are numeric - please check data and reload") 
      }
        
      return(NULL)
    })

        
    ### Effects
    
    # Function that imports effects
    observe({
      inFile <- input$effectsFile      
      if (is.null(inFile)) return(NULL)
      
      dat <- read.csv(inFile$datapath, sep=input$sep3, dec=input$dec3, encoding = 'UTF-8')
      cache$uploadedEffects <- cache$effects <- dat
    })
  
    # Function that checks sanity of effects file
    output$textCheckTabEffects <- renderText({
      x3 <- input$effectsFile 
      effects <- cache$uploadedEffects
      if (is.null(effects)) return(NULL)
      
      if (sum(is.na(effects)) > 0) return("There are missing values - please check data and reload")
      
      if (NCOL(effects) == 1) return("There must be at least two decision options. 
                                    If you have a single set of 
                                      incremental effects for a two-decision option problem, 
                                     either upload the absolute effects, or include a column of zeroes.")
      
      if (!prod(unlist(c(lapply(effects, function(x) {class(x) == "numeric" | class(x) == "integer"}))))) {
        return("Not all columns are numeric - please check data and reload")
      } 
      
      return(NULL)
    })
    
    # Function that checks that files have the right number of rows and columns
    output$textCheckTab <- renderText({
      x1 <- input$parameterFile 
      x2 <- input$costsFile 
      x3 <- input$effectsFile 
      
      if (!valuesImportedFLAG(cache, input)) return(NULL)

      params <- cache$params
      costs <- cache$uploadedCosts
      effects <- cache$uploadedEffects
      if(!((NROW(params) == NROW(costs)) & (NROW(effects) == NROW(costs)))) {
        return("Loaded files have different numbers of rows - please check data and reload")
      } 
      
      if(NCOL(effects) != NCOL(costs)) {
        return("Costs and effect have different numbers of columns - please check data and reload")
      } 
      
      return(NULL)
      
    })
    
    
    ### DOWNLOAD TEST FILES
    
    # Download csv file
    output$testParams <- downloadHandler(
      filename = "parameters.csv",
      content = function(file) {
        write.csv(testParams, file, row.names = FALSE)
      },
      contentType = "text/plain"
    )
    
    output$testCosts <- downloadHandler(
      filename = "costs.csv",
      content = function(file) {
        write.csv(testCosts, file, row.names = FALSE)
      },
      contentType = "text/plain"
    )
    
    output$testEffects <- downloadHandler(
      filename = "effects.csv",
      content = function(file) {
        write.csv(testEffects, file, row.names = FALSE)
      },
      contentType = "text/plain"
    )
    
    
    
  
    
    
    
    
    
    
    
    
    
    
    
    ####################
    # CHECK UPLOAD TAB #
    ####################

    # Functions that render the data files and pass them to ui.R
    
    output$checktable1 <- renderTable({
      x <- input$parameterFile 
      y <- input$loadSession
      tableValues <- cache$params
      if (is.null(tableValues)) return(NULL)
      head(tableValues, n=5)
    }, rownames = TRUE)
   
    output$checktable2 <- renderTable({
      x <- input$costsFile 
      y <- input$loadSession
      tableValues <- cache$costs
      if (is.null(tableValues)) return(NULL)
      head(tableValues, n=5)  
    }, rownames = TRUE)
    
    output$checktable3 <- renderTable({
      x <- input$effectsFile 
      y <- input$loadSession
      tableValues <- cache$effects
      if (is.null(tableValues)) return(NULL)
      head(tableValues, n=5)
    }, rownames = TRUE)
    

    # Functions that make reactive text to tell user the number of rows and columns
    output$textParamsTable <- renderText({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      #dummy <- input$indSim # ensure update with ind sim box tick
      paste("<em>There are ", NROW(cache$params), " rows and ", NCOL(cache$params), 
      " columns in the uploaded parameter file</em>")
    })  
    
    output$textCostsTable <- renderText({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      #dummy <- input$indSim # ensure update with ind sim box tick
      paste("<em>There are ", NROW(cache$costs), " rows and ", NCOL(cache$costs), 
            " columns in the uploaded costs file</em>")
    })  
    
    output$textEffectsTable <- renderText({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      #dummy <- input$indSim # ensure update with ind sim box tick
      paste("<em>There are ", NROW(cache$effects), " rows and ", NCOL(cache$effects), 
            " columns in the uploaded effects file</em>")
    })  
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    ###################
    # PSA RESULTS TAB #
    ###################
  
    
    ### CE PLANE
      
    # This function gets the parameter names
    # The output is the checkbox list for the intervention for the CE plane
    observe({
      x <- input$costsFile
      y <- input$loadSession
      namesOptions <- cache$namesDecisions
      updateRadioButtons(session, "decisionOptionCE1", 
                         choices = namesOptions, selected = namesOptions[2])
    })    
    
    # The output is the checkbox list for the comparator for the CE plane
    observe({
      x <- input$costsFile
      y <- input$loadSession
      namesOptions <- cache$namesDecisions
      updateRadioButtons(session, "decisionOptionCE0", 
                         choices = namesOptions, selected = namesOptions[1])
    }) 
    
    
    
    # if the ind sim flag is set and the cache$modelledCosts is still null 
    # then get the modelled costs and effects
    observe({
      # cache$indSim <- input$indSim
      if (!valuesImportedFLAG(cache, input)) return(NULL)
#       if (input$indSim == "Yes") {
#         if (is.null(cache$modelledCosts)) {
#           getModelledCostsAndEffects(cache, session)
#         }
#         cache$costs <- cache$modelledCosts
#         cache$effects <- cache$modelledEffects
#       } else {
        cache$costs <- cache$uploadedCosts
        cache$effects <- cache$uploadedEffects
      #}
    })
    
    
    # CE plane
    output$plots1 <- renderPlot({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      #cache$indSim <- input$indSim # ensure update with ind sim box tick / untick    
      cache$lambdaOverall <- input$lambdaOverall
      costs <- cache$costs
      effects <- cache$effects
      makeCEPlanePlot(costs, effects, 
                      lambda=input$lambdaOverall, input$decisionOptionCE1, 
                      input$decisionOptionCE0, cache)
    })
    

    # Functions that make reactive text to accompany plots
    output$textCEplane1 <- renderText({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      #dummy <- input$indSim # ensure update with ind sim box tick
      
      cache$pCostsavingVal <- pCostsaving(cache$costs, input$decisionOptionCE1, 
                input$decisionOptionCE0, cache)
      cache$incValueCosts <- incValue(cache$costs, input$decisionOptionCE1, 
               input$decisionOptionCE0, cache)
      
      cache$confIntCE025costs <- confIntCE(cache$costs, input$decisionOptionCE1, 
                                             input$decisionOptionCE0, 0.025, cache)
      
      cache$confIntCE975costs <- confIntCE(cache$costs, input$decisionOptionCE1, 
                                             input$decisionOptionCE0, 0.975, cache)
      
      cache$moreLessCosts <- moreLess(cache$costs, input$decisionOptionCE1, 
               input$decisionOptionCE0, cache)
      
      cache$textCEplane1 <- paste("The figure above shows the (standardised) 
                                  cost-effectiveness plane based on the ", 
            cache$nIterate, " model runs in the probabilistic sensitivity analysis. 
              The willingness-to-pay threshold is shown as a 45 degree line. 
            The mean incremental cost of ", input$decisionOptionCE1, " versus ",  
              input$decisionOptionCE0," is ",
            input$currency, cache$incValueCosts, ". This suggests that ", 
            input$decisionOptionCE1, " is ", 
            cache$moreLessCosts, " costly. 
              The incremental cost is uncertain because the model parameters are uncertain. 
            The 95% credible interval for the incremental cost ranges from ", 
              input$currency, cache$confIntCE025costs," to ", 
             input$currency, cache$confIntCE975costs,". The probability that ", 
             input$decisionOptionCE1, " is cost 
            saving compared to ", input$decisionOptionCE0," is ", 
            cache$pCostsavingVal, ".", sep="")
      
      cache$textCEplane1
    })                       
    
    output$textCEplane2 <- renderText({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      #dummy <- input$indSim # ensure update with ind sim box tick
      
      cache$incValueEffects <- incValue(cache$effects, input$decisionOptionCE1, 
                input$decisionOptionCE0, cache)
      
      cache$confIntCE025effects <- confIntCE(cache$effects, input$decisionOptionCE1, 
                input$decisionOptionCE0, 0.025, cache)
      
      cache$confIntCE975effects <- confIntCE(cache$effects, input$decisionOptionCE1, 
                input$decisionOptionCE0, 0.975, cache)
      
      cache$pMorebenVal <- pMoreben(cache$effects, input$decisionOptionCE1, 
                input$decisionOptionCE0, cache)
      
      cache$moreLessEffects <- moreLess(cache$effects, input$decisionOptionCE1, 
                input$decisionOptionCE0, cache)
      
      paste("The mean incremental benefit of ", input$decisionOptionCE1, " versus ", 
            input$decisionOptionCE0, " is ", 
            cache$incValueEffects, 
            " ",input$unitBens, "s.  This suggests that ", input$decisionOptionCE1," is ", 
            cache$moreLessEffects, 
              " beneficial. Again, there is uncertainty in the incremental benefit 
            due to uncertainty in the model parameters. The 95% 
            credible interval for the incremental benefit ranges from ", 
            cache$confIntCE025effects, " ", input$unitBens, "s to ", 
            cache$confIntCE975effects, " ", 
            input$unitBens,"s. The probability that ", input$decisionOptionCE1, 
            " is more beneficial than ", input$decisionOptionCE0, " is ", 
            cache$pMorebenVal, ".", sep="")
    })                        
    
    output$textCEplane3 <- renderText({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      #dummy <- input$indSim # ensure update with ind sim box tick
      
      cache$pCEVal <- pCE(input$decisionOptionCE1, input$decisionOptionCE0, 
          input$lambdaOverall, cache) 
      
      cache$iCERVal <- iCER(cache$costs, 
          cache$effects, input$decisionOptionCE1, input$decisionOptionCE0, cache)
      
      paste("The expected incremental cost per ", input$unitBens," (ICER) is estimated at ", 
            input$currency, cache$iCERVal, 
            ". There is a probability of ", cache$pCEVal, 
            " that ", input$decisionOptionCE1, " is more cost-effective than ", 
            input$decisionOptionCE0, " at a threshold of ",
            input$currency, input$lambdaOverall," per ",input$unitBens, sep="")
    })                         
      
    output$textCEplane4 <- renderText({
      if (!valuesImportedFLAG(cache, input)) return(NULL) 
      paste(input$decisionOptionCE1, "versus", input$decisionOptionCE0)
    })
    
                
    
    # Table of Key Cost-Effectiveness Statistics
    output$tableCEplane <- renderTable({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      #dummy <- input$indSim # ensure update with ind sim box tick
      
      tableCEplane <- makeTableCePlane(lambda=input$lambdaOverall, input$decisionOptionCE0, cache)
      print(class(tableCEplane))
      cache$lambdaOverall <- input$lambdaOverall
      rownames(tableCEplane) <- c(paste("Threshold (", input$currency, " per ", input$unitBens, ")", sep=""), 
                                  "Comparator", 
                                  "Number of PSA runs", 
                                  paste("Mean inc. Effect per Person (", input$unitBens, ")", sep=""), 
                                  paste("Mean inc. Cost per Person (", input$currency, ")", sep=""),
                                  paste("ICER Estimate (", input$currency, " per ", input$unitBens, ")", sep=""),
                                  paste("2.5th centile for inc. Effects (", input$unitBens, ")", sep=""), 
                                  paste("97.5th centile for inc. Effects (", input$unitBens, ")", sep=""),
                                  paste("2.5th centile for inc. Costs (", input$currency, ")", sep=""),
                                  paste("97.5th centile for inc. Costs (", input$currency, ")", sep=""),
                                  "Probability intervention is cost saving", 
                                  "Probability intervention provides more benefit", 
                                  "Probability that intervention is cost-effective against comparator")
      cache$tableCEplane <- tableCEplane
      tableCEplane
    }, rownames = TRUE)  
    
    # Download table as a csv file
    output$downloadTableCEplane <- downloadHandler(
      
      filename = "Cost-Effectiveness\ Statistics.csv",
      content = function(file) {
        tableOut <- cache$tableCEplane
        if(!is.null(cache$tableCEplane)) {
          tableOut <- cbind(rownames(tableOut), tableOut)
          colnames(tableOut) <- c("Intervention", colnames(tableOut)[-1])
        }
        write.csv(tableOut, file, row.names = FALSE)
      },
      contentType = "text/plain"
    )
    
    

    ### CEAC

    # function that calculates ceac
    ceac <- reactive({ 
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      #dummy <- input$indSim # ensure update with ind sim box tick
      makeCeac(cache$costs, cache$effects, input$lambdaOverall, session)
    })

    output$textCEAC1 <- renderText({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
     # dummy <- input$indSim # ensure update with ind sim box tick
      
      cache$bestCEVal <- bestCE(cache$costs, cache$effects, 
             input$lambdaOverall, cache$nInt)
      
      cache$highestpCE <- highestCE(cache$costs, cache$effects, 
              input$lambdaOverall)
      
      paste("This graph shows the cost-effectiveness acceptability curve for the 
            comparison of strategies. The results show that at a threshold 
            value for cost-effectiveness of ",input$currency, input$lambdaOverall,
            " per ",input$unitBens," the strategy with the highest 
            probability of being most cost-effective is ", cache$bestCEVal, 
            ", with a probability of ", cache$highestpCE,
            ". More details on how to interpret CEACs are available from the literature.", sep="")
    })                       
                 
    # CEAC plot
    output$plots2 <- renderPlot({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      #dummy <- input$indSim # ensure update with ind sim box tick
      ceac.obj <- cache$ceac.obj <- ceac()
      cache$lambdaOverall <- input$lambdaOverall
      makeCeacPlot(ceac.obj, lambda=input$lambdaOverall,
                   names=colnames(cache$costs))
    })  
    
    
    
   ### NET BENEFIT
    
    output$textNB1 <- renderText({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
     # dummy <- input$indSim # ensure update with ind sim box tick
      
      paste("Net benefit is a calculation that puts ", input$costDef, " and ", 
        input$effectDef, " onto the same scale.  This is done by calculating 
           the monetary value of ", input$effectDef, " using a simple multiplication i.e. ", 
        input$unitBens, "s * lambda, where:", sep="")
    })  

    output$textNB2 <- renderText({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      paste("Net benefit for a strategy = ", input$unitBens, "s * ", input$lambdaOverall, 
            " - Cost (" ,input$currency, ").", sep="")
    }) 
    
    output$textNB3 <- renderText({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
     # dummy <- input$indSim # ensure update with ind sim box tick
      
      cache$bestnetBenVal <- bestnetBen(cache$costs, 
                 cache$effects, input$lambdaOverall, cache$nInt)
      
      cache$netBencostsVal <- netBencosts(cache$costs, cache$effects, 
                  input$lambdaOverall, cache$nInt)
      
      cache$netBeneffectsVal <- netBeneffects(cache$costs, cache$effects, 
                    input$lambdaOverall, cache$nInt)
      
      paste("The plot below shows the expected net benefit of the ", cache$nInt, 
            " strategies, together with the 95% credible 
            interval for each one.  The strategy with highest expected net benefit is ", 
            cache$bestnetBenVal, ", with an expected net benefit of 
           ", input$currency, cache$netBencostsVal,
           " (equivalent to a net benefit on the effectiveness scale of ", 
           cache$netBeneffectsVal, " ", input$unitBens, "s). 
                Net benefit and 95% credible intervals for all strategies 
           are presented in the above table. ", sep="")
    }) 
    
    # Table of Summary of Absolute Net Benefit Statistics
    output$tableNetBenefit <- renderTable({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      #dummy <- input$indSim # ensure update with ind sim box tick
      
      tableNetBenefit <- makeTableNetBenefit(cache$costs, cache$effects, 
                                             lambda=input$lambdaOverall, cache$nInt)
      cache$lambdaOverall <- input$lambdaOverall
      rownames(tableNetBenefit) <- c(paste("Mean", input$effectDef), 
                                    paste("Mean", input$costDef), 
                                    paste("Expected Net Benefit at", 
                                          input$currency, input$lambdaOverall, 
                                          "per", input$unitBens), 
                                    "95% Lower CI (on Costs Scale)", 
                                    "95% Upper CI (on Costs Scale)", 
                                    "Expected Net Benefit on Effects Scale", 
                                    "95% Lower CI (on Effects Scale)", 
                                    "95% Upper CI (on Effects Scale)")
      cache$tableNetBenefit <- tableNetBenefit
      tableNetBenefit
    }, rownames = TRUE)  
    
    # Download table as a csv file
    output$downloadTableNetBenefit <- downloadHandler(
      filename = "Net\ benefit\ statistics.csv",
      content = function(file) {
        tableOut <- cache$tableNetBenefit
        if(!is.null(tableOut)) {
          tableOut <- cbind(rownames(tableOut), tableOut)
          colnames(tableOut) <- c("Intervention", colnames(tableOut)[-1])
        }
        write.csv(tableOut, file, row.names = FALSE)
      },
      contentType = "text/plain"
    )

    # EVPI INB bar plot
    output$plots5a <- renderPlot({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      #dummy <- input$indSim # ensure update with ind sim box tick
      
      makeInbOptBar(cache$costs, cache$effects, 
                   lambda=input$lambdaOverall)
    })
    
    # Absolute net benefit densities
    output$plots5 <- renderPlot({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
     # dummy <- input$indSim # ensure update with ind sim box tick
      
      make2wayDensity(cache$costs, cache$effects, 
                     lambda=input$lambdaOverall)
    })
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   ############
   # EVPI TAB #
   ############
   
  

   output$textEVPI1 <- renderText({
     if (!valuesImportedFLAG(cache, input)) return(NULL)
     dummy <- input$indSim # ensure update with ind sim box tick
     
     paste("The overall EVPI per person affected by the decision is estimated to be ", 
           input$currency, format(calcEvpi(cache$costs, 
          cache$effects, input$lambdaOverall), digits = 4, nsmall=2), ".  This is equivalent to ", 
          format(calcEvpi(cache$costs, cache$effects, input$lambdaOverall)/input$lambdaOverall, 
                 digits = 4, nsmall=1), " ", input$unitBens,
          "s per person on the health effects scale.", sep="")
   })     

    output$textEVPI2 <- renderText({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      dummy <- input$indSim # ensure update with ind sim box tick
      
      cache$calcEvpiVal <- calcEvpi(cache$costs, cache$effects, input$lambdaOverall)
      
      paste("If the number of people affected by the decision per year is " , 
            input$annualPrev, ", then the overall EVPI per year is ", input$currency,
            format(cache$calcEvpiVal * input$annualPrev, 
              digits = 4, nsmall=2), " for ", input$jurisdiction, ".", sep="")
    }) 

    output$textEVPI3 <- renderText({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      dummy <- input$indSim # ensure update with ind sim box tick
      
      paste("When thinking about the overall expected value of removing decision uncertainty, 
            one needs to consider how long the current comparison 
            will remain relevant. If the decision relevance horizon is ", input$horizon, 
            " years, then the overall expected value of removing 
            decision uncertainty for ", 
            input$jurisdiction, " would be ", input$currency, 
            format(cache$calcEvpiVal * input$annualPrev * input$horizon, 
            	digits = 4, nsmall=2),".", sep="")
    }) 

    output$textEVPI4 <- renderText({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      dummy <- input$indSim # ensure update with ind sim box tick
      
      paste("Research or data collection exercises costing more than this amount 
            would not be considered an efficient use of resources. This is because 
            the return on investment from the research – as measured by the 
            health gain and cost savings resulting from enabling the decision maker to better 
            identify the best decision  option – is expected to be no higher than ",
            input$currency, 
            format(cache$calcEvpiVal * input$annualPrev * input$horizon, 
              digits = 4, nsmall=2),".", sep="")  
      }) 

    output$textEVPI5 <- renderText({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      
      paste("The EVPI estimates in the table below quantify the expected value to 
            decision makers within ", input$jurisdiction, " of removing all current 
            decision uncertainty at a threshold of ", input$currency, input$lambdaOverall, 
            " per ", input$unitBens, ".  This will enable comparison against 
            previous analyses to provide an idea of the scale of decision uncertainty 
            in this topic compared with other previous decisions. The EVPI estimate 
            for a range of willingness-to-pay thresholds are illustrated in 
            the figures below the table.", sep="")
    })


    # Table Overall EVPI
    output$tableEVPI <- renderTable({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      dummy1 <- input$indSim # ensure update with ind sim box tick
      dummy2 <- input$lambdaOverall
      
      tableEVPI <- matrix(NA, nrow = 7, ncol = 2)
      colnames(tableEVPI) <- c(paste("Overall EVPI (", 
                                     input$currency, ")", sep=""), 
                               paste("Overall EVPI (", input$unitBens, ")", sep=""))
      rownames(tableEVPI) <- c("Per Person Affected by the Decision", 
                              paste("Per Year in", input$jurisdiction, "Assuming", 
                                    input$annualPrev, "Persons Affected per Year"), 
                              "Over 5 Years", 
                              "Over 10 Years", 
                              "Over 15 Years", 
                              "Over 20 years", 
                              paste("Over Specified Decision Relevance Horizon (", 
                                    input$horizon, " years)", sep=""))
      overallEvpi <- calcEvpi(cache$costs, cache$effects, 
                                           lambda=input$lambdaOverall)
      cache$overallEvpi <- overallEvpi
      cache$lambdaOverall <- input$lambdaOverall
      evpiVector <- c(overallEvpi, overallEvpi * input$annualPrev, overallEvpi * input$annualPrev * 5, 
                     overallEvpi * input$annualPrev * 10, overallEvpi * input$annualPrev * 15,
                     overallEvpi * input$annualPrev * 20,
                     overallEvpi * input$annualPrev * input$horizon)     
      tableEVPI[, 1] <- signif(evpiVector, 4)          
      tableEVPI[, 2] <- signif(evpiVector / input$lambdaOverall, 4)   
      cache$tableEVPI <- tableEVPI
      tableEVPI
    }, rownames = TRUE, digits=cbind(rep(1, 7), rep(1, 7), rep(2, 7))) 
   
    output$downloadTableEVPI <- downloadHandler(
      filename = "Overall\ EVPI.csv",
      content = function(file) {
        tableOut <- cache$tableEVPI
        write.csv(tableOut, file)#, row.names = FALSE)
      },
      contentType = "text/plain"
    )



    # EVPI versus lambda (costs)
    output$plots3 <- renderPlot({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      dummy <- input$indSim # ensure update with ind sim box tick
      
      cache$lambdaOverall <- input$lambdaOverall
      makeEvpiPlot(cache$costs, cache$effects, lambda=input$lambdaOverall,
                   main=input$main3, 
                   xlab="Threshold willingness to pay", 
                   ylab="Overall EVPI per person affected (on costs scale)",
                   col="blue",  costscale = TRUE, session)
    })
   
    # EVPI versus lambda (effects)
    output$plots4 <- renderPlot({
      dummy <- input$indSim # ensure update with ind sim box tick
      
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      makeEvpiPlot(cache$costs, cache$effects, lambda=input$lambdaOverall,
                   main=input$main4, 
                   xlab="Threshold willingness to pay", 
                   ylab="Overall EVPI per person affected (on effects scale)",
                   col="blue",  costscale = FALSE, session)
    })
   
    output$plots6 <- renderPlot({
      dummy <- input$indSim # ensure update with ind sim box tick
      
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      make4wayEvpiPlot(cache$costs, cache$effects, lambda=input$lambdaOverall, 
                       prevalence=input$annualPrev, horizon=input$horizon, 
                       measure1 = input$currency, 
                       measure2 = input$unitBens, session)
    })
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
    ###############################
    # EVPPI SINGLE PARAMETERS TAB #
    ###############################
     
    output$tableEVPPI <- renderTable({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      lambda <- input$lambdaOverall # re-run if labmda changes
      dummy <- input$indSim # ensure update with ind sim box tick
      
      cache$lambdaOverall <- input$lambdaOverall
      params <- cache$params
      costs <- cache$costs
      effects <- cache$effects
  
      overallEvpi <- calcEvpi(costs, effects, lambda)
      cache$overallEvpi <- overallEvpi
     
      inb <- createInb(costs, effects, lambda)
      pEVPI <- applyCalcSingleParamGam(params, inb, session, cache)
      cache$pEVPI <- pEVPI
     
      tableEVPPI <- matrix(NA, nrow = ncol(params), ncol = 5)
      tableEVPPI[, 1] <- round(pEVPI[, 1], 2)
      tableEVPPI[, 2] <- round(pEVPI[, 2], 2)
      tableEVPPI[, 3] <- round(pEVPI[, 1] / overallEvpi , 2)
      tableEVPPI[, 4] <- signif(pEVPI[, 1] * input$annualPrev, 4)
      tableEVPPI[, 5] <- signif(pEVPI[, 1] * input$annualPrev * input$horizon, 4)
      colnames(tableEVPPI) <- c(paste("Per Person EVPPI (", input$currency, ")", sep=""), 
                                "Standard Error","Indexed to Overall EVPI = 1.00", 
                                paste("EVPPI for ", input$jurisdiction, 
                                      " Per Year (", input$currency, ")", sep=""), 
                                paste("EVPPI for ", input$jurisdiction, " over ", 
                                      input$horizon, " years (", input$currency, ")", sep=""))
      rownames(tableEVPPI) <- colnames(cache$params)
      cache$tableEVPPI <- tableEVPPI
      tableEVPPI
    }, rownames = TRUE) 
   
    # Download single parameter EVPPI values as csv file
    output$downloadSingleEVPPI <- downloadHandler(
    filename = "EVPPI\ for\ individual\ parameters.csv",
      content = function(file) {
        write.csv(cache$tableEVPPI, file)
      },
    contentType = "text/plain"
    )


    # EVPPi horizontal bar chart
    output$plot7 <- renderPlot({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      dummy <- input$lambdaOverall
      makeEvppiBar(cache$pEVPI[, 1], cache$params)
    })
  

   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
    
    ################
    # EVPPI GROUPS #
    ################  
    
    # This function gets the parameter names
    # The output is the checkbox list
    observe({
      x <- input$parameterFile
      y <- input$loadSession
      params <- cache$params
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
      params <- cache$params
      if (is.null(params)) return(NULL)
      paramNames <- paste(1:ncol(params), ") ", colnames(params), sep="")
      currentSelection <- which(paramNames%in%currentSelectionNames)
      cache$currentSelection <- currentSelection
    })

    # This function responds to the add button being pressed
    # This function saves the current selection and then increase counter
    # It does the calculation and then outputs the selection table   
    output$selectedTable <- renderTable({
      dummy <- input$calculateSubsetsEvpi
      if (dummy == 0) return(NULL)
      if (!isolate(valuesImportedFLAG(cache, input))) return(NULL)
      if (dummy == 0) return(NULL)
      
      counterAdd <- cache$counterAdd
      counterAdd <- counterAdd + 1
      cache$counterAdd <- counterAdd
      
      setStore <- cache$setStore
      currentSelection <- cache$currentSelection
      setStore[[counterAdd]] <- currentSelection
      cache$setStore <- setStore
      
      calc <- function(x, inp, cache, session) { # pass session so the progress bar will work
        calSubsetEvpi(x, inp, cache, session)
      }
      
      #first pull down the existing values
      subsetEvpiValues <- cache$subsetEvpiValues
      if (is.null(subsetEvpiValues)) {
        subsetEvpiValues <- t(sapply(setStore[1:counterAdd], calc, 
                                input$lambdaOverall, cache, session))
      } else {
        newEvpiValue <- t(sapply(setStore[(NROW(subsetEvpiValues) + 1):counterAdd], 
                                 calc, input$lambdaOverall, cache, session))
        subsetEvpiValues <- rbind(subsetEvpiValues, newEvpiValue)
      }
      
      cache$subsetEvpiValues <- subsetEvpiValues
      cache$setStoreMatchEvpiValues <- setStore # cache these for the report in case they change

      cache$groupTable <- buildSetStoreTable(setStore[1:counterAdd], subsetEvpiValues, cache)
      cache$groupTable
    }, rownames = TRUE, sanitize.rownames.function = bold.allrows)

    # Download group EVPPI values as csv file
    output$downloadGroupEVPPI <- downloadHandler(
      filename = "EVPPI\ for\ parameter\ groups.csv",
      content = function(file) {
        contents <- cache$groupTable
        if(!is.null(contents)) {
          contents[, 1] <- as.character(contents[, 1])
          print(contents <- as.matrix(contents))
          colnames(contents) <- c("Parameters", 
            paste("Per Person EVPPI (", cache$currency, ")", sep=""), 
            "Standard Error",
            "Indexed to Overall EVPI", 
            paste("EVPPI for ", cache$jurisdiction, 
              " Per Year (", cache$currency, ")", sep=""), 
            paste("EVPPI for ", cache$jurisdiction, 
              " over ", cache$horizon, " years (", cache$currency, ")", sep=""))
        }
        write.csv(contents, file)
      },
      contentType = "text/plain"
    )


     # This clears everything on loading new data.
    observe({ # clear the selections
      # dummy <- input$clearSubsetsEvpi
      dummy1 <- valuesImportedFLAG(cache, input)
      setStore <- vector("list", 100)
      cache$setStore <- setStore
      cache$counterAdd <- 0
      cache$subsetEvpiValues <- NULL
      cache$setStoreMatchEvpiValues <- NULL # cache these for the report in case they change
    })



   
    
    
    
    
   
   
   
    
    
    ############
    # PSUB TAB #
    ############
    
    output$tablePSUB <- renderTable({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      #      dummy1 <- input$indSim # ensure update with ind sim box tick
      dummy2 <- input$lambdaOverall
      
      tablePSUB <- matrix(NA, nrow = 3, ncol = cache$nInt)
      
      cache$lambdaOverall <- input$lambdaOverall
      .nb <- colMeans(createNb(cache$costs, cache$effects, cache$lambdaOverall))
      psb <- as.numeric(max(.nb) - .nb)
      
      overallEvpi <- calcEvpi(cache$costs, cache$effects, cache$lambdaOverall)
      
      tablePSUB[1, ] <- signif(psb, 4)
      tablePSUB[2, ] <- signif(overallEvpi, 4)
      tablePSUB[3, ] <- signif(psb + overallEvpi, 4)
      
      
      colnames(tablePSUB) <- colnames(cache$costs)
      rownames(tablePSUB) <- c("Payer Strategy Burdens","Payer Uncertainty Burdens","P-SUBS")
      
      cache$tablePSUB <- tablePSUB
      tablePSUB
    }, rownames = TRUE)
    
    
    
    output$downloadTablePSUB <- downloadHandler(
      filename = "PSUB.csv",
      content = function(file) {
        tableOut <- cache$tablePSUB
        write.csv(tableOut, file)#, row.names = FALSE)
      },
      contentType = "text/csv"
    )
    
    
    
    output$plotsPSUBstacked <- renderPlot({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      makePSUBplot(cache$costs, cache$effects, lambda=input$lambdaOverall, input$annualPrev, benUnit = input$unitBens,
                   beside = FALSE)
    })
    
    
    output$plotsPSUBsideBySide <- renderPlot({
      if (!valuesImportedFLAG(cache, input)) return(NULL)
      makePSUBplot(cache$costs, cache$effects, lambda=input$lambdaOverall, input$annualPrev, benUnit = input$unitBens,
                   beside = TRUE)
    })
    
    
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
    #################
    # REPORT TAB #
    #################
    
    ## Functions that download things

    ## DOWNLOAD REPORT
    
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
        file.copy(src, 'report.Rmd', overwrite=TRUE)

        library(rmarkdown)
        out <- render(input = 'report.Rmd', #pdf_document()
                      output_format = switch(
                        input$format,
                        PDF = pdf_document(), HTML = html_document(), 
                        Word = word_document()),
                      envir = cache
        )
        file.copy(out, file)
      },
      contentType = "text/plain"
    )
    
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
    ####################
    # SAVE SESSION TAB #
    ####################
   
    # CURRENTLY OUT OF ACTIONS
   
    # Download .Rdata file
#     output$saveSession <- downloadHandler(
#       filename =  function() paste(input$RdataFileName),
#       content = function(file) {
#         save(list = ls(envir=cache), file = file, envir=cache)
#       },
#       contentType = "text/plain")
    






}) # END OF SHINYSERVER FUNCTION


######################################################## ENDS #############################################















