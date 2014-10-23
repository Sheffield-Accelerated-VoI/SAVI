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
    assign("counterAdd", 0, envir = cache)     
    
    # these three rows autoload values for testing purposes - to avoid having to load them manually. MS
    # ###########
    #   load.parameters <- function() read.csv("parameters.csv")                                   
    #   load.costs <- function() read.csv("costs.csv")
    #   load.effects <- function() read.csv("effects.csv")
    # ########### 
    
    
    
    #  Function that imports parameters
    load.parameters <- observe({
      in.file = input$parameter.file
      print("reactive called")
      if (is.null(in.file))
        return(NULL)
      
      if (input$rownames1) {
        dat <- read.csv(in.file$datapath, header=input$header1, #sep=input$sep,
                        row.names=1)#, dec=input$dec)
        assign("params", dat, envir = cache)
      } else {
        dat <- read.csv(in.file$datapath, header=input$header1)#, sep=input$sep, dec=input$dec)
        assign("params", dat, envir = cache)
      }
    })
    
    #  Function that imports costs    
    load.costs <- observe({
      in.file = input$costs.file
      
      if (is.null(in.file))
        return(NULL)
      
      if (input$rownames2) {
        dat <- read.csv(in.file$datapath, header=input$header2, #sep=input$sep,
                        row.names=1)#, dec=input$dec)
        assign("costs", dat, envir = cache)
      } else {
        dat <- read.csv(in.file$datapath, header=input$header2)#, sep=input$sep, dec=input$dec)
        assign("costs", dat, envir = cache)
      }
    })
    
    # Function that imports effects
    load.effects <- observe({
      in.file = input$effects.file
      
      if (is.null(in.file))
        return(NULL)
      
      if (input$rownames3) {
        dat <- read.csv(in.file$datapath, header=input$header3, #sep=input$sep,
                        row.names=1)#, dec=input$dec)
        assign("effects", dat, envir = cache)
      } else {
        dat <- read.csv(in.file$datapath, header=input$header3)#, sep=input$sep, dec=input$dec)
        assign("effects", dat, envir = cache)
      }
    })
    
    # Functions that render the data files and pass them to ui.R
    
    output$checktable1 <- renderTable({
      x <- input$parameter.file
      if (is.null(x)) return(NULL)
      table.values <- get("params", envir=cache)
      if (ncol(table.values) > 10) table.values = table.values[, 1:10]
      head(table.values, n=5)
    })
    
    output$checktable2 <- renderTable({
      x <- input$costs.file
      if (is.null(x)) return(NULL)
      table.values <- get("costs", envir=cache)
      if (ncol(table.values) > 10) table.values = table.values[, 1:10]
      head(table.values, n=5)   
    })
    
    output$checktable3 <- renderTable({
      x <- input$effects.file
      if (is.null(x)) return(NULL)
      table.values <- get("effects", envir=cache)
      if (ncol(table.values) > 10) table.values = table.values[, 1:10]
      head(table.values, n=5)   
    })
    
    
    # Function that calculates the single partial EVPI outputs to be sent to the main panel in ui.R
    
    calcPartialEvpi <- reactive({
      if (!valuesImportedFLAG(input)) return(NULL)
      inb <- createInb(get("costs", envir=cache), get("effects", envir=cache), 
                       input$lambda, input$incremental)
      pEVPI <- applyCalcSingleParamGam(get("params", envir=cache), inb)
      cbind(pEVPI)
    })
    
    output$summary <- renderTable(calcPartialEvpi())
    
    # function that calculates ceac
    ceac <- reactive({ 
      if (!valuesImportedFLAG(input)) return(NULL)
      makeCeac(get("costs", envir=cache), get("effects", envir=cache), input$incremental)
    })
    
    
    # Function that creates a download button
    output$downloadSummary = downloadHandler(
      filename = "evpi\ values.csv",
      content = function(file) {
        write.csv(calcPartialEvpi(), file)
      })
    
    # Functions that make reactive text to accompany plots
    output$textCEplane1 <- renderText({
      paste("This graph shows the standardised cost-effectiveness plane per person based on",input$n3,"model runs,
            in which uncertain model parameters are varied simultaneously in a probabilistic sensitivity analysis.  
            The mean incremental cost of", input$t3, "versus", input$current,"is",input$t6,"X. This suggests that
            ",input$t3,"is more/less costly over the",input$n7,"year time horizon. There is some uncertainty due to model 
            parameters, with the 95% CI for the incremental cost ranging from (lower CI, upper CI).  
            The probability that",input$t3,"is cost saving (i.e. cheaper over the",input$n7,"year time horizon) compared 
            to",input$current,"is XX%.")
    })                       ###THIS FUNCTION STILL NEEDS TO BE MADE REACTIVE TO RESULTS
    
    output$textCEplane2 <- renderText({
      paste("The mean incremental benefit of",input$t3,"versus",input$current,"is",input$t6,"X.  This suggests that",input$t3,"
            is more/or less beneficial over the",input$n7,"year time horizon.  Again, there is some uncertainty due to 
            model parameters, with the 95% CI for the incremental benefit ranging from (lower credible interval, upper CI).
            The probability that",input$t3,"is more beneficial than",input$current,"is XX%.")
    })                        ###THIS FUNCTION STILL NEEDS TO BE MADE REACTIVE TO RESULTS
    
    output$textCEplane3 <- renderText({
      paste("The incremental expected cost per unit of benefit is estimated at",input$t6,"XX per",input$t7,". This 
            is above/below the threshold of",input$t6,input$lambda2,"per",input$t7,"indicating that",input$t3,"would (not) be considered cost-effective
            at this threshold.  There is uncertainty with a XX% probability that",input$t3,"is more cost-effective (XX% of the 
            probabilistic model run ‘dots’ are below and to the right of the diagonal threshold line).")
    })                         ###THIS FUNCTION STILL NEEDS TO BE MADE REACTIVE TO RESULTS
    
    output$textCEplane4 <- renderText({
      paste(input$t3,"vs.",input$current)
    })
    
    output$textCEplane5 <- renderText({
      paste("$XX%$ probability that",input$t3,"is more cost-effective than",input$current,"at a threshold 
            of",input$t6,input$lambda2,"per",input$t7)
    })                       ###THIS FUNCTION STILL NEEDS TO BE MADE REACTIVE TO RESULTS
    
    output$textCEAC1 <- renderText({
      paste("This graph shows the cost-effectiveness acceptability curve for the comparison of strategies. The results show that at a threshold 
            value for cost-effectiveness of",input$t6,input$lambda2,"per",input$t7,"the strategy with the highest probability of being most cost-effective 
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
      if (!valuesImportedFLAG(input)) return(NULL)
      tableCEplane <- matrix(c(input$lambda2, input$current, input$n3, rep(NA, 9)), nrow=12, ncol=1)
      colnames(tableCEplane) <- input$t3
      rownames(tableCEplane) <- c("Threshold","Comparator","Number of PSA runs","Mean inc. Benefit per Person", "Mean inc. Cost per Person",
                                  "ICER Estimate","PSA Results","95% CI for inc. Costs","95% CI for inc. Benefits","Probability
                                  intervention is cost saving","Probability intervention provides more benefit","Probability that
                                  intervention is cost-effective")
      tableCEplane
    })  
    
    
    
    # Functions that make plots
    output$plots1 <- renderPlot({
      if (!valuesImportedFLAG(input)) return(NULL)
      makeCEPlanePlot(get("costs", envir=cache), get("effects", envir=cache), 
                      lambda=input$lambda2, xlab=input$t4, 
                      ylab=input$t5, col="orangered")
    })  ###NEED TO ADD POINT FOR MEAN ON PLOT - SHOULD BE LARGER AND BRIGHTER (E.G.DARK RED, STANDARD SIZE AND SOLID WOULD WORK WELL)
    
    output$plots2 <- renderPlot({
      if (!valuesImportedFLAG(input)) return(NULL)
      ceac.obj <- assign("ceac.obj", ceac(), envir=cache)
      makeCeacPlot(ceac.obj, lambda=input$lambda3, 
                   main="Cost-effectiveness Acceptability Curve", 
                   xlab="Threshold willingness to pay", 
                   ylab="Probability strategy is cost-effective",col="red")
    })  ###NEED TO ADD % COST-EFFECTIVENESS AT LINE AS A LABEL AND COLOUR CODE LINES
    
    output$plots3 <- renderPlot({
      if (!valuesImportedFLAG(input)) return(NULL)
      makeEvpiPlot(get("costs", envir=cache), get("effects", envir=cache), 
                   main=input$main3, 
                   xlab="Threshold willingness to pay", 
                   ylab="Overall EVPI per person affected (on costs scale)",
                   col="red", input$incremental, costscale = TRUE)
    })
    
    output$plots4 <- renderPlot({
      if (!valuesImportedFLAG(input)) return(NULL)
      makeEvpiPlot(get("costs", envir=cache), get("effects", envir=cache), 
                   main=input$main4, 
                   xlab="Threshold willingness to pay", 
                   ylab="Overall EVPI per person affected (on effects scale)",
                   col="red", input$incremental, costscale = FALSE)
    })
    
    output$plots4way <- renderPlot({
      if (!valuesImportedFLAG(input)) return(NULL)
      make4wayPlot(get("costs", envir=cache), get("effects", envir=cache), 
                   get("ceac.obj", envir=cache), lambda=input$lambda2, main=input$main1, 
                   xlab=input$xlab2, ylab=input$ylab2, col=input$color2, 
                                       main2=input$main4, xlab2=input$xlab4, 
                                       ylab2=input$ylab4,
                                       col2=input$color4)
    })
    
  
    
    observe({
      x <- input$parameter.file
      updateCheckboxGroupInput(session, "pevpiParameters", 
                               choices = colnames(get("params", envir=cache)))
    })
      
    output$selection <- reactive({
      x <- input$Add
      
      # update counter
      counterAdd <- get("counterAdd", envir=cache)
      counterAdd <- counterAdd + 1
      assign("counterAdd", counterAdd, envir=cache)
      assign(paste("setStore", counterAdd), input$pevpiParameters)
      
      input$pevpiParameters
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
    
    
    
    })
