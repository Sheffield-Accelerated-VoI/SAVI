source("scripts.R")
library(mgcv)
library(knitr)
library(rmarkdown)



shinyServer(function(input, output, session) {
  
  dInput <- function() read.csv("parameters.csv")
  dInput2 <- function() read.csv("costs.csv")
  dInput3 <- function() read.csv("effects.csv")
  
  
  
#  Function that imports the data file
     dInput <<- reactive({
       in.file = input$file1
       
       if (is.null(in.file))
         return(NULL)
       
       if (input$rownames) {
         read.csv(in.file$datapath, header=input$header, #sep=input$sep,
                  row.names=1)#, dec=input$dec)
       } else {
         read.csv(in.file$datapath, header=input$header)#, sep=input$sep, dec=input$dec)
       }
     })
     
     dInput2 <<- reactive({
       in.file = input$file2
       
       if (is.null(in.file))
         return(NULL)
       
       if (input$rownames) {
         read.csv(in.file$datapath, header=input$header2, #sep=input$sep2,
                  row.names=1)#, dec=input$dec2)
       } else {
         read.csv(in.file$datapath, header=input$header2)#, sep=input$sep2, dec=input$dec2)
       }
     })
     
     dInput3 <<- reactive({
       in.file = input$file3
       
       if (is.null(in.file))
         return(NULL)
       
       if (input$rownames) {
         read.csv(in.file$datapath, header=input$header3, #sep=input$sep3,
                  row.names=1)#, dec=input$dec3)
       } else {
         read.csv(in.file$datapath, header=input$header3)#, sep=input$sep3, dec=input$dec3)
       }
     })
   
  
  # Function that render the data file and passes it to ui.R
  output$view <- renderTable({
    d.input = dInput()
    if (is.null(d.input)) return(NULL)
    if (ncol(d.input) > 10) d.input = d.input[, 1:10]
    head(d.input, n=5)  
  })
  
  output$view2 = renderTable({
    d.input = dInput2()
    if (is.null(d.input)) return(NULL)
    if (ncol(d.input) > 10) d.input = d.input[, 1:10]
    head(d.input, n=5)  
  })
  
  output$view3 = renderTable({
    d.input = dInput3()
    if (is.null(d.input)) return(NULL)
    if (ncol(d.input) > 10) d.input = d.input[, 1:10]
    head(d.input, n=5)  
  })
  
  
# Function that calculates the single partial EVPI outputs to be sent to the main panel in ui.R
  
  partialEVPI <- reactive({
    if (is.null(dInput()) | is.null(dInput2())  | is.null(dInput3())) return(NULL)
    dI2 <<- dInput2()
    dI3 <<- dInput3()
    inb <- createINB(dInput2(), dInput3(), input$lambda, input$incremental)
    d.input <- dInput()
    pEVPI <<- apply.singleParamGam(d.input, inb)
    cbind(pEVPI)
    
  })
  
  output$summary <- renderTable(partialEVPI())
  
  # function that calculates ceac
  ceac <- reactive({
    if (is.null(dInput()) | is.null(dInput2())  | is.null(dInput3())) return(NULL)
    make.CEAC(dInput2(), dInput3(), input$incremental)
  })
  
  
  # Function that creates a download button
  output$downloadSummary = downloadHandler(
    filename = "evpi\ values.csv",
    content = function(file) {
      write.csv(partialEVPI(), file)
    })
  
  # Functions that make reactive text to accompany plots
  output$textCEplane1 <- renderText({
    paste("This graph shows the standardised cost-effectiveness plane per person based on",input$n3,"model runs,
      in which uncertain model parameters are varied simultaneously in a probabilistic sensitivity analysis.  
      The mean incremental cost of",input$t3,"versus",input$t2,"is",input$t6,"X. This suggests that
      ",input$t3,"is more/less costly over the",input$n7,"year time horizon. There is some uncertainty due to model 
      parameters, with the 95% CI for the incremental cost ranging from (lower CI, upper CI).  
      The probability that",input$t3,"is cost saving (i.e. cheaper over the",input$n7,"year time horizon) compared 
      to",input$t2,"is XX%.")
  })                       ###THIS FUNCTION STILL NEEDS TO BE MADE REACTIVE TO RESULTS

  output$textCEplane2 <- renderText({
    paste("The mean incremental benefit of",input$t3,"versus",input$t2,"is",input$t6,"X.  This suggests that",input$t3,"
      is more/or less beneficial over the",input$n7,"year time horizon.  Again, there is some uncertainty due to 
      model parameters, with the 95% CI for the incremental benefit ranging from (lower credible interval, upper CI).
      The probability that",input$t3,"is more beneficial than",input$t2,"is XX%.")
  })                        ###THIS FUNCTION STILL NEEDS TO BE MADE REACTIVE TO RESULTS

  output$textCEplane3 <- renderText({
    paste("The incremental expected cost per unit of benefit is estimated at",input$t6,"XX per",input$t7,". This 
      is above/below the threshold of",input$t6,input$lambda2,"per",input$t7,"indicating that",input$t3,"would (not) be considered cost-effective
      at this threshold.  There is uncertainty with a XX% probability that",input$t3,"is more cost-effective (XX% of the 
      probabilistic model run ‘dots’ are below and to the right of the diagonal threshold line).")
  })                         ###THIS FUNCTION STILL NEEDS TO BE MADE REACTIVE TO RESULTS
  
  output$textCEplane4 <- renderText({
    paste(input$t3,"vs.",input$t2)
  })

  output$textCEplane5 <- renderText({
    paste("$XX%$ probability that",input$t3,"is more cost-effective than",input$t2,"at a threshold 
    of",input$t6,input$lambda2,"per",input$t7)
  })                       ###THIS FUNCTION STILL NEEDS TO BE MADE REACTIVE TO RESULTS

  output$textCEAC1 <- renderText({
    paste("This graph shows the cost-effectiveness acceptability curve for the comparison of strategies. The results show that at a threshold 
    value for cost-effectiveness of",input$t6,input$lambda2,"per",input$t7,"the strategy with the highest probability of being most cost-effective 
    is X, with a probability of XX%. More details on how to interpret CEACs are available from the literature")
  })                       ###THIS FUNCTION STILL NEEDS TO BE MADE REACTIVE TO RESULTS

  output$textCEAC2 <- renderText({
    paste(input$t2)
  })                       

  output$textCEAC3 <- renderText({
    paste(input$t3)
  })                       



# Functions that make tables  
  output$tableCEplane <- renderTable({
    tableCEplane <- matrix(c(input$lambda2,input$t2,input$n3,NA,NA,NA,NA,NA,NA,NA,NA,NA),nrow=12,ncol=1)
    colnames(tableCEplane) <- input$t3
    rownames(tableCEplane) <- c("Threshold","Comparator","Number of PSA runs","Mean inc. Benefit per Person", "Mean inc. Cost per Person",
                                   "ICER Estimate","PSA Results","95% CI for inc. Costs","95% CI for inc. Benefits","Probability
                                   intervention is cost saving","Probability intervention provides more benefit","Probability that
                                  intervention is cost-effective")
    tableCEplane
  })  



# Functions that make plots
  output$plots1 = renderPlot({
    if (is.null(dInput()) | is.null(dInput2())  | is.null(dInput3())) return(NULL)
    make.CEPlaneplot(dInput2(), dInput3(), lambda=input$lambda2, xlab=input$t4, 
                     ylab=input$t5, col="orangered")
  })  ###NEED TO ADD POINT FOR MEAN ON PLOT - SHOULD BE LARGER AND BRIGHTER (E.G.DARK RED, STANDARD SIZE AND SOLID WOULD WORK WELL)
  
  output$plots2 = renderPlot({
    if (is.null(dInput()) | is.null(dInput2())  | is.null(dInput3())) return(NULL)
    ceac.obj <<- ceac()
    make.CEACplot(ceac.obj, lambda=input$lambda3, main="Cost-effectiveness Acceptability Curve", xlab="Threshold willingness to pay", 
                  ylab="Probability strategy is cost-effective",col="red")
  })  ###NEED TO ADD % COST-EFFECTIVENESS AT LINE AS A LABEL AND COLOUR CODE LINES
  
  output$plots3 = renderPlot({
    if (is.null(dInput()) | is.null(dInput2())  | is.null(dInput3())) return(NULL)
    dI2 <<- dInput2()
    dI3 <<- dInput3()
    make.EVPIplot(dI2, dI3, main=input$main3, xlab="Threshold willingness to pay", ylab="Overall EVPI per person affected (on costs scale)",
                  col="red", input$incremental, costscale = TRUE)
  })
  
  output$plots4 = renderPlot({
    if (is.null(dInput()) | is.null(dInput2())  | is.null(dInput3())) return(NULL)
    make.EVPIplot(dInput2(), dInput3(), main=input$main4, xlab="Threshold willingness to pay", ylab="Overall EVPI per person affected (on effects scale)",
                  col="red", input$incremental, costscale = FALSE)
  })
  
  output$plots4way = renderPlot({
    if (is.null(dInput()) | is.null(dInput2())  | is.null(dInput3())) return(NULL)
    make.4way.plot(dInput2(), dInput3(), ceac.obj, lambda=input$lambda2, main=input$main1, 
                   xlab=input$xlab2, ylab=input$ylab2, col=input$color2, 
                   main2=input$main4, xlab2=input$xlab4, 
                   ylab2=input$ylab4,
                   col2=input$color4)
  })
  
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
                    PDF = pdf_document(), HTML = html_document(), Word = word_document_local())
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

output$selection1 <- renderPrint({input$parameters})
# output$cnames <- reactive(if (exists(dInput())) {colnames(dInput())} else {print("HELP!")}) 

observe({

  updateCheckboxGroupInput(session, "parameters", label = input$Add)
})
  
})
