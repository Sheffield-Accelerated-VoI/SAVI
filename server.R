source("scripts.R")
library(mgcv)
library(knitr)
library(rmarkdown)

shinyServer(function(input, output) {
  
 # dInput <- function() read.csv("parameters.csv")
#  dInput2 <- function() read.csv("costs.csv")
#  dInput3 <- function() read.csv("effects.csv")
  
  
  
  # Function that imports the data file
  dInput <- reactive({
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
  
  dInput2 <- reactive({
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
  
  dInput3 <- reactive({
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
  
  ## Functions that make plots
  
  output$plots1 = renderPlot({
    if (is.null(dInput()) | is.null(dInput2())  | is.null(dInput3())) return(NULL)
    make.CEPlaneplot(dInput2(), dInput3(), lambda=input$lambda2, main=input$main, xlab=input$xlab, 
                     ylab=input$ylab, col=input$color)
  })
  
  output$plots2 = renderPlot({
    if (is.null(dInput()) | is.null(dInput2())  | is.null(dInput3())) return(NULL)
    ceac.obj <<- ceac()
    make.CEACplot(ceac.obj, lambda=input$lambda2, main=input$main2, xlab=input$xlab2, ylab=input$ylab2,
                  col=input$color2)
  })
  
  output$plots3 = renderPlot({
    if (is.null(dInput()) | is.null(dInput2())  | is.null(dInput3())) return(NULL)
    dI2 <<- dInput2()
    dI3 <<- dInput3()
    make.EVPIplot(dI2, dI3, main=input$main3, xlab=input$xlab3, ylab=input$ylab3,
                  col=input$color3, input$incremental, costscale = TRUE)
  })
  
  output$plots4 = renderPlot({
    if (is.null(dInput()) | is.null(dInput2())  | is.null(dInput3())) return(NULL)
    make.EVPIplot(dInput2(), dInput3(), main=input$main4, xlab=input$xlab4, ylab=input$ylab4,
                  col=input$color4, input$incremental, costscale = FALSE)
  })
  
  output$plots4way = renderPlot({
    if (is.null(dInput()) | is.null(dInput2())  | is.null(dInput3())) return(NULL)
    make.4way.plot(dInput2(), dInput3(), ceac.obj, lambda=input$lambda2, main=input$main1, 
                  xlab=input$xlab2, ylab=input$ylab2, col=input$color2, 
                  main2=input$main4, xlab2=input$xlab4, 
                  ylab2=input$ylab4,
                  col2=input$color4)
  })

  ## Function that makes the reports
  
  output$report <- downloadHandler(
    filename == function() {paste('my-report', sep = '.', switch(
      input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
    ))
    },
    content = function(file) {
      library(rmarkdown)
      #knit('report.Rnw')
      #system("pdflatex -synctex=1 -interaction=nonstopmode report.tex")
      #out = knit2pdf('report.Rnw', clean = TRUE)
      out <- render('report2.Rmd', switch(
                    input$format,
                    PDF = pdf_document(), HTML = html_document(), Word = word_document())
      )
      file.copy(out, file)
    },
    contentType = 'application/pdf'
  )
  
  
})

