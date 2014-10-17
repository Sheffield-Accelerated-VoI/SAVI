# dInput <<- function() {
# a <- cbind(1)
# colnames(a) <- "No inputs selected"
# a
# }

#comment
#add second comment


fluidPage(
  # Application title
  headerPanel("SAVI - Sheffield Accelerated Value of Information"),
  
  
  # Main panel (on the right hand side)
  mainPanel(
    tabsetPanel(
      tabPanel("Getting started",
               h3("Basic user guide"),
               p(HTML("WWelcome to this early draft version!")),
               p(HTML("<b><div style='background-color:#FADDF2;border:1px solid
                      black;'>WARNING: THIS IS AN EARLY DRAFT WEB-BASED IMPLIMENTATION. USE AT YOUR OWN RISK.</div>
                      </b>")),br(),
               
               p(HTML("For more information see Mark Strong's website  
                      <a href='http://www.sheffield.ac.uk/scharr/sections/ph/staff/profiles/mark' 
                      target='_blank'>here</a>")),
               p(HTML("Some more text can go here"))
               ),
      
      
      
      tabPanel("Import files",   	# Button to import parameter data
               h3("Parameter importation"),
               p(HTML("To run the application, import your data set using the 
                      import buttons below. You data must
                      be supplied on the form of a csv file. If the importation is
                      done properly, the data are displayed in the next tab")),
               fileInput('file1', 'Choose CSV File',
                         accept=c('text/csv', 'text/comma-separated-values,text/plain')),
               # Various checkboxes and input fields to specify the data file format
               checkboxInput('header', 'Is there a header row?', TRUE),
               checkboxInput('rownames', 'Does the first column contain row names?', FALSE),
               #selectInput('sep', 'Separator:',
               #	c(Comma=',',Semicolon=';',Tab='\t', Space=' '), ','),
               #selectInput('quote', 'Quote:',
               #            c('None'='none', 'Double Quote'="\"'", 'Single Quote'="\''"),
               #             selected="\"'"),
               #selectInput('dec', 'Decimal mark', c(Dot='.', Comma=','), '.'),
               br(),
               
               
               # Button to import costs  data    
               h3("Costs importation"),
               fileInput('file2', 'Choose CSV File',
                         accept=c('text/csv', 'text/comma-separated-values,text/plain')),
               # Various checkboxes and input fields to specify the data file format
               checkboxInput('header2', 'Is there a header row?', TRUE),
               checkboxInput('rownames2', 'Does the first column contain row names?', FALSE),
               #selectInput('sep2', 'Separator:',
               #	c(Comma=',',Semicolon=';',Tab='\t', Space=' '), ','),
               #selectInput('quote2', 'Quote:',
               #       c(None='','Double Quote'='"','Single Quote'="'"),
               #            '"'),
               #selectInput('dec2', 'Decimal mark', c(Dot='.', Comma=','), '.'),
               br(),
               
               # Button to import effects data
               h3("Effects importation"),
               fileInput('file3', 'Choose CSV File',
                         accept=c('text/csv', 'text/comma-separated-values,text/plain')),
               # Various checkboxes and input fields to specify the data file format
               checkboxInput('header3', 'Is there a header row?', TRUE),
               checkboxInput('rownames3', 'Does the first column contain row names?', FALSE),
               #selectInput('sep3', 'Separator:',
               #	c(Comma=',',Semicolon=';',Tab='\t', Space=' '), ','),
               #selectInput('quote3', 'Quote:',
               #           c(None='','Double Quote'='"','Single Quote'="'"),
               #            '"'),
               #selectInput('dec3', 'Decimal mark', c(Dot='.', Comma=','), '.')
               
               h3("Are uploaded costs and effects incremental or absolute?"),
               radioButtons('incremental', label="", c("Incremental" = "TRUE", "Absolute" = "FALSE"), "FALSE")
               ),
      
      tabPanel("Check upload",
               
               h3("Check data import"),
               h4("The parameters, costs and effects you have uploaded are displayed below:"),
               p("(only the first 5 first rows if the dataset contains more
                 than 50 rows, and the first 10 columns if the dataset contains
                 more than 10 columns)"),br(),
               
               h4("Parameters"),
               tableOutput("view"),
               h4("Costs"),
               tableOutput("view2"),
               h4("Effects"),
               tableOutput("view3")
      ),
      
      
      tabPanel("Results", 
               h3("Specify lambda"),
               sliderInput('lambda', label="", 0, 60000, 10000, 1000),
               h3("Partial EVPI for single parameters"),
               tableOutput("summary")
      ),
      
      # Graphic
      # coming from the function output$boxplots in server.R
      tabPanel("Plots",
               
               h4("Figures"),
               textInput("main",strong("Graphic title:"), "CE plane"),
               textInput("xlab",strong("X axis label:"), "Effects"),
               textInput("ylab",strong("Y axis label:"), "Costs"),
               textInput("color","Color:","red"),
               h4("Specify lambda"),
               sliderInput('lambda2', label="", 500, 60000, 10000, 500, width="600px"),
               #sliderInput('lambda2', label="", -6, 6, 4, 0.1),
               plotOutput("plots4way", width="600px", height="600px"),
               
               
               h4("CE plane"),
               textInput("main",strong("Graphic title:"), "CE plane"),
               textInput("xlab",strong("X axis label:"), "Effects"),
               textInput("ylab",strong("Y axis label:"), "Costs"),
               textInput("color","Color:","red"),
               h4("Specify lambda"),
               sliderInput('lambda2', label="", 500, 60000, 10000, 500, width="600px"),
               #sliderInput('lambda2', label="", -6, 6, 4, 0.1),
               plotOutput("plots1", width="600px", height="600px"),
               
               h3("CEAC"),
               textInput("main2",strong("Graphic title:"), "CEAC"),
               textInput("xlab2",strong("X axis label:"), "lambda"),
               textInput("ylab2",strong("Y axis label:"), "P(cost effective)"),
               textInput("color2","Color:","black"),
               plotOutput("plots2"),
               
               h3("Overall EVPI versus lambda"),
               textInput("main3",strong("Graphic title:"), "EVPI (on costs scale) vs lambda"),
               textInput("xlab3",strong("X axis label:"), "lambda"),
               textInput("ylab3",strong("Y axis label:"), "Overall EVPI (on costs scale)"),
               textInput("color3","Color:","red"),
               plotOutput("plots3"),
               
               h3("Overall EVPI versus lambda"),
               textInput("main4",strong("Graphic title:"), "EVPI (on effects scale) vs lambda"),
               textInput("xlab4",strong("X axis label:"), "lambda"),
               textInput("ylab4",strong("Y axis label:"), "Overall EVPI (on effects scale)"),
               textInput("color4","Color:","red"),
               plotOutput("plots4")
      ),
      
      tabPanel("EVPPI", 
               p(HTML("Here you can define subsets of parameters to add to your EVPPI. Choose a subset of parameters
                      using the tick boxes and press the Add button to add the combination to the analysis.
                      You can add as many combinations as you wish. Press the Calculate EVPPI button
                      when you have finished your selections.")),
               br(),
               sidebarLayout(
                 sidebarPanel(
                   h3("Select Parameters for EVPPI"),
                   textOutput("selection"),
                   checkboxGroupInput("parameters", NULL, 
                                      c("a","b","c"), 
                                      selected = NULL),
                   br(),
                   submitButton("Add")),
                
                 mainPanel(
                   h3("selected parameter combinations"),
                   fluidRow(verbatimTextOutput("selection1")),
                   br(),
                   actionButton("calculate1", "Calculate EVPPI"),
                   br(),
                   actionButton("clear1",label="Clear Selection")))       
               ),
      
      
      
      # Numerical summary of the dataset,
      # coming from the function output$summary in server.R
      tabPanel("Downloads",downloadButton('downloadSummary', 'Download EVPI values'),
               br(),br(),#,tableOutput("summary")
               radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                             inline = TRUE),
               downloadButton('downloadReport', 'Download report')
      )
      ))
)