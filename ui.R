
fluidPage(
  headerPanel("SAVI - Sheffield Accelerated Value of Information"),
  mainPanel(
    tabsetPanel(  # Application title
 #
  
  # Main panel (on the right hand side)
 # mainPanel(
   # tabsetPanel(
      tabPanel("Getting started", 
               #div(class="outer", tags$head(
                 # Include our custom CSS
              #   includeCSS("styles.css")
              # ),
               h3("Basic user guide"),
               p(HTML("Welcome to this early draft version!")),
               p(HTML("<b><div style='background-color:#FADDF2;border:1px solid
                      black;width:800px'>WARNING: This application is free and comes with NO WARRANTY. USE AT YOUR OWN RISK. </div>
                      </b>")), br(),
               
               p(HTML("For more information on the method see Mark Strong's website  
                      <a href='http://www.sheffield.ac.uk/scharr/sections/ph/staff/profiles/mark' 
                      target='_blank'>here</a>")), br(),
               
               fileInput('loadSession', label = h4('Load previously saved session')),
                            
               
               p(HTML("Please add some information about your model")),
               textInput("modelName",label = h5("Name of your model"),value ="My Model"),
               #numericInput("n1",label = h5("Number of strategies compared in the model (including current/standard care)"), value = 2, min = 2),
               #textInput("current",label = h5("Name of strategy considered to be current/standard care"),value ="Current Care"),
               #textInput("t3",label = h5("Names of other strategies"),value ="Intervention 1"),#Need some way of adding more than one name to box
               #numericInput("nParam",label = h5("Number of uncertain model parameters that vary as inputs in your PSA run?"), value = 0, min = 0),
               #numericInput("nIterate",label = h5("Number of Monte Carlo iterations used in PSA"),value = 1000, min = 0, step = 100),
               #selectInput("indSim",label = h5("Is your model an individual level simulation?"), choices = list("yes","no"), selected = "no"),
               #numericInput("nPeople",label = h5("If yes, how many individuals were run per PSA sample?"),value = 0, min = 0, step = 100),
               textInput("effectDef",label = h5("Definition of effectiveness measure"),value ="Discounted Lifetime QALYs"),
               textInput("costDef",label = h5("Definition of cost measure"),value ="Discounted Lifetime Costs (£)"),
               textInput("currency",label = h5("Units used for costs"),value ="£"),
               textInput("unitBens",label = h5("Units used for benefits"),value ="QALY"),
               #numericInput("n5",label = h5("Value of lambda (the threshold value of cost that the decision maker is willing to pay for one unit of effectiveness)"), value = 20000, min = 0, step = 1000),
               #lambda set using sliders rather than here.
               textInput("jurisdiction", label = h5("Name of jurisdiction (e.g. country, region, city)"),value = "England"),
               numericInput("annualPrev", label = h5("Annual prevalence within jurisdiction (number of patients affected by the decision each year)"), value = 1000, min = 0, step = 10),
               numericInput("horizon", label = h5("Decision relevance horizon (number of years that decision between these strategies is likely to be relevant)"), value = 10, min = 1),
               numericInput("lambdaOverall", label = h5("Value of one unit of health effect (lambda)"),  value = 20000, min = 10, step = 100),
               br()
              # submitButton("Submit")
               ),
    
      
      
      tabPanel("Import files",     # Button to import parameter data
               h3("Parameter importation"),
               p(HTML("To run the application, import your data set using the 
                      import buttons below. You data must
                      be supplied on the form of a csv file. If the importation is
                      done properly, the data are displayed in the next tab")),
               fileInput('parameterFile', 'Choose CSV File',
                         accept=c('text/csv', 'text/comma-separated-values,text/plain')),
               # Various checkboxes and input fields to specify the data file format
               checkboxInput('header1', 'Is there a header row?', TRUE),
               checkboxInput('rownames1', 'Does the first column contain row names?', FALSE),
               #selectInput('sep', 'Separator:',
               #	c(Comma=',',Semicolon=';',Tab='\t', Space=' '), ','),
               #selectInput('quote', 'Quote:',
               #            c('None'='none', 'Double Quote'="\"'", 'Single Quote'="\''"),
               #             selected="\"'"),
               #selectInput('dec', 'Decimal mark', c(Dot='.', Comma=','), '.'),
               br(),
               
               
               # Button to import costs  data    
               h3("Costs importation"),
               fileInput('costsFile', 'Choose CSV File',
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
               fileInput('effectsFile', 'Choose CSV File',
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
               tableOutput("checktable1"),
               h4("Costs"),
               tableOutput("checktable2"),
               h4("Effects"),
               tableOutput("checktable3")
      ),
      
      
      tabPanel("PSA Results",
               h1("Cost-Effectiveness Plane"),
               textOutput("textCEplane1"),
               br(),
               textOutput("textCEplane2"),
               br(),
               textOutput("textCEplane3"),
               h6("Reference"),
               p("Section 5.1 in Briggs, Claxton & Sculpher. Decision Modelling for Health Economic Evaluation 
                 (Handbooks for Health Economic Evaluation). OUP Oxford; 1 edition (2006).  ISBN-13: 
                 978-0198526629"),
               br(),
               
        sidebarLayout(
              sidebarPanel(
                          sliderInput("lambda2", label = h5("Specify lambda"), 1, 100000, 20000, 1000, width="500px"),
                          #submitButton("Change"), # this button stops everything else auto-updating!
                          br(),
                          br(),
                          p(strong("Strategies Compared"),textOutput("textCEplane4")),
                          br(),
                          p(strong("Summary")),
                          textOutput("textCEplane5")
                          ),
          
                mainPanel(plotOutput("plots1", width="500px", height="500px")
                          )
                    ),

               
               h3("Table of Key Cost-Effectiveness Statistics"),
               tableOutput("tableCEplane"),
               br(),
               br(),
               h1("Cost-Effectiveness Acceptability Curve (CEAC)"),
               textOutput("textCEAC1"),
               h6("Reference"),
               p("A guide to cost-effectiveness acceptability curves. Fenwick & Byford. The British Journal of 
                 Psychiatry (2005) 187: 106-108 doi: 10.1192/bjp.187.2.106"),
               br(),
               plotOutput("plots2", width="500px", height="500px"),

        
               br(),
               br(),
               h1("Net Benefit Densities"),
                 mainPanel(plotOutput("plots5", width="700px", height="400px")),
               br(),
               h3("Expected Incremental Net Benefit versus Current Care"),
               tableOutput("tableNetBenefit")
               
               ),
      
      # Graphic
      # coming from the function output$boxplots in server.R
      tabPanel("EVPI",
               
               #h4("Figures"), ##HASHED OUT AS NOT SURE WHERE TO PUT IT
               #textInput("main",strong("Graphic title:"), "CE plane"),
               #textInput("xlab",strong("X axis label:"), "Effects"),
               #textInput("ylab",strong("Y axis label:"), "Costs"),
               #textInput("color","Color:","red"),
               #h4("Specify lambda"),
               #sliderInput('lambda2', label="", 500, 60000, 20000, 1000, width="600px"),
               #sliderInput('lambda2', label="", -6, 6, 4, 0.1),
               #plotOutput("plots4way", width="600px", height="600px"),    
               
               h3("Overall EVPI versus lambda"),
               textInput("main3", strong("Graphic title:"), "EVPI (on costs scale) vs lambda"),               
               plotOutput("plots3"),
               
               h3("The Expected Value of Removing all Current Decision Uncertainty: Overall Expected Value of Information"),
               tableOutput("tableEVPI"),               
               br(),
               
               h3("Overall EVPI versus lambda"),
               textInput("main4", strong("Graphic title:"), "EVPI (on effects scale) vs lambda"),
               plotOutput("plots4")

               
      ),
      
      tabPanel("EVPPI",
               h3("Specify lambda"),
               sliderInput('lambda', label="", 0, 60000, 20000, 1000),
               h3("Partial EVPI for single parameters"),
               tableOutput("summary"),
               br(),
               p(HTML("Here you can define subsets of parameters to add to your EVPPI. Choose a subset of parameters
                      using the tick boxes and press the Add button to add the combination to the analysis.
                      You can add as many combinations as you wish. Press the Calculate EVPPI button
                      when you have finished your selections.")),
               br(),
               sidebarLayout(
                 sidebarPanel(
                   h3("Select Parameters for EVPPI"),
                   checkboxGroupInput("pevpiParameters", NULL, 
                                      c("null"), 
                                      selected = NULL),
                   br(),
                   actionButton("addSelection", "Add selection"),
                   br(),
                   br(),
                   br(),
                   actionButton("calculateSubsetsEvpi", "Calculate EVPPI values")),
                 
                 mainPanel(
                   h3("Selected parameter combinations"),
                   br(),
                   tableOutput("selectedTable"),
                   br(),
                   tableOutput("selectedEvpiTable"),
                   br(),
                   actionButton("clearSubsetsEvpi",label="Clear Selections"))),
                                 
               br(),    
               h3("The Expected Value of Removing Current Decision Uncertainty on Particular Parameters: EVPPI"),
               tableOutput("tableEVPPI")
               
      ),
    
      # Numerical summary of the dataset,
      # coming from the function output$summary in server.R
      tabPanel("Downloads", 
               downloadButton('downloadSummary', 'Download EVPI values'),
               br(), br(),#,tableOutput("summary")
               radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                            inline = TRUE),
               downloadButton('downloadReport', 'Download report')
      ),
      
      tabPanel("Save session", 
               textInput("RdataFileName", strong("Filename"), value="SAVISession.Rdata"),
               br(), br(),
               downloadButton('saveSession', 'Save SAVI session')
     )
     , type = "pills" # this controls the look of the tabs
  ), 
 tags$style(type="text/css", ".tab-content { overflow: visible; }")
  ,width = 12 # 12 is the max width of the mainPanel page
 )
#,theme="bootstrapDefault.css"
)
