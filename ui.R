
fluidPage(
  headerPanel("SAVI - Sheffield Accelerated Value of Information"),
  
#   tags$head(
#     tags$style(HTML("
#       @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
#       
#       h1 {
#         font-family: 'Lobster', cursive;
#         font-weight: 500;
#         line-height: 1.1;
#         color: #48ca3b;
#       } 
# 
#       h2 {
#         font-family: 'Lobster', cursive;
#         font-weight: 500;
#         line-height: 1.1;
#         color: #D8F781;
#       }
# 
#       h3 {
#         font-family: 'Lobster', cursive;
#         font-weight: 500;
#         line-height: 1.1;
#         color: #FF8000;
#       }
# 
#       h4 {
#         font-family: 'Lobster', cursive;
#         font-weight: 500;
#         line-height: 1.1;
#         color: #2E9AFE;
#       }
# 
#     "))
#   ),
  
  
  mainPanel(
    h4("Release version 1.002 (2014-11-8)"),
    tabsetPanel(  # Application title
 #
  
  # Main panel (on the right hand side)
 # mainPanel(
   # tabsetPanel(
      tabPanel("Home",
          sidebarLayout(position = "right",
               sidebarPanel(
               h3("Returning Users"),
               fileInput('loadSession', label = h4('Load previously saved session'))
               ),
               mainPanel(
     

               p(HTML("Using"),strong("only"), HTML("PSA results from your model")),
              # p(HTML("(For individual simulation models you can do PSA with each PSA run on just 1 individual)")),
               p(HTML("In a matter of seconds from the SAVI online application you can receive:")),
               tags$ol(
                      tags$li("Standardised assessment of uncertainty (C-E planes and CEACs)"), 
                      tags$li("Overall EVPI per patient, per jurisdiction per year and over your decision relevance horizon"), 
                      tags$li("Expected Value of Perfect Parameter Information (EVPPI) for single and groups of parameters")
               ),
               br(),
               p(HTML("Disclaimer: This application is made available to all users free of charge.  
                      The application is based on peer-reviewed statistical approximation methods.  
                      It comes with no warranty and should be utilised at the user’s own risk.")),                      
               p(HTML("For more information on the method see Mark Strong's website  
                      <a href='http://www.sheffield.ac.uk/scharr/sections/ph/staff/profiles/mark' 
                      target='_blank'>here</a>")), br(),
               p(HTML("The SAVI process has 4 steps (using the TABS from left to right)")),
               p(HTML("Step 1: Save PSA inputs and outputs as .csv files")),
               img(src = "step1_excel.png", height = 300, width = 300),
               br(),
               br(),
               p(HTML("Step 2: Input details about your model, import data, and check data")),
               img(src = "step2_modelsetup.png", height = 300, width = 900),
               br(),
               br(),
               p(HTML("Step 3: View your uncertainty analysis")),
               img(src = "step5_viewVOI.png", height = 300, width = 300),
               br(),
               br(),
               p(HTML("Step 4: After you have viewed the VoI tabs download your results in PDF, HTML or word file")),
               img(src = "step6_download.png", height = 300, width = 300),
               br(),
               p(HTML("To register please email"), a("savi@sheffield.ac.uk",href="mailto:savi@sheffield.ac.uk"), 
                      HTML("with an email subject Register"))
               )
               
               
      )),
 
      tabPanel("About your model", 

               h4("Specify details about your model here"),
               br(),
               textInput("modelName", label = h5("Name of your model"), value ="My Model"),
               #checkboxInput("indSim", label = h5("Is your model an individual level simulation with a single patient per PSA run?"), FALSE),
               #textInput("current", label = h5("Name of strategy considered to be current/standard care"), value ="Current Care"),
               #textInput("t3", label = h5("Names of other strategies"), value ="Intervention 1"),#Need some way of adding more than one name to box
               numericInput("lambdaOverall", label = h5("Threshold value of one unit of health effect (lambda)"), value = 20000, min = 0, step = 1000),
               textInput("effectDef", label = h5("Definition of effectiveness measure"), value ="Discounted Lifetime QALYs"),
               textInput("costDef", label = h5("Definition of cost measure"), value ="Discounted Lifetime Costs (£)"),
               numericInput("annualPrev", label = h5("Annual prevalence within jurisdiction (number of patients affected by the decision each year)"), value = 1000, min = 0, step = 10),
               numericInput("horizon", label = h5("Decision relevance horizon (number of years that decision between these strategies is likely to be relevant)"), value = 10, min = 1),
               #numericInput("n1",label = h5("Number of strategies compared in the model (including current/standard care)"), value = 2, min = 2),
               #numericInput("nPeople",label = h5("If yes, how many individuals were run per PSA sample?"),value = 0, min = 0, step = 100),
               textInput("currency", label = h5("Units used for costs"), value ="£"),
               textInput("unitBens", label = h5("Units used for benefits"), value ="QALY"),
               textInput("jurisdiction", label = h5("Name of jurisdiction (e.g. country, region, city)"), value = "England"),

               br()
               ),
    
      
      
      tabPanel("Import files",     # Button to import parameter data         
               
               p(HTML("<div style='border:1px solid
                      black;width:800px;padding-left: 1em'><h4>Setting up your files for import</h4>
                      Import your PSA samples of parameters, costs and effects using the 
                      import buttons below.
                      <br>Please supply the PSA samples in the form of three csv files.
                      <br><br>
                        SAVI assumes that the first row of the parameter file contains the parameter names.<br>
                        SAVI assumes that the first row of the costs file and the first row of the effects file
                        both contain the decision option names.
                      <br><br>
                        Costs and effects are assumed to be absolute rather than incremental.<br>
                        Check the import in the next tab<br><br>
                        </div>")),      
               
               br(),br(),
               h4("Parameter importation"),
               fileInput('parameterFile', 'Choose CSV File',
                         accept=c('text/csv', 'text/comma-separated-values,text/plain')),
               # Various checkboxes and input fields to specify the data file format
               #checkboxInput('header1', 'Is there a header row?', TRUE),
               #checkboxInput('rownames1', 'Does the first column contain row names?', FALSE),
               #selectInput('sep', 'Separator:',
               #	c(Comma=',',Semicolon=';',Tab='\t', Space=' '), ','),
               #selectInput('quote', 'Quote:',
               #            c('None'='none', 'Double Quote'="\"'", 'Single Quote'="\''"),
               #             selected="\"'"),
               #selectInput('dec', 'Decimal mark', c(Dot='.', Comma=','), '.'),
               #br(),
               
               
               # Button to import costs  data    
               h4("Costs importation"),
               fileInput('costsFile', 'Choose CSV File',
                         accept=c('text/csv', 'text/comma-separated-values,text/plain')),
               # Various checkboxes and input fields to specify the data file format
               #checkboxInput('header2', 'Is there a header row?', TRUE),
               #checkboxInput('rownames2', 'Does the first column contain row names?', FALSE),
               #selectInput('sep2', 'Separator:',
               #	c(Comma=',',Semicolon=';',Tab='\t', Space=' '), ','),
               #selectInput('quote2', 'Quote:',
               #       c(None='','Double Quote'='"','Single Quote'="'"),
               #            '"'),
               #selectInput('dec2', 'Decimal mark', c(Dot='.', Comma=','), '.'),
               #br(),
               
               # Button to import effects data
               h4("Effects importation"),
               fileInput('effectsFile', 'Choose CSV File',
                         accept=c('text/csv', 'text/comma-separated-values,text/plain'))
               # Various checkboxes and input fields to specify the data file format
               #checkboxInput('header3', 'Is there a header row?', TRUE),
               #checkboxInput('rownames3', 'Does the first column contain row names?', FALSE),
               #selectInput('sep3', 'Separator:',
               #	c(Comma=',',Semicolon=';',Tab='\t', Space=' '), ','),
               #selectInput('quote3', 'Quote:',
               #           c(None='','Double Quote'='"','Single Quote'="'"),
               #            '"'),
               #selectInput('dec3', 'Decimal mark', c(Dot='.', Comma=','), '.')
               
               #h4("Are uploaded costs and effects incremental or absolute?"),
               #radioButtons('incremental', label="", c("Incremental" = "TRUE", "Absolute" = "FALSE"), "FALSE")
               ),
      
 
 
      tabPanel("Check upload",
               
               h3("Check data import"),
               h4("The parameters, costs and effects you have uploaded are displayed below:"),
               p("(only the first 5 first rows of each dataset are shown)"),br(),
               
               h4("Parameters"),
               tableOutput("checktable1"),
               h4("Costs"),
               tableOutput("checktable2"),
               h4("Effects"),
               tableOutput("checktable3")
      ),
      
      
      tabPanel("PSA Results",
               h1("Cost-Effectiveness Plane"),
               p(HTML("<div id='textCEplane1' class='shiny-text-output'></div>")),
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
                          #sliderInput("lambda2", label = h5("Specify lambda"), 1000, 100000, 20000, 1000, width="500px"),
                          #submitButton("Change"), # this button stops everything else auto-updating!
                          #br(),
                          #br(),
                          p(strong("Strategies Compared"), textOutput("textCEplane4")),
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
        
               h1("Net Benefit"),
               textOutput("textNB1"),
               br(),
        
               textOutput("textNB2"),
               br(),
        
               p("This is particularly useful when comparing several strategies because the analyst and decision maker can 
               see in one single measure the expected net value of each strategy, rather than looking at many comparisons of 
               incremental cost-effectiveness ratios between different options.  Under the rules of decision theory, the strategy 
               with the highest expected net benefit is the one which a decision maker would choose as the optimal strategy."),
               br(),
        
               h3("Summary of Absolute Net Benefit Statistics"),
               tableOutput("tableNetBenefit"),
               br(),
        
               textOutput("textNB3"),
               br(),
        
               plotOutput("plots5a", width="500px", height="500px"),
               br(),

               h1("Net Benefit Densities"),
               plotOutput("plots5", width="700px", height="400px"),
               br(),
        
               p("Analysis of the expected incremental net benefit helps to visualise whether particular strategies are better than others 
               and how certain a decision maker can be about the differences."),
               br(),
        
               p("If the credible intervals are small relative to the size of the bars and one decision stands out as being highest in terms 
               of expected incremental net benefit then the decision maker has an easy choice and there is little decision uncertainty."),
               br(),
        
               p("If the credible intervals are large compared to the size of the bars and there are several strategies with ‘overlapping’ 
               credible intervals, then several strategies are close in terms of their expected value to a decision maker, and given the 
               relatively large decision uncertainty, it might be valuable to consider further research to reduce uncertainty. The value of 
               reducing uncertainty to the decision maker by undertaking further research is the subject of the analyses using expected value 
               of information calculations. These calculations can consider all decision uncertainty (the overall expected value of perfect 
               information – overall EVPI) or for particular uncertain parameters within the PSA (expected value of perfect parameter 
               information – EVPPI)."),
               br()#,
        
               #h3("Summary of Incremental Net Benefit Statistics")
               #tableOutput("tableNetBenefitInc") Not yet made

               ),
      
      # Graphic
      # coming from the function output$boxplots in server.R
      tabPanel("EVPI",
                
               p("The Table below quantifies the expected value to decision makers of removing all current decision uncertainty.  
                 This will enable comparison against previous analyses to provide an idea of the scale of decision uncertainty in this
                  topic compared with previous decisions."),
               br(),
               
               h6("Understanding the EVPI – providing a non technical intuition for the meaning"),
               br(),
               
               p("The calculation begins with the existing confidence intervals (or credible intervals) for the model parameters as used 
                 in the probabilistic sensitivity analysis.  We then imagine a world in which we become absolutely (perfectly) certain 
                 about all of the model parameters i.e. the confidence interval for every single parameter is ‘shrunk right down to zero.’  
                 The decision maker would then be absolutely certain which strategy to select and would choose the one with highest net 
                 benefit.  One can visualise this idea by imagining that instead of seeing the cloud of dots on the cost-effectiveness plane 
                 (representing current uncertainty in costs and benefits) and having to choose, the decision maker now knows exactly which 
                 ‘dot’ is the true value (because all of the uncertainty is removed) and so can be certain to choose the strategy which 
                 gives the best net benefit. In a two strategy comparison of new versus current care, if the ‘true dot’ turns out to be 
                 below and to the right of the threshold lambda line, then the decision maker would select the new strategy.  If the ‘true dot’ 
                 is above and to the left, then current care would be selected.  Under the current uncertainty, the decision maker will 
                 choose the strategy based on the expected costs and benefits (essentially on whether the ‘centre of gravity’ of the cloud 
                 is above or below the threshold line)."),
               br(),
               
               textOutput("textEVPI1"),
               br(),
               
               textOutput("textEVPI2"),
               br(),
               
               textOutput("textEVPI3"),
               br(),
               
               textOutput("textEVPI4"),
               br(),
               
               textOutput("textEVPI5"),
               br(),
               
               h3("The Expected Value of Removing all Current Decision Uncertainty: Overall Expected Value of Information"),
               tableOutput("tableEVPI"),               
               br(),
               
               h3("Overall EVPI (on costs scale) versus lambda"),               
               plotOutput("plots3", width="500px", height="500px"),

               h3("Overall EVPI (on effects scale) versus lambda"),
               plotOutput("plots4", width="500px", height="500px"),
               br(),
               
               h3("Overall EVPI per population and time horizon"),
               plotOutput("plots6", width="700px", height="600px")        
               
      ),

      tabPanel("EVPPI single parameters",
               #h3("Specify lambda"),
               #sliderInput('lambdaSingleEvppiTab', label="", 0, 60000, 20000, 1000),
               h3("Partial EVPI for each parameter separately"),
               h4("This is the expected value of removing current uncertainty about each parameter."),
               tableOutput("tableEVPPI"),
               
               #h3("Partial EVPI for single parameters"),
               #tableOutput("summary"),
               
               h3("Partial EVPI barplot for single parameters"),
               plotOutput("plot7", width="500px", height="500px")
               
      ),
      
      tabPanel("EVPPI groups",
               p(HTML("Here you can define subsets of parameters for which to calculate partial EVPI. Choose a subset of parameters
                      using the tick boxes and press the Calculate EVPPI button.")),
               p(HTML("For subsets with up to five parameters, the GAM regression method is used. 
                      For subsets with five or more parameters the GP regresison method is used. 
                      See <a href='http://mdm.sagepub.com/content/34/3/311' target='_blank'>this paper</a> for details.")),               
               br(),
               sidebarLayout(
                 sidebarPanel(
                   h3("Select Parameters for EVPPI"),
                   checkboxGroupInput("pevpiParameters", NULL, 
                                      c(""), 
                                      selected = NULL),
                   br(),
#                    actionButton("addSelection", "Add selection"),
#                    br(),
#                    br(),
                   actionButton("calculateSubsetsEvpi", "Calculate EVPPI values"), width=5),
                 
                 mainPanel(
                   br(),
                   h3("Results"),
                   br(),
                   tableOutput("selectedTable"),
                  # br(),
                   #h4("Partial EVPI values"),
                   #tableOutput("selectedEvpiTable"),
                #   br(),
                #   actionButton("clearSubsetsEvpi", "Clear Selections"),
                width=7))     
      ),
    
      # Numerical summary of the dataset,
      # coming from the function output$summary in server.R
      tabPanel("Downloads", 
               downloadButton('downloadSummary', 'Download EVPI values')#,
#               br(), br(),#,tableOutput("summary")
#                radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
#                             inline = TRUE),
#                downloadButton('downloadReport', 'Download report')
      ),
      
      tabPanel("Save session", 
               textInput("RdataFileName", strong("Filename"), value="SAVISession.Rdata"),
               br(), br(),
               downloadButton('saveSession', 'Save SAVI session')
     ),

      tabPanel("About us", 
               h3("About us"),
               p(HTML("This web tool is an <a href = 'http://shiny.rstudio.com/' target='_blank'>R Shiny Server application</a>.")),
               p(HTML("It was written at the University of Sheffield's <a href = 'http://www.sheffield.ac.uk/scharr' target='_blank'>School of Health and Related Research</a> by 
                      <a href='https://www.shef.ac.uk/scharr/sections/ph/staff/profiles/mark' target='_blank'>Mark Strong</a>, 
                      <a href='https://www.sheffield.ac.uk/scharr/sections/heds/staff/watson_p' target='_blank'>Penny Breeze</a>, 
                      <a href='https://www.sheffield.ac.uk/scharr/sections/heds/staff/thomas_c' target='_blank'>Chloe Thomas</a> and 
                      <a href='https://www.sheffield.ac.uk/scharr/sections/heds/staff/brennan_a' target='_blank'>Alan Brennan</a>.")), 
                p(HTML("The regression-based method for approximating partial EVPI was developed by 
                 <a href='https://www.shef.ac.uk/scharr/sections/ph/staff/profiles/mark' target='_blank'>Mark Strong</a>
                      in collaboration with 
                      <a href='http://www.jeremy-oakley.staff.shef.ac.uk/' target='_blank'>Jeremy Oakley</a>
                      and <a href='https://www.sheffield.ac.uk/scharr/sections/heds/staff/brennan_a' target='_blank'>Alan Brennan</a>.")),
               p(HTML("The source code is available on GitHub at 
                <a href = 'https://github.com/Sheffield-Accelerated-VoI/SAVI' target='_blank'>https://github.com/Sheffield-Accelerated-VoI/SAVI.</a>")),
               p(HTML("Please cite the method as")),
               p(HTML("<div style='border:1px solid
                      black;width:800px;padding-left: 1em'>Strong M, Oakley JE, Brennan A. 
                  Estimating multi-parameter partial Expected Value of 
                  Perfect Information from a probabilistic sensitivity analysis sample: 
                  a non-parametric regression approach. 
                  <em>Medical Decision Making.</em> 2014;<b>34(3)</b>:311-26. Available open access <a href='http://mdm.sagepub.com/content/34/3/311' target='_blank'>here.</a></div>")),
               h3("Contact"), 
               p(HTML("Please email us at <a href='mailto:savi@sheffield.ac.uk?Subject=SAVI%20query' target='_top'>
                savi@sheffield.ac.uk</a>"))
     )
   
     , type = "pills" # this controls the look of the tabs
  ), 
 tags$style(type="text/css", ".tab-content { overflow: visible; }")
  ,width = 12 # 12 is the max width of the mainPanel page
 )
#,theme="bootstrap.css"
)
