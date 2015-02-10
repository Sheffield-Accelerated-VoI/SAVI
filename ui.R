# Copyright (c) 2014, the SAVI authors (see AUTHORS.txt).
# Licensed under the BSD 3-clause license (see LICENSE.txt)

fluidPage(
  tags$head(
    tags$style("body {background-color: #F8F8F8; }")),
    tags$style(type='text/css', '#textCheckTab {color: red;}'),
    tags$style(type='text/css', '#textCheckTabParams {color: red;}'),
    tags$style(type='text/css', '#textCheckTabCosts {color: red;}'),
    tags$style(type='text/css', '#textCheckTabEffects {color: red;}'),
  
    # uni is #F1F2F2
  headerPanel(HTML("<a href='http://www.sheffield.ac.uk/' target='_blank'><img src = 'uni2.gif' width = '200' 
                   alt='University of Sheffield logo' /></a> SAVI - Sheffield Accelerated Value of Information")),
  
  mainPanel(
    h4("Release version 1.014 (2015-02-10)"),    
    h6(HTML("Copyright &copy; 2015 University of Sheffield")),  
    tabsetPanel( 

      tabPanel("Home",
          sidebarLayout(position = "right",
               sidebarPanel(
               h3("Returning Users"),
               fileInput('loadSession', label = h4('Load previously saved session')),
               br(),br(),
               h3("Sign up for SAVI news and updates"),
               p(HTML("Send a blank email to"), 
                 a("savi@sheffield.ac.uk", href='mailto:savi@sheffield.ac.uk?Subject=Register')),               
               p("We won't share your email address with anyone."),
               br(),br(),
               h3("New features and bug fixes"),
               h5("New in version 1.013"),
               p("You can now download some test files to try out on SAVI. See the Import Files tab."),           
               h5("New in version 1.009"),
               p("SAVI now accepts text files with the tab or the semicolon separator, and with the comma as the decimal mark."),              
               h5("New in version 1.005"),
               p("You can now download a report containing all the results from the SAVI analysis. Click on the Downloads tab."), 
               p("Different interventions and comparators can now be specified for the CE plane."), 
               br()
               ),
               
  
               
               mainPanel(  
                h3("What SAVI does"),
               p(HTML("Using"),strong("only"), HTML("PSA results from your model")),
              # p(HTML("(For individual simulation models you can do PSA with each PSA run on just 1 individual)")),
               p(HTML("In a matter of seconds from the SAVI online application you can generate:")),
               tags$ol(
                      tags$li("Standardised assessment of uncertainty (C-E planes and CEACs)"), 
                      tags$li("Overall EVPI per patient, per jurisdiction per year and over your decision relevance horizon"), 
                      tags$li("Expected Value of Perfect Parameter Information (EVPPI) for single and groups of parameters")
               ),
               br(),
               p(HTML("Disclaimer: This application is based on peer-reviewed statistical approximation methods.  
                      It comes with no warranty and should be utilised at the user’s own risk.
                      The <a href = 'https://github.com/Sheffield-Accelerated-VoI/SAVI' target='_blank'>underlying code</a> is made available under the 
                      <a href = 'https://github.com/Sheffield-Accelerated-VoI/SAVI/blob/master/LICENSE.txt' target = '_blank'>BSD 3-clause license</a>.")),                      
               p(HTML("For more information on the method see   
                      <a href='http://www.sheffield.ac.uk/scharr/sections/ph/staff/profiles/mark' 
                      target='_blank'>Mark Strong's website</a> or 
                      <a href = 'http://mdm.sagepub.com/content/34/3/311' target='_blank'>this paper</a>.")), br(),
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
               br(),br(),
              
               h3("If you get stuck, or if something doesn't work, please let us know"),
               p(HTML("Our email address is"), a("savi@sheffield.ac.uk", href='mailto:savi@sheffield.ac.uk')),               
               br(),br(),br())
               
               
      )),
      
#       tabPanel("User Guide",
#                h3("User Guide")
#       ),
 
      tabPanel("About your model", 

               h4("Specify details about your model here (you can change these at any time - results will automatically update)"),
               br(),
               textInput("modelName", label = h5("Name of your model"), value ="Model name goes here"),
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
               sidebarLayout(position = "right",
                             sidebarPanel(
                               h4("Dowload test files"),
                               p("Try out SAVI using these test files that we have generated from a hypothetical model. 
                                 The model has 19 uncertain parameters and two decision options."),
                               br(),
                               downloadButton('testParams', 'Download parameters.csv'),
                               br(),
                               br(),
                               downloadButton('testCosts', 'Download costs.csv'),
                               br(),
                               br(),
                               downloadButton('testEffects', 'Download effects.csv')
                             ),
                             
                             
                             
                             mainPanel(  
               p(HTML("<div style='border:1px solid
                      black;width:7;padding-left: 1em'><h4>Setting up your files for import</h4>
                      Import your PSA samples of parameters, costs and effects using the 
                      import buttons below.
                      <br>Please supply the PSA samples in the form of three csv files.
                      <br><br>
                        SAVI assumes that the first row of the parameter file contains the parameter names.<br>
                        SAVI assumes that the first row of the costs file holds the decision option names. <br>
                        The first row of the effects file should also hold names, but these names are not used by SAVI.
                      <br><br>
                        The csv files must each have the same number of rows, and the rows must correspond, i.e. 
                          the parameters in row 1 must be those that correspond to the costs and effects in row 1, and so on.
                        <br><br>
                        Costs and effects are assumed to be per-person, and to be absolute rather than incremental.<br>
                        <strong>Check the import in the next tab</strong><br><br>
                        </div>")),      
               
               br(),br(),
               h3("Parameter importation"),
               # Various checkboxes and input fields to specify the data file format
               #checkboxInput('header1', 'Is there a header row?', TRUE),
               #checkboxInput('rownames1', 'Does the first column contain row names?', FALSE),
               radioButtons('sep', h5('Separator:'),
               	c(Comma=',',Semicolon=';',Tab='\t', Space=' '), ',', inline=TRUE),
               #radioButtons('quote', 'Quote:',
                #           c('None'='none', 'Double Quote'="\"'", 'Single Quote'="\''"),
                 #           selected="\"'", inline=TRUE),
               radioButtons('dec', h5('Decimal mark'), c(Dot='.', Comma=','), '.', inline=TRUE),
               fileInput('parameterFile', 'Choose CSV File',
                         accept=c('text/csv')),
               #br(),
               h4(textOutput("textCheckTabParams")),
               
               # Button to import costs  data    
               h3("Costs importation"),      
               # Various checkboxes and input fields to specify the data file format
               #checkboxInput('header2', 'Is there a header row?', TRUE),
               #checkboxInput('rownames2', 'Does the first column contain row names?', FALSE),
               radioButtons('sep2', h5('Separator:'),
               	c(Comma=',',Semicolon=';',Tab='\t', Space=' '), ',', inline=TRUE),
               #radioButtons('quote2', 'Quote:',
               #        c(None='','Double Quote'='"','Single Quote'="'"),
                #           '"', inline=TRUE),
               radioButtons('dec2', h5('Decimal mark'), c(Dot='.', Comma=','), '.', inline=TRUE),
               fileInput('costsFile', 'Choose CSV File',
                         accept=c('text/csv')),
               #br(),
               h4(textOutput("textCheckTabCosts")),
               
               # Button to import effects data
               h3("Effects importation"),
               # Various checkboxes and input fields to specify the data file format
               #checkboxInput('header3', 'Is there a header row?', TRUE),
               #checkboxInput('rownames3', 'Does the first column contain row names?', FALSE),
               radioButtons('sep3', h5('Separator:'),
               	c(Comma=',',Semicolon=';',Tab='\t', Space=' '), ',', inline=TRUE),
               #radioButtons('quote3', 'Quote:',
                #          c(None='','Double Quote'='"','Single Quote'="'"),
                 #          '"', inline=TRUE),
               radioButtons('dec3', h5('Decimal mark'), c(Dot='.', Comma=','), '.', inline=TRUE),
               fileInput('effectsFile', 'Choose CSV File',
                         accept=c('text/csv')),
               #h4("Are uploaded costs and effects incremental or absolute?"),
               #radioButtons('incremental', label="", c("Incremental" = "TRUE", "Absolute" = "FALSE"), "FALSE")
               
               h4(textOutput("textCheckTabEffects")),
               h4(textOutput("textCheckTab"))            
               )
               )
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
               h3("Cost-Effectiveness Plane"),
               
        sidebarLayout(
              sidebarPanel(
                          #sliderInput("lambda2", label = h5("Specify lambda"), 1000, 100000, 20000, 1000, width="500px"),
                          #submitButton("Change"), # this button stops everything else auto-updating!
                          #br(),
                          #br(),
                          h5("Choose intervention"),
                          radioButtons("decisionOptionCE1", NULL, 
                          c(""), 
                          selected = NULL),
                          br(),
                        
                          h5("Choose comparator"),
                          radioButtons("decisionOptionCE0", NULL, 
                                             c(""), 
                                             selected = NULL),
                          br()
                       , width=5),
          
                mainPanel(
                  p(strong("Strategies Compared"), textOutput("textCEplane4")),
                  br(),
                  plotOutput("plots1", width="500px", height="500px"),
                  p(strong("Summary")),
                  textOutput("textCEplane5")
                  , width = 7)
                    ),
                
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
        
               h3("Table of Key Cost-Effectiveness Statistics"),
               tableOutput("tableCEplane"),
               br(),
               br(),
        
               h3("Cost-Effectiveness Acceptability Curve (CEAC)"),
               textOutput("textCEAC1"),
               h6("Reference"),
               p("A guide to cost-effectiveness acceptability curves. Fenwick & Byford. The British Journal of 
                 Psychiatry (2005) 187: 106-108 doi: 10.1192/bjp.187.2.106"),
               br(),
        
               plotOutput("plots2", width="500px", height="500px"),    
               br(),
        
               h3("Net Benefit"),
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
        
#                p("If the credible intervals are small relative to the size of the bars and one decision stands out as being highest in terms 
#                of expected incremental net benefit then the decision maker has an easy choice and there is little decision uncertainty."),
#                br(),
        
               p("If there are strategies with  
               credible intervals for incremental net benefit that include zero, then there is decision uncertainty. 
               Whether it is valuable to consider further research to reduce uncertainty is the subject of the analyses using expected value 
               of information calculations. These calculations can consider decision uncertainty arising from all uncertain parameters together (the overall expected value of perfect 
               information – overall EVPI) or for particular sets of uncertain parameters (the expected value of perfect parameter 
               information – EVPPI)."),
               br()#,
        
               #h3("Summary of Incremental Net Benefit Statistics")
               #tableOutput("tableNetBenefitInc") Not yet made

               ),
      
      # Graphic
      # coming from the function output$boxplots in server.R
      tabPanel("EVPI",
               h3("Overall Expected Value of Perfect Information"),
               #p("The Table below quantifies the expected value to decision makers of removing all 
                # current decision uncertainty."),  
              #   This will enable comparison against previous analyses to provide an idea of the scale of decision uncertainty in this
               #   topic compared with previous decisions."),          
               
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
               

               h4("Overall EVPI"),
                h6("The Expected Value of Removing all Current Decision Uncertainty"),
               tableOutput("tableEVPI"),               
               br(),
               
               h4("Overall EVPI (on costs scale) versus lambda"),               
               plotOutput("plots3", width="500px", height="500px"),

               h4("Overall EVPI (on effects scale) versus lambda"),
               plotOutput("plots4", width="500px", height="500px"),
               br(),
               
               h4("Overall EVPI per population and time horizon"),
               plotOutput("plots6", width="700px", height="600px"),
               
               h4("Understanding the EVPI: a non-technical explanation"),
               br(),
               
               p("The calculation begins with the existing confidence intervals (or credible intervals) for the model parameters as used 
                 in the probabilistic sensitivity analysis.  We then imagine a world in which we become absolutely (perfectly) certain 
                 about all of the model parameters i.e. the confidence interval for every single parameter is shrunk right down to zero.  
                 The decision maker would then be absolutely certain which strategy to select and would choose the one with highest net 
                 benefit.  One can visualise this idea by imagining that instead of seeing the cloud of dots on the cost-effectiveness plane 
                 (representing current uncertainty in costs and benefits) and having to choose, the decision maker now knows exactly which 
                 ‘dot’ is the true value (because all of the uncertainty is removed) and so can be certain to choose the strategy which 
                 gives the best net benefit. In a two strategy comparison of new versus current care, if the ‘true dot’ turns out to be 
                 below and to the right of the threshold lambda line, then the decision maker would select the new strategy.  If the ‘true dot’ 
                 is above and to the left, then current care would be selected.  Under the current uncertainty, the decision maker will 
                 choose the strategy based on the expected costs and benefits (essentially on whether the ‘centre of gravity’ of the cloud 
                 is above or below the threshold line)."),
               br()
               
      ),

      tabPanel("EVPPI single parameters",
               #h3("Specify lambda"),
               #sliderInput('lambdaSingleEvppiTab', label="", 0, 60000, 20000, 1000),
               h3("Partial EVPI for each parameter separately"),
               p("Partial EVPI enables identification of those parameters that contribute particularly
                 highly to decision uncertainty. For each parameter, the expected value of removing
                 current uncertainty is displayed in the table below. The barplot shows parameters in 
                 descending order of importance."),
               br(),
               
               h4("EVPPI for individual parameters"),
               tableOutput("tableEVPPI"),
               
               #h3("Partial EVPI for single parameters"),
               #tableOutput("summary"),
               
               h3("Partial EVPI barplot for single parameters"),
               plotOutput("plot7", width="500px", height="500px")
               
      ),
      
      tabPanel("EVPPI groups",
               h3("Partial EVPI for groups of parameters"),
               p(HTML("Although EVPPI information about individual parameters is useful, often it is more informative if EVPPI can
                 be performed on groups of associated parameters e.g. all parameters associated with efficacy data. This will
                 enable a maximum value to be put on further research to jointly inform this set of parameters. Previously, such
                 calculations have been very computationally intensive. Our method will allow you to calculate EVPPI for different
                 parameter groups in a matter of seconds.")),
               p(HTML("First, define groups of parameters for which to calculate EVPPI. Choose a subset of parameters
                      using the tick boxes and press the Calculate EVPPI button.")),
               p(HTML("When calculation of the first parameter group is complete, select a new subset (remember to untick
                      your original choices) and press the Calculate EVPPI button again. This can be repeated as many times 
                      as you wish, with all results appearing below on an expanding results table.")),
               p(HTML("For subsets with up to five parameters, the GAM regression method is used. 
                      For subsets with five or more parameters the GP regression method is used. 
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

              h3("Download summary report"),
              p("This document contains all the tables and figures generated from the SAVI analysis of your PSA."),
              radioButtons('format', 'Please select the document format you require', c('PDF', 'HTML', 'Word'),
                            inline = TRUE),
               downloadButton('downloadReport', 'Download summary report'),
              br(), br(), 
              p("NB generating the document can take some time."),
              br(),           br(),  
              h3("Download results as a csv file"),
              p("At present this file contains only the partial EVPI values for single parameters."),
              downloadButton('downloadSummary', 'Download EVPI values')
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
                  <em>Medical Decision Making.</em> 2014;<b>34(3)</b>:311-26. Available open access 
                      <a href='http://mdm.sagepub.com/content/34/3/311' target='_blank'>here.</a></div>")),
               br(),h3("Contact us"), 
               p(HTML("Please email us at <a href='mailto:savi@sheffield.ac.uk?Subject=SAVI%20query' target='_top'>
                savi@sheffield.ac.uk</a>")),
               p("Please tell us about any bugs!"),
               br(),
               h3("Acknowledgements"), 
               p(HTML("The method for partial EVPI computation that is implemented in this web application arose from independent research 
                      supported by the National Institute for Health Research (Mark Strong, 
                      postdoctoral fellowship PDF-2012-05-258). 
                      The views expressed in this publication are those 
                      of the authors and not necessarily those of the National Health Service, 
                      the National Institute for Health Research, or the Department of Health. ")),
               
               br(),
               p(HTML("This website complies with The University of Sheffield's <a href='http://www.sheffield.ac.uk/privacy' target='_blank'>Privacy
                 Policy</a>"))
     )
   
     ,  type = "pills" # this controls the look of the tabs
  ), 
 tags$style(type="text/css", ".tab-content { overflow: visible; }", "footer {background-color: #F8F8F8;
            width: 100%;
            bottom: 0;
            position: relative; }")
  , width = 12 # 12 is the max width of the mainPanel page
 )
#,theme="bootstrap.css"
,title="SAVI - Sheffield Accelerated Value of Information")
#can add id="tabId", before type = "pills"
